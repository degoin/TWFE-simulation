# set path for when CR runs on the server (comment these 2 lines out otherwise)
riddellPath <- c( "/data/riddell//R/x86_64-pc-linux-gnu-library/4.1/" , .libPaths() )
.libPaths(riddellPath)

library(readxl)
library(tidyverse)
library(did)
library(sandwich)
library(ggrepel)
library(lubridate)
library(forecast)
library(staggered)

set.seed(461)
#source in helpfer functions used in Ben-Micheal approach
source("helper_func_ed.R")
source("helper_func_ed_sum.R")
source("helper_func_ed_sum_hte.R")
source("helper_func_ed_sum_C.R")

# combine all years of state-month PTB rates into one data frame
df_ls <- list()
for(i in 2011:2016) {
  df_ls[[i - 2010]] <- read.csv(paste0("./data/simulation-data/after-exclusions/state-month-ptb-2-",i,".csv"))
  if (i >2015) {
    df_ls[[i-2010]] <- df_ls[[i-2010]] %>% rename(MRSTATEPSTL = MRSTATE) 
  }
}

df_ptb <- do.call(rbind, df_ls)


# merge on Medicaid expansion dates 
df_e <- read_xlsx("./data/ACA-expansion.xlsx")
states <- read.csv("./data/state-abbrev.csv")
states <- states %>% rename(State = Name, MRSTATEPSTL = Postal.Code)
df_e <- left_join(df_e, states, by = "State")


df_ptb <- left_join(df_ptb, df_e, by="MRSTATEPSTL")

# define month 
df_ptb <- df_ptb %>% mutate(month = as.POSIXct(paste0(year,"-",DOB_MM,"-01"), format="%Y-%m-%d"))

num_months <- 12 * (2016 - 2011 + 1)

# Alaska, Massachusetts, Minnesota, Mississippi, and Virginia have fewer observations because they implemented revised birth certificate later
# don't want to include this type of heterogeneity at this point, so add more data for them 
#ggplot(df_ptb %>% filter(state_name %in% c("Alaska", "Massachusetts","Minnesota", "Mississippi","Virginia"))) + geom_line(aes(x=month, y=ptb_prop, color=factor(state_name)))

d1 <- data.frame(MRSTATEPSTL = "AK", DOB_MM = NA, ptb_prop = NA, ptb_prop2 = NA, year = NA, 
                 state_name="Alaska", 
                 Census_Region = "West", State="Alaska", Revised_bcert_year=2013, Expanded_Medicaid=1, 
                 Expansion_Date = as.POSIXct("2015-09-01"), Year_prior_expansion= NA, FPL_early_expansion = NA, 
                 Section_1115_waiver = 0, Expansion_type= "Full", Notes_births = NA, Notes = NA, FIPS = 2, 
                 month = seq(as.POSIXct("2011-01-01", format="%Y-%m-%d"), by = "month", length.out = num_months)) 
d1$year <- as.numeric(substr(as.character(d1$month),1,4))
m_add1 <- d1[!d1$month %in% df_ptb$month[df_ptb$state_name=="Alaska"],]


d2 <- data.frame(MRSTATEPSTL = "MA", DOB_MM = NA, ptb_prop = NA, ptb_prop2 = NA, year = NA, 
                 state_name="Massachusetts", 
                 Census_Region = "Northeast", State="Massachusetts", Revised_bcert_year=2011, Expanded_Medicaid=1, 
                 Expansion_Date = as.POSIXct("2014-01-01"), Year_prior_expansion= as.POSIXct("2006-04-12"), FPL_early_expansion = 150, 
                 Section_1115_waiver = 0, Expansion_type= "Mild", Notes_births = NA, Notes = "Romneycare expanded insurance to low income adults in 2006 (free if < 150%, subsidized for < 300% FPL)", 
                 FIPS = 25, 
                 month = seq(as.POSIXct("2011-01-01", format="%Y-%m-%d"), by = "month", length.out = num_months)) 
d2$year <- as.numeric(substr(as.character(d2$month),1,4))
m_add2 <- d2[!d2$month %in% df_ptb$month[df_ptb$state_name=="Massachusetts"],]

d3 <- data.frame(MRSTATEPSTL = "MN", DOB_MM = NA, ptb_prop = NA, ptb_prop2 = NA, year = NA, 
                 state_name="Minnesota", 
                 Census_Region = "Midwest", State="Minnesota", Revised_bcert_year=2011, Expanded_Medicaid=1, 
                 Expansion_Date = as.POSIXct("2014-01-01"), Year_prior_expansion= as.POSIXct("2010-03-01"), FPL_early_expansion = 75, 
                 Section_1115_waiver = 0, Expansion_type= "Substantial", Notes_births = NA, Notes = "Early expansion in 2010 with Medicaid for incomes < 75% FPL and MinnesotaCare for 75-200%", 
                 FIPS = 27, 
                 month = seq(as.POSIXct("2011-01-01", format="%Y-%m-%d"), by = "month", length.out = num_months)) 
d3$year <- as.numeric(substr(as.character(d3$month),1,4))
m_add3 <- d3[!d3$month %in% df_ptb$month[df_ptb$state_name=="Minnesota"],]


d4 <- data.frame(MRSTATEPSTL = "MS", DOB_MM = NA, ptb_prop = NA, ptb_prop2 = NA, year = NA, 
                 state_name="Mississippi", 
                 Census_Region = "South", State="Mississippi", Revised_bcert_year=2013, Expanded_Medicaid=0, 
                 Expansion_Date = NA, Year_prior_expansion= NA, FPL_early_expansion = NA, 
                 Section_1115_waiver = 0, Expansion_type= NA, Notes_births = NA, Notes = NA, 
                 FIPS = 28, 
                 month = seq(as.POSIXct("2011-01-01", format="%Y-%m-%d"), by = "month", length.out = num_months)) 
d4$year <- as.numeric(substr(as.character(d4$month),1,4))
m_add4 <- d4[!d4$month %in% df_ptb$month[df_ptb$state_name=="Mississippi"],]


d5 <- data.frame(MRSTATEPSTL = "VA", DOB_MM = NA, ptb_prop = NA, ptb_prop2 = NA, year = NA, 
                 state_name="Virginia", 
                 Census_Region = "South", State="Virginia", Revised_bcert_year=2012, Expanded_Medicaid=1, 
                 Expansion_Date = as.POSIXct("2019-01-01"), Year_prior_expansion= NA, FPL_early_expansion = NA, 
                 Section_1115_waiver = 0, Expansion_type= NA, Notes_births = NA, Notes = NA, 
                 FIPS = 51, 
                 month = seq(as.POSIXct("2011-01-01", format="%Y-%m-%d"), by = "month", length.out = num_months)) 
d5$year <- as.numeric(substr(as.character(d5$month),1,4))
m_add5 <- d5[!d5$month %in% df_ptb$month[df_ptb$state_name=="Virginia"],]


dat <- data.frame(rbind(df_ptb %>% dplyr::select(-n), m_add1, m_add2, m_add3, m_add4, m_add5))
dat <- dat %>% arrange(State, month)


# create month indicators from beginning to end of study 
dat <- dat %>% arrange(FIPS, month)
dat <- dat %>% group_by(FIPS) %>% mutate(month_ind = 1:num_months)

# 21 sates expanded Medicaid -- for those that expanded, draw expansion date uniformly in post-period  

# create treatment indicator by month by randomly sampling post-exposure month 
dat <- dat %>% group_by(FIPS) %>%  mutate(first_A = ifelse(Expanded_Medicaid==1, sample(c(37,45,53,61), size=1), NA)) 

dat <- dat %>% mutate(A = as.numeric(month_ind>=first_A)) 
dat$A[is.na(dat$A)] <- 0

# simulate future ptb rates for another 72 months 


add_ptb_yrs <- function(state) {
  f1 <- auto.arima(y = dat %>% ungroup() %>% filter(State==state) %>% arrange(month_ind) %>% select(ptb_prop2), 
                   lambda = "auto")
  
  coef <- coef(f1)
  try(rm(ar))
  if (is.na(coef["ar1"])) ar <- NULL 
  if (!is.na(coef["ar1"])) ar <- coef["ar1"] 
  if (!is.na(coef["ar2"])) ar <- c(ar,coef["ar2"]) 
  if (!is.na(coef["ar3"])) ar <- c(ar,coef["ar3"]) 
  if (!is.na(coef["ar4"])) ar <- c(ar,coef["ar4"]) 
  if (!is.na(coef["ar5"])) ar <- c(ar,coef["ar5"]) 
  
  try(rm(ma)) 
  if (is.na(coef["ma1"])) ma <- NULL 
  if (!is.na(coef["ma1"])) ma <- coef["ma1"] 
  if (!is.na(coef["ma2"])) ma <- c(ma,coef["ma2"]) 
  if (!is.na(coef["ma3"])) ma <- c(ma,coef["ma3"]) 
  if (!is.na(coef["ma4"])) ma <- c(ma,coef["ma4"]) 
  if (!is.na(coef["ma5"])) ma <- c(ma,coef["ma5"]) 
  
  
  diff <- f1$arma[length(f1$arma)-1] 
  ar_order <- ifelse(is.null(ar), 0, length(ar))
  ma_order <- ifelse(is.null(ma), 0, length(ma))
  intercept <- ifelse(is.na(coef["intercept"]), 0,  coef["intercept"])
  
  # simulate from model, using the SD of the original series to make sure it's on the correct scale 
  f2 <- arima.sim(n=72, model=list(order=c(ar_order, diff, ma_order), ar=ar, ma=ma), sd=sqrt(var(dat$ptb_prop2[dat$State==state], na.rm=T)))
  
  # add mean back to create ptb rates for 72 more months 
  if(diff==1) {
    df_t <- dat %>% filter(State==state)  %>% mutate(ptb_prop2 = as.numeric(f2[-1] + mean(dat$ptb_prop2[dat$State==state], na.rm=T)), 
                                                     year = c(rep(2017,12), rep(2018, 12), rep(2019, 12),rep(2020, 12), rep(2021, 12), rep(2022,12)), 
                                                     month_ind = 73:144, A = max(A),
                                                     month = seq(as.POSIXct("2017-01-01", format="%Y-%m-%d"), by = "month", length.out = num_months)) %>% 
      mutate(ptb_prop = ptb_prop2/100)
  }
  if(diff==0) {
    df_t <- dat %>% filter(State==state)  %>% mutate(ptb_prop2 = as.numeric(f2 + mean(dat$ptb_prop2[dat$State==state], na.rm=T)), 
                                                     year = c(rep(2017,12), rep(2018, 12), rep(2019, 12),rep(2020, 12), rep(2021, 12), rep(2022,12)), 
                                                     month_ind = 73:144, A=max(A),
                                                     month = seq(as.POSIXct("2017-01-01", format="%Y-%m-%d"), by = "month", length.out = num_months)) %>% 
      
      mutate(ptb_prop = ptb_prop2/100)
  }
  
  #ggplot(dat %>% filter(State==state)) + geom_line(aes(x=month_ind, y=ptb_prop2)) +
  #  geom_line(aes(x=month_ind, y=f1$fitted), col="blue") + geom_line(data=df_t, aes(x=month_ind, y=ptb_prop2))
  
  return(df_t)
}

states <- unique(dat$State)
ls_ptb_add <- lapply(1:length(states), function(x) add_ptb_yrs(states[x]))
df_ptb_add <- do.call(rbind, ls_ptb_add)

dat <- data.frame(rbind(dat, df_ptb_add)) %>% arrange(State, month)

# update number of months to reflect additional follow up time
num_months <- 144


##############################################################################################################################
# SIMULATION 
##############################################################################################################################


# iteration = number of iterations of the simulation
# dat = data frame being used for the simulation
# CTE = continuous treatment effect, which we set to -0.02
# HTE = vector of heterogeneous treatment effects, which we set to -0.02 and -0.01
# DTE = vector of dynamic effects, which we set to -0.01, -0.015, and -0.02

sim_rep <- function(iteration, dat, CTE, HTE, DTE) {
  print(iteration)
  print("-----BEGINNING OF THE ITERATION-----")
  # create state-level intercept and noise for each state-month 
  dat <- dat %>% group_by(state_name) %>% mutate(intercept = mean(ptb_prop, na.rm=T), 
                                                 e_Y = rnorm(n = num_months, mean = 0, sd = sqrt(var(ptb_prop, na.rm=T))), 
                                                 A_time = min(month_ind[which(A==1)]), 
                                                 A_month = min(month[which(A==1)])) 
  
dat$A_time[is.infinite(dat$A_time)] <- 0 

# identify those who ever get the intervention 
dat <- dat %>% group_by(State) %>% mutate(ever_A = max(A))

# classify the time since the intervention for those who got it 
dat <- dat %>% mutate(time_since_A = ifelse(ever_A==0, 0, month_ind-A_time))
                          

##############################################################################################################################
# CONSTANT TREATMENT EFFECT 
##############################################################################################################################
dat <- dat %>%  mutate(Y = intercept + A*CTE + e_Y)
                      
# estimate effects using TWFE 
print("TWFE")
m1 <- glm(Y ~ A + factor(State) + factor(month_ind), data=dat, family="gaussian")
# get variance from sandwich estimator -- type = "HC0"
#m1_var<- sandwich(m1)
m1_var <- vcovHC(m1, type="HC3")
#summary(m1)$coefficients["A", "Std. Error"]
#sqrt(m1_var["A","A"])
#sqrt(test["A","A"])

# estimate effects using group-time ATT
print("gt ATT")

m2 <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat, anticipation=0)
m2_ag <- aggte(m2, type="simple")
#m2_ag$overall.att

print("sun abraham")
# estimate effects using Sun and Abraham approach 
dat <- dat %>% mutate(A_time_sa = ifelse(A_time!=0, A_time, Inf))
m3 <- staggered_sa(df = dat, i = "FIPS", t = "month_ind", g = "A_time_sa", y = "Y", estimand = "simple")


print("target trial")
dat<- dat %>% ungroup()
dat$month <- as.Date(dat$month)
dat$A_month <- as.Date(dat$A_month)

# estimate effects using Ben Michael target trial approach 
m4 <- fit_event_jack_sum_C(outcome_var = "Y", date_var = "month", unit_var = "state_name", policy_var = "A_month", data = dat, max_time_to = 10000)


print("TWFE ever-treated")
# TWFE if you only include those who eventually get the intervention 
dat_i <- dat %>% filter(ever_A==1)

m1_i <- glm(Y ~ A + factor(State) + factor(month_ind), data=dat_i, family="gaussian")
# get variance from sandwich estimator
#m1_var_i <- sandwich(m1_i)
m1_var_i <- vcovHC(m1_i, type="HC3")

print("gt ATT not yet treated")
# estimate effects using group-time ATT for only those who are not yet treated
m2_ea <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_i, anticipation=0, control_group = "notyettreated")
m2_ea_ag <- aggte(m2_ea, type="simple")


# define truth 
cte_truth <- CTE

print("combine results")
# combine results
df_cte <- data.frame(rbind(cbind(estimator = "truth", result = cte_truth, lb = NA, ub = NA, power=NA), 
                           cbind(estimator = "TWFE", result = summary(m1)$coefficients["A", "Estimate"], 
                                 lb = summary(m1)$coefficients["A", "Estimate"] - 1.96*sqrt(m1_var["A","A"]), 
                                 ub = summary(m1)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_var["A","A"]), 
                                 power = as.numeric(summary(m1)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_var["A","A"])<0)), 
                            cbind(estimator = "group.time.ATT", result = m2_ag$overall.att, 
                                 lb = m2_ag$overall.att - 1.96*m2_ag$overall.se, 
                                 ub = m2_ag$overall.att + 1.96*m2_ag$overall.se, 
                                 power = as.numeric(m2_ag$overall.att + 1.96*m2_ag$overall.se<0)), 
                           cbind(estimator = "staggered.SA", result = m3$estimate, 
                                 lb = m3$estimate - 1.96*m3$se_neyman, 
                                 ub = m3$estimate + 1.96*m3$se_neyman, 
                                 power = as.numeric(m3$estimate + 1.96*m3$se_neyman<0)), 
                           cbind(estimator="target.trial", result=m4$estimate, 
                                 lb = m4$estimate - 1.96*m4$se, 
                                 ub = m4$estimate + 1.96*m4$se, 
                                 power = as.numeric(m4$estimate + 1.96*m4$se<0)), 
                           cbind(estimator = "TWFE.ever.adopted", result = summary(m1_i)$coefficients["A", "Estimate"], 
                                 lb = summary(m1_i)$coefficients["A", "Estimate"] - 1.96*sqrt(m1_var["A","A"]), 
                                 ub = summary(m1_i)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_var["A","A"]), 
                                 power = as.numeric(summary(m1_i)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_var["A","A"])<0)), 
                           cbind(estimator = "group.time.ATT.ever.adopted", result = m2_ea_ag$overall.att, 
                                 lb = m2_ea_ag$overall.att - 1.96*m2_ea_ag$overall.se, 
                                 ub = m2_ea_ag$overall.att + 1.96*m2_ea_ag$overall.se, 
                                 power = as.numeric(m2_ea_ag$overall.att + 1.96*m2_ea_ag$overall.se<0))))

df_cte$result <- as.numeric(df_cte$result)
df_cte$lb <- as.numeric(df_cte$lb)
df_cte$ub <- as.numeric(df_cte$ub)
df_cte$power <- as.numeric(df_cte$power)

df_cte$type <- "CTE"

# make wide so results can be stacked
df_cte_wide <- df_cte %>% pivot_wider(names_from=c(type,estimator), values_from=c(result, lb, ub, power))



##############################################################################################################################
# HETEROGENEOUS TREATMENT EFFECT 
##############################################################################################################################

# define the heterogeneous treatment effect and generate the outcome 
dat_hte <- dat %>% mutate(HTE = ifelse(A_time<40 & ever_A==1, HTE[1], ifelse(A_time>=40 & ever_A==1, HTE[2], 0))) %>%  mutate(Y = intercept + A*HTE  + e_Y)

print("hetergeneous: TWFE")
# estimate effects using TWFE 
m1_hte <- glm(Y ~ A  + factor(State) + factor(month_ind), data=dat_hte, family="gaussian")
#m1_hte_var <- sandwich(m1_hte)
m1_hte_var <- vcovHC(m1_hte, type="HC3")

print("hetergeneous: gtATT")
# estimate effects using group-time ATT
m2_hte <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_hte, anticipation=0)
m2_hte_ag <- aggte(m2_hte, type="group")

#ggdid(m2_hte_ag)
print("hetergeneous: SA")
# estimate effects using Sun and Abraham approach 
m3_hte <- staggered_sa(df = dat_hte, i = "FIPS", t = "month_ind", g = "A_time_sa", y = "Y", estimand = "cohort")



print("target trial")
dat_hte<- dat_hte %>% ungroup()
dat_hte$month <- as.Date(dat_hte$month)
dat_hte$A_month <- as.Date(dat_hte$A_month)

# estimate effects using Ben Michael target trial approach 
m4_hte <- fit_event_jack_sum_hte(outcome_var = "Y", date_var = "month", unit_var = "state_name", policy_var = "A_month", data = dat_hte, max_time_to = 10000)


print("hetergeneous: TWFE ever treated")
# TWFE if you only include those who eventually get the intervention 
dat_hte_i <- dat_hte %>% filter(ever_A==1)

m1_hte_i <- glm(Y ~ A  +  factor(State) + factor(month_ind), data=dat_hte_i, family="gaussian")
# get variance from sandwich estimator
#m1_hte_var_i <- sandwich(m1_hte_i)
m1_hte_var_i <- vcovHC(m1_hte_i, type="HC3")

print("hetergeneous: gtATT among those who eventually get intervention") 
# estimate effects using group-time ATT among those who eventually get the intervention 
m2_hte_ea <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_hte_i, anticipation=0, control_group = "notyettreated")
m2_hte_ea_ag <- aggte(m2_hte_ea, type="group")


# calculate the truth for the HTE parameter
hte_truth <- (HTE[1]*length(dat_hte$HTE[dat_hte$A_time<40 & dat_hte$A_time==dat_hte$month_ind]) + HTE[2]*length(dat_hte$HTE[dat_hte$A_time>=40 & dat_hte$A_time==dat_hte$month_ind]))/length(unique(dat_hte$State[dat_hte$ever_A==1]))


# calculate different truth for ever-adopted 
hte_truth_ea <- (HTE[1]*length(dat_hte_i$HTE[dat_hte_i$A_time<40 & dat_hte_i$A_time==dat_hte_i$month_ind]) + HTE[2]*length(dat_hte_i$HTE[dat_hte_i$A_time>=40 & dat_hte_i$A_time<max(m2_hte$group) & dat_hte_i$A_time==dat_hte_i$month_ind]))/length(unique(dat_hte_i$State[dat_hte_i$ever_A==1 & dat_hte_i$A_time<max(m2_hte$group)]))


print("combine heterogeneous")
# combine results
df_hte <- data.frame(rbind(cbind(estimator="truth", result=hte_truth, lb = NA, ub = NA, power=NA), 
                           cbind(estimator = "TWFE", result = summary(m1_hte)$coefficients["A", "Estimate"], 
                                 lb = summary(m1_hte)$coefficients["A", "Estimate"] - 1.96*sqrt(m1_hte_var["A","A"]), 
                                 ub = summary(m1_hte)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_hte_var["A","A"]), 
                                 power = as.numeric(summary(m1_hte)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_hte_var["A","A"])<0)), 
                           cbind(estimator = "group.time.ATT", result = m2_hte_ag$overall.att, 
                                 lb = m2_hte_ag$overall.att - 1.96*m2_hte_ag$overall.se, 
                                 ub = m2_hte_ag$overall.att + 1.96*m2_hte_ag$overall.se, 
                                 power = as.numeric(m2_hte_ag$overall.att + 1.96*m2_hte_ag$overall.se<0)), 
                           cbind(estimator = "staggered.SA", result = m3_hte$estimate, 
                                 lb = m3_hte$estimate - 1.96*m3_hte$se_neyman, 
                                 ub = m3_hte$estimate + 1.96*m3_hte$se_neyman, 
                                 power = as.numeric(m3_hte$estimate + 1.96*m3_hte$se_neyman<0)),
                           cbind(estimator="target.trial", result=m4_hte$estimate, 
                                 lb = m4_hte$estimate - 1.96*m4_hte$se, 
                                 ub = m4_hte$estimate + 1.96*m4_hte$se, 
                                 power = as.numeric(m4_hte$estimate + 1.96*m4_hte$se<0))))



df_hte_ea <-  data.frame(rbind(cbind(estimator="truth", result=hte_truth_ea, lb=NA, ub=NA, power=NA), 
                               cbind(estimator = "TWFE.ever.adopted", result = summary(m1_hte_i)$coefficients["A", "Estimate"], 
                                     lb = summary(m1_hte_i)$coefficients["A", "Estimate"] - 1.96*sqrt(m1_hte_var_i["A","A"]), 
                                     ub = summary(m1_hte_i)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_hte_var_i["A","A"]), 
                                     power = as.numeric(summary(m1_hte_i)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_hte_var_i["A","A"])<0)), 
                              cbind(estimator = "group.time.ATT.ever.adopted", result = m2_hte_ea_ag$overall.att, 
                                     lb = m2_hte_ea_ag$overall.att - 1.96*m2_hte_ea_ag$overall.se, 
                                     ub = m2_hte_ea_ag$overall.att + 1.96*m2_hte_ea_ag$overall.se, 
                                     power = as.numeric(m2_hte_ea_ag$overall.att + 1.96*m2_hte_ea_ag$overall.se<0))))


df_hte$result <- as.numeric(df_hte$result)
df_hte$lb <- as.numeric(df_hte$lb)
df_hte$ub <- as.numeric(df_hte$ub)
df_hte$power <- as.numeric(df_hte$power)
df_hte$type <- "HTE"

df_hte_ea$result <- as.numeric(df_hte_ea$result)
df_hte_ea$lb <- as.numeric(df_hte_ea$lb)
df_hte_ea$ub <- as.numeric(df_hte_ea$ub)
df_hte_ea$power <- as.numeric(df_hte_ea$power)

df_hte_ea$type <- "HTE.EA"

# make wide so results can be stacked
df_hte_wide <- df_hte %>% pivot_wider(names_from=c(type, estimator), values_from=c(result, lb, ub, power))
df_hte_ea_wide <- df_hte_ea %>% pivot_wider(names_from=c(type, estimator), values_from=c(result, lb, ub, power))

##############################################################################################################################
# DYNAMIC TREATMENT EFFECT 
##############################################################################################################################

# first 12 months one effect, second 12 months different effect, and third twelve months through the end of the study period there is a third effect 
dat_dte <- dat %>% mutate(DTE = ifelse(time_since_A>=0 & time_since_A<12 & ever_A==1, DTE[1], 
                                       ifelse(time_since_A>=12 & time_since_A<24 & ever_A==1, DTE[2], 
                                              ifelse(time_since_A>=24 & ever_A==1, DTE[3], 0)))) %>%  mutate(Y = intercept + A*DTE  + e_Y)




##############################################################################################################################
# AVERAGE EFFECT IN THE POST-PERIOD  
##############################################################################################################################
print("dynamic: TWFE")
# estimate effects using TWFE 
m1_dte <- glm(Y ~ A  + factor(State) + factor(month_ind), data=dat_dte, family="gaussian")
# get variance from sandwich estimator
m1_dte_var <- vcovHC(m1_dte, type="HC3")

print("dynamic: gtATT")
# estimate effects using group-time ATT 
m2_dte <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_dte, anticipation=0)
m2_dte_ag <- aggte(m2_dte, type="simple")
#summary(m2_dte_ag) 
#ggdid(m2_dte_ag)

print("dynamic: SA")
# estimate effects using Sun and Abraham approach 
m3_dte <- staggered_sa(df = dat_dte, i = "FIPS", t = "month_ind", g = "A_time_sa", y = "Y", estimand = "simple")

print("target trial")
dat_dte<- dat_dte %>% ungroup()
dat_dte$month <- as.Date(dat_dte$month)
dat_dte$A_month <- as.Date(dat_dte$A_month)

# estimate effects using Ben Michael target trial approach 
m4_dte <- fit_event_jack_sum_C(outcome_var = "Y", date_var = "month", unit_var = "state_name", policy_var = "A_month", data = dat_dte, max_time_to = 10000)



print("dynamic: TWFE eventually treated")
# TWFE if you only include those who eventually get the intervention 
dat_dte_i <- dat_dte %>% filter(ever_A==1)
m1_dte_i <- glm(Y ~ A  +  factor(State) + factor(month_ind), data=dat_dte_i, family="gaussian")
# get variance from sandwich estimator
#m1_dte_var_i <- sandwich(m1_dte_i)
m1_dte_var_i <- vcovHC(m1_dte_i, type="HC3")

print("dynamic gtATT eventually treated")
# estimate effects using group-time ATT among those who eventually get the intervention 
m2_dte_ea <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_dte_i, anticipation=0, control_group = "notyettreated")
m2_dte_ea_ag <- aggte(m2_dte_ea, type="simple")

# calculate the truth for the average effect in the post-period 
dte_truth_avg <- (DTE[1]*sum(dat_dte$time_since_A>=0 & dat_dte$time_since_A<12 & dat_dte$ever_A==1, na.rm=T) + 
                    DTE[2]*sum(dat_dte$time_since_A>=12 & dat_dte$time_since_A<24 & dat_dte$ever_A==1, na.rm=T) +
                    DTE[3]*sum(dat_dte$time_since_A>=24 & dat_dte$ever_A==1, na.rm=T))/sum(dat_dte$time_since_A>=0 & dat_dte$ever_A==1,na.rm=T)

#dte_truth_avg <- (DTE[1]*12 + DTE[2]*12 + DTE[3]*(max(dat_dte$month_ind) - min(dat_dte$A_time[dat_dte$ever_A==1]) - 24))/(max(dat_dte$month_ind) - min(dat_dte$A_time[dat_dte$ever_A==1]))

# calculate truth for average effect in the post-period for ever adopted group -- need to exclude last treated group 
dte_truth_avg_ea <- (DTE[1]*sum(dat_dte_i$time_since_A>=0 & dat_dte_i$time_since_A<12 & dat_dte_i$ever_A==1 & dat_dte_i$A_time<=max(m2_dte_ea$group), na.rm=T) + 
                       DTE[2]*sum(dat_dte_i$time_since_A>=12 & dat_dte_i$time_since_A<24  & dat_dte_i$ever_A==1 & dat_dte_i$A_time<=max(m2_dte_ea$group), na.rm=T) +
                       DTE[3]*sum(dat_dte_i$time_since_A>=24  & dat_dte_i$ever_A==1 & dat_dte_i$A_time<max(m2_dte_ea$group), na.rm=T))/sum(dat_dte_i$time_since_A>=0 & dat_dte_i$ever_A==1 & dat_dte_i$A_time<max(m2_dte_ea$group), na.rm=T)

#dte_truth_avg_ea <- (DTE[1]*12 + DTE[2]*12 + DTE[3]*(max(dat_dte_i$A_time) - min(dat_dte_i$A_time) - 24))/(max(dat_dte_i$A_time) - min(dat_dte_i$A_time))


print("dynamic combine")
# combine results 
df_dte_avg <- data.frame(rbind(cbind(estimator="truth", result=dte_truth_avg, lb = NA, ub = NA, power=NA), 
                               cbind(estimator = "TWFE", result = summary(m1_dte)$coefficients["A", "Estimate"], 
                                     lb = summary(m1_dte)$coefficients["A", "Estimate"] - 1.96*sqrt(m1_dte_var["A","A"]), 
                                     ub = summary(m1_dte)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_dte_var["A","A"]), 
                                     power = as.numeric(summary(m1_dte)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_dte_var["A","A"])<0)), 
                               cbind(estimator = "group.time.ATT", result = m2_dte_ag$overall.att, 
                                     lb = m2_dte_ag$overall.att - 1.96*m2_dte_ag$overall.se, 
                                     ub = m2_dte_ag$overall.att + 1.96*m2_dte_ag$overall.se, 
                                     power = as.numeric(m2_dte_ag$overall.att + 1.96*m2_dte_ag$overall.se<0)), 
                               cbind(estimator = "staggered.SA", result = m3_dte$estimate, 
                                     lb = m3_dte$estimate - 1.96*m3_dte$se_neyman, 
                                     ub = m3_dte$estimate + 1.96*m3_dte$se_neyman, 
                                     power = as.numeric(m3_dte$estimate + 1.96*m3_dte$se_neyman<0)),
                               cbind(estimator="target.trial", result=m4_dte$estimate, 
                                     lb = m4_dte$estimate - 1.96*m4_dte$se, 
                                     ub = m4_dte$estimate + 1.96*m4_dte$se, 
                                     power = as.numeric(m4_dte$estimate + 1.96*m4_dte$se<0))))


df_dte_avg_ea <- data.frame(rbind(cbind(estimator="truth", result=dte_truth_avg_ea, lb = NA, ub = NA, power=NA), 
                                  cbind(estimator = "TWFE.ever.adopted", result = summary(m1_dte_i)$coefficients["A", "Estimate"], 
                                        lb = summary(m1_dte_i)$coefficients["A", "Estimate"] - 1.96*sqrt(m1_dte_var_i["A","A"]), 
                                        ub = summary(m1_dte_i)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_dte_var_i["A","A"]), 
                                        power = as.numeric(summary(m1_dte_i)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_dte_var_i["A","A"])<0)), 
                                cbind(estimator = "group.time.ATT.ever.adopted", result = m2_dte_ea_ag$overall.att, 
                                        lb = m2_dte_ea_ag$overall.att - 1.96*m2_dte_ea_ag$overall.se, 
                                        ub = m2_dte_ea_ag$overall.att + 1.96*m2_dte_ea_ag$overall.se, 
                                        power = as.numeric(m2_dte_ea_ag$overall.att + 1.96*m2_dte_ea_ag$overall.se<0))))


df_dte_avg$result <- as.numeric(df_dte_avg$result)
df_dte_avg$lb <- as.numeric(df_dte_avg$lb)
df_dte_avg$ub <- as.numeric(df_dte_avg$ub)
df_dte_avg$power <- as.numeric(df_dte_avg$power)

df_dte_avg$type <- "DTE.avg"

df_dte_avg_ea$result <- as.numeric(df_dte_avg_ea$result)
df_dte_avg_ea$lb <- as.numeric(df_dte_avg_ea$lb)
df_dte_avg_ea$ub <- as.numeric(df_dte_avg_ea$ub)
df_dte_avg_ea$power <- as.numeric(df_dte_avg_ea$power)

df_dte_avg_ea$type <- "DTE.avg.EA"

# make wide so results can be stacked 
df_dte_avg_wide <- df_dte_avg %>% pivot_wider(names_from=c(type, estimator), values_from=c(result, lb, ub, power))
df_dte_avg_ea_wide <- df_dte_avg_ea %>% pivot_wider(names_from=c(type, estimator), values_from=c(result, lb, ub, power))


##############################################################################################################################
# YEARLY EFFECT IN THE POST-PERIOD  
##############################################################################################################################

print("yearly effect in post period glm")
# estimate effects using TWFE  
m1_dte_yr <- glm(Y ~ factor(time_since_A)  + factor(State) + factor(month_ind), data=dat_dte, family="gaussian")
# I think because of small sample size, the sandwich estimator for variance with small sample size correction returns 
# only NaN values. Those that don't make the small sample size correction return values but they are approximatley 
# half the SE's of the model. Therefore, just using the model SE's 
#m1_dte_yr_var <- sandwich(m1_dte_yr)
#test <- vcovHC(m1_dte_yr, type="HC1")
#summary(m1_dte_yr)$coefficients["factor(time_since_A)0", "Std. Error"]
#sqrt(m1_dte_yr_var["factor(time_since_A)0","factor(time_since_A)0"])
#sqrt(test["factor(time_since_A)0","factor(time_since_A)0"])

print("yearly effect in post period gtatt")
# estimate effects using group-time ATT  
m2_dte_yr <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_dte, anticipation=0, cband=TRUE)
m2_dte_yr_ag <- aggte(m2_dte_yr, type="dynamic", cband=TRUE)
#summary(m2_hte_ag) 

print("yearly effect in post period Ben Micheal")
# estimate effects using stacked regression from Ben Michael paper 
dat_dte <- dat_dte %>% ungroup()
dat_dte$month <- as.Date(dat_dte$month)
dat_dte$A_month <- as.Date(dat_dte$A_month)

print("jackknife")
m3_dte <- fit_event_jack(outcome = "Y", date_var = "month", unit_var = "state_name", policy_var = "A_month", data = dat_dte, max_time_to = 10000) %>% 
  filter(cohort=="average")

print("dynamic: SA")
# estimate effects using Sun and Abraham approach 
m4_dte <- staggered_sa(df = dat_dte, i = "FIPS", t = "month_ind", g = "A_time_sa", y = "Y", estimand = "eventstudy", eventTime = 0:max(dat_dte$time_since_A))


#ggdid(m2_dte_yr_ag)

print("yearly effect TWFE ever treated")
# estimate TWFE model only among those who ever get the intervention 
m1_dte_yr_i <- glm(Y ~ factor(time_since_A)  +  factor(State) + factor(month_ind), data=dat_dte_i, family="gaussian")
# don't use sandwich estimator for variance -- same reason as above
#m1_dte_yr_var_i <- sandwich(m1_dte_yr_i)
#test <- vcovHC(m1_dte_yr_i, type="HC3")
#summary(m1_dte_yr_i)$coefficients["factor(time_since_A)0", "Std. Error"]
#sqrt(m1_dte_yr_var_i["factor(time_since_A)0","factor(time_since_A)0"])
#sqrt(test["factor(time_since_A)0","factor(time_since_A)0"])

print("yearly effect gtatt ever treated")
# estimate effects using group-time ATT among those who ever get the intervention 
m2_dte_yr_ea <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_dte_i, anticipation=0, cband=TRUE, control_group = "notyettreated")
m2_dte_yr_ea_ag <- aggte(m2_dte_yr_ea, type="dynamic", cband=TRUE)


# dynamic results 
# define the truth 
dte_truth <- data.frame(estimator="truth", time= (min(dat_dte$time_since_A, na.rm=T)):max(dat_dte$time_since_A, na.rm=T), 
                        result=c(rep(0,-1*min(dat_dte$time_since_A, na.rm=T)), 
                                 rep(DTE[1], 12), 
                                 rep(DTE[2], 12), 
                                 rep(DTE[3], max(dat_dte$time_since_A, na.rm=T) - 23)), lb=NA, ub=NA, power=NA)

# twfe -- save estimates from model and get standard errors from sandwich variance matrix 
# note: sandwich variance matrix is giving smaller estimates of variance than we're getting from the model 
# update: this is likely because of small sample size, see above for more detail.  use model estimates of SE instead
twfe <- data.frame(cbind(result = summary(m1_dte_yr)$coefficients))

# keep only coefficients that define effects of interest
twfe <- twfe %>% filter(grepl("time_since_A", rownames(twfe))) %>% rename(result = Estimate, SE = Std..Error) %>% dplyr::select(result, SE)

# calculate upper and lower 95% confidence interval bounds
twfe <- twfe %>% mutate(lb = result - 1.96*SE, ub = result + 1.96*SE, power = as.numeric(result + 1.96*SE <0), estimator="TWFE", time=(min(dat_dte$time_since_A, na.rm=T)+1):max(dat_dte$time_since_A, na.rm=T)) %>% dplyr::select(estimator, time, result, lb, ub, power)


# group-time ATT 
gt <- data.frame(cbind(result = m2_dte_yr_ag$att.egt, SE = m2_dte_yr_ag$se.egt, time=m2_dte_yr_ag$egt, c = m2_dte_yr_ag$crit.val.egt))
gt$estimator <-  "group.time.ATT"

# the critical value is used instead of 1.96 to get the simultaneous 95% CI's 
gt$lb <- gt$result - gt$c*gt$SE
gt$ub <- gt$result + gt$c*gt$SE
gt$SE <- gt$c <- NULL 
gt$power <- as.numeric(gt$ub<0)


# stacked regression 
stacked <- data.frame(cbind(result = m3_dte$estimate, SE = m3_dte$se, time = m3_dte$event_time))
stacked$lb <- stacked$result - 1.96*stacked$SE 
stacked$ub <- stacked$result + 1.96*stacked$SE
stacked$SE <- NULL
stacked$power <- as.numeric(stacked$ub<0)
stacked$estimator <- "stacked.regression"

# sun and abraham

sun_abr <- data.frame(m4_dte) %>% select(estimate, se_neyman, eventTime) %>% rename(result = estimate, SE = se_neyman, time=eventTime)
sun_abr <- sun_abr %>% mutate(lb = result - 1.96*SE, 
                              ub = result + 1.96*SE) %>% mutate(power = as.numeric(ub<0), estimator = "staggered.SA") %>% select(-SE)



# TWFE for ever adopted 

twfe_ea <- data.frame(cbind(result = summary(m1_dte_yr_i)$coefficients))

# keep only coefficients that define effects of interest
twfe_ea <- twfe_ea %>% filter(grepl("time_since_A", rownames(twfe_ea))) %>% rename(result = Estimate, SE = Std..Error) %>% dplyr::select(result, SE)

# calculate upper and lower 95% confidence interval bounds
twfe_ea <- twfe_ea %>% mutate(lb = result - 1.96*SE, ub = result + 1.96*SE, power = as.numeric(result + 1.96*SE <0), estimator="TWFE.ever.adopted", time=(min(dat_dte$time_since_A, na.rm=T)+1):max(dat_dte$time_since_A, na.rm=T)) %>% dplyr::select(estimator, time, result, lb, ub, power)


# group-time ATT for ever adopted
gt_ea <- data.frame(cbind(result = m2_dte_yr_ea_ag$att.egt, SE = m2_dte_yr_ea_ag$se.egt, time=m2_dte_yr_ea_ag$egt, c = m2_dte_yr_ea_ag$crit.val.egt))
gt_ea$estimator <-  "group.time.ATT.ever.adopted"

# the critical value is used instead of 1.96 to get the simultaneous 95% CI's 
gt_ea$lb <- gt_ea$result - gt_ea$c*gt_ea$SE
gt_ea$ub <- gt_ea$result + gt_ea$c*gt_ea$SE
gt_ea$SE <- gt_ea$c <- NULL 
gt_ea$power <- as.numeric(gt_ea$ub <0)

# combine all dynamic results
df_dyn <- data.frame(rbind(dte_truth, twfe, gt, stacked, sun_abr, twfe_ea, gt_ea))
df_dyn$type <- paste0("DTE.",df_dyn$time)

# make wide so results can be stacked 
df_dyn_wide <- df_dyn %>% dplyr::select(-time) %>%  pivot_wider(names_from=c(type, estimator), values_from=c(result, lb, ub, power))

# output overall results
overall_result <- data.frame(cbind(df_cte_wide, df_hte_wide, df_hte_ea_wide, df_dte_avg_wide, df_dte_avg_ea_wide, df_dyn_wide))
return(overall_result)
print("-----END OF THE ITERATION-----")
}


results_ls <- lapply(1:1000, function(x) sim_rep(x, dat=dat, CTE = -0.02, HTE = c(-0.01, -0.02), DTE = c(-0.01, -0.015, -0.02)))

results_df <- data.frame(do.call(rbind, results_ls))

write.csv(results_df, file="./results/twfe_uniform_extended_sim_results_PTB_n1000_07112022.csv", row.names = F)

results_df_calc <- results_df %>% pivot_longer(cols= everything(), names_to=c("estimand", "parameter", "method"), names_sep="_")
results_df_calc <- results_df_calc %>% group_by(estimand, parameter, method) %>% mutate(iteration = row_number())


results_df_calc <- results_df_calc %>% pivot_wider(id_cols = c(estimand, parameter, method, iteration), names_from=estimand, values_from=value)


results_df_calc <- results_df_calc %>% group_by(iteration, parameter) %>% mutate(truth = result[method=="truth"])  


results_df_calc <- results_df_calc %>%  mutate(coverage = as.numeric(lb<=truth & ub>=truth),  
                                               bias = result - truth, 
                                               MSE = (result - truth)^2)


results_df_calc <- results_df_calc %>% filter(method!="truth")
# rename parameters so they all show up on same plot 
results_df_calc$parameter[results_df_calc$parameter=="HTE.EA"] <-  "HTE"
results_df_calc$parameter[results_df_calc$parameter=="DTE.avg.EA"] <-  "DTE.avg"


results_df_summary <- results_df_calc %>% group_by(parameter, method) %>% summarise(coverage = mean(coverage), 
                                                                                    bias = mean(bias), 
                                                                                    MSE = mean(MSE), 
                                                                                    power = mean(power))


write.csv(results_df_summary, file="./results/twfe_uniform_sim_results_extended_followup_summary_PTB_n1000_07112022.csv", row.names = F)
