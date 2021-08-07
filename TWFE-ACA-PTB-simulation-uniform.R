rm(list=ls())
library(readxl)
library(tidyverse)
library(did)
library(sandwich)
library(ggrepel)

set.seed(38525753)

# combine all years into one data frame
df_ls <- list()
for(i in 2011:2017) {
df_ls[[i - 2010]] <- read.csv(paste0("/Users/danagoin/Documents/Research projects/TWFE/data/state-month-ptb-2-",i,".csv"))
if (i >2015) {
  df_ls[[i-2010]] <- df_ls[[i-2010]] %>% rename(MRSTATEPSTL = MRSTATE) 
}
}

df_ptb <- do.call(rbind, df_ls)

# merge on Medicaid expansion dates 
df_e <- read_xlsx("/Users/danagoin/Documents/Research projects/TWFE/data/ACA-expansion.xlsx")
states <- read.csv("/Users/danagoin/Documents/Research projects/TWFE/data/state-abbrev.csv")
states <- states %>% rename(State = Name, MRSTATEPSTL = Postal.Code)
df_e <- left_join(df_e, states, by="State")


df_ptb <- left_join(df_ptb, df_e, by="MRSTATEPSTL")

# define month 
df_ptb <- df_ptb %>% mutate(month = as.POSIXct(paste0(year,"-",DOB_MM,"-01"), format="%Y-%m-%d"))


# Alaska, Massachusetts, Minnesota, Mississippi, and Virginia have fewer observations because they implemented revised birth certificate later
# don't want to include this type of heterogeneity at this point, so add more data for them 
#ggplot(df_ptb %>% filter(state_name %in% c("Alaska", "Massachusetts","Minnesota", "Mississippi","Virginia"))) + geom_line(aes(x=month, y=ptb_prop, color=factor(state_name)))

d1 <- data.frame(MRSTATEPSTL = "AK", DOB_MM = NA, ptb_prop = NA, ptb_prop2 = NA, year = NA, 
                 state_name="Alaska", 
                 Census_Region = "West", State="Alaska", Revised_bcert_year=2013, Expanded_Medicaid=1, 
                 Expansion_Date = as.POSIXct("2015-09-01"), Year_prior_expansion= NA, FPL_early_expansion = NA, 
                 Section_1115_waiver = 0, Expansion_type= "Full", Notes_births = NA, Notes = NA, FIPS = 2, 
                 month = seq(as.POSIXct("2011-01-01", format="%Y-%m-%d"), by = "month", length.out = 84)) 
d1$year <- as.numeric(substr(as.character(d1$month),1,4))
m_add1 <- d1[!d1$month %in% df_ptb$month[df_ptb$state_name=="Alaska"],]


d2 <- data.frame(MRSTATEPSTL = "MA", DOB_MM = NA, ptb_prop = NA, ptb_prop2 = NA, year = NA, 
                 state_name="Massachusetts", 
                 Census_Region = "Northeast", State="Massachusetts", Revised_bcert_year=2011, Expanded_Medicaid=1, 
                 Expansion_Date = as.POSIXct("2014-01-01"), Year_prior_expansion= as.POSIXct("2006-04-12"), FPL_early_expansion = 150, 
                 Section_1115_waiver = 0, Expansion_type= "Mild", Notes_births = NA, Notes = "Romneycare expanded insurance to low income adults in 2006 (free if < 150%, subsidized for < 300% FPL)", 
                 FIPS = 25, 
                 month = seq(as.POSIXct("2011-01-01", format="%Y-%m-%d"), by = "month", length.out = 84)) 
d2$year <- as.numeric(substr(as.character(d2$month),1,4))
m_add2 <- d2[!d2$month %in% df_ptb$month[df_ptb$state_name=="Massachusetts"],]

d3 <- data.frame(MRSTATEPSTL = "MN", DOB_MM = NA, ptb_prop = NA, ptb_prop2 = NA, year = NA, 
                 state_name="Minnesota", 
                 Census_Region = "Midwest", State="Minnesota", Revised_bcert_year=2011, Expanded_Medicaid=1, 
                 Expansion_Date = as.POSIXct("2014-01-01"), Year_prior_expansion= as.POSIXct("2010-03-01"), FPL_early_expansion = 75, 
                 Section_1115_waiver = 0, Expansion_type= "Substantial", Notes_births = NA, Notes = "Early expansion in 2010 with Medicaid for incomes < 75% FPL and MinnesotaCare for 75-200%", 
                 FIPS = 27, 
                 month = seq(as.POSIXct("2011-01-01", format="%Y-%m-%d"), by = "month", length.out = 84)) 
d3$year <- as.numeric(substr(as.character(d3$month),1,4))
m_add3 <- d3[!d3$month %in% df_ptb$month[df_ptb$state_name=="Minnesota"],]


d4 <- data.frame(MRSTATEPSTL = "MS", DOB_MM = NA, ptb_prop = NA, ptb_prop2 = NA, year = NA, 
                 state_name="Mississippi", 
                 Census_Region = "South", State="Mississippi", Revised_bcert_year=2013, Expanded_Medicaid=0, 
                 Expansion_Date = NA, Year_prior_expansion= NA, FPL_early_expansion = NA, 
                 Section_1115_waiver = 0, Expansion_type= NA, Notes_births = NA, Notes = NA, 
                 FIPS = 28, 
                 month = seq(as.POSIXct("2011-01-01", format="%Y-%m-%d"), by = "month", length.out = 84)) 
d4$year <- as.numeric(substr(as.character(d4$month),1,4))
m_add4 <- d4[!d4$month %in% df_ptb$month[df_ptb$state_name=="Mississippi"],]


d5 <- data.frame(MRSTATEPSTL = "VA", DOB_MM = NA, ptb_prop = NA, ptb_prop2 = NA, year = NA, 
                 state_name="Virginia", 
                 Census_Region = "South", State="Virginia", Revised_bcert_year=2012, Expanded_Medicaid=1, 
                 Expansion_Date = as.POSIXct("2019-01-01"), Year_prior_expansion= NA, FPL_early_expansion = NA, 
                 Section_1115_waiver = 0, Expansion_type= NA, Notes_births = NA, Notes = NA, 
                 FIPS = 51, 
                 month = seq(as.POSIXct("2011-01-01", format="%Y-%m-%d"), by = "month", length.out = 84)) 
d5$year <- as.numeric(substr(as.character(d5$month),1,4))
m_add5 <- d5[!d5$month %in% df_ptb$month[df_ptb$state_name=="Virginia"],]


dat <- data.frame(rbind(df_ptb, m_add1, m_add2, m_add3, m_add4, m_add5))
dat <- dat %>% arrange(State, month)


# create month indicators from beginning to end of study 
dat <- dat %>% arrange(FIPS, month)
dat <- dat %>% group_by(FIPS) %>% mutate(month_ind = 1:84)

# 21 sates expanded Medicaid -- for those that expanded, draw expansion date uniformly in post-period  

# create treatment indicator by month by randomly sampling post-exposure month 
#dat <- dat %>% group_by(FIPS) %>%  mutate(first_A = ifelse(Expanded_Medicaid==1, sample(37:84, size=1), NA)) 
dat <- dat %>% group_by(FIPS) %>%  mutate(first_A = ifelse(Expanded_Medicaid==1, sample(c(37,52,65,72), size=1), NA)) 

dat <- dat %>% mutate(A = as.numeric(month_ind>=first_A)) 
dat$A[is.na(dat$A)] <- 0


##############################################################################################################################
# SIMULATION 
##############################################################################################################################


sim_rep <- function(iteration, dat, CTE, HTE, DTE) {
  print(iteration)
# create state-level intercept and noise for each state-month 
dat <- dat %>% group_by(state_name) %>% mutate(intercept = mean(ptb_prop, na.rm=T),
                                               e_Y = rnorm(n=84, mean=0, sd=sqrt(var(ptb_prop, na.rm=T))), 
                                               A_time = min(month_ind[which(A==1)]))

dat$A_time[is.infinite(dat$A_time)] <- 0 

# identify those who ever get the intervention 
dat <- dat %>% group_by(State) %>% mutate(ever_A = max(A))
# classify the time since the intervention for those who got it 
dat <- dat %>% mutate(time_since_A = ifelse(ever_A==0, NA, month_ind-A_time))
                          

##############################################################################################################################
# CONSTANT TREATMENT EFFECT 
##############################################################################################################################
dat <- dat %>%  mutate(Y = intercept + A*CTE + e_Y)
                      
# estimate effects using TWFE 
m1 <- glm(Y ~ A + factor(State) + factor(month_ind), data=dat, family="gaussian")
# get variance from sandwich estimator -- type = "HC0"
#m1_var<- sandwich(m1)
m1_var <- vcovHC(m1, type="HC3")
#summary(m1)$coefficients["A", "Std. Error"]
#sqrt(m1_var["A","A"])
#sqrt(test["A","A"])

# estimate effects using group-time ATT
m2 <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat, anticipation=0)
m2_ag <- aggte(m2, type="simple")
#m2_ag$overall.att

# TWFE if you only include those who eventually get the intervention 
dat_i <- dat %>% filter(ever_A==1)

m1_i <- glm(Y ~ A + factor(State) + factor(month_ind), data=dat_i, family="gaussian")
# get variance from sandwich estimator
#m1_var_i <- sandwich(m1_i)
m1_var_i <- vcovHC(m1_i, type="HC3")


# estimate effects using group-time ATT for only those who are not yet treated
m2_ea <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_i, anticipation=0, control_group = "notyettreated")
m2_ea_ag <- aggte(m2_ea, type="simple")


# define truth 
cte_truth <- CTE

# combine results
df_cte <- data.frame(rbind(cbind(estimator = "truth", result = cte_truth, lb = NA, ub = NA), 
                           cbind(estimator = "TWFE", result = summary(m1)$coefficients["A", "Estimate"], 
                                 lb = summary(m1)$coefficients["A", "Estimate"] - 1.96*sqrt(m1_var["A","A"]), 
                                 ub = summary(m1)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_var["A","A"])), 
                           cbind(estimator = "group.time.ATT", result = m2_ag$overall.att, 
                                 lb = m2_ag$overall.att - 1.96*m2_ag$overall.se, 
                                 ub = m2_ag$overall.att + 1.96*m2_ag$overall.se), 
                           cbind(estimator = "TWFE.ever.adopted", result = summary(m1_i)$coefficients["A", "Estimate"], 
                                 lb = summary(m1_i)$coefficients["A", "Estimate"] - 1.96*sqrt(m1_var["A","A"]), 
                                 ub = summary(m1_i)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_var["A","A"])), 
                           cbind(estimator = "group.time.ATT.ever.adopted", result = m2_ea_ag$overall.att, 
                                 lb = m2_ea_ag$overall.att - 1.96*m2_ea_ag$overall.se, 
                                 ub = m2_ea_ag$overall.att + 1.96*m2_ea_ag$overall.se)))

df_cte$result <- as.numeric(df_cte$result)
df_cte$lb <- as.numeric(df_cte$lb)
df_cte$ub <- as.numeric(df_cte$ub)
df_cte$type <- "CTE"

# make wide so results can be stacked
df_cte_wide <- df_cte %>% pivot_wider(names_from=c(type,estimator), values_from=c(result, lb, ub))


##############################################################################################################################
# HETEROGENEOUS TREATMENT EFFECT 
##############################################################################################################################

# define the heterogeneous treatment effect and generate the outcome 
dat_hte <- dat %>% mutate(HTE = ifelse(A_time<65 & ever_A==1, HTE[1], ifelse(A_time>=65 & ever_A==1, HTE[2], 0))) %>%  mutate(Y = intercept + A*HTE  + e_Y)

# estimate effects using TWFE 
m1_hte <- glm(Y ~ A  + factor(State) + factor(month_ind), data=dat_hte, family="gaussian")
#m1_hte_var <- sandwich(m1_hte)
m1_hte_var <- vcovHC(m1_hte, type="HC3")


# estimate effects using group-time ATT
m2_hte <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_hte, anticipation=0)
m2_hte_ag <- aggte(m2_hte, type="group")

#ggdid(m2_hte_ag)

# TWFE if you only include those who eventually get the intervention 
dat_hte_i <- dat_hte %>% filter(ever_A==1)

m1_hte_i <- glm(Y ~ A  +  factor(State) + factor(month_ind), data=dat_hte_i, family="gaussian")
# get variance from sandwich estimator
#m1_hte_var_i <- sandwich(m1_hte_i)
m1_hte_var_i <- vcovHC(m1_hte_i, type="HC3")


# estimate effects using group-time ATT among those who eventually get the intervention 
m2_hte_ea <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_hte_i, anticipation=0, control_group = "notyettreated")
m2_hte_ea_ag <- aggte(m2_hte_ea, type="group")


# calculate the truth for the HTE parameter
hte_truth <- (HTE[1]*length(dat_hte$HTE[dat_hte$A_time<65 & dat_hte$A_time==dat_hte$month_ind]) + HTE[2]*length(dat_hte$HTE[dat_hte$A_time>=65 & dat_hte$A_time==dat_hte$month_ind]))/length(unique(dat_hte$State[dat_hte$ever_A==1]))

#### figure out how to incorporate different truth for ever-adopted 
hte_truth_ea <- (HTE[1]*length(dat_hte_i$HTE[dat_hte_i$A_time<65 & dat_hte_i$A_time==dat_hte_i$month_ind]) + HTE[2]*length(dat_hte_i$HTE[dat_hte_i$A_time>=65 & dat_hte_i$A_time<72 & dat_hte_i$A_time==dat_hte_i$month_ind]))/length(unique(dat_hte_i$State[dat_hte_i$ever_A==1 & dat_hte_i$A_time<72]))


# combine results
df_hte <- data.frame(rbind(cbind(estimator="truth", result=hte_truth, lb = NA, ub = NA), 
                           cbind(estimator = "TWFE", result = summary(m1_hte)$coefficients["A", "Estimate"], 
                                 lb = summary(m1_hte)$coefficients["A", "Estimate"] - 1.96*sqrt(m1_hte_var["A","A"]), 
                                 ub = summary(m1_hte)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_hte_var["A","A"])), 
                           cbind(estimator = "group.time.ATT", result = m2_hte_ag$overall.att, 
                                 lb = m2_hte_ag$overall.att - 1.96*m2_hte_ag$overall.se, 
                                 ub = m2_hte_ag$overall.att + 1.96*m2_hte_ag$overall.se)))

 
df_hte_ea <-  data.frame(rbind(cbind(estimator="truth", result=hte_truth_ea, lb=NA, ub=NA), 
                               cbind(estimator = "TWFE.ever.adopted", result = summary(m1_hte_i)$coefficients["A", "Estimate"], 
                                 lb = summary(m1_hte_i)$coefficients["A", "Estimate"] - 1.96*sqrt(m1_hte_var_i["A","A"]), 
                                 ub = summary(m1_hte_i)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_hte_var_i["A","A"])), 
                           cbind(estimator = "group.time.ATT.ever.adopted", result = m2_hte_ea_ag$overall.att, 
                                 lb = m2_hte_ea_ag$overall.att - 1.96*m2_hte_ea_ag$overall.se, 
                                 ub = m2_hte_ea_ag$overall.att + 1.96*m2_hte_ea_ag$overall.se)))


df_hte$result <- as.numeric(df_hte$result)
df_hte$lb <- as.numeric(df_hte$lb)
df_hte$ub <- as.numeric(df_hte$ub)
df_hte$type <- "HTE"

df_hte_ea$result <- as.numeric(df_hte_ea$result)
df_hte_ea$lb <- as.numeric(df_hte_ea$lb)
df_hte_ea$ub <- as.numeric(df_hte_ea$ub)
df_hte_ea$type <- "HTE.EA"

# make wide so results can be stacked
df_hte_wide <- df_hte %>% pivot_wider(names_from=c(type, estimator), values_from=c(result, lb, ub))
df_hte_ea_wide <- df_hte_ea %>% pivot_wider(names_from=c(type, estimator), values_from=c(result, lb, ub))

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

# estimate effects using TWFE 
m1_dte <- glm(Y ~ A  + factor(State) + factor(month_ind), data=dat_dte, family="gaussian")
# get variance from sandwich estimator
m1_dte_var <- vcovHC(m1_dte, type="HC3")


# estimate effects using group-time ATT 
m2_dte <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_dte, anticipation=0)
m2_dte_ag <- aggte(m2_dte, type="simple")
#summary(m2_dte_ag) 
#ggdid(m2_dte_ag)


# TWFE if you only include those who eventually get the intervention 
dat_dte_i <- dat_dte %>% filter(ever_A==1)
m1_dte_i <- glm(Y ~ A  +  factor(State) + factor(month_ind), data=dat_dte_i, family="gaussian")
# get variance from sandwich estimator
#m1_dte_var_i <- sandwich(m1_dte_i)
m1_dte_var_i <- vcovHC(m1_dte_i, type="HC3")


# estimate effects using group-time ATT among those who eventually get the intervention 
m2_dte_ea <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_dte_i, anticipation=0, control_group = "notyettreated")
m2_dte_ea_ag <- aggte(m2_dte_ea, type="simple")

# calculate number of state-months of treatment for denominator, state-months of each treatment size in numerator
dat_dte <- dat_dte %>% group_by(State) %>% mutate(num_post_months = max(time_since_A))
dat_dte$num_post_months <- ifelse(dat_dte$time_since_A==dat_dte$num_post_months, dat_dte$num_post_months, NA)
# I think this is wrong because it excludes time t=0, which is an intervention period 

# calculate the truth for the average effect in the post-period 
dte_truth_avg <- (DTE[1]*sum(dat_dte$time_since_A>=0 & dat_dte$time_since_A<12 & dat_dte$ever_A==1, na.rm=T) + 
                    DTE[2]*sum(dat_dte$time_since_A>=12 & dat_dte$time_since_A<24  & dat_dte$ever_A==1, na.rm=T) +
                    DTE[3]*sum(dat_dte$time_since_A>=24  & dat_dte$ever_A==1, na.rm=T))/sum(dat_dte$time_since_A>=0, na.rm=T)
  
  #sum(dat_dte$num_post_months,na.rm=T)


# calculate number of state-months of treatment for denominator, state-months of each treatment size in numerator
# but for ever adopted group, need to exclude last treated group 
dat_dte_i <- dat_dte_i %>% group_by(State) %>% mutate(num_post_months = max(time_since_A))
dat_dte_i$num_post_months_ea <- ifelse(dat_dte_i$time_since_A==dat_dte_i$num_post_months & dat_dte_i$A_time<72, dat_dte_i$num_post_months, NA)

# calculate the truth for the average effect in the post-period 
dte_truth_avg_ea <- (DTE[1]*sum(dat_dte_i$time_since_A>=0 & dat_dte_i$time_since_A<12 & dat_dte_i$ever_A==1 & dat_dte_i$A_time<72, na.rm=T) + 
                    DTE[2]*sum(dat_dte_i$time_since_A>=12 & dat_dte_i$time_since_A<24  & dat_dte_i$ever_A==1 & dat_dte_i$A_time<72, na.rm=T) +
                    DTE[3]*sum(dat_dte_i$time_since_A>=24  & dat_dte_i$ever_A==1 & dat_dte_i$A_time<72, na.rm=T))/sum(dat_dte_i$time_since_A>=0, na.rm=T)
  
  #sum(dat_dte_i$num_post_months_ea,na.rm=T)



# combine results 
df_dte_avg <- data.frame(rbind(cbind(estimator="truth", result=dte_truth_avg, lb = NA, ub = NA), 
                               cbind(estimator = "TWFE", result = summary(m1_dte)$coefficients["A", "Estimate"], 
                                     lb = summary(m1_dte)$coefficients["A", "Estimate"] - 1.96*sqrt(m1_dte_var["A","A"]), 
                                     ub = summary(m1_dte)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_dte_var["A","A"])), 
                               cbind(estimator = "group.time.ATT", result = m2_dte_ag$overall.att, 
                                     lb = m2_dte_ag$overall.att - 1.96*m2_dte_ag$overall.se, 
                                     ub = m2_dte_ag$overall.att + 1.96*m2_dte_ag$overall.se))) 

df_dte_avg_ea <- data.frame(rbind(cbind(estimator="truth", result=dte_truth_avg_ea, lb = NA, ub = NA), 
                               cbind(estimator = "TWFE.ever.adopted", result = summary(m1_dte_i)$coefficients["A", "Estimate"], 
                                     lb = summary(m1_dte_i)$coefficients["A", "Estimate"] - 1.96*sqrt(m1_dte_var_i["A","A"]), 
                                     ub = summary(m1_dte_i)$coefficients["A", "Estimate"] + 1.96*sqrt(m1_dte_var_i["A","A"])), 
                               cbind(estimator = "group.time.ATT.ever.adopted", result = m2_dte_ea_ag$overall.att, 
                                     lb = m2_dte_ea_ag$overall.att - 1.96*m2_dte_ea_ag$overall.se, 
                                     ub = m2_dte_ea_ag$overall.att + 1.96*m2_dte_ea_ag$overall.se)))


df_dte_avg$result <- as.numeric(df_dte_avg$result)
df_dte_avg$lb <- as.numeric(df_dte_avg$lb)
df_dte_avg$ub <- as.numeric(df_dte_avg$ub)

df_dte_avg$type <- "DTE.avg"

df_dte_avg_ea$result <- as.numeric(df_dte_avg_ea$result)
df_dte_avg_ea$lb <- as.numeric(df_dte_avg_ea$lb)
df_dte_avg_ea$ub <- as.numeric(df_dte_avg_ea$ub)

df_dte_avg_ea$type <- "DTE.avg.EA"

# make wide so results can be stacked 
df_dte_avg_wide <- df_dte_avg %>% pivot_wider(names_from=c(type, estimator), values_from=c(result, lb, ub))
df_dte_avg_ea_wide <- df_dte_avg_ea %>% pivot_wider(names_from=c(type, estimator), values_from=c(result, lb, ub))

##############################################################################################################################
# YEARLY EFFECT IN THE POST-PERIOD  
##############################################################################################################################

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

# estimate effects using group-time ATT  
m2_dte_yr <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_dte, anticipation=0, cband=TRUE)
m2_dte_yr_ag <- aggte(m2_dte_yr, type="dynamic", cband=TRUE)
#summary(m2_hte_ag) 

#ggdid(m2_dte_yr_ag)

# estimate TWFE model only among those who ever get the intervention 
dat_dte_i <- dat_dte_i %>% filter(ever_A==1)
m1_dte_yr_i <- glm(Y ~ factor(time_since_A)  +  factor(State) + factor(month_ind), data=dat_dte_i, family="gaussian")
# don't use sandwich estimator for variance -- same reason as above
#m1_dte_yr_var_i <- sandwich(m1_dte_yr_i)
#test <- vcovHC(m1_dte_yr_i, type="HC3")
#summary(m1_dte_yr_i)$coefficients["factor(time_since_A)0", "Std. Error"]
#sqrt(m1_dte_yr_var_i["factor(time_since_A)0","factor(time_since_A)0"])
#sqrt(test["factor(time_since_A)0","factor(time_since_A)0"])

# estimate effects using group-time ATT among those who ever get the intervention 
m2_dte_yr_ea <- att_gt(yname="Y", tname="month_ind", idname="FIPS", gname="A_time", data=dat_dte_i, anticipation=0, cband=TRUE, control_group = "notyettreated")
m2_dte_yr_ea_ag <- aggte(m2_dte_yr_ea, type="dynamic", cband=TRUE)


# dynamic results 
# define the truth 
dte_truth <- data.frame(estimator="truth", time= (min(dat_dte$time_since_A, na.rm=T)+1):max(dat_dte$time_since_A, na.rm=T), 
                        result=c(rep(0,-1*min(dat_dte$time_since_A, na.rm=T) - 1), 
                                 rep(DTE[1], 12), 
                                 rep(DTE[2], 12), 
                                 rep(DTE[3], max(dat_dte$time_since_A, na.rm=T) - 23)), lb=NA, ub=NA)

# twfe -- save estimates from model and get standard errors from sandwich variance matrix 
# note: sandwich variance matrix is giving smaller estimates of variance than we're getting from the model 
# update: this is likely because of small sample size, see above for more detail.  use model estimates of SE instead
twfe <- data.frame(cbind(result = summary(m1_dte_yr)$coefficients))

# keep only coefficients that define effects of interest
twfe <- twfe %>% filter(grepl("time_since_A", rownames(twfe))) %>% rename(result = Estimate, SE = Std..Error) %>% select(result, SE)

# calculate upper and lower 95% confidence interval bounds
twfe <- twfe %>% mutate(lb = result - 1.96*SE, ub = result + 1.96*SE, estimator="TWFE", time=(min(dat_dte$time_since_A, na.rm=T)+1):max(dat_dte$time_since_A, na.rm=T)) %>% dplyr::select(estimator, time, result, lb, ub)


# group-time ATT 
gt <- data.frame(cbind(result = m2_dte_yr_ag$att.egt, SE = m2_dte_yr_ag$se.egt, time=m2_dte_yr_ag$egt, c = m2_dte_yr_ag$crit.val.egt))
gt$estimator <-  "group.time.ATT"

# the critical value is used instead of 1.96 to get the simultaneous 95% CI's 
gt$lb <- gt$result - gt$c*gt$SE
gt$ub <- gt$result + gt$c*gt$SE
gt$SE <- gt$c <- NULL 

# TWFE for ever adopted 

twfe_ea <- data.frame(cbind(result = summary(m1_dte_yr_i)$coefficients))

# keep only coefficients that define effects of interest
twfe_ea <- twfe_ea %>% filter(grepl("time_since_A", rownames(twfe_ea))) %>% rename(result = Estimate, SE = Std..Error) %>% select(result, SE)

# calculate upper and lower 95% confidence interval bounds
twfe_ea <- twfe_ea %>% mutate(lb = result - 1.96*SE, ub = result + 1.96*SE, estimator="TWFE.ever.adopted", time=(min(dat_dte$time_since_A, na.rm=T)+1):max(dat_dte$time_since_A, na.rm=T)) %>% dplyr::select(estimator, time, result, lb, ub)


# group-time ATT for ever adopted
gt_ea <- data.frame(cbind(result = m2_dte_yr_ea_ag$att.egt, SE = m2_dte_yr_ea_ag$se.egt, time=m2_dte_yr_ea_ag$egt, c = m2_dte_yr_ea_ag$crit.val.egt))
gt_ea$estimator <-  "group.time.ATT.ever.adopted"

# the critical value is used instead of 1.96 to get the simultaneous 95% CI's 
gt_ea$lb <- gt_ea$result - gt_ea$c*gt_ea$SE
gt_ea$ub <- gt_ea$result + gt_ea$c*gt_ea$SE
gt_ea$SE <- gt_ea$c <- NULL 

# combine all dynamic results
df_dyn <- data.frame(rbind(dte_truth, twfe, gt, twfe_ea, gt_ea))
df_dyn$type <- paste0("DTE.",df_dyn$time)

# make wide so results can be stacked 
df_dyn_wide <- df_dyn %>% select(-time) %>%  pivot_wider(names_from=c(type, estimator), values_from=c(result, lb, ub))

# output overall results
overall_result <- data.frame(cbind(df_cte_wide, df_hte_wide, df_hte_ea_wide, df_dte_avg_wide, df_dte_avg_ea_wide, df_dyn_wide))
return(overall_result)
}


results_ls <- lapply(1:10, function(x) sim_rep(x, dat=dat, CTE = -0.02, HTE = c(-0.01, -0.02), DTE = c(-0.01, -0.015, -0.02)))

results_df <- data.frame(do.call(rbind, results_ls))

write.csv(results_df, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_results_PTB_uniform.csv", row.names = F)


results_df_calc <- results_df %>% pivot_longer(cols= everything(), names_to=c("estimand", "parameter", "method"), names_sep="_")
results_df_calc <- results_df_calc %>% group_by(estimand, parameter, method) %>% mutate(iteration = row_number())

results_df_calc <- results_df_calc %>% group_by(iteration, parameter) %>% mutate(truth = value[estimand=="result" & method=="truth"])  

results_df_calc <- results_df_calc %>% group_by(iteration, parameter, method) %>% mutate(coverage = as.numeric(value[estimand=="lb"]<=truth & value[estimand=="ub"]>=truth), 
                                                                                                bias = value[estimand=="result"] - truth, 
                                                                                                MSE = (value[estimand=="result"] - truth)^2)


results_df_summary <- results_df_calc %>% filter(estimand=="result" & method!="truth")
results_df_summary$parameter[results_df_summary$parameter=="HTE.EA"] <-  "HTE"
results_df_summary$parameter[results_df_summary$parameter=="DTE.avg.EA"] <-  "DTE.avg"

results_df_summary <- results_df_summary %>% group_by(parameter, method) %>% summarise(coverage = mean(coverage), 
                                                                                       bias = mean(bias), 
                                                                                       MSE = mean(MSE))

write.csv(results_df_summary, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_results_summary_PTB_uniform.csv", row.names = F)

# plot results 
facet_labels <- c(CTE = "Constant treatment effect", HTE = "Heterogeneous treatment effect", DTE.avg = "Average of dynamic treatment effect")


p1 <- ggplot(results_df_summary %>% filter(parameter %in% c("CTE","HTE","DTE.avg"))) + 
  geom_point(aes(x=method, y=coverage, shape=method, color=method), size=5) + theme_bw() + 
  facet_wrap(~parameter, labeller = labeller(parameter = facet_labels)) + 
  geom_hline(aes(yintercept=0.95), linetype=3) + labs(x="", y="95% confidence interval coverage") + 
  scale_x_discrete(labels=c("group-time \nATT", "ever-treated \ngroup-time ATT","TWFE", "ever-treated \nTWFE")) + 
  theme(legend.position = "none") + scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"))

p1
#ggsave(p1, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_coverage_PTB.pdf", width=10)

p2 <- ggplot(results_df_summary %>% filter(parameter %in% c("CTE","HTE","DTE.avg")))  + 
  geom_point(aes(x=method, y=bias, shape=method, color=method), size=5) + theme_bw() + 
  facet_wrap(~parameter, labeller = labeller(parameter = facet_labels)) + 
  geom_hline(aes(yintercept=0), linetype=3) + labs(x="", y="Bias") + 
  scale_x_discrete(labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) + 
  theme(legend.position = "none") + scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"))

p2
#ggsave(p2, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_bias_PTB.pdf", width=10)


p3 <- ggplot(results_df_summary %>% filter(parameter %in% c("CTE","HTE","DTE.avg"))) + 
  geom_point(aes(x=method, y=MSE, shape=method, color=method), size=5) + theme_bw() + 
  facet_wrap(~parameter, labeller = labeller(parameter = facet_labels)) + 
  geom_hline(aes(yintercept=0), linetype=3) + labs(x="", y="MSE") + 
  scale_x_discrete(labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) + 
  theme(legend.position = "none") + scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3", "#e34a33"))

p3
#ggsave(p3, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_mse_PTB.pdf", width=10)


# dynamic effects 
results_df_summary <- results_df_summary %>% mutate(time_pt = substr(parameter,4,8))
results_df_summary <- results_df_summary %>% mutate(time_pt = ifelse(parameter=="DTE.avg", "", time_pt))
results_df_summary <- results_df_summary %>% mutate(time_pt = sub("..","-",time_pt, fixed = TRUE))
results_df_summary <- results_df_summary %>% mutate(time_pt = as.numeric(sub(".","",time_pt, fixed = TRUE)))
results_df_summary <- results_df_summary %>% mutate(pre_post = as.numeric(time_pt>=0))


# coverage
p4 <- ggplot(results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg"))) + 
  geom_point(aes(x=method, y=coverage, shape=method, color=method, alpha=pre_post), size=5) + theme_bw() + 
  labs(x="", y="95% confidence interval coverage") + 
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"), labels=c("group-time \nATT", "ever-treated \ngroup-time ATT","TWFE", "ever-treated \nTWFE")) +
  theme_bw() +   scale_x_discrete(labels=c("group-time \nATT", "ever-treated \ngroup-time ATT","TWFE", "ever-treated \nTWFE")) +
 theme(legend.position = "none")  +   geom_text_repel(aes(x=method, y=coverage, label = time_pt)) 
  
  
p4

# bias
p5 <- ggplot(results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg"))) + 
  geom_point(aes(x=method, y=bias, shape=method, color=method, alpha=pre_post), size=5) + theme_bw() + 
  labs(x="", y="bias") + 
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"), labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) +
  theme_bw() +   scale_x_discrete(labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) +
  geom_text_repel(aes(x=method, y=bias, label = time_pt)) +  theme(legend.position = "none") 

p5

# MSE
p6 <- ggplot(results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg"))) + 
  geom_point(aes(x=method, y=MSE, shape=method, color=method, alpha=pre_post), size=5) + theme_bw() + 
  labs(x="", y="MSE") + 
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"), labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) +
  theme_bw() +   scale_x_discrete(labels=c("group-time \nATT","ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) +
  geom_text_repel(aes(x=method, y=MSE, label = time_pt)) +  theme(legend.position = "none") 

p6

