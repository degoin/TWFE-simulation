library(tidyverse)
library(ggrepel)
library(magrittr)
library(forcats)
library(patchwork)

results_df_summary <- read_csv("./results/twfe_sim_results_extended_followup_summary_PTB_n1000_ext_05092022.csv")
results_df_summary_compare <- read_csv("./results/twfe_sim_results_summary_PTB_n1000_05022022.csv")


table(results_df_summary$parameter)
#CTE, many DTE and HTE

table(results_df_summary$method)

results_df_summary %<>% 
  mutate(method2 = fct_relevel(method, c("TWFE", "group.time.ATT", "staggered.SA", "stacked.regression",
                                         "TWFE.ever.adopted", "group.time.ATT.ever.adopted")),
         method3 = case_when(method == "TWFE" ~ 1, 
                             method == "group.time.ATT" ~ 2, 
                             method == "staggered.SA" ~ 3, 
                             #method == "stacked.regression" ~ 4, 
                             method ==  "TWFE.ever.adopted" ~ 4, 
                             method == "group.time.ATT.ever.adopted" ~ 5))

results_df_summary_compare %<>% 
  mutate(method2 = fct_relevel(method, c("TWFE", "group.time.ATT", "staggered.SA", "stacked.regression",
                                         "TWFE.ever.adopted", "group.time.ATT.ever.adopted")),
         method3 = case_when(method == "TWFE" ~ 1, 
                             method == "group.time.ATT" ~ 2, 
                             method == "staggered.SA" ~ 3, 
                             #method == "stacked.regression" ~ 4, 
                             method ==  "TWFE.ever.adopted" ~ 4, 
                             method == "group.time.ATT.ever.adopted" ~ 5))


#check that the reorder worked:
table(results_df_summary$method2)

#start with the non-dynamic plots
results2 <- results_df_summary %>% 
  filter(parameter %in% c("CTE","HTE","DTE.avg")) %>%
  mutate(parameter2 = fct_relevel(parameter, "CTE", "HTE", "DTE.avg"))

results2_compare <- results_df_summary_compare %>% 
  filter(parameter %in% c("CTE","HTE","DTE.avg")) %>%
  mutate(parameter2 = fct_relevel(parameter, "CTE", "HTE", "DTE.avg"))

table(results2$parameter2)

col_ref <- "#3399ff"
col_new <- "black"
# plot results 
facet_labels <- c(CTE = "Constant treatment effect", HTE = "Heterogeneous treatment effect", DTE.avg = "Average dynamic treatment effect")

p1 <- ggplot(results2, aes(x = method3, y = coverage)) + 
  geom_rect(aes(xmin = 3.5, xmax = 5.5, ymin = 0.0, ymax = 1.1),
            fill = "lightgrey", alpha = 0.5) +
  geom_point(data = results2_compare, aes(col = "Previous results"), size = 5) + 
  geom_point(aes(col = "Extended follow-up"), size=5) + #color = method
  geom_text(aes(label = coverage), position = position_nudge(x = 0.1, y = 0.04)) + 
  theme_bw(base_size = 15) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  geom_hline(aes(yintercept=0.95), linetype=3) + 
  labs(x = "", y = "Coverage") + 
  scale_x_continuous(breaks = c(1:5), labels=c("TWFE", "Group-time \nATT", 
                                               "Staggered SA", "Ever-treated \nTWFE",
                                               "Ever-treated \ngroup-time ATT")) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.1)) + #angle = 45) 
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c(col_new, col_ref))


p1
#ggsave(p1, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_coverage_PTB.pdf", width=10)
ggsave(p1, file="../TWFE-simulation/results/twfe_sim_coverage_PTB_n1000_ext.png", 
       width=15, height = 5, device = "png")

extremes <- results2 %>% summarise(min = min(bias), max = max(bias))

p2 <- ggplot(results2, aes(x= method3, y = bias))  + 
  geom_rect(aes(xmin = 3.5, xmax = 5.5, 
                ymin = extremes %>% pull(min) - 0.002, 
                ymax = extremes %>% pull(max) + 0.002),
                #ymin = -0.002, ymax = 0.005),
            fill = "lightgrey", alpha = 0.5) +
  geom_point(data = results2_compare, aes(col = "Previous results"), size = 5) + 
  geom_point(aes(col = "Extended follow-up"), size=5) +
  geom_text_repel(aes(label = round(bias, 5))) + # position = position_nudge(x = 0.2, y = 0.00001)
  theme_bw(base_size = 15) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  labs(x="", y="Bias") + 
  scale_x_continuous(breaks = c(1:5), labels=c("TWFE", "Group-time \nATT", 
                                               "Staggered SA", "Ever-treated \nTWFE",
                                               "Ever-treated \ngroup-time ATT")) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.1))  + 
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c(col_new, col_ref))

p2
#ggsave(p2, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_bias_PTB.pdf", width=10)
#ggsave(p2, file="../TWFE-simulation/results/twfe_sim_bias_PTB.pdf", width=10)
ggsave(p2, file="../TWFE-simulation/results/twfe_sim_bias_PTB_n1000_ext.png", 
       width=15, height = 5, device = "png")

extremes <- results2 %>% summarise(min = min(MSE), max = max(MSE))

p3 <- ggplot(results2, aes(x = method3, y = MSE)) + 
  geom_rect(aes(xmin = 3.5, xmax = 5.5,
                ymin = extremes %>% pull(min) - 0.000002, 
                ymax = extremes %>% pull(max) + 0.000002),
            #ymin = 0, ymax = 2.4e-05),
            fill = "lightgrey", alpha = 0.5) +
  geom_point(data = results2_compare, aes(col = "Previous results"), size = 5) + 
  geom_point(aes(col = "Extended follow-up"), size=5) + 
  geom_text_repel(aes(label = round(MSE, 7))) +
  theme_bw(base_size = 15) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  labs(x="", y="Mean Squared Error") + 
  scale_x_continuous(breaks = c(1:5), labels=c("TWFE", "Group-time \nATT", 
                                               "Staggered SA", "Ever-treated \nTWFE",
                                               "Ever-treated \ngroup-time ATT")) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.1))  + 
  theme(legend.title = element_blank())  +
  scale_color_manual(values = c(col_new, col_ref))

p3

ggsave(p3, file="../TWFE-simulation/results/twfe_sim_mse_PTB_n1000_ext.png", 
       width=15, height = 5, device = "png")

extremes <- results2 %>% summarise(min = min(power), max = max(power))

p4 <- ggplot(results2, aes(x = method3, y = power)) + 
  geom_rect(aes(xmin = 3.5, xmax = 5.5, 
                ymin = extremes %>% pull(min) - 0.1, 
                ymax = extremes %>% pull(max) + 0.1),
            #ymin = 0, ymax = 2.4e-05),
            #ymin = 0.5, ymax = 1.1),
            fill = "lightgrey", alpha = 0.5) +
  geom_point(data = results2_compare, aes(col = "Previous results"), size = 5) + 
  geom_point(aes(col = "Extended follow-up"), size=5) + 
  geom_text_repel(aes(label = sprintf(power, fmt = '%#.2f'))) +
  theme_bw(base_size = 15) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  #geom_hline(aes(yintercept=0), linetype=3) + 
  labs(x="", y="Power") + 
  scale_x_continuous(breaks = c(1:5), labels=c("TWFE", "Group-time \nATT", 
                                               "Staggered SA", "Ever-treated \nTWFE",
                                               "Ever-treated \ngroup-time ATT")) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.1))  + 
  theme(legend.title = element_blank())  +
  scale_color_manual(values = c(col_new, col_ref)) + 
  scale_y_continuous(labels = scales::percent) 

p4
ggsave(p4, file="../TWFE-simulation/results/twfe_sim_power_PTB_n1000_ext.png", 
       width=15, height = 5, device = "png")

results_df_summary %<>% mutate(ever.adopted.est = case_when(method %in% c("TWFE.ever.adopted", "group.time.ATT.ever.adopted") ~ "Ever-treated only", 
                                                            !(method %in% c("TWFE.ever.adopted", "group.time.ATT.ever.adopted")) ~ "All states"))

