library(tidyverse)
library(magrittr)

results_df_summary <- read_csv("./results/twfe_sim_results_summary_PTB_n1000_12062022.csv") %>%
  mutate(setting = "Original")
results_df_summary_u <- read_csv("./results/twfe_uniform_sim_results_summary_PTB_n1000_07112022.csv") %>%
  mutate(setting = "Uniform event times")
results_df_summary_ue <- read_csv("./results/twfe_uniform_sim_results_extended_followup_summary_PTB_n1000_07112022.csv") %>%
  mutate(setting = "Extended follow-up")
results_df_summary_e <- read_csv("./results/twfe_sim_results_extended_followup_summary_PTB_n1000_ext_07112022.csv") %>%
  mutate(setting = "Uniform and extended")

results_all <- bind_rows(results_df_summary, results_df_summary_u, results_df_summary_e, results_df_summary_ue)

results_all %<>% 
  mutate(method2 = fct_relevel(method, c("TWFE",
                                         "group.time.ATT", "staggered.SA", 
                                         "target.trial", "stacked.regression",
                                         "TWFE.ever.adopted", 
                                         "group.time.ATT.ever.adopted")),
         method3 = case_when(method == "TWFE" ~ 1, 
                             method == "group.time.ATT" ~ 2, 
                             method == "staggered.SA" ~ 3, 
                             method %in% c("target.trial", "stacked.regression") ~ 4, 
                             method ==  "TWFE.ever.adopted" ~ 5, 
                             method == "group.time.ATT.ever.adopted" ~ 6),
         method4 = case_when(method2 == "TWFE" ~ "TWFE",
                             method2 == "group.time.ATT" ~ "Group-time ATT",
                             method2 == "staggered.SA" ~ "Cohort ATT",
                             method2 == "target.trial" ~ "Target trial",
                             method2 == "stacked.regression" ~ "Target trial",
                             method2 == "TWFE.ever.adopted" ~ "Ever-treated TWFE",
                             method2 == "group.time.ATT.ever.adopted" ~ "Ever-treated group-time ATT"),
         method4 = fct_relevel(method4, c("TWFE", "Group-time ATT", "Cohort ATT",
                                          "Target trial", 
                                          "Ever-treated TWFE", "Ever-treated group-time ATT"))
  )


results_all %<>% mutate(setting = fct_relevel(setting, c("Original", "Uniform event times", 
                                              "Extended follow-up", "Uniform and extended")))

#start with the non-dynamic plots
results2 <- results_all %>% filter(parameter %in% c("CTE","HTE","DTE.avg"))

results2 %<>% mutate(parameter2 = fct_relevel(parameter, "CTE", "HTE", "DTE.avg"))
table(results2$parameter2)

# plot results 
facet_labels <- c(CTE = "Constant treatment effect", 
                  HTE = "Heterogeneous treatment effect", 
                  DTE.avg = "Average dynamic treatment effect")

p1 <- ggplot(results2, aes(x = method3, y = coverage)) + 
  geom_rect(aes(xmin = 4.5, xmax = 6.5, ymin = 0.0, ymax = 1.1),
            fill = "lightgrey", alpha = 0.5) +
  geom_point(aes(fill = setting), size=5, pch = 21, alpha = 0.7) + #color = method
  geom_text_repel(aes(label = coverage)) + #, position = position_nudge(x = 0.1, y = 0.04)) + 
  theme_bw(base_size = 15) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  geom_hline(aes(yintercept=0.95), linetype=3) + 
  labs(x = "", y = "Coverage") + 
  scale_x_continuous(breaks = c(1:6), labels=c("TWFE", "Group-time \nATT",
                                               "Cohort ATT", "Target trial", 
                                               "Ever-treated \nTWFE",
                                               "Ever-treated \ngroup-time ATT")) +
  scale_fill_manual(values = c("black", "red", "blue", "purple")) + 
  theme(axis.text.x=element_text(angle = 45, vjust = 0.1)) + #angle = 45) 
  scale_y_continuous(labels = scales::percent) 

p1

# ggsave(p1, file="../TWFE-simulation/results/twfe_all_sim_coverage_PTB_n1000.png", 
#        width=15, height = 5, device = "png")

p2 <- ggplot(results2, aes(x= method3, y = bias))  + 
  geom_rect(aes(xmin = 4.5, xmax = 6.5, ymin = -0.006, ymax = 0.01),
            fill = "lightgrey", alpha = 0.5) +
  geom_point(aes(fill = setting), size=5, pch = 21, alpha = 0.7) + #color = method
  geom_text_repel(aes(label = round(bias, 5))) + # position = position_nudge(x = 0.2, y = 0.00001)
  theme_bw(base_size = 15) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  labs(x="", y="Bias") + 
  scale_x_continuous(breaks = c(1:6), labels=c("TWFE", "Group-time \nATT",
                                               "Cohort ATT", "Target trial", 
                                               "Ever-treated \nTWFE",
                                               "Ever-treated \ngroup-time ATT")) +
  scale_fill_manual(values = c("black", "red", "blue", "purple")) + 
  theme(axis.text.x=element_text(angle = 45, vjust = 0.1))  + 
  theme(legend.position = "none") 

p2

# ggsave(p2, file="../TWFE-simulation/results/twfe_all_sim_bias_PTB_n1000.png", 
#        width=15, height = 5, device = "png")

p3 <- ggplot(results2, aes(x = method3, y = MSE)) + 
  geom_rect(aes(xmin = 4.5, xmax = 6.5, ymin = 0, ymax = 9.2e-05),
            fill = "lightgrey", alpha = 0.5) +
  geom_point(aes(fill = setting), size=5, pch = 21, alpha = 0.7) + #color = method
  geom_text_repel(aes(label = round(MSE, 7))) +
  theme_bw(base_size = 15) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  labs(x="", y="Mean Squared Error") + 
  scale_x_continuous(breaks = c(1:6), labels=c("TWFE", "Group-time \nATT",
                                               "Cohort ATT", "Target trial", 
                                               "Ever-treated \nTWFE",
                                               "Ever-treated \ngroup-time ATT")) +
  scale_fill_manual(values = c("black", "red", "blue", "purple")) + 
  theme(axis.text.x=element_text(angle = 45, vjust = 0.1))  + 
  theme(legend.position = "none")  

p3

# ggsave(p3, file="../TWFE-simulation/results/twfe_all_sim_mse_PTB_n1000.png", 
#        width=15, height = 5, device = "png")

p4 <- ggplot(results2, aes(x = method3, y = power)) + 
  geom_rect(aes(xmin = 4.5, xmax = 6.5, ymin = 0.5, ymax = 1.1),
            fill = "lightgrey", alpha = 0.5) +
  geom_point(aes(fill = setting), size=5, pch = 21, alpha = 0.7) + #color = method 
  geom_text_repel(aes(label = sprintf(power, fmt = '%#.2f'))) +
  theme_bw(base_size = 15) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  #geom_hline(aes(yintercept=0), linetype=3) + 
  labs(x="", y="Power") + 
  scale_x_continuous(breaks = c(1:6), labels=c("TWFE", "Group-time \nATT",
                                               "Cohort ATT", "Target trial", 
                                               "Ever-treated \nTWFE",
                                               "Ever-treated \ngroup-time ATT")) +
  scale_fill_manual(values = c("black", "red", "blue", "purple")) + 
  theme(axis.text.x=element_text(angle = 45, vjust = 0.1))  + 
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) 

p4
# ggsave(p4, file="../TWFE-simulation/results/twfe_all_sim_power_PTB_n1000.png", 
#        width = 15, height = 5, device = "png")

all_1 <- p1 + p2 + p3 + p4 + plot_layout(nrow = 4)
all_1
# ggsave(all_1, file="../TWFE-simulation/results/twfe_sim_all2_PTB_n1000.png", 
#        width=15, height = 20, device = "png")


results_all %<>% 
  mutate(ever.adopted.est = 
           case_when(method %in% c("TWFE.ever.adopted", "group.time.ATT.ever.adopted") ~ "Ever-treated only",
                     !(method %in% c("TWFE.ever.adopted", "group.time.ATT.ever.adopted")) ~ "All states"))

# dynamic effects 
results_all <- results_all %>% mutate(time_pt = substr(parameter, 4, 8))
results_all <- results_all %>% mutate(time_pt = ifelse(parameter=="DTE.avg", "", time_pt))
results_all <- results_all %>% mutate(time_pt = sub("..","-",time_pt, fixed = TRUE))
results_all <- results_all %>% mutate(time_pt = as.numeric(sub(".","",time_pt, fixed = TRUE)))
results_all <- results_all %>% mutate(pre_post = as.numeric(time_pt>=0))

# coverage
results3 <- results_all %>% filter(!parameter %in% c("CTE","HTE","DTE.avg"))

p41 <- ggplot(results3,
              aes(x = time_pt, y = coverage)) +
  geom_line(aes(col = setting), alpha = 0.75) +
  geom_hline(yintercept = 0.95) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(y = "Coverage", x = "Event Time") +
  theme_bw(base_size = 15) + 
  theme(legend.title=element_blank()) + 
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("black", "red", "blue", "purple")) + 
  facet_wrap(~method4, nrow = 1)
p41

ggsave(p41, file="../TWFE-simulation/results/all_dyn_coverage_n1000.png", 
       width=15,height = 3.5, device = png)

p51 <- ggplot(results3,
              aes(x = time_pt, y = bias)) +
  geom_line(aes(col = setting), alpha = 0.75) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(y = "Bias", x = "Event Time") +
  theme_bw(base_size = 15) + 
  theme(legend.title=element_blank()) + 
  scale_color_manual(values = c("black", "red", "blue", "purple")) + 
  facet_wrap(~method4, nrow = 1)

p51

ggsave(p51, file="../TWFE-simulation/results/all_dyn_bias_n1000_PTB.png", 
       width=15,height = 3.5, device = png)

p61 <- ggplot(results3,
              aes(x = time_pt, y = MSE)) +
  geom_line(aes(col = setting), alpha = 0.75) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(y = "Mean squared error", x = "Event Time") +
  theme_bw(base_size = 15)  + 
  theme(legend.title=element_blank()) + 
  scale_color_manual(values = c("black", "red", "blue", "purple")) +
  scale_y_continuous(limits = c(NA, 0.000125)) + 
  facet_wrap(~method4, nrow = 1)

p61

ggsave(p61, file="../TWFE-simulation/results/all_dyn_mse_n1000_PTB.png", 
       width=15,height = 3.5, device = png)

#power 

p71 <- ggplot(results3,
              aes(x = time_pt, y = power)) +
  geom_line(aes(col = setting), alpha = 0.75) +
  labs(y = "Power", x = "Event Time") +
  theme_bw(base_size = 15) + 
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank()) + 
  scale_color_manual(values = c("black", "red", "blue", "purple")) +
  scale_x_continuous(limits = c(0, NA)) +
  facet_wrap(~method4, nrow = 1)

p71

ggsave(p71, file="../TWFE-simulation/results/all_dyn_power_n1000_PTB.png", 
       width=15, height = 3.5, device = png)

dyn_all <- p41 + p51 + p61 + p71 + plot_layout(nrow = 4, guides = "collect") & theme(legend.position = "bottom")

dyn_all

ggsave(dyn_all, file="../TWFE-simulation/results/dyn_all2_n1000_PTB.png", 
       width=18, height = 18, device = png)
