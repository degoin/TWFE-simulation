library(tidyverse)
library(ggrepel)
library(magrittr)
library(forcats)
library(patchwork)

results_df_summary <- read_csv("./results/twfe_sim_results_extended_followup_summary_PTB_n1000_ext_06012022.csv")
results_df_summary_compare <- read_csv("./results/twfe_sim_results_summary_PTB_n1000_06012022.csv")


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

extremes2 <- results2 %>% summarise(min = min(bias), max = max(bias))

p2 <- ggplot(results2, aes(x = method3, y = bias))  + 
  geom_rect(aes(xmin = 3.5, xmax = 5.5, 
                ymin = extremes2 %>% pull(min) - 0.002, 
                ymax = extremes2 %>% pull(max) + 0.002),
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

extremes3 <- results2 %>% summarise(min = min(MSE), max = max(MSE))

p3 <- ggplot(results2, aes(x = method3, y = MSE)) + 
  geom_rect(aes(xmin = 3.5, xmax = 5.5,
                ymin = extremes3 %>% pull(min) - 0.000002, 
                ymax = extremes3 %>% pull(max) + 0.000002),
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

extremes4 <- results2 %>% summarise(min = min(power), max = max(power))

p4 <- ggplot(results2, aes(x = method3, y = power)) + 
  geom_rect(aes(xmin = 3.5, xmax = 5.5, 
                ymin = extremes4 %>% pull(min) - 0.1, 
                ymax = extremes4 %>% pull(max) + 0.1),
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

all_1 <- p1 + p2 + p3 + p4 + plot_layout(nrow = 4)
all_1
ggsave(all_1, file="../TWFE-simulation/results/twfe_sim_all_PTB_n1000_ext.png", 
       width=15, height = 20, device = "png")

results_df_summary %<>% 
  mutate(ever.adopted.est = case_when(
    method %in% c("TWFE.ever.adopted", "group.time.ATT.ever.adopted") ~ "Ever-treated only", 
    !(method %in% c("TWFE.ever.adopted", "group.time.ATT.ever.adopted")) ~ "All states")
    )

# dynamic effects 
results_df_summary <- results_df_summary %>% mutate(time_pt = substr(parameter, 4, 8))
results_df_summary <- results_df_summary %>% mutate(time_pt = ifelse(parameter=="DTE.avg", "", time_pt))
results_df_summary <- results_df_summary %>% mutate(time_pt = sub("..","-",time_pt, fixed = TRUE))
results_df_summary <- results_df_summary %>% mutate(time_pt = as.numeric(sub(".","",time_pt, fixed = TRUE)))
results_df_summary <- results_df_summary %>% mutate(pre_post = as.numeric(time_pt>=0))

# coverage
results3 <- results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg"))

#reference to old results
results_df_summary_compare %<>% 
  mutate(ever.adopted.est = case_when(
    method %in% c("TWFE.ever.adopted", "group.time.ATT.ever.adopted") ~ "Ever-treated only", 
    !(method %in% c("TWFE.ever.adopted", "group.time.ATT.ever.adopted")) ~ "All states")
  )

# dynamic effects 
results_df_summary_compare <- results_df_summary_compare %>% mutate(time_pt = substr(parameter, 4, 8))
results_df_summary_compare <- results_df_summary_compare %>% mutate(time_pt = ifelse(parameter=="DTE.avg", "", time_pt))
results_df_summary_compare <- results_df_summary_compare %>% mutate(time_pt = sub("..","-",time_pt, fixed = TRUE))
results_df_summary_compare <- results_df_summary_compare %>% mutate(time_pt = as.numeric(sub(".","",time_pt, fixed = TRUE)))
results_df_summary_compare <- results_df_summary_compare %>% mutate(pre_post = as.numeric(time_pt>=0))

# coverage
results3 <- results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg"))
results3_compare <- results_df_summary_compare %>% filter(!parameter %in% c("CTE","HTE","DTE.avg"))

results3 %<>% mutate(method2_f = case_when(method2 == "TWFE" ~ "TWFE",
                                           method2 == "group.time.ATT" ~ "Group-time ATT",
                                           method2 == "staggered.SA" ~ "Staggered SA",
                                           method2 == "stacked.regression" ~ "Stacked regression",
                                           method2 == "TWFE.ever.adopted" ~ "Ever-treated TWFE",
                                           method2 == "group.time.ATT.ever.adopted" ~ "Ever-treated group-time ATT")) %>%
  mutate(method2_f = fct_relevel(method2_f, c("TWFE", "Group-time ATT", "Staggered SA", "Stacked regression",
                                              "Ever-treated TWFE", "Ever-treated group-time ATT")))

results3_compare %<>% mutate(method2_f = case_when(method2 == "TWFE" ~ "TWFE",
                                           method2 == "group.time.ATT" ~ "Group-time ATT",
                                           method2 == "staggered.SA" ~ "Staggered SA",
                                           method2 == "stacked.regression" ~ "Stacked regression",
                                           method2 == "TWFE.ever.adopted" ~ "Ever-treated TWFE",
                                           method2 == "group.time.ATT.ever.adopted" ~ "Ever-treated group-time ATT")) %>%
  mutate(method2_f = fct_relevel(method2_f, c("TWFE", "Group-time ATT", "Staggered SA", "Stacked regression",
                                              "Ever-treated TWFE", "Ever-treated group-time ATT")))



p41 <- ggplot(results3,
              aes(x = time_pt, y = coverage)) +
  geom_line(data = results3_compare, aes(col = method2, alpha = "Previous results")) +
  geom_line(aes(col = method2, alpha = "Extended time")) +
  #geom_point(aes(col = method2)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0.95) +
  scale_color_manual(values=c("#9e0142", #burgandy
                              "#66c2a5",  #seafoam green
                              "grey",
                              "#4393c3", #cornflower blue)
                              "#e34a33",  #fire engine red
                              "black"
                              ),
                     labels=c("TWFE",
                              "Group-time \nATT",
                              "Staggered SA",
                              "Stacked regression",
                              "Ever-treated \nTWFE",
                              "Ever-treated \ngroup-time ATT"
                              )) +
  labs(y = "Coverage", x = "Method") +
  theme_bw(base_size = 15) + 
  theme(legend.title=element_blank()) + 
  scale_y_continuous(labels = scales::percent) +
  scale_alpha_manual(values = c(1, 0.5)) + 
  facet_wrap(~method2, nrow = 1)
  #facet_wrap(~ever.adopted.est)

p41
#ggsave(p41, file="../TWFE-simulation/results/twfe_sim_coverage3_PTB.pdf", width=10)
ggsave(p41, file="../TWFE-simulation/results/dyn_coverage_n1000_ext.png", 
       width=20,height = 4, device = png)

p51 <- ggplot(results3,
              aes(x = time_pt, y = bias)) +
  geom_line(data = results3_compare, aes(col = method2, alpha = "Previous results")) +
  geom_line(aes(col = method2, alpha = "Extended time")) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values=c("#9e0142", #burgandy
                              "#66c2a5",  #seafoam green
                              "grey",
                              "#4393c3", #cornflower blue)
                              "#e34a33",  #fire engine red
                              "black"
  ),
  labels=c("TWFE",
           "Group-time \nATT",
           "Staggered SA",
           "Stacked regression",
           "Ever-treated \nTWFE",
           "Ever-treated \ngroup-time ATT"
  )) +
  labs(y = "Bias", x = "Method") +
  theme_bw(base_size = 15) + 
  theme(legend.title=element_blank()) + 
  scale_alpha_manual(values = c(1, 0.5)) + 
  facet_wrap(~method2, nrow = 1)

p51
#ggsave(p51, file="../TWFE-simulation/results/twfe_sim_bias3_PTB.pdf", width=10)
ggsave(p51, file="../TWFE-simulation/results/dyn_bias_n1000_ext.png", 
       width=20,height = 4, device = png)

p61 <- ggplot(results3,
              aes(x = time_pt, y = MSE)) +
  geom_line(data = results3_compare, aes(col = method2, alpha = "Previous results")) +
  geom_line(aes(col = method2, alpha = "Extended time")) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values=c("#9e0142", #burgandy
                              "#66c2a5",  #seafoam green
                              "grey",
                              "#4393c3", #cornflower blue)
                              "#e34a33",  #fire engine red
                              "black"
  ),
  labels=c("TWFE",
           "Group-time \nATT",
           "Staggered SA",
           "Stacked regression",
           "Ever-treated \nTWFE",
           "Ever-treated \ngroup-time ATT"
  )) +
  labs(y = "Mean squared error", x = "Method") +
  theme_bw(base_size = 15)  + 
  theme(legend.title=element_blank()) + 
  scale_alpha_manual(values = c(1, 0.5)) + 
  facet_wrap(~method2, nrow = 1)

p61
#ggsave(p61, file="../TWFE-simulation/results/twfe_sim_mse3_PTB.pdf", width=10)

ggsave(p61, file="../TWFE-simulation/results/dyn_mse_n1000_ext.png", 
       width=20,height = 4, device = png)

#power 

p71 <- ggplot(results3,
              aes(x = time_pt, y = power)) +
  geom_line(data = results3_compare, aes(col = method2, alpha = "Previous results")) +
  geom_line(aes(col = method2, alpha = "Extended time")) +
  geom_vline(xintercept = 0, linetype = 2) +
  #geom_hline(yintercept = 1) +
  scale_color_manual(values=c("#9e0142", #burgandy
                              "#66c2a5",  #seafoam green
                              "grey",
                              "#4393c3", #cornflower blue)
                              "#e34a33",  #fire engine red
                              "black"
  ),
  labels=c("TWFE",
           "Group-time \nATT",
           "Staggered SA",
           "Stacked regression",
           "Ever-treated \nTWFE",
           "Ever-treated \ngroup-time ATT"
  )) +
  labs(y = "Power", x = "Method") +
  theme_bw(base_size = 15) + 
  theme(legend.title=element_blank()) + 
  scale_y_continuous(labels = scales::percent) +
  scale_alpha_manual(values = c(1, 0.5)) + 
  facet_wrap(~method2, nrow = 1)

p71

ggsave(p71, file="../TWFE-simulation/results/dyn_power_n1000_ext.png", 
       width=20, height = 4, device = png)

dyn_all <- p41 + p51 + p61 + p71 + plot_layout(nrow = 4, guides = "collect")

ggsave(dyn_all, file="../TWFE-simulation/results/dyn_all_n1000_ext.png", 
       width=20, height = 20, device = png)
