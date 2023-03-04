library(tidyverse)
library(ggrepel)
library(magrittr)
library(forcats)
library(patchwork)
library(scales)

results_df_summary <- read_csv("./results/twfe_sim_results_summary_PTB_n1000_12062022.csv")
#results_old <- read_csv("./results/twfe_sim_results_summary_PTB_n1000_06012022.csv")

table(results_df_summary$parameter)
#CTE, many DTE and HTE

table(results_df_summary$method)
# group.time.ATT group.time.ATT.ever.adopted          stacked.regression 
# staggered.SA                        TWFE           TWFE.ever.adopted 
# target.trial

results_df_summary %<>% 
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
  
#check that the reorder worked:
table(results_df_summary$method2)
table(results_df_summary$method3)
table(results_df_summary$method4)

#start with the non-dynamic plots
results2 <- results_df_summary %>% filter(parameter %in% c("CTE","HTE","DTE.avg"))

results2 %<>% mutate(parameter2 = fct_relevel(parameter, "CTE", "HTE", "DTE.avg"))
table(results2$parameter2)

# plot results 
facet_labels <- c(CTE = "Constant treatment effect", 
                  HTE = "Heterogeneous treatment effect", 
                  DTE.avg = "Average dynamic treatment effect")


p1 <- ggplot(results2, aes(x = method3, y = coverage)) + 
  annotate(geom = "rect", xmin = 4.5, xmax = 6.5, ymin = 0.0, ymax = 1.1,
           fill = "lightgrey", colour = "lightgrey", alpha = 0.5) +
  geom_point(size=5) + #color = method
  geom_text(aes(label = format(round(coverage, 2), nmsall = 2)), position = position_nudge(y = 0.08), size = 5) + 
  theme_bw(base_size = 25) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  geom_hline(aes(yintercept=0.95), linetype=3) + 
  labs(x = "", y = "Coverage") + 
  scale_x_continuous(breaks = c(1:6), expand = expansion(mult = 0.1),
                     labels=c("TWFE", "Group-time \nATT",
                                               "Cohort ATT", "Target trial", 
                                               "Ever-treated \nTWFE",
                                               "Ever-treated \ngroup-time ATT"),
guide = guide_axis(angle = 90)) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) 

p1
 ggsave(p1, file="../TWFE-simulation/results/twfe_sim_coverage_PTB_n1000.png", 
        width=15, height = 5, device = "png")

p2 <- ggplot(results2, aes(x= method3, y = bias))  + 
  annotate(geom = "rect", xmin = 4.5, xmax = 6.5, ymin = -0.002, ymax = 0.005,
           fill = "lightgrey", colour = "lightgrey", alpha = 0.5) +
  geom_point(size=5) +
  geom_text(aes(label = signif(bias, digits = 2)), nudge_y = 0.0005, size = 5) + 
  theme_bw(base_size = 25) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  labs(x="", y="Bias") + 
  scale_x_continuous(breaks = c(1:6), expand = expansion(mult = 0.1),
                     labels=c("TWFE", "Group-time \nATT",
                                               "Cohort ATT", "Target trial", 
                                               "Ever-treated \nTWFE",
                                               "Ever-treated \ngroup-time ATT"),
                     guide = guide_axis(angle = 90)) +
  theme(legend.position = "none") 

p2
 ggsave(p2, file="../TWFE-simulation/results/twfe_sim_bias_PTB_n1000.png", 
        width=15, height = 5, device = "png")

p3 <- ggplot(results2, aes(x = method3, y = MSE)) + 
  annotate(geom = "rect", xmin = 4.5, xmax = 6.5, ymin = 0, ymax = 2.4e-05,
           fill = "lightgrey", colour = "lightgrey", alpha = 0.5) +
  geom_point(size=5) + 
  geom_text(aes(label = signif(MSE, digits = 2)), nudge_y = 0.0000017, size = 5) +
  theme_bw(base_size = 25) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  labs(x="", y="Mean Squared Error") + 
  scale_x_continuous(breaks = c(1:6), expand = expansion(mult = 0.1),
                     labels=c("TWFE", "Group-time ATT",
                                               "Cohort ATT", "Target trial", 
                                               "Ever-treated\n TWFE",
                                               "Ever-treated \ngroup-time ATT"),
                     guide = guide_axis(angle = 90)) +
  scale_y_continuous(labels = scales::label_comma()) + 
  theme(legend.position = "none")  

p3
 ggsave(p3, file="../TWFE-simulation/results/twfe_sim_mse_PTB_n1000.png", 
        width=15, height = 5, device = "png")

p4 <- ggplot(results2, aes(x = method3, y = power)) + 
  geom_rect(aes(xmin = 4.5, xmax = 6.5, ymin = 0.5, ymax = 1.1),
            fill = "lightgrey", alpha = 0.5) +
  geom_point(size=5) + 
  geom_text_repel(aes(label = sprintf(power, fmt = '%#.2f'))) +
  theme_bw(base_size = 25) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  #geom_hline(aes(yintercept=0), linetype=3) + 
  labs(x="", y="Power") + 
  scale_x_continuous(breaks = c(1:6), labels=c("TWFE", "Group-time \nATT",
                                               "Cohort ATT", "Target trial", 
                                               "Ever-treated \nTWFE",
                                               "Ever-treated \ngroup-time ATT"),
                     guide = guide_axis(angle = 45)) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) 
#+ scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3", "#e34a33"))

p4
 ggsave(p4, file="../TWFE-simulation/results/twfe_sim_power_PTB_n1000.png", 
        width = 15, height = 5, device = "png")

all_1 <- p1 + p2 + p3 + plot_layout(nrow = 3)
all_1
ggsave(all_1, file="../TWFE-simulation/results/twfe_sim_all_PTB_n1000_20230303.png", 
       width=18, height = 20, device = "png")

results_df_summary %<>% 
  mutate(ever.adopted.est = 
           case_when(method %in% c("TWFE.ever.adopted", "group.time.ATT.ever.adopted") ~ "Ever-treated only",
                     !(method %in% c("TWFE.ever.adopted", "group.time.ATT.ever.adopted")) ~ "All states"))

# dynamic effects 
results_df_summary <- results_df_summary %>% mutate(time_pt = substr(parameter, 4, 8))
results_df_summary <- results_df_summary %>% mutate(time_pt = ifelse(parameter=="DTE.avg", "", time_pt))
results_df_summary <- results_df_summary %>% mutate(time_pt = sub("..","-",time_pt, fixed = TRUE))
results_df_summary <- results_df_summary %>% mutate(time_pt = as.numeric(sub(".","",time_pt, fixed = TRUE)))
results_df_summary <- results_df_summary %>% mutate(pre_post = as.numeric(time_pt>=0))

# coverage
results3 <- results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg"))

p41 <- ggplot(results3,
              aes(x = time_pt, y = coverage)) +
  geom_line() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point() +
  geom_hline(yintercept = 0.95) +
  labs(y = "Coverage", x = "Event Time") +
  theme_bw(base_size = 15) + 
  theme(legend.title=element_blank()) + 
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~method4, nrow = 1)

p41
ggsave(p41, file="../TWFE-simulation/results/dyn_coverage_n1000.png", 
       width=15,height = 3.5, device = png)

p51 <- ggplot(results3,
              aes(x = time_pt, y = bias)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0) +
  labs(y = "Bias", x = "Event Time") +
  theme_bw(base_size = 15) + 
  theme(legend.title=element_blank()) + 
  facet_wrap(~method4, nrow = 1) +
  scale_y_continuous(labels = scales::label_comma()) 

p51
#ggsave(p51, file="../TWFE-simulation/results/twfe_sim_bias3_PTB.pdf", width=10)
ggsave(p51, file="../TWFE-simulation/results/dyn_bias_n1000_PTB.png", 
       width=15,height = 3.5, device = png)

p61 <- ggplot(results3,
              aes(x = time_pt, y = MSE)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0, lty = 2) +
  labs(y = "Mean squared error", x = "Event Time") +
  theme_bw(base_size = 15)  + 
  theme(legend.title=element_blank()) + 
  facet_wrap(~method4, nrow = 1) 

p61

ggsave(p61, file="../TWFE-simulation/results/dyn_mse_n1000_PTB.png", 
       width=15,height = 3.5, device = png)

#power 

p71 <- ggplot(results3,
              aes(x = time_pt, y = power)) +
  geom_line() +
  geom_point() +
  labs(y = "Power", x = "Event Time") +
  theme_bw(base_size = 15) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(0, NA)) +
  theme(legend.title = element_blank()) + 
  facet_wrap(~method4, nrow = 1)

p71

ggsave(p71, file="../TWFE-simulation/results/dyn_power_n1000_PTB.png", 
       width=15, height = 3.5, device = png)

dyn_all <- p41 + p51 + p61 + plot_layout(nrow = 3, guides = "collect")

dyn_all

ggsave(dyn_all, file="../TWFE-simulation/results/dyn_all_n1000_PTB.png", 
       width=18, height = 15, device = png)
