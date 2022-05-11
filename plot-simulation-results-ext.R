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

# plot results 
facet_labels <- c(CTE = "Constant treatment effect", HTE = "Heterogeneous treatment effect", DTE.avg = "Average dynamic treatment effect")

p1 <- ggplot(results2, aes(x = method3, y = coverage)) + 
  geom_rect(aes(xmin = 3.5, xmax = 5.5, ymin = 0.0, ymax = 1.1),
            fill = "lightgrey", alpha = 0.5) +
  geom_point(data = results2_compare, aes(col = "Previous results"), size = 5) + 
  geom_point(size=5) + #color = method
  geom_text(aes(label = coverage), position = position_nudge(x = 0.1, y = 0.04)) + 
  theme_bw(base_size = 15) + 
  facet_wrap(~parameter2, labeller = labeller(parameter2 = facet_labels)) + 
  geom_hline(aes(yintercept=0.95), linetype=3) + 
  labs(x = "", y = "Coverage") + 
  scale_x_continuous(breaks = c(1:5), labels=c("TWFE", "Group-time \nATT", 
                                               "Staggered SA", "Ever-treated \nTWFE",
                                               "Ever-treated \ngroup-time ATT")) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.1)) + #angle = 45) 
  #theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = "#737373")
#coord_flip()
#+ 
#scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"))

p1
#ggsave(p1, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_coverage_PTB.pdf", width=10)
ggsave(p1, file="../TWFE-simulation/results/twfe_sim_coverage_PTB_n1000_ext.png", 
       width=15, height = 5, device = "png")

