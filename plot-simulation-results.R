library(tidyverse)
library(ggrepel)

results_df_summary <- read.csv("../TWFE-simulation/results/twfe_sim_results_summary_PTB_n100.csv")

results2 <- results_df_summary %>% filter(parameter %in% c("CTE","HTE","DTE.avg"))


# plot results 
facet_labels <- c(CTE = "Constant treatment effect", HTE = "Heterogeneous treatment effect", DTE.avg = "Average of dynamic treatment effect")


p1 <- ggplot(results2, aes(x = method, y = coverage)) + 
  geom_point(size=5) + #color = method
  geom_text(aes(label = coverage), position = position_nudge(x = 0.1, y = 0.04)) + 
  theme_bw() + 
  facet_wrap(~parameter, labeller = labeller(parameter = facet_labels)) + 
  geom_hline(aes(yintercept=0.95), linetype=3) + 
  labs(x = "", y = "95% confidence interval coverage") + 
  scale_x_discrete(labels=c("Group-time \nATT", "Ever-treated \ngroup-time ATT",
                            "Stagged SA", "TWFE", "TWFE alt", 
                            "Ever-treated \n TWFE alt", "Ever-treated \nTWFE")) +
  #theme(axis.text.x=element_text()) + #angle = 45) 
  theme(legend.position = "none") #+ 
  #scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"))

p1
#ggsave(p1, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_coverage_PTB.pdf", width=10)
ggsave(p1, file="../TWFE-simulation/results/twfe_sim_coverage_PTB_n100.png", 
       width=15, height = 5, device = "png")

p2 <- ggplot(results2, aes(x= method, y = bias))  + 
  geom_point(size=5) +
  geom_text_repel(aes(label = round(bias, 5))) + # position = position_nudge(x = 0.2, y = 0.00001)
  theme_bw() + 
  facet_wrap(~parameter, labeller = labeller(parameter = facet_labels)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  labs(x="", y="Bias") + 
  scale_x_discrete(labels=c("Group-time \nATT", "Ever-treated \ngroup-time ATT",
                            "Stagged SA", "TWFE", "TWFE alt", 
                            "Ever-treated \n TWFE alt", "Ever-treated \nTWFE")) + 
  theme(legend.position = "none") 
#+ scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"))

p2
#ggsave(p2, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_bias_PTB.pdf", width=10)
#ggsave(p2, file="../TWFE-simulation/results/twfe_sim_bias_PTB.pdf", width=10)
ggsave(p2, file="../TWFE-simulation/results/twfe_sim_bias_PTB_n100.png", 
       width=15, height = 5, device = "png")


p3 <- ggplot(results2, aes(x = method, y = MSE)) + 
  geom_point(size=5) + 
  geom_text_repel(aes(label = round(MSE, 5))) +
  theme_bw() + 
  facet_wrap(~parameter, labeller = labeller(parameter = facet_labels)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  labs(x="", y="MSE") + 
  scale_x_discrete(labels=c("Group-time \nATT", "Ever-treated \ngroup-time ATT",
                            "Stagged SA", "TWFE", "TWFE alt", 
                            "Ever-treated \n TWFE alt", "Ever-treated \nTWFE")) + 
  theme(legend.position = "none") 
#+ scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3", "#e34a33"))

p3
#ggsave(p3, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_mse_PTB.pdf", width=10)
ggsave(p3, file="../TWFE-simulation/results/twfe_sim_mse_PTB.pdf", width=10)

p4 <- ggplot(results2, aes(x = method, y = power)) + 
  geom_point(size=5) + 
  geom_text_repel(aes(label = power)) +
  theme_bw() + 
  facet_wrap(~parameter, labeller = labeller(parameter = facet_labels)) + 
  #geom_hline(aes(yintercept=0), linetype=3) + 
  labs(x="", y="Power") + 
  scale_x_discrete(labels=c("Group-time \nATT", "Ever-treated \ngroup-time ATT",
                            "Stagged SA", "TWFE", "TWFE alt", 
                            "Ever-treated \n TWFE alt", "Ever-treated \nTWFE")) + 
  theme(legend.position = "none") 
#+ scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3", "#e34a33"))

p4
#ggsave(p3, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_mse_PTB.pdf", width=10)
ggsave(p4, file="../TWFE-simulation/results/twfe_sim_power_PTB.pdf", width=10)

# dynamic effects 
results_df_summary <- results_df_summary %>% mutate(time_pt = substr(parameter, 4, 8))
results_df_summary <- results_df_summary %>% mutate(time_pt = ifelse(parameter=="DTE.avg", "", time_pt))
results_df_summary <- results_df_summary %>% mutate(time_pt = sub("..","-",time_pt, fixed = TRUE))
results_df_summary <- results_df_summary %>% mutate(time_pt = as.numeric(sub(".","",time_pt, fixed = TRUE)))
results_df_summary <- results_df_summary %>% mutate(pre_post = as.numeric(time_pt>=0))

# coverage
results3 <- results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg"))

p4 <- ggplot(data = results3, aes(x=method, y=coverage)) + 
  geom_point(aes(shape=method, color=method, alpha=pre_post), size=5) + 
  labs(x="", y="95% confidence interval coverage") + 
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33", "black"), 
                     labels=c("group-time \nATT", "ever-treated \ngroup-time ATT",
                              "stacked regression",
                              "TWFE", "ever-treated \nTWFE")) +
  theme_bw() +   
  scale_x_discrete(labels=c("group-time \nATT", "ever-treated \ngroup-time ATT",
                            "stacked \nregression", "TWFE", "ever-treated \nTWFE")) +
  theme(legend.position = "none")  + 
  geom_text_repel(aes(x=method, y=coverage, label = time_pt)) 


p4
#ggsave(p4, file="../TWFE-simulation/results/twfe_sim_coverage2_PTB.pdf", width=10)

#coverage CR attempt #1
p41 <- ggplot(results3,
              aes(x = time_pt, y = coverage, col = method)) +
  geom_line(aes(col = method)) +
  geom_point(aes(col = method)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0.95) +
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33", "black"), 
                     labels=c("group-time \nATT", "ever-treated \ngroup-time ATT",
                              "stacked \nregression", "TWFE", "ever-treated \nTWFE")) +
  labs(y = "Coverage", x = "Method") +
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)

p41
#ggsave(p41, file="../TWFE-simulation/results/twfe_sim_coverage3_PTB.pdf", width=10)
ggsave(p41, file="../TWFE-simulation/results/twfe_sim_coverage3_n100_PTB.png", 
       width=12,height = 4, device = png)


# bias
# p5 <- ggplot(results3) + 
#   geom_point(aes(x=method, y=bias, shape=method, color=method, alpha=pre_post), size=5) + theme_bw() + 
#   labs(x="", y="bias") + 
#   scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"), labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) +
#   theme_bw() +   scale_x_discrete(labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) +
#   geom_text_repel(aes(x=method, y=bias, label = time_pt)) +  theme(legend.position = "none") 
# 
# #p5
# ggsave(p5, file="../TWFE-simulation/results/twfe_sim_bias2_PTB.pdf", width=10)

p51 <- ggplot(results3,
              aes(x = time_pt, y = bias)) +
  geom_line(aes(col = method)) +
  geom_point(aes(col = method)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33", "black"), 
                     labels=c("group-time \nATT", "ever-treated \ngroup-time ATT",
                              "stacked \nregression", "TWFE", "ever-treated \nTWFE")) +
  labs(y = "Bias", x = "Method") +
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)

p51
#ggsave(p51, file="../TWFE-simulation/results/twfe_sim_bias3_PTB.pdf", width=10)
ggsave(p51, file="../TWFE-simulation/results/twfe_sim_bias3_n100_PTB.png", 
       width=12,height = 4, device = png)

# MSE
# p6 <- ggplot(results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg"))) +
#   geom_point(aes(x=method, y=MSE, shape=method, color=method, alpha=pre_post), size=5) + theme_bw() +
#   labs(x="", y="MSE") +
#   scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"), labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) +
#   theme_bw() +   scale_x_discrete(labels=c("group-time \nATT","ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) +
#   geom_text_repel(aes(x=method, y=MSE, label = time_pt)) +  theme(legend.position = "none")
# 
# #p6
# ggsave(p6, file="../TWFE-simulation/results/twfe_sim_mse2_PTB.pdf", width=10)

p61 <- ggplot(results3,
              aes(x = time_pt, y = MSE)) +
  geom_line(aes(col = method)) +
  geom_point(aes(col = method)) +
  geom_vline(xintercept = 0, linetype = 2) +
  #geom_hline(yintercept = 0) +
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33", "black"), 
                     labels=c("group-time \nATT", "ever-treated \ngroup-time ATT",
                              "stacked \nregression", "TWFE", "ever-treated \nTWFE")) +
  labs(y = "Mean squared error", x = "Method") +
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)

p61
#ggsave(p61, file="../TWFE-simulation/results/twfe_sim_mse3_PTB.pdf", width=10)

ggsave(p61, file="../TWFE-simulation/results/twfe_sim_mse3_n100_PTB.png", 
       width=12,height = 4, device = png)

#power 

p71 <- ggplot(results3,
              aes(x = time_pt, y = power)) +
  geom_line(aes(col = method)) +
  geom_point(aes(col = method)) +
  geom_vline(xintercept = 0, linetype = 2) +
  #geom_hline(yintercept = 1) +
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33", "black"), 
                     labels=c("group-time \nATT", "ever-treated \ngroup-time ATT",
                              "stacked \nregression", "TWFE", "ever-treated \nTWFE")) +
  labs(y = "Power", x = "Method") +
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)

p71

ggsave(p71, file="../TWFE-simulation/results/twfe_sim_power_n100_PTB.png", 
       width=12, height = 4, device = png)
