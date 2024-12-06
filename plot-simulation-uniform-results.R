library(tidyverse)
library(ggrepel)

results_df_summary <- read.csv("../TWFE-simulation/results/twfe_sim_results_summary_PTB_uniform.csv")

# plot results 
facet_labels <- c(CTE = "Constant treatment effect", HTE = "Heterogeneous treatment effect", DTE.avg = "Average of dynamic treatment effect")


p1 <- ggplot(results_df_summary %>% filter(parameter %in% c("CTE","HTE","DTE.avg"))) + 
  geom_point(aes(x=method, y=coverage, shape=method, color=method), size=5) + theme_bw() + 
  facet_wrap(~parameter, labeller = labeller(parameter = facet_labels)) + 
  geom_hline(aes(yintercept=0.95), linetype=3) + labs(x="", y="95% confidence interval coverage") + 
  scale_x_discrete(labels=c("group-time \nATT", "ever-treated \ngroup-time ATT","TWFE", "ever-treated \nTWFE")) + 
  theme(legend.position = "none") + scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"))

#p1
#ggsave(p1, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_coverage_PTB.pdf", width=10)
ggsave(p1, file="../TWFE-simulation/results/uniform/twfe_sim_coverage_PTB.pdf", width=10)

p2 <- ggplot(results_df_summary %>% filter(parameter %in% c("CTE","HTE","DTE.avg")))  + 
  geom_point(aes(x=method, y=bias, shape=method, color=method), size=5) + theme_bw() + 
  facet_wrap(~parameter, labeller = labeller(parameter = facet_labels)) + 
  geom_hline(aes(yintercept=0), linetype=3) + labs(x="", y="Bias") + 
  scale_x_discrete(labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) + 
  theme(legend.position = "none") + scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"))

#p2
#ggsave(p2, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_bias_PTB.pdf", width=10)
ggsave(p2, file="../TWFE-simulation/results/uniform/twfe_sim_bias_PTB.pdf", width=10)


p3 <- ggplot(results_df_summary %>% filter(parameter %in% c("CTE","HTE","DTE.avg"))) + 
  geom_point(aes(x=method, y=MSE, shape=method, color=method), size=5) + theme_bw() + 
  facet_wrap(~parameter, labeller = labeller(parameter = facet_labels)) + 
  geom_hline(aes(yintercept=0), linetype=3) + labs(x="", y="MSE") + 
  scale_x_discrete(labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) + 
  theme(legend.position = "none") + scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3", "#e34a33"))

#p3
#ggsave(p3, file="/Users/danagoin/Documents/Research projects/TWFE/results/twfe_sim_mse_PTB.pdf", width=10)
ggsave(p3, file="../TWFE-simulation/results/uniform/twfe_sim_mse_PTB.pdf", width=10)


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


#p4
ggsave(p4, file="../TWFE-simulation/results/uniform/twfe_sim_coverage2_PTB.pdf", width=10)

#make sure this works
p41 <- ggplot(results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg")),
              aes(x = time_pt, y = coverage, col = method)) +
  geom_line(aes(col = method)) +
  geom_point(aes(col = method)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0.95) +
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"), 
                     labels=c("group-time \nATT", "ever-treated \ngroup-time ATT","TWFE", "ever-treated \nTWFE")) +
  labs(y = "Coverage", x = "Method") +
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)

p41
ggsave(p41, file="../TWFE-simulation/results/uniform/twfe_sim_coverage3_PTB.pdf", width=10)


# bias
p5 <- ggplot(results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg"))) + 
  geom_point(aes(x=method, y=bias, shape=method, color=method, alpha=pre_post), size=5) + theme_bw() + 
  labs(x="", y="bias") + 
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"), labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) +
  theme_bw() +   scale_x_discrete(labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) +
  geom_text_repel(aes(x=method, y=bias, label = time_pt)) +  theme(legend.position = "none") 

#p5
ggsave(p5, file="../TWFE-simulation/results/uniform/twfe_sim_bias2_PTB.pdf", width=10)

p51 <- ggplot(results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg")),
              aes(x = time_pt, y = bias, col = method)) +
  geom_line(aes(col = method)) +
  geom_point(aes(col = method, pch = method)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"), 
                     labels=c("group-time \nATT", "ever-treated \ngroup-time ATT","TWFE", "ever-treated \nTWFE")) +
  labs(y = "Bias", x = "Method") +
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)

p51
ggsave(p51, file="../TWFE-simulation/results/uniform/twfe_sim_bias3_PTB.pdf", width=10)


# MSE
p6 <- ggplot(results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg"))) + 
  geom_point(aes(x=method, y=MSE, shape=method, color=method, alpha=pre_post), size=5) + theme_bw() + 
  labs(x="", y="MSE") + 
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"), labels=c("group-time \nATT", "ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) +
  theme_bw() +   scale_x_discrete(labels=c("group-time \nATT","ever-treated \ngroup-time ATT", "TWFE", "ever-treated \nTWFE")) +
  geom_text_repel(aes(x=method, y=MSE, label = time_pt)) +  theme(legend.position = "none") 

#p6
ggsave(p6, file="../TWFE-simulation/results/uniform/twfe_sim_mse2_PTB.pdf", width=10)

p61 <- ggplot(results_df_summary %>% filter(!parameter %in% c("CTE","HTE","DTE.avg")),
              aes(x = time_pt, y = MSE, col = method)) +
  geom_line(aes(col = method)) +
  geom_point(aes(col = method, pch = method)) +
  geom_vline(xintercept = 0, linetype = 2) +
  #geom_hline(yintercept = 0) +
  scale_color_manual(values=c("#9e0142", "#66c2a5", "#4393c3","#e34a33"), 
                     labels=c("group-time \nATT", "ever-treated \ngroup-time ATT","TWFE", "ever-treated \nTWFE")) +
  labs(y = "Mean squared error", x = "Method") +
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)

p61
ggsave(p61, file="../TWFE-simulation/results/uniform/twfe_sim_mse3_PTB.pdf", width=10)
