modelfit_4 <- readRDS(file="../stanout/sequential_countmodel.rda")

d4 %>%
  group_by(responses) %>%
  summarise(freq = n()) %>%
  mutate(resp_present = .55) %>%
  rename(est = resp_present) -> dfreq

newdata <- make_conditions(d4, vars = c("TP", "responses")) 
plot_resp <- conditional_effects(modelfit_4, conditions = newdata)$responses %>%
  unique() %>%
  mutate(TP = paste0("Target ", TP)) %>%
  ggplot(aes(x = responses, y = estimate__, shape = TP, color = TP, group = TP)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), alpha = .75, position = position_dodge(.25)) +
  scale_color_colorblind("") +
  scale_shape_discrete("") +
  geom_line(alpha = .5, size = .5, 
            linetype = "dotted", position = position_dodge(.25), 
            show.legend = F) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0,.6),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_few() +
  labs(x = "Lineup ID",
       y = "False-positive response probability\nwith 95% HPDIs",
       title = "Sequential process model: sequential parade") +
  theme(legend.position = "bottom",
        legend.justification = "right") +
  geom_point(data= dfreq, inherit.aes = F, aes(y = est, x = responses, size = freq, alpha = freq)) +
  scale_size_continuous("No. of observations:") +
  scale_alpha_continuous("No. of observations:") 


newdata <- make_conditions(d4, vars = c("TP", "corr_response")) %>%
  mutate(corr_response = ifelse(corr_response == "incorrect", 0, 1))

plot_tp <- conditional_effects(modelfit_4, effects = "TP", conditions = newdata)$TP %>%
  unique() %>%
  mutate(TP = paste0("Target ", TP),
         corr_response = ifelse(corr_response == 1, "correct", "incorrect")) %>%
  ggplot(aes(x = corr_response, y = estimate__, shape = TP, color = TP, group = TP)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), alpha = .75, position = position_dodge(.25)) +
  geom_line(position = position_dodge(.25), size = .5, alpha = .65, linetype = "dotted", show.legend = F) +
  scale_color_colorblind("") +
  scale_shape_discrete("") +
  scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_few() +
  labs(x = "Response accuracy",
       y = "Positive response probability\nwith 95% HPDIs") +
  theme(legend.position = "none")


#newdata <- data.frame(corr_response = rep(seq(0,1,.1), 2),
#           TP = rep(c("absent", "present"), each = 11))

#plot_corr <- conditional_effects(modelfit_4, conditions = newdata)$corr_response %>%
#  unique() %>%
#  mutate(TP = paste0("Target ", TP)) %>%
#  ggplot(aes(x = corr_response, y = estimate__, fill = TP)) +
#  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = .25, show.legend = F) +
#  geom_line(aes(color = TP))+
#  scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0,1),
#                     labels = scales::percent_format(accuracy = 1)) +
#  scale_x_continuous(breaks = seq(0, 1, .25), limits = c(0,1),
#                     labels = scales::percent_format(accuracy = 1)) +
#  theme_few() +
#  scale_color_colorblind("") +
#  scale_fill_colorblind("") +
#  labs(x = "Response accuracy",
#       y = "Positive response probability\nwith 95% HPDIs") +
#  theme(legend.position = "none")

#ggpubr::ggarrange(plot_resp, plot_tp, 
#                  nrow = 1, 
#                  labels = c("a.", "b."), widths = c(2,1)) 

plotsm <- ggpubr::ggarrange(plot_resp,
                            ggpubr::ggarrange(plot_tp, ncol = 2, labels = c("b.")), 
                            nrow = 2, 
                            heights = c(1.65, 1.5),
                            labels = "a.") 

path <- "../plots/smplot.png"
ggsave(path, plot = plotsm, width = 8, height = 8)
path <- "../plots/smplot.pdf"
ggsave(path, plot = plotsm, width = 8, height = 8)
