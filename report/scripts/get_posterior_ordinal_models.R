
m_serial <- readRDS(file="../stanout/ordinal_model_serial_cs.rda") 
m_seq <- readRDS(file="../stanout/sequential_countmodel_sequential.rda")

#m_serial <- readRDS(file="stanout/ordinal_model_serial.rda") 
#m_seq <- readRDS(file="stanout/sequential_countmodel_sequential.rda")

conditional_effects(m_serial, 
                    robust = TRUE, # median instead of mean
                    effects = "resp_expec", 
                    categorical = T)$resp_expec %>%
  select(resp_expec, effect2__, estimate__, lower__, upper__) %>%
  rename(responses = effect2__,
         est = estimate__,
         lo = lower__,
         up = upper__) %>%
  mutate(resp_expec = ifelse(resp_expec == 10, "Target absent (10)",
                             paste0("Target present (", resp_expec, ")")),
         resp_expec = factor(resp_expec, levels = unique(resp_expec)[c(3,1,2)], ordered = T)) %>%
  as_tibble() %>%
  ggplot(aes(x= responses, y = est, ymin = lo, ymax = up, 
             color = resp_expec,
             shape = resp_expec,
             group = resp_expec)) +
  geom_pointrange(fatten = 6,
                  position = position_dodge(.5), show.legend = T) +
#  geom_line(position = position_dodge(.5), show.legend = F) +
  scale_color_colorblind("") +
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0,.5),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Serial parade",
      x = "Lineup ID", #(Target absent decision indicated as 10)
       y = "Response probability",
       shape = "") + 
  theme(axis.ticks.x = element_blank(), 
        legend.position = "bottom",
        legend.key.height = unit(1, "cm"), 
        strip.text = element_text(colour = "transparent"),
        legend.justification = "right") -> plot_serial; plot_serial


conds <- tibble(responses = rep(1:9, 4),
       corr_response = rep(c("incorrect","correct"), each = 9*2),
       TP = rep(rep(c("present", "absent"), each = 9), 2)) %>%
  data.frame()


conditional_effects(m_seq, 
                    robust = TRUE, # median instead of mean
                    effects = "TP",
                    conditions = conds)$TP %>%
  select(responses, corr_response, TP, estimate__, lower__, upper__) %>%
  rename(est = estimate__,
         lo = lower__,
         up = upper__) %>%
  as_tibble() %>%
  filter(responses %in% c(3,7)) %>%
  mutate(responses = paste0("Lineup ID: ", responses),
         TP = paste0("Target ", TP)) %>%
  ggplot(aes(x= corr_response, y = est, ymin = lo, ymax = up, 
             color = TP,
             shape = TP,
             linetype = TP,
             group = TP)) +
  geom_pointrange(fatten = 4,
                  position = position_dodge(.5), show.legend = T) +
  geom_line(position = position_dodge(.5), show.legend = F) +
  scale_color_colorblind("") +
  scale_y_continuous(breaks = seq(0, 1, .25), 
                     labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~responses, nrow = 1) +
  labs(x = "Response correct",
       title = "Sequential parade",
       y = "Positive response probability",
       linetype = "",
       shape = "") + 
  theme(axis.ticks.x = element_blank(), 
        strip.text = element_text(hjust = 0),
        legend.position = "bottom",
        legend.key.height = unit(1, "cm"), 
        legend.justification = "right") -> plot_seq


plotsm <- cowplot::plot_grid(plot_serial, plot_seq,
                             labels = c("a.", "b."), nrow = 2);plotsm

path <- "../plots/smplotmixed.png"
ggsave(path, plot = plotsm, width = 6, height = 6)
path <- "../plots/smplotmixed.pdf"
ggsave(path, plot = plotsm, width = 6, height = 6)
