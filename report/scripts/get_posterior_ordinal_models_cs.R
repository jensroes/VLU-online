
m_serial <- readRDS(file="../stanout/ordinal_model_serial_cs.rda") 
m_seq <- readRDS(file="../stanout/sequential_sratio_cs.rda")

#m_serial <- readRDS(file="stanout/ordinal_model_serial_cs.rda") 
#m_seq <- readRDS(file="stanout/sequential_sratio_cs.rda")

#m_serial <- readRDS(file="stanout/ordinal_model_serial.rda") 
#m_seq <- readRDS(file="stanout/sequential_sratio.rda")


conditional_effects(m_serial, 
                    robust = TRUE, # median instead of mean
                    effects = "resp_expec", 
                    categorical = T)$resp_expec %>%
  select(resp_expec, effect2__, estimate__, lower__, upper__) %>%
  mutate(Parade = "serial") -> ps_serial

conditional_effects(m_seq, 
                    robust = TRUE, # median instead of mean
                    effects = "resp_expec", 
                    categorical = T)$resp_expec %>%
  select(resp_expec, effect2__, estimate__, lower__, upper__) %>%
  mutate(Parade = "sequential") -> ps_seq


bind_rows(ps_serial, ps_seq) %>%
  rename(responses = effect2__,
         est = estimate__,
         lo = lower__,
         up = upper__) %>%
  mutate(resp_expec = ifelse(resp_expec == 10, "Target absent (10)",
                             paste0("Target present (", resp_expec, ")")),
         resp_expec = factor(resp_expec, levels = unique(resp_expec)[c(1,2,3)], ordered= T),
         Parade = recode(Parade,sequential = "a. Parade type: sequential",
                                serial = "b. Parade type: serial")) %>%
  as_tibble() -> ps


ggplot(ps, aes(x= responses, y = est, ymin = lo, ymax = up, 
             color = resp_expec,
             shape = resp_expec,
             group = resp_expec)) +
  geom_pointrange(fatten = 5,
                  position = position_dodge(.5), show.legend = T) +
  scale_color_colorblind("Target presence: ") +
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0,.5),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Lineup voice ID", 
       y = "Response probability",
       shape = "Target presence: ") + 
  facet_wrap(~Parade, nrow = 2) +
  theme(legend.position = "bottom",
        legend.key.height = unit(1, "cm"), 
        axis.title.x = element_text(hjust = 0),
        axis.title.y = element_text(hjust = 0),
        strip.text = element_text(hjust = 0),
        strip.background = element_blank(),
        legend.justification = "right") -> plotsm;plotsm


path <- "../plots/smplotmixed.png"
ggsave(path, plot = plotsm, width = 6, height = 6)
path <- "../plots/smplotmixed.pdf"
ggsave(path, plot = plotsm, width = 6, height = 6)
