source("../functions/functions.R")
#source("functions/functions.R")

m_serial <- readRDS(file="../stanout/sequential_countmodel_serial_fulltrial.rda") 
m_seq <- readRDS(file="../stanout/sequential_countmodel_sequential.rda")

#m_serial <- readRDS(file="stanout/sequential_countmodel_serial_fulltrial.rda") 
#m_seq <- readRDS(file="stanout/sequential_countmodel_sequential.rda")


ps_serial <- conditional_effects(m_serial, 
                    robust = TRUE, # median instead of mean
                    effects = "responses", 
                    conditions = tibble(
                      corr_response = NA,
                      TP = NA
                    ))$responses %>%
  select(responses, estimate__, lower__, upper__) %>%
  mutate(Parade = "Serial parade")

ps_seq <- conditional_effects(m_seq, 
                                 robust = TRUE, # median instead of mean
                                 effects = "responses", 
                                 conditions = data.frame(
                                   corr_response = NA,# c("incorrect"),
                                   TP = NA# c("present", "absent")
                                 ))$responses %>%
  select(responses, estimate__, lower__, upper__) %>%
  mutate(Parade = "Sequential parade")

ps_responses <- bind_rows(ps_serial, ps_seq) 

conds <- make_conditions(d.seq, vars = c("responses", "corr_response")) %>% 
  filter(responses %in% c(3,7))

ps_serial <- conditional_effects(m_serial, 
                    robust = TRUE, # median instead of mean
                    effects = "TP",
                    conditions = conds)$TP %>%
  select(responses, corr_response, TP, estimate__, lower__, upper__) %>%
  mutate(Parade = "Serial parade")

ps_seq <- conditional_effects(m_seq, 
                    robust = TRUE, # median instead of mean
                    effects = "TP",
                    conditions = conds)$TP %>%
  select(responses, corr_response, TP, estimate__, lower__, upper__) %>%
  mutate(Parade = "Sequential parade")

ps_tp <- bind_rows(ps_serial, ps_seq) %>%
  mutate(TP = paste0("Target ", TP),
         responses = paste0("Lineup ID: ", responses)) 

ggplot(ps_responses, aes(x = responses, y = estimate__, 
                      color = Parade, 
                      group = interaction(Parade))) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__),
                  fatten = 5,
                  position = position_dodge(.5), show.legend = T) +
  scale_color_colorblind("") +
  facet_grid(~1) +
  geom_line(alpha = .75, size = .75, position = position_dodge(.5), 
            show.legend = F, linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0,.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_few() +
  labs(x = "Lineup ID",
       y = "Positive response threshold") + 
  theme(axis.ticks.x = element_blank(), 
        legend.position = "bottom",
        legend.key.height = unit(1, "cm"), 
        strip.text = element_text(colour = "transparent"),
        legend.justification = "right") -> plot_resp; plot_resp

ps_tp %>%
  mutate(cond = paste0(Parade," / ",TP)) %>%
  ggplot(aes(x = corr_response, y = estimate__, 
                      color = cond, 
                      linetype = cond,
                      shape = cond,
                      group = cond)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), 
                  position = position_dodge(.5), 
                  fatten = 5,
                  show.legend = T) +
  scale_color_manual("", values = c("black", "black", "#E69f00", "#E69f00")) +
  scale_linetype_manual("", values = c("solid", "dashed","solid", "dashed")) +
  scale_shape_manual("", values = c(16,17,16,17)) +
  facet_grid(~responses) +
  geom_line(alpha = .75, size = .5, position = position_dodge(.5), show.legend = F) +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme_few() +
  labs(x = "Response",
       y = "Probability of positive response") +
  theme(axis.ticks.x = element_blank(),
        legend.key.height = unit(1, "cm"), 
        legend.position = "bottom",
        legend.justification = "right") +
  guides(colour=guide_legend(nrow=2,byrow=TRUE))-> plot_target;plot_target

plotsm <- cowplot::plot_grid(plot_resp, plot_target,
                             labels = c("a.", "b."), nrow = 2, 
                             rel_heights = c(.75,1));plotsm

path <- "../plots/smplotmixed.png"
ggsave(path, plot = plotsm, width = 8, height = 9)
path <- "../plots/smplotmixed.pdf"
ggsave(path, plot = plotsm, width = 8, height = 8)
