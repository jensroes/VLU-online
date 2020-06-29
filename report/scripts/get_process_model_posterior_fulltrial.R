source("functions/functions.R")
load(file="data/VoiceLineUpOnline.Rda")

# Preprocess data
d %>% 
  mutate(dup = duplicated(subj)) %>%
  filter(!dup) %>% select(-dup) %>%
  select(subj, lineup, FA, order, TP, resp_given, resp_expec) %>%
  filter(order == "sequential") %>% dplyr::select(-order) %>%
  select(resp_given, resp_expec, subj, FA, TP) %>%
  mutate(resp_given = ifelse(resp_given == 0, 10, resp_given),
         resp_expec = ifelse(resp_expec == 0, 10, resp_expec)) -> d2

d2 %>% 
  tidyr::expand(subj, responses = 1:9) %>%
  left_join(d2, by = "subj") %>%
  mutate(resp_present = as.numeric(resp_given == responses)) %>%
  mutate(correct = ifelse(TP == "present" & resp_expec == responses, 1, 0),
         acc = as.numeric(resp_present == correct)) %>%
#  filter(responses <= resp_given) %>%
  select(-correct) -> d3

d3 %>% 
  select(subj, responses, TP, resp_present, FA, acc, resp_expec) %>%
  mutate(acc = ifelse(acc == 1, "correct", "incorrect")) %>%
  rename(corr_response = acc) %>% 
  select(-resp_expec) %>% mutate(responses = factor(responses)) -> d.seq; d.seq 

# Preprocess data
d %>% 
  mutate(dup = duplicated(subj)) %>%
  filter(!dup) %>% select(-dup) %>%
  select(subj, lineup, FA, order, TP, resp_given, resp_expec) %>%
  filter(order == "serial") %>% dplyr::select(-order) %>%
  select(resp_given, resp_expec, subj, FA, TP) %>%
  mutate(resp_given = ifelse(resp_given == 0, 10, resp_given),
         resp_expec = ifelse(resp_expec == 0, 10, resp_expec)) -> d2

d2 %>% 
  tidyr::expand(subj, responses = 1:9) %>%
  left_join(d2, by = "subj") %>%
  mutate(resp_present = as.numeric(resp_given == responses)) %>%
  mutate(correct = ifelse(TP == "present" & resp_expec == responses, 1, 0),
         acc = as.numeric(resp_present == correct)) %>%
#  filter(responses <= resp_given) %>%
  select(-correct) -> d3

d3 %>% 
  select(subj, responses, TP, resp_present, FA, acc, resp_expec) %>%
  mutate(acc = ifelse(acc == 1, "correct", "incorrect")) %>%
  rename(corr_response = acc) %>% 
  select(-resp_expec) %>% mutate(responses = factor(responses)) -> d.serial; d.serial 


m_serial <- readRDS(file="stanout/sequential_countmodel_serial_fulltrial.rda")
m_seq <- readRDS(file="stanout/sequential_countmodel_sequential_fulltrial.rda")

newdata_serial <- make_conditions(d.serial, vars = c("TP", "responses")) 
newdata_seq <- make_conditions(d.seq, vars = c("TP", "responses")) 

ps_serial <- conditional_effects(m_serial, conditions = newdata_serial)$responses %>%
  mutate(TP = paste0("Target ", TP),
         Parade = "Serial parade")

ps_seq <- conditional_effects(m_seq, conditions = newdata_seq)$responses %>%
  mutate(TP = paste0("Target ", TP),
         Parade = "Sequential parade")

ps <- bind_rows(ps_serial, ps_seq)

plot_resp <- ggplot(data= ps, aes(x = responses, y = estimate__, 
                                  shape = TP, linetype = TP, color = Parade, 
                                  group = interaction(TP, Parade))) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), alpha = .75, 
                  position = position_dodge(.75)) +
  scale_color_colorblind("") +
  scale_shape_discrete("") +
  scale_linetype_discrete("") +
  geom_line(alpha = .75, size = .5, position = position_dodge(.75), 
            show.legend = F) +
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0,.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_few() +
  labs(x = "Lineup ID",
       y = "False-positive response probability\nwith 95% PIs",
       title = "Sequential process model") + 
  theme(legend.key.height = unit(1, "cm")) ; plot_resp

newdata_seq <- make_conditions(d.seq, vars = c("TP", "corr_response")) %>%
  mutate(corr_response = ifelse(corr_response == "incorrect", 0, 1))

newdata_serial<- make_conditions(d.serial, vars = c("TP", "corr_response")) %>%
  mutate(corr_response = ifelse(corr_response == "incorrect", 0, 1))

plot_tp_seq <- conditional_effects(m_seq, effects = "TP", conditions = newdata_seq)$TP %>%
  unique() %>%
  mutate(TP = paste0("Target ", TP),
         corr_response = ifelse(corr_response == 1, "correct", "incorrect"),
         Parade = "sequential")

plot_tp_serial <- conditional_effects(m_serial, effects = "TP", conditions = newdata_serial)$TP %>%
  unique() %>%
  mutate(TP = paste0("Target ", TP),
         corr_response = ifelse(corr_response == 1, "correct", "incorrect"),
         Parade = "serial")

ps <- bind_rows(plot_tp_seq, plot_tp_serial)

plot_tp <- ggplot(data = ps, aes(x = corr_response, y = estimate__, shape = TP, linetype = TP, color = Parade, group = interaction(TP, Parade))) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), 
                  alpha = .85, position = position_dodge(.5)) +
  geom_line(position = position_dodge(.5), 
            size = .75, alpha = .75, show.legend = F) +
  scale_color_colorblind("") +
  scale_shape_discrete("") +
  scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_few() +
  labs(x = "Response accuracy",
       y = "Positive response probability\nwith 95% PIs") +
  theme(legend.position = "none")

legend <- g_legend(plot_resp)

cowplot::plot_grid(plot_resp + theme(legend.position = "none"),
    cowplot::plot_grid(plot_tp, legend), labels = c("a.", "b."), nrow = 2)

#path <- "../plots/smplot.png"
#ggsave(path, plot = plotsm, width = 8, height = 8)
#path <- "../plots/smplot.pdf"
#ggsave(path, plot = plotsm, width = 8, height = 8)
