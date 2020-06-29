load(file="../data/VoiceLineUpOnline.Rda")

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
  filter(responses <= resp_given) %>%
  select(-correct) -> d3

m <- glm(acc ~ factor(responses)*FA*TP, family = "binomial", data = d3)
d3$fit <- predict(m, type = "response", se.fit = TRUE)$fit
d3$fit.se <- predict(m, type = "response", se.fit = TRUE)$se.fit

d3 %>% 
  group_by(TP) %>%
  mutate(Total = length(unique(subj))) %>%
  ungroup() %>%
  group_by(responses, FA, TP, Total) %>%
  summarise(fit = mean(fit),
            fit.se = mean(fit.se),
            N = n(),
            fit.lo = fit - fit.se,
            fit.up = fit + fit.se
  ) %>%
  ungroup() %>%
  mutate(fit.up = ifelse(fit.up > 1, 1, fit.up)) %>%
  mutate(TP = paste0("Target: ", TP, " (N=", Total, ")")) ->  d.plot

paccseq <- d.plot %>% ggplot(aes(y = fit, x = as.numeric(responses), colour = FA)) +
  geom_hline(yintercept = .5, linetype = "dashed", colour = "grey50")+
  geom_pointrange(aes(ymin = fit.lo, ymax = fit.up, fill = FA), 
                  alpha = .5, show.legend = F) +
  geom_line(size = .4, linetype = "dotted" )+
  geom_point(data = d.plot, inherit.aes = F, aes(y=fit, x = as.numeric(responses), colour = FA, size = N, alpha = N)) +
  facet_grid(~TP) +
  #geom_text(aes(label = N, group = 1), size = 2, show.legend = F) +
  scale_color_colorblind("FA warning:") +
  scale_linetype("FA warning:") +
  scale_size_continuous("No. of observations:") +
  scale_alpha_continuous("No. of observations:") +
  theme_few() +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(y = "Response accuracy with SEs",
       x = "Lineup ID", 
       title = "Observed response accuracy: sequential parade"
  ) +
  theme(legend.position = "bottom",
        legend.justification = "right")

path <- "../plots/seqacc.png"
ggsave(path, plot = paccseq, width = 8, height = 5)
path <- "../plots/seqacc.pdf"
ggsave(path, plot = paccseq, width = 8, height = 5)

