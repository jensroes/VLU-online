load(file="../data/VoiceLineUpOnline.Rda")
#load(file="data/VoiceLineUpOnline.Rda")

# Preprocess data
d %>% 
  mutate(dup = duplicated(subj)) %>%
  filter(!dup) %>% select(-dup) %>%
  select(subj, lineup, FA, order, TP, resp_given, resp_expec) %>%
  select(order, resp_given, resp_expec, subj, FA, TP) %>%
  mutate(resp_given = ifelse(resp_given == 0, 10, resp_given),
         resp_expec = ifelse(resp_expec == 0, 10, resp_expec)) -> d2

d2 %>% 
  tidyr::expand(subj, responses = 1:9) %>%
  left_join(d2, by = "subj") %>%
  mutate(resp_present = as.numeric(resp_given == responses)) %>%
  mutate(correct = ifelse(TP == "present" & resp_expec == responses, 1, 0),
         acc = as.numeric(resp_present == correct)) %>%
  filter(order == "serial" | (order == "sequential" & responses <= resp_given)) %>%
  select(-correct) -> d3

#d3 %>% count(order)

m <- glm(acc ~ factor(responses)*FA*TP*order, family = "binomial", data = d3)
d3$fit <- predict(m, type = "response", se.fit = TRUE)$fit
d3$fit.se <- predict(m, type = "response", se.fit = TRUE)$se.fit

d3 %>% 
  group_by(TP) %>%
  mutate(Total = length(unique(subj))) %>%
  ungroup() %>%
  group_by(responses, FA, TP, order, Total) %>%
  summarise(fit = mean(fit),
            fit.se = mean(fit.se),
            N = n(),
            fit.lo = fit - fit.se,
            fit.up = fit + fit.se ) %>%
  ungroup() %>%
  mutate(fit.up = ifelse(fit.up > 1, 1, fit.up)) %>%
  mutate(order = recode(order, sequential = "Sequential parade",
                        serial = "Serial parade"),
         FA = recode(FA, `no warning` =  "Standard warning",
                         `warning` = "Strong warning"),
         TP = paste0("Target presence: ", TP, " (N=", Total, ")")) %>% 
  mutate(COND = paste0(order, " / ", FA)) ->  d.plot

ggplot(d.plot, aes(y = fit, x = as.numeric(responses), 
                   linetype = COND, 
                   colour = COND,
                   shape = COND)) +
  geom_pointrange(aes(ymin = fit.lo, ymax = fit.up), 
                  alpha = .75, show.legend = F) +
  geom_line(size = .5 )+
  geom_point(data = d.plot, inherit.aes = F, 
             aes(y=fit, x = as.numeric(responses), shape = COND,
                 colour = COND, size = N, alpha = N)) +
  facet_wrap(TP~., strip.position = "top", nrow = 2) +
  scale_color_manual("", values = c("black", "black", "#E69F00","#E69F00" )) +
  scale_linetype_manual("", values = c("solid", "dashed", "solid", "dashed")) +
  scale_shape_manual("", values = c(16,16,17,17)) +
  scale_size_continuous("No. of observations:") +
  scale_alpha_continuous("No. of observations:") +
  theme_few() +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Response accuracy",
       x = "Lineup ID") +
  theme(legend.position = "right",
        legend.justification = "top",
        axis.title.x = element_text(hjust = 0),
        axis.title.y = element_text(hjust = 0),
        strip.text = element_text(hjust = 0),
        legend.key.width = unit(.85, "cm")) -> paccseq;paccseq



path <- "../plots/seqaccmixed.png"
ggsave(path, plot = paccseq, width = 8, height = 5)
path <- "../plots/seqaccmixed.pdf"
ggsave(path, plot = paccseq, width = 8, height = 5)

