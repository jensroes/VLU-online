load(file="../data/VoiceLineUpOnline.Rda")
#load(file="data/VoiceLineUpOnline.Rda")

d %>% 
  mutate(dup = duplicated(subj)) %>%
  filter(!dup) %>% select(-dup) %>%
  select(subj, lineup, FA, order, TP, resp_given, resp_expec) %>%
  filter(order == "sequential") %>% dplyr::select(-order) %>%
  select(resp_given, resp_expec, subj, FA, TP) %>%
  mutate(resp_given = ifelse(resp_given == 0, 10, resp_given),
         resp_expec = ifelse(resp_expec == 0, 10, resp_expec)) -> d2;d2

d2 %>% 
  tidyr::expand(subj, responses = 1:9) %>%
  left_join(d2, by = "subj") %>%
  mutate(resp_present = as.numeric(resp_given == responses)) %>%
  mutate(correct = ifelse(TP == "present" & resp_expec == responses, 1, 0),
         acc = as.numeric(resp_present == correct)) %>%
  filter(responses <= resp_given) %>%
  select(-correct) -> d3;d3

d3 %>% 
  select(subj, responses, TP, resp_present, FA, acc, resp_expec) %>%
  rename(corr_response = acc) %>%
  #  mutate(corr_response = recode(corr_response, `1` = "correct",
  #                                                `0` = "incorrect")) %>%
  select(-resp_expec) %>% mutate(responses = factor(responses)) -> d.seq

# Preprocess data
d %>% 
  mutate(dup = duplicated(subj)) %>%
  filter(!dup) %>% select(-dup) %>%
  select(subj, lineup, FA, order, TP, resp_given, resp_expec) %>%
  filter(order == "serial") %>% dplyr::select(-order) %>%
  select(resp_given, resp_expec, subj, FA, TP) %>%
  mutate(resp_given = ifelse(resp_given == 0, 10, resp_given),
         resp_expec = ifelse(resp_expec == 0, 10, resp_expec)) %>% 
  mutate(corr_response = ifelse(resp_expec == resp_given, 1, 0),
         resp_expec = factor(resp_expec)) %>%
  #  mutate(corr_response = recode(corr_response, `1` = "correct",
  #                                `0` = "incorrect")) %>%
  select(subj, resp_given, resp_expec, TP, FA, corr_response ) -> d.serial
