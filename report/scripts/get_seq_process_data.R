# Preprocess data
# Load DF
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

d3 %>% 
  select(subj, responses, TP, resp_present, FA, acc, resp_expec) %>%
  mutate(acc = ifelse(acc == 1, "correct", "incorrect")) %>%
  rename(corr_response = acc) %>% 
  select(-resp_expec) %>% mutate(responses = factor(responses)) -> d4