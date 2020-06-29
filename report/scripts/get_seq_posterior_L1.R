samps_seq <- read_csv("../stanout/posterior_accmodel_sequential_L1.csv") %>% 
  gather(Param, value) %>%
  mutate(Param = gsub(pattern = "b_COND", replacement = "", Param)) %>%
  separate(Param, into = c("Target", "FA")) %>%
  mutate(FA = ifelse(FA == "warning", "FA warning", "No FA warning")) %>%
  mutate(value = logit2prob(value)) 

samps_seq %>%
  group_by(FA) %>%
  dplyr::summarise(M = dmode(value),
                   lower = HPDI(value, prob = .95)[1],
                   upper = HPDI(value, prob = .95)[2]
  ) %>%
  ungroup() %>%
  mutate(Target = "absent",
         value = .1) -> chanceexp

samps_seq %>%
  group_by(Target) %>%
  mutate(row_id = 1:n()) %>%
  spread(Target, value) %>%
  transmute(FA = FA, diff = present - absent) -> samps_diff


samps_diff %>% group_by(FA) %>%
  dplyr::summarise(M = dmode(diff),
                   lower = HPDI(diff, prob = .95)[1],
                   upper = HPDI(diff, prob = .95)[2]
  ) -> by_fa

samps_diff %>% 
  mutate(diff = diff * 100) %>%
  dplyr::summarise(M = dmode(diff),
                   lower = HPDI(diff, prob = .95)[1],
                   upper = HPDI(diff, prob = .95)[2]) %>%
  mutate_if(is.numeric, round) -> target_diff


samps_seq %>%
  group_by(Target) %>%
  mutate(value = value * 100) %>%
  dplyr::summarise(M = dmode(value),
                   lower = HPDI(value, prob = .95)[1],
                   upper = HPDI(value, prob = .95)[2]) %>%
  mutate_if(is.numeric, round) -> chance


samps_seq %>%
  mutate( value = value * 100) %>%
  group_by(FA) %>%
  summarise(P = mean(value < 10)*100) %>%
  mutate_if(is.numeric, round) -> p_chance


ppdif <- abs(diff(p_chance$P))