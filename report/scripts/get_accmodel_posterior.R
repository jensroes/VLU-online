samps <- read_csv("../stanout/posterior_accmodel.csv") %>% 
  gather(Param, value) %>%
  mutate(Param = gsub(pattern = "b_COND", replacement = "", Param)) %>%
  separate(Param, into = c("Target", "Parade", "FA")) %>%
  mutate(FA = ifelse(FA == "warning", "Strong FA warning", "Standard FA warning")) %>%
  mutate(value = logit2prob(value)) 

samps %>%
  group_by(Parade, FA) %>%
  dplyr::summarise(M = dmode(value),
                   lower = hpdi(value, prob = .95)[1],
                   upper = hpdi(value, prob = .95)[2]
  ) %>%
  ungroup() %>%
  mutate(Target = "absent",
         value = .1) -> chanceexp

samps %>%
  group_by(Target, Parade) %>%
  mutate(row_id = 1:n()) %>%
  spread(Target, value) %>%
  ungroup() %>%
  transmute(FA = FA, 
            Parade = Parade, 
            diff = present - absent) -> samps_diff

samps_diff %>% 
  group_by(Parade, FA) %>%
  mutate(diff = diff * 100) %>%
  dplyr::summarise(M = dmode(diff),
                   lower = hpdi(diff, prob = .95)[1],
                   upper = hpdi(diff, prob = .95)[2],
                   P = mean(diff < 0)) %>%
  ungroup() %>%
  mutate_if(is.numeric, round, 2) -> target_diff


samps_diff %>% group_by(FA, Parade) %>%
  dplyr::summarise(M = dmode(diff),
                   lower = hpdi(diff, prob = .95)[1],
                   upper = hpdi(diff, prob = .95)[2]
  ) -> by_fa_parade


samps %>%
  group_by(Target, Parade) %>%
  mutate(value = value * 100) %>%
  dplyr::summarise(M = dmode(value),
                   lower = hpdi(value, prob = .95)[1],
                   upper = hpdi(value, prob = .95)[2]) %>%
  ungroup() %>%
  mutate_if(is.numeric, round) -> chance

samps %>%
  group_by(FA, Parade, Target) %>%
  summarise(P = mean(value < .1)*100) %>%
  ungroup() %>%
  mutate_if(is.numeric, round) -> p10


samps %>%
  group_by(FA, Parade, Target) %>%
  summarise(P = mean(value < .1)*100) %>%
  ungroup() %>%
  mutate_if(is.numeric, round, 2) -> p_chance



ppdif <- p_chance %>%
  group_by(Parade, Target) %>%
  summarise(p = abs(diff(P)))

