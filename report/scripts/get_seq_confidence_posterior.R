m <- readRDS("../stanout/confidencemodelsequential.rda")
fixef(m) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  filter(rowname == "eta_acc") %>%
  select(-rowname, -Est.Error) %>%
  round(2) %>% gather(p,v) %>% pull(v) -> accconf

conf <- conditional_effects(m, effects = "COND")$COND
conf <- conf %>% select(COND, estimate__, lower__, upper__) 
names(conf) <- c("COND", "est", "low", "up")

rownames(conf) <- NULL
conf %<>% separate(COND, into = c("Target", "FA")) %>%
  select(FA, Target, est, low, up) %>%
  arrange(FA, Target) %>%
  mutate(FA = c("no warning", "", "warning", "") ) %>%
  round_df(2)