source("functions/functions.R")
library(tidyverse)
library(rethinking)
library(brms)
library(ggeffects)
library(magrittr)

# Load DF
load(file="data/VoiceLineUpOnline.Rda")
d <- d[!duplicated(d$subj),]
d %<>% dplyr::select(acc, subj, lineup, GFMT, CFMT);d

d %<>% mutate(CFMTscaled = scale(CFMT)[,1],
              GFMTscaled = scale(GFMT)[,1])

ggplot(d, aes(x = GFMTscaled, y = CFMTscaled)) +
  geom_point() +
  geom_smooth(method = "lm")


fit <- brm(data = d, 
      family = student,
      mvbind(GFMTscaled, CFMTscaled) ~ 1,
      prior = c(prior(gamma(2, .1), class = nu),
                prior(normal(0, 100), class = Intercept),
                prior(normal(0, 100), class = sigma, resp = GFMTscaled),
                prior(normal(0, 100), class = sigma, resp = CFMTscaled),
                prior(lkj(1), class = rescor)),
      iter = 20000, warmup = 10000, chains = 3, cores = 3, seed = 365)

fit

# Save posterior samples
saveRDS(fit,
        file="stanout/correlationfaceskills.rda",
        compress="xz")


posterior_samples(fit, "^rescor") %>%
  as_tibble() %>%
  rename(cor = starts_with("rescor")) -> corr

corr %>% 
  summarise(M = mean(cor),
            lo = HPDI(cor, prob = .95)[1],
            up = HPDI(cor, prob = .95)[2])

write_csv(corr, "stanout/posterior_correlationfaceskills.csv")

