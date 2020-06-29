source("functions/functions.R")
library(tidyverse)
library(rethinking)
library(brms)
library(ggeffects)
library(magrittr)

# Load DF
load(file="data/VoiceLineUpOnline.Rda")
d <- d[!duplicated(d$subj),]
d %<>% dplyr::select(conf, acc, subj, lineup, FA, order, TP, L1eng) %>%
  mutate(conf = ordered(conf),
         L1eng = factor(L1eng));d

d %>% count(L1eng)

formula <- bf(acc ~ 0 + L1eng + (L1eng|lineup)) #

get_prior(formula,
          data = d)

priors <- prior("lkj(2)", class = "cor", group = "lineup") +
  prior("cauchy(0, 2.5)",  class = "b") 


# Item response models (see also Rasch)
#https://cran.r-project.org/web/packages/brms/vignettes/brms_nonlinear.html
fit <- brm(formula,
           data = d, 
           chains = 3, 
           cores = 3, 
           iter = 20000, 
           family = brmsfamily("bernoulli"), 
           prior = priors,
           control = list(adapt_delta = 0.99, max_treedepth = 16)
);fit

#plot(marginal_effects(fit), points = F)
#marginal_effects(fit)$L1eng

# Save posterior samples
saveRDS(fit,
        file="stanout/accuracymodelL1.rda",
        compress="xz")

me <- marginal_effects(fit)$`L1eng`
write_csv(me, "stanout/posterior_accuracymodelL1eng.csv")

posterior_samples(fit, "^b") %>%
  as_tibble() %>%
  gather(Param, value) %>%
  mutate(Param = gsub("b_L1eng", "", Param)) %>%
  rename(L1eng = Param) %>%
  mutate(L1eng = ifelse(L1eng == 1, "English", "Others")) %>%
  mutate(value = logistic(value)) -> samps
  
write_csv(samps, "stanout/posterior_accL1en.csv")

# Null model
formula <- bf(acc ~ 1 + (L1eng|lineup)) #

get_prior(formula,
          data = d)

priors <- prior("lkj(2)", class = "cor", group = "lineup") 


# Item response models (see also Rasch)
#https://cran.r-project.org/web/packages/brms/vignettes/brms_nonlinear.html
fit <- brm(formula,
           data = d, 
           chains = 3, 
           cores = 3, 
           iter = 20000, 
           family = brmsfamily("bernoulli"), 
           prior = priors,
           control = list(adapt_delta = 0.99, max_treedepth = 16)
);fit

# Save posterior samples
saveRDS(fit,
        file="stanout/accuracymodelL1_null.rda",
        compress="xz")