source("functions/functions.R")
library(tidyverse)
library(rethinking)
library(brms)
library(ggeffects)
library(magrittr)
inv_logit <- function(x) 1 / (1 + exp(-x))

# Load DF
load(file="data/VoiceLineUpOnline.Rda")
d <- d[!duplicated(d$subj),]
d %<>% dplyr::select(acc, subj, lineup, GFMT, CFMT);d


ggplot(d, aes(x = GFMT, y = CFMT)) +
  geom_point() +
  geom_smooth(method = "lm")


d %<>% mutate(CFMTscaled = scale(CFMT)[,1],
              GFMTscaled = scale(GFMT)[,1]) %>%
  mutate(FaceAbility = (CFMTscaled + GFMTscaled)/2)

range(d$CFMTscaled)
range(d$GFMTscaled)

d %>%
  mutate(FaceAbility = round(FaceAbility)) %>%
  group_by(FaceAbility) %>%
  summarise(prob = mean(acc)) %>%
  ggplot(aes(y = prob, x = FaceAbility)) +
  geom_jitter() +
  geom_smooth(method = "glm", 
            method.args = list(family = "binomial"), 
            se = F) 

formula <- bf(acc ~ .1 + .9*inv_logit(eta),
              eta ~ FaceAbility + (FaceAbility|lineup),
              nl = TRUE) 

get_prior(formula,
          data = d,
          family = bernoulli("identity"))

prior <- prior("normal(0, 3)", class = "b", nlpar = "eta") +
         prior("normal(0, 1)", class = "sd", group = "lineup", nlpar = "eta")

fit <- brm(
  formula,
  family = bernoulli("identity"),
  prior = prior,
  chains = 3, 
  cores = 3,
  iter = 20000, 
  data = d, 
  control = list(adapt_delta = 0.99, max_treedepth = 16)
);summary(fit)

#pp_check(fit)
#plot(fit)
#plot(marginal_effects(fit), points = T)
#marginal_effects(fit)$

# Save posterior samples
saveRDS(fit,
        file="stanout/accuracyfaceskills.rda",
        compress="xz")

me <- marginal_effects(fit)$FaceAbility
write_csv(me, "stanout/posterior_accuracyfaceskills_mes.csv")

posterior_samples(fit, "^b") %>%
  as_tibble() %>%
  rename(Intercept = b_eta_Intercept,
         Effect = b_eta_FaceAbility) %>%
  mutate(Effect = logit2prob(Intercept + Effect) - logit2prob(Intercept),
         Intercept = logit2prob(Intercept)) %>%
  ggplot(aes(y = Intercept, x = Effect)) +
  geom_smooth() +
  geom_point(size = .1)
  

posterior_samples(fit, "^b") %>%
  as_tibble() %>%
  rename(Intercept = b_eta_Intercept,
         Effect = b_eta_FaceAbility) %>%
  mutate(Effect = logit2prob(Intercept + Effect) - logit2prob(Intercept),
         Intercept = logit2prob(Intercept)) %>%
  ggplot(aes(x = Effect)) +
  geom_density()

  
write_csv(samps, "stanout/posterior_accuracyfaceskills_samples.csv")


# NUll model
formula <- bf(acc ~ .1 + .9*inv_logit(eta),
              eta ~ 1 + (FaceAbility|lineup),
              nl = TRUE) 

get_prior(formula,
          data = d,
          family = bernoulli("identity"))

prior <- prior("normal(0, 3)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "lineup", nlpar = "eta")

fit <- brm(
  formula,
  family = bernoulli("identity"),
  prior = prior,
  chains = 3, 
  cores = 3,
  iter = 20000, 
  data = d, 
  control = list(adapt_delta = 0.99, max_treedepth = 16)
);summary(fit)


# Save posterior samples
saveRDS(fit,
        file="stanout/accuracyfaceskills_null.rda",
        compress="xz")
