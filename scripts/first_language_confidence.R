source("functions/functions.R")
library(tidyverse)
library(rethinking)
library(brms)
library(ggeffects)
library(magrittr)

# Load DF
load(file="data/VoiceLineUpOnline.Rda")
d %<>% dplyr::select(conf, acc, subj, lineup, FA, order, TP, L1eng) %>%
  mutate(conf = ordered(conf),
         L1eng = factor(L1eng));d

formula <- bf(conf ~ 0.1 + 0.9 * inv_logit(eta),
              eta ~ acc * L1eng + (1|lineup), nl = TRUE) #

get_prior(formula,
          data = d,family = brmsfamily("cumulative", "logit"))

prior <- prior("normal(0, 3)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "lineup", nlpar = "eta")

fit <- brm(
  formula,
  family = brmsfamily("cumulative", "logit"), 
  prior = prior,
  chains = 3, 
  cores = 3,
  iter = 10000, 
  data = d, 
  control = list(adapt_delta = 0.99, max_treedepth = 16)
);summary(fit)

# Save posterior samples
saveRDS(fit,
        file="stanout/confidencemodelL1.rda",
        compress="xz")

me <- marginal_effects(fit)$`acc:L1eng`
write_csv(me, "stanout/posterior_confidencemodelL1eng.csv")
