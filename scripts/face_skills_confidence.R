#https://cran.r-project.org/web/packages/brms/vignettes/brms_nonlinear.html
source("functions/functions.R")
library(tidyverse)
library(rethinking)
library(brms)
library(loo)
library(ggeffects)
library(magrittr)
inv_logit <- function(x) 1 / (1 + exp(-x))

# Load DF
load(file="data/VoiceLineUpOnline.Rda") 
d <- d[!duplicated(d$subj),]
d %<>% dplyr::select(conf, acc, subj, lineup, GFMT, CFMT) %>%
  mutate(CFMTscaled = scale(CFMT)[,1],
        GFMTscaled = scale(GFMT)[,1],
        conf = ordered(conf)) %>%
  mutate(FaceAbility = (CFMTscaled + GFMTscaled)/2); d

d %>% ggplot(aes(y = conf, x = FaceAbility )) +
  facet_grid(~acc) +
  geom_point()

formula <- bf(conf ~ 0.1 + 0.9 * inv_logit(eta),
              eta ~ acc * FaceAbility + (FaceAbility|lineup), nl = TRUE) #

#formula <- bf(conf ~ acc * COND + (1|lineup)) #

get_prior(formula,
          data = d,family = brmsfamily("cumulative", "logit"))

prior <- prior("normal(0, 3)", class = "b", nlpar = "eta") +
          prior("normal(0, 1)", class = "sd", group = "lineup", nlpar = "eta")

#vignette("brms_families")

fit <- brm(
  formula,
  family = brmsfamily("cumulative", "logit"), 
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

#names(fit1)
#traceplot(as.mcmc(fit1))
# Save posterior samples
saveRDS(fit,
        file="stanout/confidencemodelfaceability.rda",
        compress="xz")

#me <- marginal_effects(fit)$`acc:Face`
conds <- tibble(conf = rep(1:11, 4),
                acc = rep(seq(0,1,.1), each = 4),
                FaceAblity = seq(-3,1.5,.1)) %>%
  data.frame()

conds <- make_conditions(d, vars = c("acc", "conf", "FaceAbility"))

conditional_effects(fit, 
                    conditions = conds)

write_csv(me, "stanout/posterior_confidencemodelfaceability.csv")

# Null model

formula <- bf(conf ~ 0.1 + 0.9 * inv_logit(eta),
              eta ~ acc + (FaceAbility|lineup), nl = TRUE) #

#formula <- bf(conf ~ acc * COND + (1|lineup)) #

get_prior(formula,
          data = d,family = brmsfamily("cumulative", "logit"))

prior <- prior("normal(0, 3)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "lineup", nlpar = "eta")

#vignette("brms_families")

fit <- brm(
  formula,
  family = brmsfamily("cumulative", "logit"), 
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

#names(fit1)
#traceplot(as.mcmc(fit1))
# Save posterior samples
saveRDS(fit,
        file="stanout/confidencemodelfaceability_null.rda",
        compress="xz")



