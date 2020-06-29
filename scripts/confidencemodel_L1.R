#https://cran.r-project.org/web/packages/brms/vignettes/brms_nonlinear.html
source("functions/functions.R")
library(tidyverse)
library(brms)
library(loo)
library(ggeffects)
library(magrittr)
inv_logit <- function(x) 1 / (1 + exp(-x))

# Load DF
load(file="data/VoiceLineUpOnline.Rda")
d <- d[!duplicated(d$subj),]
d %<>% 
  filter(L1eng == 1) %>%
  dplyr::select(conf, acc, subj, lineup, FA, order, TP) %>%
  mutate(conf = ordered(conf))

d$COND <- paste(d$TP, d$order, d$FA, sep ="_"); 
d$COND <- factor(d$COND);summary(d$COND)

d$COND2 <- paste(d$TP, d$FA, sep ="_"); 
d$COND2 <- factor(d$COND2);summary(d$COND2)

formula <- bf(conf ~ 0.1 + 0.9 * inv_logit(eta),
              eta ~ acc * COND + (COND2|lineup), nl = TRUE) 

prior <- prior("normal(0, 3)", class = "b", nlpar = "eta") +
          prior("normal(0, 1)", class = "sd", group = "lineup", nlpar = "eta")

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


#names(fit1)
#traceplot(as.mcmc(fit1))
# Save posterior samples
saveRDS(fit,
        file="stanout/confidencemodel_L1.rda",
        compress="xz")

me <- marginal_effects(fit)$`acc:COND`
write_csv(me, "stanout/posterior_confidencemodel_L1.csv")

