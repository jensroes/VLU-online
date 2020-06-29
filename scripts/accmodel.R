rm(list=ls());gc()
library(MASS)
library(tidyverse)
library(brms)
library(loo)
library(magrittr)
source("functions/functions.R")
source("functions/fit_brms.R")

# Load DF
load(file="data/VoiceLineUpOnline.Rda")
d <- d[!duplicated(d$subj),]
      
d$COND <- paste(d$TP, d$order, d$FA, sep ="_"); 
d$COND <- factor(d$COND);summary(d$COND)

d$COND2 <- paste(d$TP, d$FA, sep ="_"); 
d$COND2 <- factor(d$COND2);summary(d$COND2)

# Model formulas
#f_1 <- bf(acc ~  order + (COND2|lineup))
#f_2 <- bf(acc ~  order:FA + (COND2|lineup))
#f_3 <- bf(acc ~  order:TP + (COND2|lineup))
#f_4 <- bf(acc ~  order:(FA + TP) + (COND2|lineup))
#f_5 <- bf(acc ~  order:TP:FA + (COND2|lineup))
f_6 <- bf(acc ~  0 + COND + (COND2|lineup))

# Model fits
(fs <- ls(pattern = "f_"))
for(i in fs){
  output_name <- paste0("modelfit_", str_split(i, "_")[[1]][2])
  print(paste0("Model:", i))
  assign(output_name,
         fit_brms_binom(formula = eval(as.name(i)), data = d)
  )
}

# Compare models
(loos <- ls(pattern = "modelfit_"))

mc <- do.call(loo, lapply(loos, as.name))

mc$diffs %>% as.data.frame() %>%
  mutate(model=row.names(.)) %>%
  separate(model, into = c("remove", "id"), sep = "_") %>%
  select(-remove) -> mc2;mc2

fs <- paste0("f_", substr(loos, start = nchar(loos), stop = nchar(loos) ))
forms <- lapply(lapply(fs, as.name), eval)
forms <- unlist(sapply(forms, '[[', 1))
forms <- unlist(sapply(forms, '[[', 3))
forms <- paste(forms)

tibble(fs) %>%
  separate(fs, into = c("remove", "id"), sep = "_", remove = F) %>%
  select(-remove) %>%
  left_join(mc2) %>%
  select(-id) %>%
  mutate(model = forms) %>%
  select(model, everything()) %>%
  arrange(desc(elpd_diff)) -> mc3;mc3

mc3 %>% select(model, fs, elpd_diff, se_diff)

# Save model comparison
file_out <- "stanout/loo_results_accmodel.csv"
write_csv(x = mc3, path = file_out)

#pp_check(fit)
#plot(fit)
plot(marginal_effects(modelfit_6), points = F)
#marginal_effects(fit)

saveRDS(modelfit_6,
        file="stanout/accmodel.rda",
        compress="xz")

samps <- posterior_samples(modelfit_6, "^b_COND") %>%
  as_tibble() 

write_csv(samps, "stanout/posterior_accmodel.csv")

