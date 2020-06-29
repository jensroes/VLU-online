rm(list = ls()); gc()
source("functions/functions.R")
source("functions/fit_brms.R")
library(tidyverse)
library(brms)
library(loo)
library(ggeffects)
library(magrittr)

# Load DF
load(file="data/VoiceLineUpOnline.Rda")
# Preprocess data
d %>% 
  mutate(dup = duplicated(subj)) %>%
  filter(!dup) %>% select(-dup) %>%
  select(subj, lineup, FA, order, TP, resp_given, resp_expec) %>%
  filter(order == "serial") %>% dplyr::select(-order) %>%
  select(resp_given, resp_expec, subj, FA, TP) %>%
  mutate(resp_given = ifelse(resp_given == 0, 10, resp_given),
         resp_expec = ifelse(resp_expec == 0, 10, resp_expec)) %>% 
  mutate(corr_response = ifelse(resp_expec == resp_given, 1, 0),
         resp_expec = factor(resp_expec)) %>%
#  mutate(corr_response = recode(corr_response, `1` = "correct",
#                                `0` = "incorrect")) %>%
  select(subj, resp_given, resp_expec, TP, FA, corr_response ) -> data;data  
 

# Model formulas
f_1 <- bf(resp_given ~  1 )
f_2 <- bf(resp_given ~  1 + resp_expec)
f_3 <- bf(resp_given ~  1 + cs(resp_expec))
f_4 <- bf(resp_given ~  1 + FA)
f_5 <- bf(resp_given ~  1 + cs(resp_expec) + FA)

# Model fits
(fs <- ls(pattern = "f_"))
for(i in fs){
  output_name <- paste0("modelfit_", str_split(i, "_")[[1]][2])
  print(paste0("Model:", i))
  assign(output_name,
    fit_brms_acat(formula = eval(as.name(i)), data = data)
  )
}

# Compare models
(loos <- ls(pattern = "modelfit_")[c(2,3,5)])

mc <- do.call(loo, lapply(loos, as.name))

mc$diffs %>% as.data.frame() %>%
  mutate(model=row.names(.)) %>%
  separate(model, into = c("remove", "id"), sep = "_") %>%
  select(-remove) -> mc2;mc2

fs <- paste0("f_", gsub("modelfit_", "", loos))
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

file_out <- "stanout/loo_results_serial_acat.csv"
write_csv(x = mc3, path = file_out)

# Save posterior of best models
saveRDS(modelfit_2,
        file="stanout/ordinal_model_serial.rda",
        compress="xz")

saveRDS(modelfit_3,
        file="stanout/ordinal_model_serial_cs.rda",
        compress="xz")




conditional_effects(modelfit_2, 
                  robust = TRUE, # median instead of mean
                  effects = "resp_expec",
                    categorical = TRUE
            #        conditions = make_conditions(data, c("TP", "corr_response"))
)


conditional_effects(modelfit_3, 
                    robust = TRUE, # median instead of mean
                    effects = "resp_expec",
                    categorical = TRUE
)


