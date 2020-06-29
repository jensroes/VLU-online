# Specify model fit parameters
fit_brms <- function(formula, data){
  nchains <- ncores <- 3
  niter <- 20000
  npred <- length(trimws(str_split(formula[1], "~")[[1]])[-1])
  if(npred > 1){
    prior <- #prior("normal(0, 5)", class = "Intercept") +
             prior("student_t(10, 0, 1)", class = "b") 
  }
  else{
    prior = NULL #prior("normal(0, 5)", class = "Intercept")
  }

  fit <- brm(
           formula,
           family = bernoulli("cloglog"),
           chains = nchains, 
           prior = prior,
           cores = ncores,
           iter = niter, 
           data = data, 
           control = list(adapt_delta = 0.99, max_treedepth = 16)
         )
  return(fit)
}

# Specify model fit parameters
fit_brms_binom <- function(formula, data){
  nchains <- ncores <- 3
  niter <- 20000
  form <- trimws(str_split(formula[1], "~")[[1]])[-1]
  npred <- length(form)
  intercept <- substr(form, start = 1, stop = 1)
  if(intercept != 1){
    if(npred > 1){
      prior <- prior("normal(0, 5)", class = "Intercept") +
        prior("student_t(10, 0, 1)", class = "b") 
    }
    else{
      prior <- prior("normal(0, 5)", class = "Intercept")
    }
  }
  if(intercept == 0){
    if(npred > 1){
      prior <- prior("student_t(10, 0, 1)", class = "b") 
    }
    else{
      prior = NULL
    }
  }
  fit <- brm(
    formula,
    family = bernoulli("logit"),
    chains = nchains, 
    prior = prior,
    cores = ncores,
    iter = niter, 
    data = data, 
    control = list(adapt_delta = 0.99, max_treedepth = 16)
  )
  return(fit)
}

# Specify model fit parameters
fit_brms_cm <- function(formula, data){
  nchains <- ncores <- 3
  niter <- 20000
  npred <- length(trimws(str_split(formula[1], "~")[[1]])[-1])
  if(npred > 1){
    prior <- prior("student_t(10, 0, 1)", class = "b") 
  }
  else{
    prior = NULL
  }
  
  fit <- brm(
    formula,
    family = cumulative("probit"),
    chains = nchains, 
    prior = prior,
    cores = ncores,
    iter = niter, 
    data = data, 
    control = list(adapt_delta = 0.99, max_treedepth = 16)
  )
  return(fit)
}

# Specify model fit parameters
fit_brms_acat <- function(formula, data){
  nchains <- ncores <- 3
  niter <- 20000
  npred <- length(trimws(str_split(formula[1], "~")[[1]])[-1])
  if(npred > 1){
    prior <- prior("student_t(10, 0, 1)", class = "b") 
  }
  else{
    prior = NULL
  }
  
  fit <- brm(
    formula,
    family = acat("probit"),
    chains = nchains, 
    prior = prior,
    cores = ncores,
    iter = niter, 
    data = data, 
    control = list(adapt_delta = 0.99, max_treedepth = 16)
  )
  return(fit)
}


# Specify model fit parameters
fit_brms_sratio <- function(formula, data){
  nchains <- ncores <- 3
  niter <- 20000
  npred <- length(trimws(str_split(formula[1], "~")[[1]])[-1])
  if(npred > 1){
    prior <- prior("student_t(10, 0, 1)", class = "b") 
  }
  else{
    prior = NULL
  }
  
  fit <- brm(
    formula,
    family = sratio("cloglog"),
    chains = nchains, 
    prior = prior,
    cores = ncores,
    iter = niter, 
    data = data, 
    control = list(adapt_delta = 0.99, max_treedepth = 16)
  )
  return(fit)
}

