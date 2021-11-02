library("rstan")
library("tidybayes")
library("brms")
library("cmdstanr")
library("tidyverse")
library("bayestestR")
options(contrasts=c('contr.orthonorm', 'contr.poly'))

rds_file <- list.files("data", full.names = TRUE)

d1 <- purrr::map(rds_file, readRDS)

d1 <- purrr::map(d1, ~{
  mutate(.,
         isold = oldnew
  )
})

exp2 <- d1[[4]]

#exp2 <- exp %>% mutate(condition = paste0("block",block,"_",condition))


#############################
# Make and adjust model code
#############################

model_formula_bs <- bf(rating ~  isold:condition + (0 + isold|s|id) + (1|item) ,
                       disc ~ 0 + isold:condition + (0 + isold|s|id), center = FALSE,
                       cmc = FALSE)

model_formula_ws <- bf(rating ~  isold:condition + (0 + isold:condition|s|id) + (1|item) ,
                       disc ~ 0 + isold:condition + (0 + isold:condition|s|id), center = FALSE,
                       cmc = FALSE)

if (unique(exp$manipulation) == "Between"){
  
  model_formula <- model_formula_bs
  
} else if (unique(exp$manipulation) == "Within"){
  
  model_formula <- model_formula_ws
  
}

# Add custom threshold parameters
stanvars <- stanvar(scode = "ordered[nthres] Intercept[N_1];", # Threshold parameters
                    block = "parameters", pll_args = "real intercept") +
  stanvar(scode="ordered[nthres] mu_cr;", # threshold individual mu_cr
          block = "parameters") +
  stanvar(scode="real<lower=0> sigma_cr[nthres];",# threshold individual sigma_cr
          block = "parameters")

# Add custom priors
prior <- prior_string("target += student_t_lpdf(b[1] | 4, 1.5, 1.5)", check = FALSE) +
  prior_string("target += student_t_lpdf(b[2:K] | 4, 0, 1/sqrt(2))", check = FALSE) +
  prior_string("target += student_t_lpdf(b_disc[1] | 4, log(1.0/2), 0.5)", check = FALSE) +
  prior_string("target += student_t_lpdf(b_disc[2:K] | 4, 0, 0.4)", check = FALSE) +
  prior_string("target += student_t_lpdf(sigma_cr | 3, 0, 2)  - 1 * student_t_lccdf(0 | 3, 0, 2)", check = FALSE)+
  prior_string("for (i in 1:nthres) {
  target += student_t_lpdf(mu_cr[i] | 4,
                            -0.75 + (3.0 / (nthres - 1)) * (i - 1),
                           0.5);
  Intercept[,i] ~ normal(mu_cr[i], sigma_cr[i]);
}", check = FALSE)


stanmodel <- make_stancode(model_formula,
                           family = cumulative(link = "probit"),
                           data = exp,
                           stanvars = stanvars,
                           prior = prior) %>% # add stanvars
  stringr::str_remove("ordered\\[nthres\\] Intercept\\;") %>% # remove the old declarations
  stringr::str_remove("vector\\[nthres\\] b_Intercept = Intercept\\;") %>%
  stringr::str_remove("target \\+= student_t_lpdf\\(Intercept \\| 3\\, 0\\, 2.5\\)\\;") %>%
  stringr::str_replace("target \\+= cumulative_probit_lpmf\\(Y\\[n\\] \\| mu\\[n\\]\\, disc\\[n\\]\\, Intercept\\)\\;",
                       "target \\+= cumulative_probit_lpmf\\(Y\\[n\\] \\| mu\\[n\\]\\, disc\\[n\\]\\, Intercept\\[J_1\\[n\\]\\,\\]\\)\\;") %>%
  write_stan_file() %>%
  cmdstan_model() ->
  cmdstanr_model

##################################
# Make and adjust data
##################################


tmpdat <- make_standata(model_formula,
                        family = cumulative(link = "probit"),
                        data = exp)
newX <- model.matrix(~isold * condition,exp)

selectorth <- as_tibble(newX) %>% select(c("isold",matches("isold:")))

# replace contrasts for population-level effects

tmpdat$X <- selectorth
tmpdat$X_disc <- selectorth

if (unique(exp$manipulation) == "Within"){
  
  # replace contrasts for group-level effects
  # replace Zs for mu_o
  # replace Zs for sigma_o
  tmpdat[grepl("Z_1_", names(tmpdat))] <- c(selectorth,selectorth)
  
}

str(tmpdat)
####################################
# Fit model
####################################


fit2<- stanmodel$sample(
  data = tmpdat,
  adapt_delta = 0.99,
  max_treedepth = 20,
  seed = 667667667,
  init = 0,
  chains = 6, parallel_chains = 6)

fit2$save_object(file = paste0("Fits/fit_uvsdt_full_",unique(exp2$exp),".rds"))
