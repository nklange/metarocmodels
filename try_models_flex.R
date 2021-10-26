
library("tidyr")
library("magrittr")
library("purrr")
library("dplyr")

# uncount(dsum, weights = count)

rds_file <- list.files("data/", full.names = TRUE)

d1 <- purrr::map(rds_file, readRDS)

d1 <- map(d1, ~{
  mutate(.,
         isold = if_else(oldnew == "New", 0, 1),
         oldnew = factor(oldnew, levels = c("New", "Old"))
         )
})

exp <- d1[[5]]

#exp <- exp %>% mutate(condition = paste0("block",block,"_",condition))
str(exp)

# ggplot(exp,aes(x=rating))+
#   geom_histogram()+
#   facet_wrap(isold~condition)


# Make stan code ----

library("rstan")
library("tidybayes")
library("brms")
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library("cmdstanr")


# as number of random slopes (within-subjects manipulations) is diffeerent across experiments:
# make stan model specific for each experiment
# alternative would be to change stan file to be parameterized less explicitely, i.e. use more loops and matrix coding to access random effects


model_formula <- bf(rating ~  isold:condition + (isold:condition|s|id) + (1|item) ,
                     disc ~ 0 + isold:condition + (0 + isold:condition|s|id), center = FALSE,
                     cmc = FALSE)

stanvars <- stanvar(scode = "ordered[nthres] Intercept[N_1];",
                    block = "parameters", pll_args = "real intercept") +
  stanvar(scode="ordered[nthres] mu_cr;",
          block = "parameters") +
  stanvar(scode="real<lower=0> sigma_cr[nthres];",
          block = "parameters") +
  stanvar(scode="for (n in 1:N) {
  target += cumulative_probit_lpmf(Y[n] | mu[n], disc[n], Intercept[J_1[n],]);
}",
          block = "likelihood",position="end") +
  # above: change criteria to be subject dependent
  # priors: mu_o, criteria, sigma_o
  stanvar(scode="target += student_t_lpdf(b | 4, 1, 1.5);",
          block = "model") +
  stanvar(scode="target += student_t_lpdf(sigma_cr | 3, 0, 2.5)  - 1 * student_t_lccdf(0 | 3, 0, 2.5);",
          block = "model") +
  stanvar(scode="for (i in 1:nthres) {
  target += student_t_lpdf(mu_cr[i] | 4,
                           -1.5 + (3 %/% (nthres - 1)) * (i - 1),
                           0.5);
  Intercept[,i] ~ normal(mu_cr[i], sigma_cr[i]);
}",
          block = "model") +
  stanvar(scode="target += student_t_lpdf(b_disc | 4, 1/1.5, 0.5);",
          block = "model")

stanmodel <- make_stancode(model_formula,
              family = cumulative(link = "probit"),
              data = exp,
              stanvars =stanvars) %>%
  stringr::str_remove("ordered\\[nthres\\] Intercept\\;") %>% # remove the old declarations
  stringr::str_remove("vector\\[nthres\\] b_Intercept = Intercept\\;") %>%
  stringr::str_remove("target \\+= student_t_lpdf\\(Intercept \\| 3\\, 0\\, 2.5\\)\\;") %>%
  stringr::str_remove("target \\+= cumulative_probit_lpmf\\(Y\\[n\\] \\| mu\\[n\\]\\, disc\\[n\\]\\, Intercept\\)\\;") %>%
  write_stan_file() %>%
  cmdstan_model() ->
  cmdstanr_model


tmpdat <- make_standata(model_formula,
                        family = cumulative(link = "probit"),
                        data = exp)
#str(tmpdat)


stanmodel$print()
fit <- stanmodel$sample(
  data = tmpdat,
  adapt_delta = 0.99,
  max_treedepth = 20,
  seed = 667667667, 
  init = 0,
  chains = 6, parallel_chains = 6)

# Warning: 342 of 6000 (6.0%) transitions ended with a divergence.
# This may indicate insufficient exploration of the posterior distribution.
# Possible remedies include: 
#  * Increasing adapt_delta closer to 1 (default is 0.8) 
# * Reparameterizing the model (e.g. using a non-centered parameterization)
# * Using informative or weakly informative prior distributions 

# also warning about leapfrog steps ... increased max_treedepth to 20 instead of 10

fit$save_object(file = "Fits/fit_uvsdt_e5_v1.RDS")
saveRDS(rstan::read_stan_csv(fit$output_files()),"rstanfits/fit_uvsdt_e5_v1_rstan.rds")


str(fit_e1)

                  