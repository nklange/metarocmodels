
library("tidyverse")

rds_file <- list.files("data/", full.names = TRUE)

d1 <- map(rds_file, readRDS)

str(d1, 1)

map(d1, colnames)

head(d1[[1]])


table(tmpdat$excluding)

map(d1, ~unique(.$exp))

map(d1, ~table(.$excluding))

map(d1, ~table(.$condition))

map(d1, ~table(.$condition, .$oldnew, .$block))

map(d1, ~table(.$rating))

d1 <- map(d1, ~{
  mutate(.,
         isold = if_else(oldnew == "New", 0, 1),
         oldnew = factor(oldnew, levels = c("New", "Old"))
        
         )
})

#######

library("rstan")
library("tidybayes")
library("brms")
library("cmdstanr")

# contr.orthonorm <- bayestestR::contr.orthonorm
# options(contrasts=c('contr.orthonorm', 'contr.poly'))
# 
# contr.orthonorm(3)

### Exp. 1:

e2 <- d1[[1]]
str(e2)


ggplot(e2,aes(x = rating))+
  geom_histogram()+
  facet_grid(isold~.)

uvsdt_m_base <- bf(rating ~ isold, 
              disc ~ 0 + isold, center = FALSE, 
              cmc = FALSE)

uvsdt_m_base_v2 <- bf(rating ~ isold, 
                   disc ~ 0 + isold)

make_stancode(uvsdt_m_base,
  family = cumulative(link = "probit"),
  data = e1)

tmpdat <- make_standata(uvsdt_m_base,
  family = cumulative(link = "probit"),
  data = e1)
str(tmpdat)

head(tmpdat$X)
head(tmpdat$X_disc)

prior1 <- get_prior(uvsdt_m,
              family = cumulative(link = "probit"),
              data = e1)

prior1$prior[1] <- "student_t(4, 1, 1.5)"

prior1$prior[8] <- "student_t(4, -1.5, 0.5)"
prior1$prior[9] <- "student_t(4, -0.75, 0.5)"
prior1$prior[10] <- "student_t(4, 0, 0.5)"
prior1$prior[11] <- "student_t(4, 0.75, 0.5)"
prior1$prior[12] <- "student_t(4, 1.5, 0.5)"

prior1$prior[19] <- "student_t(4, 1/1.5, 0.5)"

prior1[19,]

nrow(prior1)

make_stancode(uvsdt_m,
              family = cumulative(link = "probit"),
              data = e1, prior = prior1)

fit_uvsdt_e1_v1 <- brm(
  uvsdt_m_base, 
  family = cumulative(link = "probit"),
  data = e1,
  control = list(adapt_delta = .99),
  cores = 6, chains = 6,
  backend = "cmdstanr", inits = "0"
)


#### how to fit a model

library(cmdstanr)

duse <- e2
  
uvsdt_m <- bf(rating ~ isold:condition + (isold|s|id) + (1|item), 
              disc ~ 0 + isold:condition + (0 + isold|s|id), center = FALSE, 
              cmc = FALSE)
bs_model <- cmdstan_model("models/uvsdt_between_w-item_v2.stan")
bs_model$print()

tmpdat <- make_standata(uvsdt_m,
                        family = cumulative(link = "probit"),
                        data = duse)

str(tmpdat)
fit_uvsdt_e1_v2 <- bs_model$sample(
  data = tmpdat,
  adapt_delta = 0.95,
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

fit_uvsdt_e1_v2$save_object(file = "Fits/fit_uvsdt_e1_v2.RDS")
saveRDS(rstan::read_stan_csv(fit_uvsdt_e1_v2$output_files()),"rstanfits/fit_uvsdt_e1_v2_rstan.rds")

test <- fit_uvsdt_e1_v2$summary()



