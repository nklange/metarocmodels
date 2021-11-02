
library("rstan")
library("tidybayes")
library("brms")
library("cmdstanr")


rds_file <- list.files("data/", full.names = TRUE)

d1 <- purrr::map(rds_file, readRDS)

d1 <- map(d1, ~{
  mutate(.,
         isold = if_else(oldnew == "New", 0, 1),
         oldnew = factor(oldnew, levels = c("New", "Old"))
  )
})

exp <- d1[[1]] 

#exp <- exp %>% mutate(condition = paste0("block",block,"_",condition))
str(exp)

# 3 between subjects conditions.

model_formula_bs <- bf(rating ~  isold:condition + (0 + isold|s|id) + (1|item) ,
                       disc ~ 0 + isold:condition + (0 + isold|s|id), center = FALSE,
                       cmc = FALSE)

# model_bs <- make_stancode(model_formula_bs,
#                               family = cumulative(link = "probit"),
#                               data = exp,
#                               save_model = "models/bs_base_e1.stan")
# 


tmpdat_alt <- make_standata(model_formula_bs,
                            family = cumulative(link = "probit"),
                            data = exp)



library("bayestestR")
options(contrasts=c('contr.orthonorm', 'contr.poly'))
newX <- model.matrix(~isold * condition,exp)
str(tmpdat_alt)
head(newX)
tmpdat_alt$X <- newX[,c(2,4)]
tmpdat_alt$X_disc <- newX[,c(2,4)]

str(tmpdat_alt)


bs_model2 <- cmdstan_model("models/bs_base.stan")
bs_model2$print()


fit_e2_freecrit<- bs_model2$sample(
  data = tmpdat_alt,
  adapt_delta = 0.99,
  max_treedepth = 20,
  seed = 667667667, 
  init = 0,
  chains = 6, parallel_chains = 6)

fit_e2_freecrit$save_object(file = "Fits/fit_uvsdt_e2_freecrit_v2.RDS")



