library("tidyverse")
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

exp <- d1[[3]] 

exp <- exp %>% mutate(condition = paste0("block",block,"_",condition))
str(exp)

# 3 between subjects conditions.

model_formula_bs <- bf(rating ~  isold:condition + (0 + isold:condition|s|id) + (1|item) ,
                       disc ~ 0 + isold:condition + (0 + isold:condition|s|id), center = FALSE,
                       cmc = FALSE)

model_bs <- make_stancode(model_formula_bs,
                          family = cumulative(link = "probit"),
                          data = exp,
                          save_model = "models/ws_base_e3.stan")



tmpdat_alt <- make_standata(model_formula_bs,
                            family = cumulative(link = "probit"),
                            data = exp)



library("bayestestR")
options(contrasts=c('contr.orthonorm', 'contr.poly'))
newX <- model.matrix(~isold * condition,exp)
tmpdat_alt$X <- newX[,c(2,8:12)]
tmpdat_alt$X_disc <- newX[,c(2,8:12)]
tmpdat_alt$Z_1_1 <- newX[,c(2)]
tmpdat_alt$Z_1_2 <- newX[,c(8)]
tmpdat_alt$Z_1_3 <- newX[,c(9)]
tmpdat_alt$Z_1_4 <- newX[,c(10)]
tmpdat_alt$Z_1_5 <- newX[,c(11)]
tmpdat_alt$Z_1_6 <- newX[,c(12)]
tmpdat_alt$Z_1_disc_7 <- newX[,c(2)]
tmpdat_alt$Z_1_disc_8 <- newX[,c(8)]
tmpdat_alt$Z_1_disc_9 <- newX[,c(9)]
tmpdat_alt$Z_1_disc_10 <- newX[,c(10)]
tmpdat_alt$Z_1_disc_11 <- newX[,c(11)]
tmpdat_alt$Z_1_disc_12 <- newX[,c(12)]

head(newX)
str(tmpdat_alt)


ws_model <- cmdstan_model("models/ws_base_e3.stan")
#bs_model2$print()


fit_e3_freecrit<- ws_model$sample(
  data = tmpdat_alt,
  adapt_delta = 0.99,
  max_treedepth = 20,
  seed = 667667667, 
  init = 0,
  chains = 6, parallel_chains = 6)


fit_e3_freecrit$save_object(file = "Fits/fit_uvsdt_e3_freecrit.RDS")
