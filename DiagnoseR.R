
divergentpercent <- function(fit){
  
  divergentchains <- fit$sampler_diagnostics()
  sum(colSums(as_tibble(divergentchains[,,2])))/6000
  
}
summaryparameters <- function(fit){
  
  fit$summary()
  
}

str(fit_e1)
# E1
fit_e1 <- readRDS("~/metarocmodel/metarocmodels/Fits/fit_uvsdt_full_APH2016_e1.rds")
divergentpercent(fit_e1)
summaryparameters(fit_e1) %>% arrange(desc(rhat))
fit_e1$summary("sigma_cr")
fit_e1$summary("mu_cr")
fit_e1$summary("b")
fit_e1$summary("b_disc")
fit_e1$summary("sd_1")
fit_e1$summary("sd_2")


# E2
fit_e2 <- readRDS("~/metarocmodel/metarocmodels/Fits/fit_uvsdt_e2_freecrit.RDS")
str(fit_e2_v5)
divergentpercent(fit_e2)
fit_e2$summary()%>% arrange(desc(rhat))
fit_e2$summary("sigma_cr")
fit_e2$summary("mu_cr")
fit_e2$summary("b")
fit_e2$summary("b_disc")
fit_e2$summary("sd_1")
fit_e2$summary("sd_2")
str(fit_e2)
traceplot(fit_e2, pars = "b")
bayesplot::mcmc_trace(fit_e2$draws(variables=c("b")))
bayesplot::mcmc_areas(fit_e2$draws(variables=c("b","b_disc")))


# E3
fit_e3 <- readRDS("~/metarocmodel/metarocmodels/Fits/fit_uvsdt_e3_freecrit.RDS")

divergentpercent(fit_e3)
fit_e3$summary()%>% arrange(desc(rhat))
fit_e3$summary("sigma_cr")
fit_e3$summary("mu_cr")
fit_e3$summary("b")
fit_e3$summary("b_disc")
fit_e3$summary("sd_1")
fit_e3$summary("sd_2")


# E4
fit_e4 <- readRDS("~/metarocmodel/metarocmodels/Fits/fit_uvsdt_e4_freecrit.RDS")

divergentpercent(fit_e4)
fit_e4$summary()%>% arrange(desc(rhat))
fit_e4$summary("sigma_cr")
fit_e4$summary("mu_cr")
fit_e4$summary("b")
fit_e4$summary("b_disc")
fit_e4$summary("sd_1")
fit_e4$summary("sd_2")



# E5
fit_e5 <- readRDS("~/metarocmodel/metarocmodels/Fits/fit_uvsdt_e5_freecrit.RDS")

divergentpercent(fit_e5)
fit_e5$summary()%>% arrange(desc(rhat))
fit_e5$summary("sigma_cr")
fit_e5$summary("mu_cr")
fit_e5$summary("b")
fit_e5$summary("b_disc")
fit_e5$summary("sd_1")
fit_e5$summary("sd_2")

# traceplots fuer gruppenparameter




