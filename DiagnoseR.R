
divergentpercent <- function(fit){
  
  divergentchains <- fit$sampler_diagnostics()
  sum(colSums(as_tibble(divergentchains[,,2])))/6000
  
}

summaryparameters <- function(fit){
  
  fit$summary()
  
}

# E1
fit_e1 <- readRDS("~/metarocmodel/metarocmodels/Fits/fit_uvsdt_e1_v3.RDS")

divergentpercent(fit_e1)
summaryparameters(fit_e1)

# E2
fit_e2 <- readRDS("~/metarocmodel/metarocmodels/Fits/fit_uvsdt_e2_v3.RDS")

divergentpercent(fit_e2)
summaryparameters(fit_e2)


# E3
fit_e3 <- readRDS("~/metarocmodel/metarocmodels/Fits/fit_uvsdt_e3_v1.RDS")

divergentpercent(fit_e3)
summaryparameters(fit_e3)


# E3
fit_e3 <- readRDS("~/metarocmodel/metarocmodels/Fits/fit_uvsdt_e3_v1.RDS")

divergentpercent(fit_e3)
summaryparameters(fit_e3)


# E5
fit_e5 <- readRDS("~/metarocmodel/metarocmodels/Fits/fit_uvsdt_e5_v1.RDS")

divergentpercent(fit_e5)
summaryparameters(fit_e5)


