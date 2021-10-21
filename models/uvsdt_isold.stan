// generated with brms 2.16.1
functions {
  /* cumulative-probit log-PDF for a single response
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: ordinal thresholds
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cumulative_probit_lpmf(int y, real mu, real disc, vector thres) {
     int nthres = num_elements(thres);
     real p;
     if (y == 1) {
       p = Phi(disc * (thres[1] - mu));
     } else if (y == nthres + 1) {
       p = 1 - Phi(disc * (thres[nthres] - mu));
     } else {
       p = Phi(disc * (thres[y] - mu)) -
           Phi(disc * (thres[y - 1] - mu));
     }
     return log(p);
   }
  /* cumulative-probit log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cumulative_probit_merged_lpmf(int y, real mu, real disc, vector thres, int[] j) {
     return cumulative_probit_lpmf(y | mu, disc, thres[j[1]:j[2]]);
   }
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=2> nthres;  // number of thresholds
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> K_disc;  // number of population-level effects
  matrix[N, K_disc] X_disc;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K] b;  // population-level effects
  ordered[nthres] Intercept;  // temporary thresholds for centered predictors
  ordered[nthres] mu_cr;
  real<lower=0> sigma_cr[nthres];
  vector[K_disc] b_disc;  // population-level effects
}
transformed parameters {
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = X * b;
    // initialize linear predictor term
    vector[N] disc = X_disc * b_disc;
    for (n in 1:N) {
      // apply the inverse link function
      disc[n] = exp(disc[n]);
    }
    for (n in 1:N) {
      target += cumulative_probit_lpmf(Y[n] | mu[n], disc[n], Intercept);
    }
  }
  // priors including constants
  target += student_t_lpdf(b | 4, 1, 1.5);
  target += student_t_lpdf(sigma_cr | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  for (i in 1:nthres) {
    target += student_t_lpdf(mu_cr[i] | 4, 
                             -1.5 + (3 %/% (nthres - 1)) * (i - 1), 
                             0.5);
    Intercept[i] ~ normal(mu_cr[i], sigma_cr[i]);
  }
  
  target += student_t_lpdf(b_disc | 4, 1/1.5, 0.5);
}
generated quantities {
  // compute actual thresholds
  vector[nthres] b_Intercept = Intercept;
}
