// generated with brms 2.16.1
functions {
 /* compute correlated group-level effects
  * Args: 
  *   z: matrix of unscaled group-level effects
  *   SD: vector of standard deviation parameters
  *   L: cholesky factor correlation matrix
  * Returns: 
  *   matrix of scaled group-level effects
  */ 
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    // r is stored in another dimension order than z
    return transpose(diag_pre_multiply(SD, L) * z);
  }
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
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  vector[N] Z_1_disc_2;
  int<lower=1> NC_1;  // number of group-level correlations
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
 
}
parameters {
  vector[K] b;  // population-level effects

  ordered[nthres] Intercept[N_1];  // temporary thresholds for centered predictors
  ordered[nthres] mu_cr;
  real<lower=0> sigma_cr[nthres];
  
  vector[K_disc] b_disc;  // population-level effects
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
}
transformed parameters {
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_1;
  vector[N_1] r_1_disc_2;
  vector[N_2] r_2_1;  // actual group-level effects
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_1 = r_1[, 1];
  r_1_disc_2 = r_1[, 2];
  r_2_1 = (sd_2[1] * (z_2[1]));
  

}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = X * b;
    // initialize linear predictor term
    vector[N] disc = X_disc * b_disc;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      disc[n] += r_1_disc_2[J_1[n]] * Z_1_disc_2[n];
    }
    for (n in 1:N) {
      // apply the inverse link function
      disc[n] = exp(disc[n]);
    }
    for (n in 1:N) {
      target += cumulative_probit_lpmf(Y[n] | mu[n], disc[n], Intercept[J_1[n],]);
    }
  }
  // priors including constants
  target += student_t_lpdf(b[1] | 4, 1.5, 1.5);
  target += student_t_lpdf(b[2:K] | 4, 0, 1/sqrt(2));
  target += student_t_lpdf(b_disc[1] | 4, log(1.0/2), 0.5);
  target += student_t_lpdf(b_disc[2:K] | 4, 0, 0.4);
  target += student_t_lpdf(sigma_cr| 3, 0, 2)
    - 1 * student_t_lccdf(0 | 3, 0, 2);
    
  for (i in 1:nthres) {
  target += student_t_lpdf(mu_cr[i] | 4,
                           -0.75 + (3.0 / (nthres - 1)) * (i - 1),
                           0.5);

 // if (i < (nthres + 1)/2.0) Intercept[,i] ~ normal(mu_cr[i], sigma_cr[i]);
  //if (i == (nthres + 1)/2.0)  Intercept[,i] ~ normal(mu_cr[i], .1);
//  if (i > (nthres + 1)/2.0) Intercept[,i] ~ normal(mu_cr[i], sigma_cr[i-1]);
 Intercept[,i] ~ normal(mu_cr[i], sigma_cr[i]);
  }
  target += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 2 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(to_vector(z_1));
  target += lkj_corr_cholesky_lpdf(L_1 | 1);
  target += student_t_lpdf(sd_2 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_2[1]);
}
generated quantities {
  // compute actual thresholds
  //vector[nthres] b_Intercept = Intercept;
  vector[N] mupred1 = X * b;
  vector[N] discpred1 = X_disc * b_disc;
  vector[N] mupred;
  vector[N] discpred;
  vector[N] log_p;
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // extract upper diagonal of correlation matrix
  for (k in 1:M_1) {
    for (j in 1:(k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }

  for (n in 1:N) {
      // add more terms to the linear predictor
      mupred[n] = mupred1[n] + r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n];
      discpred[n] =  exp(discpred1[n] + r_1_disc_2[J_1[n]] * Z_1_disc_2[n]);
    }
    
  for(n in 1:N){
   # log_p[n] = cumulative_probit_lpmf(Y[n] | mupred[n], discpred[n], Intercept[J_1[n],]);
  }

 }
