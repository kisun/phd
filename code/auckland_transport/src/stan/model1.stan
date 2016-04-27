data {
  int<lower=0> N;
  int<lower=0> M;
  real<lower=0> t[N];
  real<lower=0> d[N];
  int<lower=0,upper=M> seg[N];
  int<lower=0,upper=M> stop[N];
  real<lower=0> s[M];
}
transformed data {

}
parameters {
  real<lower=0,upper=100> sigsq_obs;
  real<lower=0,upper=50> v[M-1];        // velocity
  real<lower=0> gamma;
  real<lower=0> mu_tau;
  real<lower=0> tau[M-1];
  real<lower=0,upper=1> alpha[M];
  real<lower=0,upper=1> pi[M];
}
transformed parameters {
  real<lower=0> sig_obs;
  real D[M];
  real T[M];
  
  T[1] <- 0;
  for (j in 1:(M-1)) {
    if (alpha[j] < pi[j]) {
      D[j] <- T[j] + (gamma + tau[j]);
    } else {
      D[j] <- T[j];
    }
    T[j+1] <- D[j] + (s[j+1] - s[j]) / v[j];
  }
  D[M] <- max(t);

  sig_obs <- sqrt(sigsq_obs);
}
model {
  gamma ~ uniform(0, 30);
  mu_tau ~ uniform(0, 5 * 60);
  v ~ uniform(0, 30);
  for (j in 1:(M-1)) {
    if (alpha[j] < pi[j]) {
      tau[j] ~ exponential(mu_tau);
    }
  }
  pi ~ beta(0.1,0.1);
  alpha ~ uniform(0,1);

  // piecewise linear functions
  sigsq_obs ~ uniform(0, 100);
  for (i in 1:N) {
    if (seg[i] > 0) {
      increment_log_prob(normal_log(d[i] - s[seg[i]],
				    v[seg[i]] * (t[i] - D[seg[i]]),
				    sig_obs));
    } else {
      // if (d[i] < T[stop[i]+1]) { // arrives before arrival time ...
      // 	increment_log_prob(- log(T[stop[i]+1] - d[i]));
      // } else if (d[i] > D[stop[i]+1]) { // bus departs after departure time ...
      // 	increment_log_prob(- log(d[i] - D[stop[i]+1]));
      // }
    }
  }
}




