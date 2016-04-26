data {
  int<lower=0> N;
  int<lower=0> M;
  real<lower=0> t[N];
  real<lower=0> d[N];
  real<lower=0> s[M];
  real<lower=0> smin[M];
  real<lower=0> smax[M];
}
transformed data {
  real<lower=0> ds[M];
  for (j in 1:M) {
    ds[j] <- smax[j] - smin[j];
  }
}
parameters {
  real<lower=0> T[M];                 // arrival time at stops
  real<lower=0,upper=1> pi[M];        // probability bus stops
  real<lower=0,upper=min(ds)> gamma;  // acceleration/deceleration + open/close doors
  real<lower=0> mu_tau;               // (mean) passenger boarding/alighting
  real<lower=0> tau[M];
}
transformed parameters {
  real<lower=0> D[M-1];   // departure time
  for (j in 2:(M-1)) {
    D[j] <- T[j] + p[j] * (gamma + tau[j]);
  }
}
model {
  gamma ~ uniform(0, max(ds));
  mu_tau ~ lognormal(10, 20);
  
  for (j in 2:(M-1)) {
    T[j] ~ uniform(smin[j], smax[j]);
    tau[j] ~ exponential(mu_tau) T[, ds[j] - gamma];
    
  }
}


