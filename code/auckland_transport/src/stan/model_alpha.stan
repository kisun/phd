data {
  int<lower=0> N;
  int<lower=0> M;
  real<lower=0> t[N];
  real<lower=0> d[N];
  int<lower=0,upper=M> seg[N];
  real<lower=0> s[M];
}

parameters {
  real v;
  real<lower=0,upper=20> gamma;
  real<lower=0> tau[M];
  real<lower=0,upper=100> sigsq;
}

transformed parameters {
  real<lower=0> tau_obs[M];
  real<lower=0,upper=10> sig;
  real T[M];
  real D[M];
  
  T[1] <- 0;
  for (j in 2:M) {
    T[j] <- T[j-1] + tau_obs[j-1] + (s[j] - s[j-1]) / v;
  }
  for (j in 1:M) {
    //if (tau[j] < gamma) {
    //  tau_obs[j] <- 0;
    //} else {
      tau_obs[j] <- tau[j];
    //}
    D[j] <- T[j] + tau_obs[j];
  }
  sig <- sqrt(sigsq);
}

model {
  v ~ normal(0, 50);
  //gamma ~ uniform(0, 20);
  //sigsq ~ uniform(0, 100);
  for (j in 1:M) {
    tau[j] ~ exponential(1.0 / 10);
  }
  
  for (i in 1:N) {
    if (t[i] >= T[seg[i]] && t[i] < D[seg[i]]) {
      // bus it at a bus stop
      increment_log_prob(normal_log(d[i], s[seg[i]], 1e3));
    } else {
      //increment_log_prob(normal_log(d[i], s[seg[i]] + v * (t[i] - D[seg[i]]), 1e3));
    }
  }
}


