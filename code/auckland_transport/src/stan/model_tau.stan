data {
  int N;
  int M;
  real tau[N];
  int<lower=1,upper=M> s[N];
}

parameters {
  real<lower=0> mu;
  real<lower=0> mu_tau[M];
  real<lower=0,upper=100> sigsq;
}

transformed parameters {
  real<lower=0> sig;
  sig <- sqrt(sigsq);
}


model {
  sigsq ~ uniform(0, 100);
  mu ~ uniform(0, 1000);
  for (j in 1:M) {
    mu_tau[j] ~ normal(mu, sig) T[0,];
  }
  for (i in 1:N) {
    tau[i] ~ exponential(1 / mu_tau[s[i]]);
  }
}


