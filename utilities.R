
beta_draw <- function(init_score, test_max = 57){
  alpha <- 1 + init_score
  beta <- test_max - init_score + 1
  rbeta(1, alpha, beta)
}

# vector  of 3 random numbers
hetero_draws <- function(){
  rnorm(3)
}

# wage draws - vector of 3 random numbers
wage_draws <- function(maxT = 18, neps = 3){
  matrix(rnorm((maxT+2) * neps),
         nrow = maxT + 2,
         ncol = neps)
}


constrain_corr <- function(est) {
  -0.95 + (0.95 - (-0.95)) * est
}
