
solution_unrestricted <- function(
    alpha1, alpha2, alpha3,
    eta_t_p1,
    delta1_act, delta2_act, delta1_ard, delta2_ard, delta3,
    w1, w2, NL,
    beta = 0.95, total_time = 112
) {
  varphi <- vector(mode = "list")
  times <- c("mom_act", "dad_act", "mom_ard", "dad_ard")
  varphi[times] <- beta * eta_t_p1 * c(delta1_act, delta2_act, delta1_ard, delta2_ard)
  varphi[["exp"]] <- beta * eta_t_p1 * delta3
  varphi$mom <- varphi$mom_act + varphi$mom_ard
  varphi$dad <- varphi$dad_act + varphi$dad_ard
  
  A1 <- (w1 * total_time * (alpha3 + varphi$exp) - (alpha1 + varphi$mom)*NL)/
    (w1 * (alpha1 + alpha3 + varphi$mom + varphi$exp))
  A2 <- w2 *(alpha1 + varphi$mom)/
    (w1 * (alpha1 + alpha3 + varphi$exp) - (alpha2 + varphi$dad)*NL)
  B1 <- (w2 * total_time * (alpha3 + varphi$exp) - (alpha2 + varphi$dad)*NL)/
    (w2*(alpha2 + alpha3 + varphi$dad + varphi$exp))
  B2 <- w1 * (alpha2 + varphi$dad)/
    (w2 * (alpha2 + alpha3 + varphi$dad + varphi$exp))
  
  h1 <- (A1 - A2*B1) / (1 - A2*B2)
  h2 <- (B1 - B2*A1) / (1 - A2*B2)
  
  # corner solutions
  
  # both not work
  if (A1 <= 0 & B1 <= 0) {
    h1 <- 0
    h2 <- 0
  }
  
  # dad not work
  if ((A1 - A2*B1) > 0 & (B1 - B2*A1 < 0)) {
    h1 <- A1
    h2 <- 0
  }
  
  # mom not work
  if ((A1 - A1*B1) < 0 & (B1 - B2*A1) > 0) {
    h1 <- 0
    h2 <- B1
  }
  
  income <- w1 * h1 + w2 * h2 + NL
  
  # tau
  tau1_act <- (varphi$mom_act/(alpha1 + varphi$mom)) * (total_time - h1)
  tau2_act <- (varphi$dad_act/(alpha2 + varphi$dad)) * (total_time - h2)
  tau1_ard <- (varphi$mom_ard/(alpha1 + varphi$mom)) * (total_time - h1)
  tau2_ard <- (varphi$dad_ard/(alpha2 + varphi$dad)) * (total_time - h2)
  e <- varphi$exp / (alpha3 + varphi$exp) * income
  
  tau_mom_tot <- tau1_act + tau1_ard
  tau_dad_tot <- tau2_act + tau2_ard
  
  ## return
  list(
    "h1" = h1,
    "h2" = h2,
    "tau1_ard" = tau1_ard,
    "tau2_ard" = tau2_ard,
    "tau1_act" = tau1_act,
    "tau2_act" = tau2_act,
    "e" = e,
    "tau_mom_tot" = tau_mom_tot,
    "tau_dad_tot" = tau_dad_tot
  )
}