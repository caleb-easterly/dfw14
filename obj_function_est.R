obj_function_est <- function(analytic_file, parms,
                             maxT = 16,
                             discount = 0.95){
  mean_pref1 <- parms[1] - 100
  mean_pref2 <- parms[2] - 100
  mean_pref3 <- parms[3] - 100
  sigma_pref1 <- exp(parms[4] - 100)
  sigma_pref2 <- exp(parms[5] - 100)
  sigma_pref3 <- exp(parms[6] - 100)
  
  ## corrs w constraints
  p7 <- parms[7] - 100
  p7exp <- exp(p7) / (1 + exp(p7))
  corr_pref1_pref2 <- constrain_corr(p7exp)
  
  p8 <- parms[8] - 100
  p8exp <- exp(p8) / (1 + exp(p8))
  corr_pref1_pref3 <- constrain_corr(p8exp)
  
  p9 <- parms[9] - 100
  p9exp <- exp(p9) / (1 + exp(p9))
  corr_pref2_pref3 <- constrain_corr(p9exp)
  
  ## covariance for hetero draw
  v1 <- sigma_pref1^2
  v2 <- sigma_pref2^2
  v3 <- sigma_pref3^2
  cov12 <- sigma_pref1 * sigma_pref2 * corr_pref1_pref2
  cov13 <- sigma_pref1 * sigma_pref3 * corr_pref1_pref3
  cov23 <- sigma_pref2 * sigma_pref3 * corr_pref2_pref3
  
  # matrix
  cov_matrix <- matrix(
    c(v1,    cov12,  cov13,
      cov12, v2,     cov23,
      cov13, cov23,  v3),
    byrow = TRUE,
    nrow = 3
  )
  
  # check positive definiteness
  # should throw an error if not PD
  cholcov <- chol(cov_matrix)
  
  # psi
  psi <- exp(parms[10] - 100)
  
  # alphas
  hetdraw <- hetero_draws()
  disturb_alpha <- exp(
    c(mean_pref1, mean_pref2, mean_pref3) + c(cholcov %*% hetdraw)
  )
  denom <- sum(disturb_alpha) + 1
  alpha_draws <- c(
    disturb_alpha / denom,
    1 / denom
  )
  
  # technology parameters
  delta11_act = (parms[11]-100)/10
  delta21_act = (parms[12]-100)/10
  delta11_ard = (parms[13]-100)/10
  delta21_ard = (parms[14]-100)/10
  delta31 = (parms[15]-100)/10
  delta41 = (parms[16]-100)/10
  
  delta12_act = (parms[17]-100)/100
  delta22_act = (parms[18]-100)/100
  delta12_ard = (parms[19]-100)/100
  delta22_ard = (parms[20]-100)/100
  delta32 = (parms[21]-100)/100
  delta42 = (parms[22]-100)/100

  # time-varying productivity parameters
  delta0_list <- rep(1, maxT)
  delta1_ard_list <- rep(0, maxT)
  delta2_ard_list <- rep(0, maxT)
  delta1_act_list <- rep(0, maxT)
  delta2_act_list <- rep(0, maxT)
  delta3_list <- rep(0, maxT)
  delta4_list <- rep(0, maxT)
  
  for (t in 1:maxT) {
    delta1_act_list[t] = exp(delta11_act + delta12_act*(t-1))
    delta2_act_list[t] = exp(delta21_act + delta22_act*(t-1))
    delta1_ard_list[t] = exp(delta11_ard + delta12_ard*(t-1))
    delta2_ard_list[t] = exp(delta21_ard + delta22_ard*(t-1))
    delta3_list[t] = exp(delta31 + delta32*(t-1))
    delta4_list[t] = exp(delta41 + delta42*(t-1))
  }
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # %backwards recursion of eta parameters for model solution
  # %--these don't depend on wage draws or other characteristics,
  # %so calculate here to save time later
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  eta <- rep(0, maxT + 1)
  eta[maxT + 1] <- psi * alpha_draws[4]
  for (t in maxT:1){
    eta[t] <- alpha_draws[4] + discount * eta[t+1] * delta4_list[t]
  }
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # %wage and income parameters
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  std1 = exp(parms[23]-100)
  
  std2 = exp(parms[24] - 100)
  
  cor12_raw = parms[25]-100;
  cor12_exp = exp(cor12_raw) / (1 + exp(cor12_raw))
  corr_1_2 = constrain_corr(cor12_exp)
  
  mu10 = parms[26]-100
  mu11 = (parms[27]-100)/100
  
  mu13 = (parms[28]-100)/1000
  mu14 = (parms[29]-100)/1000
  
  mu20 = parms[30]-100;
  mu22 = (parms[31]-100)/100
  
  mu23 = (parms[32]-100)/1000
  mu24 = (parms[33]-100)/1000
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # %create epsilon wage and income shocks
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  #covariance matrix for wages;
  wage_v1 = std1^2;
  wage_v2 = std2^2;
  wage_cov12 = (std1*std2*corr_1_2);
  wage_cov_matrix = matrix(
    c(wage_v1, wage_cov12,
      wage_cov12, wage_v2),
    byrow=TRUE,
    nrow=2
  )
  
  chol_wage <- chol(wage_cov_matrix)
  
  wage_d <- wage_draws()
  
  epsilon_wages <- matrix(0, nrow = maxT + 2, ncol = 3)
  
  # pull numbers from obj_funct_all (lines 74-92)
  # starting values for optim algorithm from data moments
  mu_inc <- 122.0819
  sigma_inc <- 5.2711
  
  for (t in 1:(maxT + 2)){
    wage_tmp <- chol_wage %*% wage_d[t, 1:2]
    epsilon_wages[t, 1] <- wage_tmp[1]
    epsilon_wages[t, 2] <- wage_tmp[2]
    epsilon_wages[t, 3] <- sigma_inc * wage_d[t, 3]
  }
  
  # save parameters
  pstru <- vector(mode = "list")
  pstru[["delta0_list"]] <- delta0_list
  pstru[["delta1_ard_list"]] <- delta1_ard_list
  pstru[["delta2_ard_list"]] <- delta2_ard_list
  pstru[["delta1_act_list"]] <- delta1_act_list
  pstru[["delta2_act_list"]] <- delta2_act_list
  pstru[["delta3_list"]] <- delta3_list
  pstru[["delta4_list"]] <- delta4_list
  
  #mean of TFP process (unclear if this is really being used)
  # delta0_list_mean <- rep(0, maxT)
  # delta01 <- NA
  # delta02 <- NA
  # for (t in 1:maxT) {
  #   delta0_list_mean[t] <- exp(delta01 + delta02 * (t-1))
  # }
  
  pstru[["tech"]] <- list(
    "delta11_act" = delta11_act,
    "delta12_act" = delta12_act,
    "delta11_ard" = delta11_ard,
    "delta12_ard" = delta12_ard,
    "delta21_act" = delta12_act,
    "delta22_act" = delta22_act,
    "delta21_ard" = delta21_ard,
    "delta22_ard" = delta22_ard,
    "delta31" = delta31,
    "delta32" = delta32,
    "delta41" = delta41,
    "delta42" = delta42
  )
  
  pstru[["eta"]] <- eta
  pstru[["alpha_draws"]] <- alpha_draws
  
  # mom wage
  pstru[["mom_wage"]] <- list(
    "mu10" = mu10,
    "mu11" = mu11,
    "mu13" = mu13,
    "mu14" = mu14*1000,
    "std1" = std1,
    "corr_1_2" = corr_1_2
  )
  
  pstru[["dad_wage"]] <- list(
    "mu20" = mu20,
    "mu22" = mu22,
    "mu23" = mu23,
    "mu24" = mu24*1000,
    "std2" = std2
  )
  
  pstru[["inc"]] <- list(
    "mu_inc" = mu_inc,
    "sigma_inc" = sigma_inc
  )
  
  # vp parameters
  pstru[["epsilon"]] <- epsilon_wages

  # return parameters
  pstru
}
