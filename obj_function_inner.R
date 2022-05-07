# analytic file is tabular dataset
# child_id is numeric index 
# parms is named list of parameters
obj_function_inner <- function(analytic_file, child_id, pstru,
                               sim_draw = 1,
                               tot_cols = 55, maxT = 16,
                               total_time = 112,
                               test_max = 57){
  # eta_t_p1_child1 = NA
  # eta_t_p1_child2 = NA
  # eta_t_p1 = NA
  # 
  # kt_p1_child1 = NA
  # kt_p1_child2 = NA
  # kt_child2 = NA
  # kt_meas_child2 = NA
  # kt_p1_meas_child2 = NA
  # 
  # 
  # kt_p1 = NA
  # kt_meas = NA
  # kt_p1_meas =NA
  # u = NA
  # 
  # delta_mom_act_0 = NA
  # delta_mom_ard_0 = NA
  # delta_mom_0_act = NA
  # delta_mom_0_ard = NA
  # delta_mom_act_act = NA
  # delta_mom_act_ard = NA
  # delta_mom_ard_act = NA
  # delta_mom_ard_ard = NA
  # delta_dad_act_0 = NA
  # delta_dad_ard_0 = NA
  # delta_dad_0_act = NA
  # delta_dad_0_ard = NA
  # delta_dad_act_act = NA
  # delta_dad_act_ard = NA
  # delta_dad_ard_act = NA
  # delta_dad_ard_ard = NA
  # delta1_act = NA
  # delta1_ard = NA
  # delta2_act = NA
  # delta2_ard = NA
  # delta3 = NA
  # delta4 = NA
  # 
  # # set as default
  delta0 = 1
  # 
  # delta0_child1 = NA
  # delta0_child2 = NA
  # delta3_child1 = NA
  # delta3_child2 = NA
  # delta4_child1 = NA
  # delta4_child2 = NA
  
  # fixed technology parameters
  lambda0 <- 0
  lambda1 <- 1
  
  # simulate data -----------------------------------------------------------
  # filter to rows matching child id
  data_id <- analytic_file %>%
    filter(id_1child == child_id)
  
  # single values
  child_age97 <- mean(data_id$old_age97)
  sex <- mean(data_id$old_male)
  mom_educ <- mean(data_id$mom_edu)
  dad_educ <- mean(data_id$dad_edu)
  
  # vectors
  sibling_age_vec <- data_id$yng_age
  mom_age_vec <- data_id$mom_age
  dad_age_vec <- data_id$dad_age
  year_vec <- data_id$year
  
  # initial index
  ind_init <- which(data_id$old_age == data_id$old_age97)
  
  # "quality"
  k_meas <- data_id$old_score[ind_init]
  # latent k from beta
  beta_d <- beta_draw(k_meas)
  k_latent_initial <- exp(log(beta_d / (1 - beta_d)) - lambda0)
  
  # alpha parameters
  alpha1 <- pstru$alpha_draws[1]
  alpha2 <- pstru$alpha_draws[2]
  alpha3 <- pstru$alpha_draws[3]
  alpha4 <- pstru$alpha_draws[4]
  
  # copy data frame
  new_sim_data <- data_id
  
  # "quality"
  kt <- 0
  
  # empty dataframe that will be rbinded (bad, I know)
  new_data <- NULL
  
  for (t in child_age97:maxT) {
    st <- sibling_age_vec[t]
    
    if (t == child_age97){
      kt <- k_latent_initial
    }

    mom_age <- mom_age_vec[t]
    dad_age <- dad_age_vec[t]
    year <- year_vec[t]
    
    # wages this period
    mu1 <- pstru$mom_wage$mu10 +
      pstru$mom_wage$mu11 * mom_educ +
      pstru$mom_wage$mu13 * mom_age +
      pstru$mom_wage$mu14 * mom_age^2

    mu2 <- pstru$dad_wage$mu20 +
      pstru$dad_wage$mu22 * dad_educ +
      pstru$dad_wage$mu23 * dad_age +
      pstru$dad_wage$mu24 * dad_age^2
    
    w1 <- exp(mu1 + pstru$epsilon[t, 1])
    w2 <- exp(mu2 + pstru$epsilon[t, 2])
    
    # non labor income this period
    NL <- pstru$inc$mu_inc + pstru$epsilon[t, 3]
    NL[NL < 0] <- 0
    
    # next period's eta
    eta_t_p1 <- pstru$eta[t+1]
    
    # this period's productivity parameters
    delta1_ard <- pstru$delta1_ard_list[t]
    delta2_ard <- pstru$delta2_ard_list[t]
    delta1_act <- pstru$delta1_act_list[t]
    delta2_act <- pstru$delta2_act_list[t]
    delta3 <- pstru$delta3_list[t]
    delta4 <- pstru$delta4_list[t]
    
    # analytic solution
    ansol <- solution_unrestricted(alpha1, alpha2, alpha3,
                          eta_t_p1,
                          delta1_act, delta2_act, delta1_ard, delta2_ard, delta3,
                          w1, w2, NL)
    # unpack
    h1 <- ansol$h1
    h2 <- ansol$h2
    tau1_ard <- ansol$tau1_ard
    tau2_ard <- ansol$tau2_ard
    tau1_act <- ansol$tau1_act
    tau2_act <- ansol$tau2_act
    e <- ansol$e
    tau_mom_tot <- ansol$tau_mom_tot
    tau_dad_tot <- ansol$tau_dad_tot
    
    # other objects that depend on optimal solution
    income <- w1 * h1 + w2*h2 + NL
    
    # consumption
    consumption <- income - e
    
    # leisure
    le1 <- total_time - h1 - tau_mom_tot
    le2 <- total_time - h2 - tau_dad_tot
    
    # next period's latent and measure of child "quality"
    if (t >= child_age97){
      # next period's latent child quality
      kt_p1 <- delta0 * tau1_act^delta1_act * tau2_act^delta2_act *
                      tau1_ard^delta1_ard * tau2_ard^delta2_ard *
                      e^delta3 * kt *delta4
      
      # binomial distribution for measure in this period (kt measure)
      kt_exp <- exp(lambda0 + log(kt))
      kt_p <- kt_exp / (1 + kt_exp)
      # draw score
      rn <- runif(1)
      kt_meas <- qbinom(rn, test_max, kt_p)
      
      # utility
      utility <- le1^alpha1 * le2^alpha2 * consumption^alpha3 * kt^alpha4
    }

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # %record simulated data
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # replace wages with missing if not working
  w1_acc <- w1
  w1_acc[h1 == 0] <- NA
  w2_acc <- w2
  w2_acc[h2 == 0] <- NA
  
  # placeholder for housework time (not used)
  z1 <- NA
  z2 <- NA

  new_data <- rbind(new_data,
                    new_sim_data %>%
                      filter(old_age == t) %>%
                      mutate(t_old_tot_mom_around = tau1_ard,
                             t_old_tot_dad_around = tau2_ard,
                             t_old_tot_mom_act = tau1_act,
                             t_old_tot_dad_act = tau2_act,
                             mom_hours = h1,
                             dad_hours = h2,
                             mom_wage = w1_acc,
                             dad_wage = w2_acc,
                             old_score = kt_meas,
                             noninc = NL/100,
                             mom_housework = z1,
                             dad_housework = z2,
                             mom_leisure = le1,
                             dad_leisure = le2,
                             expend = e,
                             consume = consumption,
                             kt = kt,
                             kt = kt_p1,
                             kt_p1_meas = kt_p1_meas,
                             expend_ratio = e/income,
                             utility = utility,
                             all_wage1 = w1,
                             all_wage2 = w2)
  )
  new_data$sim_draw <- sim_draw
  # update child quality
  kt <- kt_p1
  }
  new_data
}


