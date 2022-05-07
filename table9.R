
sim_data <- read.csv("sim_data.csv")
sim_data_calib <- read.csv("sim_data_calib.csv")
orig_data <- read.delim("../DFW14-Supplementary/Data and Code/child_data.tab",
                        header = TRUE, sep = "\t", na.strings = "-9")
orig_data <- orig_data %>%
  filter(size_kid == 1)

library(dplyr)
library(xtable)

age_groups <- function(var) {
  newvar <- rep(0, length(var))
  newvar[var >= 3 & var <= 5] <- "3-5"
  newvar[var >= 6 & var <= 8] <- "6-8"
  newvar[var >= 9 & var <= 11] <- "9-11"
  newvar[var >= 12 & var <= 15] <- "12-15"
  newvar <- factor(newvar, levels = c("3-5", "6-8", "9-11", "12-15"), ordered = TRUE)
  newvar
}

sim_data$age_grp <- age_groups(sim_data$old_age)
orig_data$age_grp <- age_groups(orig_data$old_age)
sim_data_calib$age_grp <- age_groups(sim_data_calib$old_age)

sim_data$type <- "starting values"
orig_data$type <- "original data"
sim_data_calib$type <- "estimated"

bigdata <- rbind(sim_data_calib, rbind(sim_data, orig_data))
bigdata$anywork_mom <- bigdata$mom_hours > 0
bigdata$anywork_dad <- bigdata$dad_hours > 0

tab9 <- bigdata %>%
  filter(age_grp != "0") %>%
  group_by(age_grp, type) %>%
  summarize(PrWorkMom = mean(anywork_mom, na.rm = TRUE),
            PrWorkDad = mean(anywork_dad, na.rm = TRUE),
            HrWorkMom = mean(mom_hours, na.rm = TRUE),
            HrWorkDad = mean(dad_hours, na.rm = TRUE),
            MomAct = mean(t_old_tot_mom_act, na.rm =TRUE),
            DadAct = mean(t_old_tot_dad_act, na.rm = TRUE),
            MomAround = mean(t_old_tot_mom_around, na.rm = TRUE),
            DadAround = mean(t_old_tot_dad_around, na.rm = TRUE))

tab9x <- xtable(tab9,
                caption = "Work and time allocation. Table 9 from DFW14.",
                label = "tab9")

print(tab9x,
      include.rownames = FALSE,
      file = "results/tab9.tex",
      scalebox = "0.70")
