
library(dplyr)
library(reshape2)
library(xtable)

pan <- read.delim("../DFW14-Supplementary/Data and Code/child_data.tab",
                  header = TRUE, sep = "\t", na.strings = "-9")

pan1 <- pan %>%
  filter(size_kid == 1)

pan2 <- pan %>%
  filter(size_kid == 2)

cds_sum <- pan1 %>%
  filter(year == 1997) %>%
  summarise(
    mom_age_mean = mean(mom_age),
    mom_age_sd = sd(mom_age),
    dad_age_mean = mean(dad_age),
    dad_age_sd = sd(dad_age),
    mom_educ_mean = mean(mom_edu),
    mom_educ_sd = sd(mom_edu),
    dad_educ_mean = mean(dad_edu),
    dad_educ_sd = sd(dad_edu),
    child_age_mean = mean(old_age),
    child_age_sd = sd(old_age),
    prop_male_mean = mean(old_male),
    prop_male_sd = sd(old_male),
    lw_score_mean = mean(old_score),
    lw_score_sd = sd(old_score),
    lw_score_med = median(old_score),
    lw_score_min = min(old_score),
    lw_score_max = max(old_score),
    n = n()
  ) %>%
  melt()

cds_sum$measure <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 1)
cds_sum$name <- c("Mother's age",
                  "Mother's age",
                  "Father's age",
                  "Father's age",
                  "Mother's education",
                  "Mother's education",
                  "Father's education",
                  "Father's education",
                  "Child's age",
                  "Child's age",
                  "Fraction male",
                  "Fraction male",
                  "Mean LW raw score",
                  "Mean LW raw score",
                  "Median LW raw score",
                  "Min. LW raw score",
                  "Max. EW raw score",
                  "N"
)

toptab <- cds_sum %>%
  dcast(name ~ measure)

# nice row names
colnames(toptab) <- c("", "Mean", "SD")

# sort
toptab <- toptab[c(9, 2, 10, 3, 1, 4, 6, 7, 8, 5, 11), ]

# make xtable
tab1 <- xtable(toptab,
               caption = "Descriptive statistics from 1997 PSID-CDS.",
               label = "desc1",
               digits = matrix(c(0, 0, 2, 2,
                                 0, 0, 2, 2,
                                 0, 0, 2, 2,
                                 0, 0, 2, 2,
                                 0, 0, 2, 2,
                                 0, 0, 3, 3,
                                 0, 0, 2, 2,
                                 0, 0, 0, 0,
                                 0, 0, 0, 0,
                                 0, 0, 0, 0,
                                 0, 0, 0, 0),
                               nrow = 11, byrow = TRUE))
# write out to file

print(tab1,
      include.rownames = FALSE,
      file = "results/desc1.tex")

#### Descriptive table 2: wages and work hours
desc2 <- pan1 %>%
  summarise(
    mom_wkhr_mean = mean(mom_hours, na.rm = TRUE),
    mom_wkhr_sd = sd(mom_hours, na.rm = TRUE),
    dad_wkhr_mean = mean(dad_hours, na.rm = TRUE),
    dad_wkhr_sd = sd(dad_hours, na.rm = TRUE),
    mom_wage_mean = mean(mom_wage, na.rm = TRUE),
    mom_wage_sd = sd(mom_wage, na.rm = TRUE),
    dad_wage_mean = mean(dad_wage, na.rm = TRUE),
    dad_wage_sd = sd(dad_wage, na.rm = TRUE),
    nonlabor_mean = mean(noninc, na.rm = TRUE),
    nonlabor_sd = sd(noninc, na.rm = TRUE),
    n = 105
) %>%
  melt()

desc2$measure <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1)
mom_wkhr_name <- "Mother's work hours per week"
dad_wkhr_name <- "Father's work hours per week"
mom_wage_name <- "Mother's hourly wage"
dad_wage_name <- "Father's hourly wage"
non_lab_name <- "Non-labor income per week"
desc2$name <- c(mom_wkhr_name,
                mom_wkhr_name,
                dad_wkhr_name,
                dad_wkhr_name,
                mom_wage_name,
                mom_wage_name,
                dad_wage_name,
                dad_wage_name,
                non_lab_name,
                non_lab_name,
                "N")

toptab2 <- desc2 %>%
  dcast(name ~ measure)

# nice row names
colnames(toptab2) <- c("", "Mean", "SD")

# reorder
toptab2_re <- toptab2[c(4, 2, 3, 1, 6, 5), ]
tab2 <- xtable(toptab2_re,
               caption = "Descriptive statistics from 1996-2002 PSID.",
               label = "desc2",
               digits = matrix(c(0, 0, 2, 2,
                                 0, 0, 2, 2,
                                 0, 0, 2, 2,
                                 0, 0, 2, 2,
                                 0, 0, 2, 2,
                                 0, 0, 0, 0),
                               nrow = 6, byrow = TRUE))
print(tab2,
      include.rownames = FALSE,
      file = "results/desc2.tex")


## figure 1
library(ggplot2)

ggplot(pan1, aes(x = old_age, y = old_score)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Child Age (Years)", y = "Letter-Word Score") +
  scale_x_continuous(breaks = seq(3, 18, by = 3), limits = c(3, 17)) +
  scale_y_continuous(breaks = seq(0, 60, by = 10), limits = c(0, 70))
ggsave("results/lw_age.png", width = 6, height = 4, units = "in")
