
sim_data <- read.csv("sim_data.csv")
sim_data_calib <- read.csv("sim_data_calib.csv")
orig_data <- read.delim("../DFW14-Supplementary/Data and Code/child_data.tab",
                               header = TRUE, sep = "\t", na.strings = "-9")
orig_data <- orig_data %>%
  filter(size_kid == 1)


library(ggplot2)

# child quality
ggplot() +
  geom_smooth(data = sim_data_calib, aes(x = old_age, y = old_score, color = "Estimated")) +
  geom_smooth(data = sim_data, aes(x = old_age, y = old_score, color = "Starting Values")) +
  geom_point(data = orig_data, aes(x = old_age, y = old_score, color = "Data")) +
  theme_bw() +
  labs(x = "Child Age", y = "Letter-Word Score") +
  scale_color_discrete(name = "Source")
ggsave("lw_sim_vs_data.png", width = 6, height = 4, units = "in")
