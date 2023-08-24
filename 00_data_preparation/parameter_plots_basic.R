library(ggplot2)
library(cowplot)
source("00_data_preparation/load_data.R")
source("00_data_preparation/utils.R")

# colorblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

######## Plots for device and access site distributions ##########

# Device distributions over the years
ggplot(baselinelabelledDT[Device != ".n" & Device != ".m"], aes(x = Device))+
  geom_bar(aes(fill = as.factor(year(`Date of procedure`))), position = "dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=20))+
  labs(y = "Count")+
  scale_fill_manual(name = "Year of procedure", values = cbPalette)
ggsave("./plots/Devices_per_Year.png", height = 7, width = 12)

# Device size distribution 
ggplot(baselinelabelledDT[!is.na(`Valve size (mm)`),], aes(x = as.factor(`Valve size (mm)`)))+
  geom_bar(aes(fill = as.factor(`Valve size (mm)`)), position = "dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=20))+
  labs(y = "Count", x = "Valve size (mm)")+
  scale_fill_manual(guide = F, values = cbPalette)+
  scale_x_discrete(breaks = c(20,23,25,26,27,29,31,34))
ggsave("./plots/Device_Sizes.png", height = 7, width = 7)

# Access site distribution
ggplot(baselinelabelledDT, aes(x = as.factor(`Main access site`)))+
  geom_bar(aes(fill = as.factor(`Main access site`)), position = "dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=20))+
  labs(y = "Log 10 Count", x = "Main access site")+
  scale_y_log10()+
  scale_fill_manual(guide = F, values = cbPalette)
ggsave("./plots/Access_site_log.png", height = 7, width = 7)

############ Distributions of boolean, continuous and factor variables #########

three_tables <- split_into_3_tables(baselineDT)
baselineContinuous <- three_tables[[1]]
baselineBoolean <- three_tables[[2]]
baselineFactor <- three_tables[[3]]

# Continuous plot: Mean +- SD for all the continuous variables over the years

meanPerYear <- calculate_means_per_year(baselineContinuous)
ggplot(meanPerYear, aes(x = year, y = mean, colour = variable))+
  geom_line()+
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd))+
  facet_wrap(~ variable, ncol = 5, scales = 'free') + theme(legend.position = "none")+
  labs(x = 'Time in Years', y = 'Mean ± SD', title = 'Development of mean and standard deviation over the years')
ggsave("./plots/Distribution_ContinuousVars_Years.png", height = 7, width = 10)

# Boolean plot: Percent of the boolean variables over the years
percentPerYear <- calculate_percent_per_year(baselineBoolean)
ggplot(percentPerYear, aes(x = Year, y = percent, colour = variable))+
  geom_line()+
  facet_wrap(~ variable, ncol = 5, scales = 'free') + theme(legend.position = "none")+
  labs(x = 'Time in Years', y = 'Percent of variable = 1', title = 'Development of the boolean variables over the years')
ggsave("./plots/Distribution_BooleanVars_Years.png", height = 26, width = 14)

#Factor plot: distribution over the years
p <- calculate_factor_percent_per_year(baselineFactor)
#takes ages to plot
#p
#save_plot("./plots/Distribution_FactorVars_Years.pdf", p, ncol = 3, nrow = 7)



