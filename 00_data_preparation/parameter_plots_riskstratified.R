library(ggplot2)
library(cowplot)
source("00_data_preparation/load_data.R")
source("00_data_preparation/utils.R")

# colorblind-friendly palette
cbPalette <-
  c(
    "#999999",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )
three_tables <- split_into_3_tables(baselineDT, include_STS = TRUE)
baselineContinuous <- three_tables[[1]]
baselineBoolean <- three_tables[[2]]
baselineFactor <- three_tables[[3]]

#Make STS score risk stratified plots for the continuous / boolean / factor variables

baselineContinuous <-
  baselineContinuous[, risk_level := .(apply(as.matrix(calc_sts), 1, calculateRiskLevels))]

baselineBoolean <-
  baselineBoolean[, risk_level := .(apply(as.matrix(calc_sts), 1, calculateRiskLevels))]
baselineBoolean <- baselineBoolean[,-1]
baselineBoolean <-
  baselineBoolean[, low_risk := lapply(risk_level, function(x) {
    if (x == 'low risk')
      return(1)
    else
      (return(0))
  })]
baselineBoolean$low_risk <- as.numeric(baselineBoolean$low_risk)

baselineFactor <-
  baselineFactor[, risk_level := .(apply(as.matrix(calc_sts), 1, calculateRiskLevels))]
baselineFactor <- baselineFactor[,-1]

# continuous plot
meanPerYear <-
  calculate_means_per_year(baselineContinuous, include_risklvl = TRUE)
ggplot(meanPerYear, aes(x = year, y = mean, colour = risk_level)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                alpha = 0.3,
                width = 0.2) +
  facet_wrap( ~ variable, ncol = 5, scales = 'free') +
  labs(x = 'Time in Years', y = 'Mean ± SD', title = 'Development of mean and standard deviation over the years') +
  scale_x_continuous(breaks = c(2012, 2015, 2019)) +
  theme_bw()
ggsave("./plots/Distribution_ContinuousVars_Years_RiskStrat.png", height = 7, width = 10)

percentPerYear <- calculate_percent_per_year(baselineBoolean, include_risklvl = TRUE)
ggplot(percentPerYear, aes(x = Year, y = percent, colour = RiskLevel)) +
  geom_line() +
  facet_wrap( ~ variable, ncol = 5, scales = 'free') +
  labs(x = 'Time in Years', y = 'Percent of variable = 1', title = 'Development of the boolean variables over the years') +
  theme_bw()

ggsave("./plots/Distribution_BooleanVars_Years_RiskStrat.pdf", height = 26, width = 14)

p <- calculate_factor_percent_per_year(baselineFactor, include_risklvl = TRUE)
#takes ages to plot
#p
save_plot("./plots/Distribution_FactorVars_Years_RiskStrat.pdf", p, ncol = 3, nrow = 7)
