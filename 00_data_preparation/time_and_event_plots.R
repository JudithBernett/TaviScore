source("00_data_preparation/create_time2event.R")
source("00_data_preparation/utils.R")
library(dplyr)
library(tidyr)

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

allCauseDeath_Distr <-
  as.data.table(select(finalTable, event, proc_date, time, calc_sts))
allCauseDeath_Distr <-
  allCauseDeath_Distr[, risk_level := .(apply(as.matrix(calc_sts), 1, calculateRiskLevels))]
allCauseDeath_Distr <- allCauseDeath_Distr[, -4]
allCauseDeath_Distr$year <- year(allCauseDeath_Distr$proc_date)
allCauseDeath_Distr <- allCauseDeath_Distr[, -2]
percentPerYear <-
  allCauseDeath_Distr[, lapply(.SD, function(x)
    sum(x, na.rm = T) / .N), by = .(risk_level, year)]
percentPerYear <-
  percentPerYear %>% gather(colnames(percentPerYear[,-c(1, 2)]), key = 'variable', value = 'percent')

# Distribution of event and time over the years
ggplot(percentPerYear, aes(x = year, y = percent, colour = risk_level)) +
  facet_wrap(~ variable, scales = 'free') +
  geom_line() +
  theme_bw() +
  labs(x = 'Time in Years', y = 'Percent (/100) of event == 1 / \n average of time in days')
#ggsave("/Users/judith_bernett/Desktop/Bachelorarbeit/Plots/Event_Time_Distr_Strat.pdf")

finalTable <- as.data.table(finalTable)
finalTable <-
  finalTable[, risk_level := .(apply(as.matrix(calc_sts), 1, calculateRiskLevels))]
finalTable$risk_level <- as.factor(finalTable$risk_level)

ggList <-
  lapply(c(
    "age",
    "height",
    "weight",
    "bmi",
    "bsa",
    "scoreii_log",
    "calc_sts",
    "creatinine",
    "gfr",
    "hb",
    "thrombo",
    "ck",
    "hrate",
    "tte_lvef",
    "tte_gradient_mean",
    "gradient_mean",
    "lvef"
  ), function(i) {
    g <- ggplot(finalTable, aes(x = as.factor(event), y = get(i))) +
      geom_boxplot() +
      labs(x = "Event", y = "Continuous Variables", title = i) + theme_bw()
    
    return(g)
  })

# Boxplots for all the continuous variables
p <- plot_grid(
  plotlist = ggList,
  align = 'v',
  ncol = 4,
  nrow = 5
)
# p
# save_plot("./plots/Distribution_Boxplot.pdf", p2, ncol = 4, nrow = 5)

##### effect of different parameter combinations #####

finalTable <-
  finalTable[, gradLvefStrat := .(apply(as.matrix(cbind(gradient_mean, lvef)), 1, function(x)
    calculateGradLVEFStrat(x[1], x[2])))]
finalTable$gradLvefStrat <- as.factor(finalTable$gradLvefStrat)

finalTable$year <- as.factor(year(finalTable$proc_date))
a <-
  finalTable[, getFactorDistr(.SD, get("gradLvefStrat"), "gradLvefStrat"), by = .(event, year)]

#Gradient + LVEF distribution in combination over the years
ggplot(a, aes(
  x = as.numeric(year),
  y = percent,
  colour = lvl
)) +
  geom_smooth() +
  facet_wrap(~ event)

b <-
  finalTable[, lapply(.SD[, c("medi_anticoagu", "medi_oac_noac")], function(x)
    sum(x, na.rm = T) / .N), by = .(event, year)]

# Marcoumar/Sintrom vs OAC/NOAC stratified by event over the years
ggplot(b) +
  geom_line(aes(
    x = as.numeric(b$year),
    y = b$medi_anticoagu,
    colour = "Marcoumar/Sintrom"
  )) +
  geom_line(aes(
    x = as.numeric(b$year),
    y = b$medi_oac_noac,
    colour = "OAC/NOAC"
  )) +
  facet_wrap(~ event) +
  labs(x = "year", y = "medication")

c <-
  finalTable[, lapply(.SD[, "sex"], function(x)
    sum(x, na.rm = T) / .N), by = .(event, year)]

# Male gender stratified by event
ggplot(c, aes(x = as.numeric(year), y = sex)) +
  geom_line() +
  facet_wrap(~ event)

####### event distributions ###########
#look at cumulative event distribution
ggplot(finalTable, aes(x = time, color = as.factor(year))) +
  stat_ecdf(geom = "line") +
  labs(x = "Time from the procedure until the event in days", y = "Percentage of all events") +
  scale_color_manual(name = "Year of procedure", values = cbPalette) +
  theme_bw() +
  theme(text = element_text(size = 20))
#ggsave("./plots/ecdf_events.pdf", height = 7, width = 12)

# percentage of events / year
tmp <- finalTable
tmp <- tmp[, perc_event := 100 * sum(event == 1) / .N, by = year]
ggplot(tmp, aes(
  x = as.factor(year),
  y = perc_event,
  fill = as.factor(year)
)) +
  geom_col(position = "dodge") +
  theme_bw() +
  labs(x = "Year of Procedure", y = "Percentage of events") +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(values = cbPalette, guide = F)
#ggsave("./plots/col_events.pdf", height = 7, width = 12)

########### baseline distributions stratified by event ##############
#device bias:
wholeTableLabelled$year <-
  as.factor(year(wholeTableLabelled$`Date of procedure`))
wholeTableLabelled <-
  wholeTableLabelled[wholeTableLabelled$Device != ".n" &
                       wholeTableLabelled$Device != ".m",]
devices <-
  as.data.table(model.matrix(as.formula("~ Device -1"), wholeTableLabelled))
colnames(devices) <-
  c(
    "Allegra NVT",
    "BSC Lotus",
    "Direct Flow Medical",
    "Edwards Centera",
    "Edwards Sapien 3",
    "Edwards Sapien 3 ULTRA",
    "Edwards Sapien XT",
    "Medtronic CoreValve",
    "Medtronic Evolut PRO",
    "Medtronic Evolut R",
    "SJM Portico",
    "Symetis Acurate Neo"
  )
devices <- cbind(devices, wholeTableLabelled[, c('event', 'year')])
percentDevices <-
  devices[, lapply(.SD, function(x)
    sum(x, na.rm = T) / .N), by = .(event, year)]
percentDevices <-
  percentDevices %>% gather(colnames(percentDevices[, -c(1, 2)]), key = 'Device', value = 'percent')
percentDevices <- as.data.table(percentDevices)
percentDevices$event <- as.factor(percentDevices$event)
percentDevices$Device <- as.factor(percentDevices$Device)
percentDevices$year <- as.numeric(percentDevices$year)

ggplot(percentDevices, aes(x = year, y = percent * 100, color = event)) +
  geom_line() +
  facet_wrap( ~ Device, scales = "free") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_color_manual(values = c("0" = "#999999", "1" = "#0072B2"),
                     name = "Deceased\nwithin\none year") +
  labs(x = "Year", y = "Percent implanted")
#ggsave("./plots/AllDevices_ToEvent.png", height = 7, width = 15)

# Violin Plots for continuous variables:
baselineContinuous <-
  select(
    finalTable,
    event,
    year,
    age,
    height,
    weight,
    bmi,
    bsa,
    scoreii_log,
    calc_sts,
    creatinine,
    gfr,
    hb,
    thrombo,
    ck,
    hrate,
    gradient_mean,
    lvef
  )
baselineContinuous$event <- as.factor(baselineContinuous$event)
colnames(baselineContinuous) <-
  c(
    "Event",
    "Year",
    "Age",
    "Height",
    "Weight",
    "BMI",
    "BSA",
    "EuroSCORE II",
    "STS Score",
    "Creatinine",
    "GFR",
    "Hb",
    "Thrombocytes",
    "Creatinine Kinase",
    "Heart Rate",
    "Mean Gradient",
    "LVEF"
  )
continuous_vars <-
  baselineContinuous[,-c("Year")] %>% melt(id.vars = "Event",
                                           variable.names = "variable",
                                           value.name = "value")

ggplot(continuous_vars, aes(x = variable, y = value, fill = as.factor(Event))) +
  geom_violin() +
  facet_wrap( ~ variable, scales = "free", ncol = 5) +
  theme_bw() +
  scale_fill_manual(values = c("0" = "#999999", "1" = "#0072B2"),
                    name = "Deceased\nwithin\none year") +
  theme(
    text = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(y = "Value")
#ggsave("./plots/ViolinPlots_continuousvars.png", height = 7, width = 12)

finalTable <- as.data.table(finalTable)
baselineBoolean <-
  finalTable[,-c(
    "Patient",
    "proc_date",
    "time",
    "age",
    "height",
    "weight",
    "bmi",
    "bsa",
    "scoreii_log",
    "calc_sts",
    "creatinine",
    "gfr",
    "hb",
    "thrombo",
    "ck",
    "hrate",
    "tte_lvef",
    "tte_gradient_mean",
    "gradient_mean",
    "lvef",
    "risk_level",
    "gradLvefStrat",
    "premeasure",
    "tte_regurg_aortic1",
    "tte_regurg_aortic2",
    "tte_regurg_aortic3",
    "tte_regurg_mitral1",
    "tte_regurg_mitral2",
    "tte_regurg_mitral3",
    "tte_regurg_tricuspid1",
    "tte_regurg_tricuspid2",
    "tte_regurg_tricuspid3",
    "regurgitation1",
    "regurgitation2"
  )]
baselineBoolean$event <- as.factor(baselineBoolean$event)
colnames(baselineBoolean) <-
  c(
    "event",
    "Male Gender",
    "Diabetes",
    "Art. hypertension",
    "Dyslipidemia",
    "COPD",
    "Cerebrov. accident",
    "History of Stroke",
    "Pacemaker",
    "Prev. BAV",
    "Coron. art. disease",
    "History of PCI",
    "History of MI",
    "Periph.l art. disease",
    "Prev. cardiac surgery",
    "Dyspnea",
    "Stable angina pectoris",
    "Syncope",
    "NYHA III/IV",
    "Aspirin",
    "P2Y12",
    "Clopidogrel",
    "Marcoumar/Sintrom",
    "NOAC",
    "OAC/NOAC",
    "Rivaroxaban",
    "Statin",
    "ACE inhibitor",
    "ATII Antagonist",
    "Betablocker",
    "Ca-antagonist",
    "Diuretics",
    "Steroid",
    "Insulin",
    "Oral antidiabetics",
    "Renal failure",
    "Atrioventr. Block",
    "Local anesthesia",
    "Concom. BAV",
    "Concom. procedure",
    "Concom. PCI",
    "Other concom. proc.",
    "Expandable device",
    "Paravalvular",
    "NYHA I",
    "NYHA II",
    "NYHA III",
    "NYHA IV",
    "CCS 0",
    "CCS 1",
    "CCS 2",
    "No Angina",
    "CCS 1/2",
    "CCS 3/4",
    "Only Aspirin",
    "Only OAC/NOAC",
    "Aspirin+P2Y12",
    "No altern. anticoag.",
    "Troponin T",
    "Troponin hs",
    "Sinus rhythm",
    "Atrial fibrillation",
    "LBBB",
    "RBBB",
    "No LBBB/RBBB",
    "Aortic regurg. 1",
    "Aortic regurg. 2",
    "Aortic regurg. 3",
    "Aortic regurg. 4",
    "Mitral regurg. 1",
    "Mitral regurg. 2",
    "Mitral regurg. 3",
    "Tricuspid regurg. 1",
    "Tricuspid regurg. 2",
    "Tricuspid regurg. 3",
    "Additional CT",
    "Additional MRI",
    "Right fem. access",
    "Left fem. access",
    "year",
    "perc_event"
  )

meanPerYear <-
  baselineContinuous[, lapply(.SD, mean, na.rm = T), by = .(Event, Year)]
meanPerYear <-
  meanPerYear %>% gather(colnames(meanPerYear[, -c(1, 2)]), key = 'variable', value = 'mean')
sdPerYear <-
  baselineContinuous[, lapply(.SD, sd, na.rm = T), by = .(Event, Year)]
sdPerYear <-
  sdPerYear %>% gather(colnames(sdPerYear[, -c(1, 2)]), key = 'variable', value = 'sd')
meanPerYear <-
  merge(meanPerYear, sdPerYear, by = c('Event', 'variable', 'Year'))

#Continuous variables distribution stratified by event
ggplot(meanPerYear, aes(
  x = as.numeric(Year),
  y = mean,
  colour = Event
)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                alpha = 0.3,
                width = 0.2) +
  facet_wrap( ~ variable, ncol = 5, scales = 'free') +
  labs(x = 'Time in Years', y = 'Mean ± SD') +
  scale_fill_manual(values = c("0" = "#999999", "1" = "#0072B2"),
                    name = "Deceased\nwithin\none year") +
  scale_color_manual(values = c("0" = "#999999", "1" = "#0072B2"),
                     name = "Deceased\nwithin\none year") +
  scale_x_continuous(breaks = c(2012, 2015, 2019)) +
  theme_bw() +
  theme(text = element_text(size = 20))
#ggsave("./plots/Distribution_ContinuousVars_Years_EventStrat.pdf", height = 7, width = 10)

percentPerYear <-
  baselineBoolean[, lapply(.SD, function(x)
    sum(x, na.rm = T) / .N), by = .(event, year)]
percentPerYear <-
  percentPerYear %>% gather(colnames(percentPerYear[, -c(1, 2)]), key = 'variable', value = 'percent')
percentPerYear <- as.data.table(percentPerYear)

#Boolean variables distribution stratified by event
ggplot(percentPerYear, aes(x = year, y = percent, color = event)) +
  geom_line() +
  scale_fill_manual(values = c("0" = "#999999", "1" = "#0072B2"),
                    name = "Deceased\nwithin\none year") +
  scale_color_manual(values = c("0" = "#999999", "1" = "#0072B2"),
                     name = "Deceased\nwithin\none year") +
  facet_wrap( ~ variable, ncol = 8, scales = 'free') +
  labs(x = 'Time in Years', y = 'Percent of variable = 1') +
  theme_bw()
#ggsave("./plots/Distribution_BooleanVars_Years_EventStrat.pdf", height = 18, width = 14)

percentPerYear[variable == "sex"]$variable <- "Male Gender"
percentPerYear[variable == "tropo_type2"]$variable <- "Troponin T"
percentPerYear[variable == "tropo_type3"]$variable <-
  "Troponin T hs"
percentPerYear[variable == "rhythm2"]$variable <-
  "Atrial fibrillation"
percentPerYear[variable == "mi"]$variable <- "History of MI"
percentPerYear[variable == "regurg_mitral2"]$variable <-
  "Mitral regurgitation grade 2"
percentPerYear[variable == "regurg_mitral3"]$variable <-
  "Mitral regurgitation grade 3"
percentPerYear[variable == "regurg_tricuspid3"]$variable <-
  "Tricuspid regurgitation grade 3"
percentPerYear[variable == "dyspnea_nyha4"]$variable <-
  "NYHA class IV"
percentPerYear[variable == "ad"]$variable <-
  "Peripheral artery disease"
percentPerYear[variable == "copd"]$variable <- "COPD"
percentPerYear[variable == "concom_other"]$variable <-
  "Other concomitant procedure"
percentPerYear[variable == "image1"]$variable <-
  "Additional CT performed"
percentPerYear[variable == "medi_anticoagu"]$variable <-
  "Marcoumar/Sintrom"
percentPerYear[variable == "medi_oac_noac"]$variable <-
  "Novel Anticoagulation Drugs"
percentPerYear[variable == "medi_diuretic"]$variable <- "Diuretics"
percentPerYear[variable == "medi_combi1"]$variable <- "Only Aspirin"
percentPerYear[variable == "proc_access1"]$variable <-
  "Femoral Access"

#Significant boolean variables stratified by event
ggplot(percentPerYear[variable %in% c(
  "Male Gender",
  "Troponin T",
  "Troponin T hs",
  "Atrial fibrillation",
  "History of MI",
  "Mitral regurgitation grade 2",
  "Mitral regurgitation grade 3",
  "Tricuspid regurgitation grade 3",
  "NYHA class IV",
  "Peripheral artery disease",
  "COPD",
  "Other concomitant procedure",
  "Additional CT performed",
  "Marcoumar/Sintrom",
  "Novel Anticoagulation Drugs",
  "Diuretics",
  "Only Aspirin",
  "Femoral Access"
),], aes(x = as.numeric(year), y = percent * 100, color = event)) +
  geom_line() +
  facet_wrap( ~ variable, ncol = 3, scales = 'free') +
  labs(x = 'Time in Years', y = 'Percentage per year') +
  scale_color_manual(values = c("0" = "#999999", "1" = "#0072B2"),
                     name = "Deceased\nwithin\none year") +
  theme_bw() +
  theme(text = element_text(size = 20))
#ggsave("./plots/significant_booleans_eventStrat.pdf", width = 13, height = 13)


percentPerYear <-
  percentPerYear[, mean := mean(percent), by = .(variable, event)]
percentPerYear <-
  percentPerYear[, sd := sd(percent), by = .(variable, event)]

#Mean +- SD for the boolean variables percentage stratified by event
ggplot(percentPerYear[year == "2015", ], aes(x = event, y = mean, fill = event)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                alpha = 0.3,
                width = 0.2) +
  facet_wrap( ~ variable, ncol = 8, scales = 'free') +
  labs(x = 'Event', y = 'Percent of variable = 1', title = 'Mean of the boolean variables stratified by all-cause death') +
  theme_bw()

#ggsave("./plots/Mean_BooleanVars_EventStrat.pdf", height = 18, width = 14)



