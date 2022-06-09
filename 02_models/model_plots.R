source("02_models/utils.R")
library(data.table)
library(ggplot2)
library(survival)
library(survAUC)
library(pheatmap)
library(RColorBrewer)

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


finalTableDT <- fread("table1yrAllCause.csv")[,-c("proc_date")]

#RANDOM FOREST
table1yrAll <-
  finalTableDT[,-c(
    "Patient",
    "scoreii_log",
    "calc_sts",
    "image1",
    "image3",
    "proc_access1",
    "proc_access2",
    "height",
    "weight",
    "tropo_type2",
    "delay1",
    "delay2",
    "delay3"
  )]


#LASSO-on-all: Smaller Model
fAllSmaller<- 'Surv(time, event) ~ sex + age + copd + ad + medi_statin + medi_diuretic + hb + block + gradient_mean + medi_combi1 + regurg_mitral3'
modelAllSmaller<- coxph(as.formula(fAllSmaller), table1yrAll, x = T)
print(summary(modelAllSmaller))
residuals <- crossvalidation2(fAllSmaller, table1yrAll)

modeltmp <-
  data.table(
    event = table1yrAll$event,
    linear.predictors = modelAllSmaller$linear.predictors,
    sex = table1yrAll$sex * modelAllSmaller$coefficients[1],
    age = table1yrAll$age * modelAllSmaller$coefficients[2],
    copd = table1yrAll$copd * modelAllSmaller$coefficients[3],
    ad = table1yrAll$ad * modelAllSmaller$coefficients[4],
    medi_statin = table1yrAll$medi_statin * modelAllSmaller$coefficients[5],
    medi_diuretic = table1yrAll$medi_diuretic * modelAllSmaller$coefficients[6],
    hb = table1yrAll$hb * modelAllSmaller$coefficients[7],
    block = table1yrAll$block * modelAllSmaller$coefficients[8],
    gradient_mean = table1yrAll$gradient_mean * modelAllSmaller$coefficients[9],
    medi_combi1 = table1yrAll$medi_combi1 * modelAllSmaller$coefficients[10],
    regurg_mitral3 = table1yrAll$regurg_mitral3 * modelAllSmaller$coefficients[11],
    residuals = modelAllSmaller$residuals
  )

#prove that the model is linear
ggplot(modeltmp, aes(
  x = (
    sex + age + copd + ad + medi_statin + medi_diuretic + hb + block + gradient_mean + medi_combi1 + regurg_mitral3
  ),
  y = linear.predictors,
  color = as.factor(event)
)) +
  geom_point() +
  scale_color_manual(values = c("lightblue", "red")) +
  facet_wrap( ~ event) +
  theme_bw()

#show the residuals
ggplot(modeltmp, aes(x = as.factor(event), y = residuals)) +
  geom_boxplot() +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x = "Event", y = "Residuals of the Cox Model")

#show the distribution of the linear predictors stratified by event
visualizePredictors(table1yrAll, modeltmp)
#ggsave("/Users/judith_bernett/Desktop/Bachelorarbeit/Illustrations_BA/KaplanMeierAllSmaller.png",height = 8, width = 12)

tmp <- survfit(Surv(time, event)~1, data = finalTableDT[calc_sts <= 4, ])
tmp2 <- survfit(Surv(time, event)~1, data = finalTableDT[calc_sts > 4 & calc_sts <=8,])
tmp3 <- survfit(Surv(time, event)~1, data = finalTableDT[calc_sts > 8, ])
finalTableDT <- finalTableDT[, stsHazard := as.factor(sapply(calc_sts, function(x){
  if(x > 8) return("high")
  else if(x <= 4) return("low")
  else return("intermediate")
} ))]
survdiff(Surv(time, event) ~ stsHazard, data = finalTableDT)
tab <- data.table(time = numeric(), surv = numeric(), std.err = numeric(), label = character())
tmptab <- data.table(time = tmp[["time"]],  surv = tmp[["surv"]], std.err = tmp[["std.err"]], label = "STS Score <= 4")
tmptab2 <- data.table(time = tmp2[["time"]],  surv = tmp2[["surv"]], std.err = tmp2[["std.err"]], label = "STS Score between 4 and 8")
tmptab3 <- data.table(time = tmp3[["time"]],  surv = tmp3[["surv"]], std.err = tmp3[["std.err"]], label = "STS Score > 8")
tab <- rbind(tab, tmptab)
tab <- rbind(tab, tmptab2)
tab <- rbind(tab, tmptab3)

#Show the curves for the STS Score
ggplot(tab, aes(x = time, y = surv, fill = label))+
  geom_line(aes(colour = label))+
  geom_ribbon(aes(ymin = surv-std.err, ymax = surv+std.err), alpha = 0.1)+
  theme_bw()+
  theme(text = element_text(size=20))+
  scale_color_manual(values = c(cbPalette[c(7,1,6)]), name = "")+
  scale_fill_manual(values = cbPalette[c(7,1,6)], name = "")+
  labs(x = "Time in days", y = "Survival")+
  xlim(0,365)+
  ylim(0,1)

#ggsave("/Users/judith_bernett/Desktop/Bachelorarbeit/Illustrations_BA/KaplanMeierSTS.png",height = 8, width = 12)


tmp <- survfit(Surv(time, event)~1, data = finalTableDT[scoreii_log <= 4, ])
tmp2 <- survfit(Surv(time, event)~1, data = finalTableDT[scoreii_log > 4 & scoreii_log <=8,])
tmp3 <- survfit(Surv(time, event)~1, data = finalTableDT[scoreii_log > 8, ])
finalTableDT <- finalTableDT[, euroHazard := as.factor(sapply(scoreii_log, function(x){
  if(x > 8) return("high")
  else if(x <= 4) return("low")
  else return("intermediate")
} ))]
survdiff(Surv(time, event) ~ euroHazard, data = finalTableDT)
tab <- data.table(time = numeric(), surv = numeric(), std.err = numeric(), label = character())
tmptab <- data.table(time = tmp[["time"]],  surv = tmp[["surv"]], std.err = tmp[["std.err"]], label = "Euro Score II <= 4")
tmptab2 <- data.table(time = tmp2[["time"]],  surv = tmp2[["surv"]], std.err = tmp2[["std.err"]], label = "Euro Score II between 4 and 8")
tmptab3 <- data.table(time = tmp3[["time"]],  surv = tmp3[["surv"]], std.err = tmp3[["std.err"]], label = "Euro Score II > 8")
tab <- rbind(tab, tmptab)
tab <- rbind(tab, tmptab2)
tab <- rbind(tab, tmptab3)

#Show the curves for the EuroSCORE II
ggplot(tab, aes(x = time, y = surv, fill = label))+
  geom_line(aes(colour = label))+
  geom_ribbon(aes(ymin = surv-std.err, ymax = surv+std.err), alpha = 0.1)+
  theme_bw()+
  theme(text = element_text(size=20))+
  scale_color_manual(values = c(cbPalette[c(7,1,6)]), name = "")+
  scale_fill_manual(values = cbPalette[c(7,1,6)], name = "")+
  labs(x = "Time in days", y = "Survival")+
  xlim(0,365)+
  ylim(0,1)

#ggsave("/Users/judith_bernett/Desktop/Bachelorarbeit/Illustrations_BA/KaplanMeierEuro.png",height = 8, width = 12)
tmpcorr2 <- cor(table1yrAll, table1yrAll[, c("medi_statin")])
tmpCorr <- cor(finalTableDT[,c("age", "sex", "copd", "ad", "medi_statin", "medi_diuretic", "hb", "block", "gradient_mean", "medi_combi1", "regurg_mitral3", "time", "event")])
mybreaks2 <- c(seq(-1, 0, length.out=ceiling(100/2) + 1), 
               seq(max(tmpcorr2)/100, max(tmpcorr2), length.out=floor(100/2)))
myBreaks <- c(seq(-1, 0, length.out=ceiling(100/2) + 1), 
              seq(max(tmpCorr)/100, max(tmpCorr), length.out=floor(100/2)))
dimnames(tmpCorr) <- list(c("Age", "Gender", "COPD", "Peripheral artery disease", "Statin", "Diuretics", "Hb", "Atrioventricular block", "Mean gradient", "Only Aspirin as anticoag.", "Mitral regurg. grade 3", "Time", "Event"), c("Age", "Gender", "COPD", "Peripheral artery disease", "Statin", "Diuretics", "Hb", "Atrioventricular block", "Mean gradient", "Only Aspirin as anticoag.", "Mitral regurg. grade 3", "Time", "Event"))

#plot correlations between the coefficients
pheatmap(tmpCorr, display_numbers = T, color = colorRampPalette(rev(brewer.pal(n = 5, name = "RdBu")))(100), breaks = myBreaks, legend = F, height = 12, width = 14, fontsize = 19, angle_col = 315)

tmpCorr <- cor(finalTableDT[,-c(1,2,112,113)])
tmpCorr <- tmpCorr[, c("medi_statin", "medi_anticoagu", "medi_insulin", "medi_diuretic", "medi_combi1", "medi_steroid", "medi_asp")]
tmpCorrDT <- data.table(tmpCorr)
#select the variables that have an absolute correlation of > 0.2 / are mentioned in the BA
tmpCorr <- tmpCorr[c("dyslip", "cad", "pci", "medi_combi12", "pacemaker", "medi_oac_noac", "medi_combi5", "rhythm2", "diab", "dyspnea_nyha34", "gfr", "rf", "lvef", "copd", "medi_p2y12", "medi_clopido", "medi_rivaroxaban", "medi_combi6", "ad", "mi", "cerebro", "csurgery", "hyper"),]
dimnames(tmpCorr) <- list(c("Dyslipidemia", "Coronary artery disease", "History of PCI", "No Anticoagulation drug", "Previous pacemaker implantation", "Novel Anticoagulation drugs", "Only OAC/NOAC as anticoagulation", "Atrial fibrillation", "Diabetes mellitus", "NYHA class 3/4", "GFR", "Renal failure", "LVEF", "COPD", "P2Y12 antagonist medication", "Clopidogrel", "Rivaroxaban", "Aspirin and P2Y12", "Peripheral artery disease", "History of MI", "History of cerebrovasc. accident", "History of cardiac surgery", "Arterial Hypertension"), c("Statin medication", "Marcoumar/Sintrom", "Insulin", "Diuretics", "Only Aspirin as anticoag.", "Steroid medication", "Aspirin"))
#Correlation between medications
pheatmap(tmpCorr, angle_col = 315, height = 12, width = 14, fontsize = 19, display_numbers = T, legend = F,color = colorRampPalette(rev(brewer.pal(n = 5, name = "RdBu")))(100))


