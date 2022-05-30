library(data.table)
library(glmnet)
library(survival)
library(survAUC)
library(ggplot2)
source("02_models/utils.R")
finalTableDT <- fread("table1yrAllCause.csv")[, -c("proc_date")]
#exclude patient, scoreii_log, calc_sts, image1, image3, proc_access1, proc_access2, tte measurements
table1yrAll <-
  finalTableDT[,-c(
    "Patient",
    "scoreii_log",
    "calc_sts",
    "image1",
    "image3",
    "proc_access1",
    "proc_access2",
    "tte_gradient_mean",
    "tte_lvef",
    "tte_regurg_aortic1",
    "tte_regurg_aortic2",
    "tte_regurg_aortic3",
    "tte_regurg_mitral1",
    "tte_regurg_mitral2",
    "tte_regurg_mitral3",
    "tte_regurg_tricuspid1",
    "tte_regurg_tricuspid2",
    "tte_regurg_tricuspid3"
  )]


#Better: 
#Run this script on the server:
#    #!/usr/bin/R
#    library(data.table)
#    library(survival)

#    #This powerSet function is adapted from the rje package:
#    #Robin Evans (2020). rje: Miscellaneous Useful Functions for Statistics. R package version 1.10.13.
#    #https://CRAN.R-project.org/package=rje

#    powerSet <- function (x) {
#      out <- list(x[c()])
#      for (i in seq_along(x)) {
#        outtmp <- sapply(out, function(y) c(y, x[i]))
#        out <- c(out, outtmp)
#      }
#      return(out)
#    }
#    finalTableDT <- fread("[Path_to_table1yrAllCause.csv]")

#    tmp <- c("sex", "age", "copd", "valvulo", "ad", "medi_anticoagu", 
#             "medi_statin", "medi_diuretic",  "medi_insulin", 
#             "hb", "block", "gradient_mean", "lvef", "medi_combi1", 
#             "rhythm2", "regurg_tricuspid3",  "regurg_mitral3")


#    tmp2 <- powerSet(tmp)
#    tmp2[[1]] <- NULL

#    tmp3 <- data.table(vars = character(), nrvars = numeric(), concordance = numeric(), pvalueAvg = numeric())
#    counter <- 1
#    for(i in tmp2){
#      if(counter %% 500 == 0){
#        fwrite(data.table(model = "Model", nr = counter), "~/Swiss_TAVI/logger.csv", append = T)
#      }
#      counter <- counter + 1
#      f <- 'Surv(time, event) ~ .'
#      vars <- c(i, c("time", "event"))
#      tmptable <- finalTableDT[,..vars]
#      modeltmp <- coxph(as.formula(f), tmptable)
#      conc <- modeltmp$concordance[6]
#      pvalAvg <- sum(as.matrix(coef(summary(modeltmp)))[,5])/length(i)
#      tmptable2 <- data.table(vars = paste(i, sep = ",", collapse = ","), nrvars = length(i), 
#                              concordance = conc, pvalueAvg = pvalAvg)
#      tmp3 <- rbind(tmp3, tmptable2)
#    }
#    tmp3 <- as.data.table(tmp3)
#    fwrite(tmp3, "[Path_to_OutputDir/concPvalTable.csv]", row.names = F)

#Read in the results:
concPvalTable <- fread("./concPvalTable.csv")
concPvalTable$concordance <- round(concPvalTable$concordance, 3)
concPvalTable$pvalueAvg <- round(concPvalTable$pvalueAvg, 5)
concPvalTable$AIC <- round(concPvalTable$AIC, 1)
concPvalTable$BIC <- round(concPvalTable$BIC, 1)
concPvalTable <- setorder(concPvalTable, -concordance, pvalueAvg)
#since the first 288 entries have a concorance >= 0.740 we can pick our model just from those
concPvalTable2 <- concPvalTable[concordance >= 0.740,]
concPvalTable2 <- setorder(concPvalTable2, pvalueAvg)
head(concPvalTable2,1)

#LASSO-on-all: Smaller Model
#fAllSmaller<- 'Surv(time, event) ~ sex + age + copd + ad + medi_statin + medi_diuretic + hb + block + gradient_mean + medi_combi1 + regurg_mitral3'

#table1yrAll <- table1yrAll[, regurg_stratified := ifelse(regurg_tricuspid3 == 1 | regurg_mitral3 == 1, 1, 0)]

fAllSmaller<- 'Surv(time, event) ~ sex + age + copd + ad + medi_diuretic + hb + regurg_mitral34'
modelAllSmaller<- coxph(as.formula(fAllSmaller), table1yrAll, x = T)
print(summary(modelAllSmaller))
residuals <- crossvalidation2(fAllSmaller, table1yrAll)

#### visualize predictors

visualizePredictors(table1yrAll, modelAllSmaller)

coxtable <- data.table(linear.predictors = modelAllSmaller$linear.predictors, table1yrAll)

tmp <- survfit(Surv(time, event)~1, data = coxtable[linear.predictors >= 0.5, ])
tmp2 <- survfit(Surv(time, event)~1, data = coxtable[linear.predictors > -0.5 & linear.predictors < 0.5,])
tmp3 <- survfit(Surv(time, event)~1, data = coxtable[linear.predictors <= -0.5])
coxtable <- coxtable[, hazard := as.factor(sapply(linear.predictors, function(x){
  if(x >= 0.5) return("high")
  else if(x <= -0.5) return("low")
  else return("intermediate")
} ))]
survdiff(Surv(time, event) ~ hazard, data = coxtable)
tab <- data.table(time = numeric(), surv = numeric(), std.err = numeric(), label = character())
tmptab <- data.table(time = tmp[["time"]],  surv = tmp[["surv"]], std.err = tmp[["std.err"]], label = "High Hazard")
tmptab2 <- data.table(time = tmp2[["time"]],  surv = tmp2[["surv"]], std.err = tmp2[["std.err"]], label = "Intermediate Hazard")
tmptab3 <- data.table(time = tmp3[["time"]],  surv = tmp3[["surv"]], std.err = tmp3[["std.err"]], label = "Low Hazard")
tab <- rbind(tab, tmptab)
tab <- rbind(tab, tmptab2)
tab <- rbind(tab, tmptab3)

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


#show the Kaplan-Meier curves for the LASSO-on-all: Smaller model
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

