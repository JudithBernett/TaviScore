library(data.table)
library(survival)
library(survAUC)
source("02_models/utils.R")

finalTableDT <- fread("./table1yrAllCause.csv")
#Benjamini-Hochberg Model

#with BH adjusted p-value
tableBH <- select(finalTableDT, c(time, event, age, creatinine, hb, lvef, gradient_mean, rhythm2, mi, regurg_mitral3, regurg_tricuspid3, dyspnea_nyha4, ad, copd, concom_other, medi_anticoagu, medi_oac_noac, medi_diuretic, medi_combi1))
fBH<- 'Surv(time, event) ~ .'
modelBH<- coxph(as.formula(fBH), tableBH, x = T)
print(summary(modelBH))
residuals <- crossvalidation2(fBH, tableBH)
