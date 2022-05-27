source("02_models/utils.R")
finalTableDT <- fread("table1yrAllCause.csv")[, -c("proc_date")]

#LASSO-on-intermediate/ low surgery risk patients

table1yrIntermediate <- finalTableDT[, -c("Patient", "scoreii_log", "image1", "image3", "proc_access1", "proc_access2", "tte_gradient_mean", "tte_lvef", "tte_regurg_aortic1", "tte_regurg_aortic2", "tte_regurg_aortic3", "tte_regurg_mitral1", "tte_regurg_mitral2", "tte_regurg_mitral3", "tte_regurg_tricuspid1", "tte_regurg_tricuspid2", "tte_regurg_tricuspid3")]
table1yrIntermediate <- table1yrIntermediate[table1yrIntermediate$calc_sts <=8, ]
table1yrIntermediate <- table1yrIntermediate[, -"calc_sts"]


coef_list <- list()
for(i in seq(1,100)){
  print(i)
  cv.fit <- cv.glmnet(as.matrix(table1yrIntermediate[,-c(1,2)]), Surv(table1yrIntermediate$time,table1yrIntermediate$event), family = "cox", maxit = 10000, type.measure = "C")
  fit <- glmnet(as.matrix(table1yrIntermediate[,-c(1,2)]), Surv(table1yrIntermediate$time,table1yrIntermediate$event), family = "cox", maxit = 10000)
  #plot(fit, label = T)
  #plot(cv.fit)

  #lambda.min is the value of λ that gives minimum mean cross-validated error
  Coefficients <- coef(fit, s = cv.fit$lambda.min)
  Active.Index <- which(Coefficients != 0)
  Active.Coefficients <- Coefficients[Active.Index]
  #print("Active Index")
  selected_vars <- dimnames(Coefficients)[[1]][Active.Index]
  coef_list[[i]] <- selected_vars
  #print("Active Coefficients")
  #Active.Coefficients
}
occurrences <- as.data.table(cbind(vec = unique(coef_list), n = tabulate(match(coef_list, unique(coef_list)))))


fintermed<- 'Surv(time, event) ~ sex + copd + ad + medi_diuretic + hb + ap_ccs1 + regurg_tricuspid3'
modelintermed<- coxph(as.formula(fintermed), table1yrIntermediate)
print(summary(modelintermed))
residuals <- crossvalidation2(fintermed, table1yrIntermediate)

#### visualize predictors

visualizePredictors(table1yrIntermediate, modelintermed)

coxtable <- data.table(linear.predictors = modelintermed$linear.predictors, table1yrIntermediate)

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

