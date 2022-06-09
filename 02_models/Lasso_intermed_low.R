library(data.table)
library(glmnet)
library(survival)
library(survAUC)
library(ggplot2)
source("02_models/utils.R")
finalTableDT <- fread("table1yrAllCause.csv")[,-c("proc_date")]

#LASSO-on-intermediate/ low surgery risk patients

table1yrIntermediate_sts <-
  finalTableDT[,-c(
    "Patient",
    "scoreii_log",
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
table1yrIntermediate_sts <- table1yrIntermediate_sts[, ccs_stratified := ifelse(ap_ccs0 == 1 | ap_ccs1 == 1, 1, 0)]
table1yrIntermediate_sts <- table1yrIntermediate_sts[, regurg_tri_mit_34 := ifelse(regurg_tricuspid34 == 1 | regurg_mitral34 == 1, 1, 0)]
table1yrIntermediate <-
  table1yrIntermediate_sts[table1yrIntermediate_sts$calc_sts <= 8,]
#table1yrIntermediate <- table1yrIntermediate[,-"calc_sts"]

repeat_lasso <- function(table) {
  coef_list <- list()
  for (i in seq(1, 20)) {
    print(i)
    cv.fit <-
      cv.glmnet(
        as.matrix(table[, -c(1, 2)]),
        Surv(table$time, table$event),
        family = "cox",
        maxit = 10000,
        type.measure = "C"
      )
    fit <-
      glmnet(
        as.matrix(table[, -c(1, 2)]),
        Surv(table$time, table$event),
        family = "cox",
        maxit = 10000
      )
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
  return(coef_list)
}
#coef_list <- repeat_lasso(table1yrIntermediate)
#occurrences <- as.data.table(cbind(vec = unique(coef_list), n = tabulate(match(coef_list, unique(coef_list)))))

fintermed <-
  'Surv(time, event) ~ sex + copd + ad + medi_diuretic + hb + ccs_stratified + regurg_tricuspid34 + regurg_mitral34'
modelintermed <- coxph(as.formula(fintermed), table1yrIntermediate)
print(summary(modelintermed))
residuals <- crossvalidation2(fintermed, table1yrIntermediate)

visualizePredictors(table1yrIntermediate, modelintermed)
coxtable <- data.table(linear.predictors = modelintermed$linear.predictors, table1yrIntermediate)
#show the Kaplan-Meier curves for the LASSO-on-all: Smaller model
compute_kaplan_meier(coxtable)
#ggsave("./plots/low_intermediate_kaplanMeier_stratified.png", height=5, width=8)

# test on all patients

tableHighRisk <- table1yrIntermediate_sts[calc_sts > 8, ]
lin_preds <- predict(modelintermed, tableHighRisk, type="lp")
tableHighRisk$linear.predictors <- lin_preds

pred_table <- coxtable[, -"hazard"]
pred_table <- rbind(pred_table, tableHighRisk)
pred_table <- pred_table[, sts_hazard := as.factor(sapply(calc_sts, function(x){
  if(x < 4){
    return("low STS")
  }else if(x > 8){
    return("high STS")
  }else{
    return("intermediate STS")
  }
}))]
pred_table <- pred_table[, hazard := as.factor(sapply(linear.predictors, function(x){
  if(x < -0.5){
    return("low hazard")
  }else if(x > 0.5){
    return("high hazard")
  }else{
    return("intermediate hazard")
  }
}))]
ggplot(pred_table, aes(x = sts_hazard, y = linear.predictors, fill=sts_hazard))+
  geom_boxplot()+
  scale_fill_manual(values = cbPalette[c(7, 1, 6)], name = "") +
  theme_bw()+
  theme(text = element_text(size=20))

ggplot(pred_table, aes(x = hazard, y = calc_sts, fill=hazard))+
  geom_boxplot()+
  scale_fill_manual(values = cbPalette[c(7, 1, 6)], name = "") +
  theme_bw()+
  theme(text = element_text(size=20))

ggplot(pred_table[calc_sts < 20, ], aes(x = hazard, y = calc_sts, fill=hazard))+
  geom_boxplot()+
  scale_fill_manual(values = cbPalette[c(7, 1, 6)], name = "") +
  theme_bw()+
  theme(text = element_text(size=20))


compute_kaplan_meier(pred_table)

# only for high risk: 
compute_kaplan_meier(tableHighRisk)

# train model on all:
modelAll <- coxph(as.formula(fintermed), table1yrIntermediate_sts)
print(summary(modelAll))
residuals <- crossvalidation2(fintermed, table1yrIntermediate_sts)
coxtable <- table1yrIntermediate_sts[, linear.predictors := modelAll$linear.predictors]
compute_kaplan_meier(coxtable)




