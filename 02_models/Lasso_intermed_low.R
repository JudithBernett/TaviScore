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
table1yrIntermediate_sts <-
  table1yrIntermediate_sts[table1yrIntermediate_sts$calc_sts <= 8,]
table1yrIntermediate <- table1yrIntermediate_sts[,-"calc_sts"]

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

table1yrIntermediate <- table1yrIntermediate[, ccs_stratified := ifelse(ap_ccs0 == 1 | ap_ccs1 == 1, 1, 0)]
table1yrIntermediate <- table1yrIntermediate[, regurg_tri_mit_34 := ifelse(regurg_tricuspid34 == 1 | regurg_mitral34 == 1, 1, 0)]

fintermed <-
  'Surv(time, event) ~ sex + copd + ad + medi_diuretic + hb + ccs_stratified + regurg_tricuspid34 + regurg_mitral34'
modelintermed <- coxph(as.formula(fintermed), table1yrIntermediate)
print(summary(modelintermed))
residuals <- crossvalidation2(fintermed, table1yrIntermediate)

#### visualize predictors

visualizePredictors(table1yrIntermediate, modelintermed)

coxtable <-
  data.table(linear.predictors = modelintermed$linear.predictors, table1yrIntermediate)

tmp <-
  survfit(Surv(time, event) ~ 1, data = coxtable[linear.predictors >= 0,])
tmp2 <-
  survfit(Surv(time, event) ~ 1, data = coxtable[linear.predictors < 0])
coxtable <-
  coxtable[, hazard := as.factor(sapply(linear.predictors, function(x) {
    if (x >= 0)
      return("intermediate")
    else
      return("low")
  }))]
survdiff(Surv(time, event) ~ hazard, data = coxtable)

tab <-
  data.table(
    time = numeric(),
    surv = numeric(),
    std.err = numeric(),
    label = character()
  )
tmptab <-
  data.table(
    time = tmp[["time"]],
    surv = tmp[["surv"]],
    std.err = tmp[["std.err"]],
    label = "Intermediate Hazard"
  )
tmptab2 <-
  data.table(
    time = tmp2[["time"]],
    surv = tmp2[["surv"]],
    std.err = tmp2[["std.err"]],
    label = "Low Hazard"
  )
tab <- rbind(tmptab, tmptab2)

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
ggplot(tab, aes(x = time, y = surv, fill = label)) +
  geom_line(aes(colour = label)) +
  geom_ribbon(aes(ymin = surv - std.err, ymax = surv + std.err), alpha = 0.1) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_color_manual(values = c(cbPalette[c(7, 1, 6)]), name = "") +
  scale_fill_manual(values = cbPalette[c(7, 1, 6)], name = "") +
  labs(x = "Time in days", y = "Survival") +
  xlim(0, 365) +
  ylim(0, 1)
ggsave("./plots/low_intermediate_kaplanMeier_stratified.png", height=5, width=8)
