library(data.table)
library(glmnet)
library(survival)
library(survAUC)
source("02_models/utils.R")

#LASSO-on-all model

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

#This does not yield deterministic outcomes
coef_list <- list()
for(i in seq(1,100)){
  print(i)
  cv.fit <-
    cv.glmnet(as.matrix(table1yrAll[, -c(1, 2)]),
              Surv(table1yrAll$time, table1yrAll$event),
              family = "cox")
  fit <-
    glmnet(as.matrix(table1yrAll[, -c(1, 2)]),
           Surv(table1yrAll$time, table1yrAll$event),
           family = "cox")
  #plot(fit, label = T)
  #plot(cv.fit)
  
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
fAll13 <- paste0('Surv(time, event) ~ ', paste(occurrences$vec[[1]], collapse = " + "))
fAll15 <- paste0('Surv(time, event) ~ ', paste(occurrences$vec[[2]], collapse = " + "))
fAll17 <- paste0('Surv(time, event) ~ ', paste(occurrences$vec[[3]], collapse = " + "))

#fAll <-
#  'Surv(time, event) ~ sex + age + copd + valvulo + ad + medi_anticoagu + medi_statin + medi_diuretic + medi_insulin + hb + block + gradient_mean + lvef + medi_combi1 + rhythm2 + regurg_mitral3 + regurg_tricuspid3'
modelAll <- coxph(as.formula(fAll15), table1yrAll, x = T)
print(summary(modelAll))
residuals <- crossvalidation2(fAll15, table1yrAll)

tableEvent0 <- table1yrAll[table1yrAll$event == 0,]
tableEvent1 <- table1yrAll[table1yrAll$event == 1,]

fold0 <- split(1:nrow(tableEvent0),
               ceiling(seq_along(1:nrow(tableEvent0)) / ceiling(nrow(tableEvent0) /
                                                                  10)))
fold1 <- split(1:nrow(tableEvent1),
               ceiling(seq_along(1:nrow(tableEvent1)) / ceiling(nrow(tableEvent1) /
                                                                  10)))
# compute model on full dataset
s <- summary(coxph(as.formula(fAll15), table1yrAll))
fits <- lapply(1:length(fold0), function(i) {
  train0 		<- table1yrAll[unlist(fold0[-i]), ]
  test0 		<- table1yrAll[unlist(fold0[i]), ]
  train1  <- table1yrAll[unlist(fold1[-i]),]
  test1 <- table1yrAll[unlist(fold1[i]),]
  
  train <- rbind(train0, train1)
  test <- rbind(test0, test1)
  
  train.fit 	<- coxph(as.formula(fAll15), train)
  auc			<-
    AUC.cd(
      Surv(train$time, train$event),
      Surv(test$time, test$event),
      predict(train.fit),
      predict(train.fit, newdata = test),
      sort(unique(test$time))
    )
  
  train.fit[["event"]] <-  train$event
  train.fit[["auc"]] <-  auc$iauc
  train.fit[["all_auc"]] <- auc
  
  return(train.fit)
})

auc <- sapply(fits, function(x)
  return(x$auc))
auc_dt <- data.table(AUC = auc, Fold = as.factor(1:10))
ggplot(auc_dt, aes(x = Fold, y = AUC)) +
  geom_point(size = 3) +
  theme_bw() +
  theme(text = element_text(size = 20))


mauc <- mean(auc)
sig <- t.test(auc, mu = .5)
print(paste('10 fold CV', mauc))
print(sig)


