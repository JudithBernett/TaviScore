library(data.table)

finalTableDT <- data.table::fread("./table1yrAllCause.csv")

########## Summary Table: ###########
continuous_table <- select(finalTableDT, event, age, height, weight, bmi, bsa, scoreii_log, calc_sts, creatinine, gfr, hb, thrombo, ck, hrate, gradient_mean, lvef)
overall_continuous <- setnames(continuous_table[, sapply(.SD[,-1], function(x) list(round(mean(x),1), round(sd(x),1)))], c(sapply(names(continuous_table[,-1]), paste0, c("_mean", "_sd"))))
continuous_table <- setnames(continuous_table[, sapply(.SD, function(x) list(round(mean(x),1), round(sd(x),1))), by = event], c("event", sapply(names(continuous_table[,-1]), paste0, c("_mean", "_sd"))))
continuous_table <- rbind(cbind(c("overall", overall_continuous)), continuous_table)
continuous_table

categorical_table <- finalTableDT[, -c("Patient", "proc_date", "time", "age", "height", "weight", "bmi", "bsa", "scoreii_log", "calc_sts", "creatinine", "gfr", "hb", "thrombo", "ck", "hrate", "tte_lvef", "tte_gradient_mean", "gradient_mean", "lvef")]
overall_categorical <- categorical_table[, sapply(.SD[,-1], function(x) list(round(sum(x, na.rm = T)/.N,3)))]
categorical_table <- setnames(categorical_table[, sapply(.SD, function(x) list(round(sum(x, na.rm = T)/.N,3))), by = event], c("event", names(categorical_table[,-1])))
categorical_table <- rbind(cbind(c("overall", overall_categorical)), categorical_table)
categorical_table

#Test the difference between the event and non-event group for the continous and the categorical variables:

####### 1. Continuous variables: t-test ############

continuous_table_testing <- select(finalTableDT, event, age, height, weight, bmi, bsa, scoreii_log, calc_sts, creatinine, gfr, hb, thrombo, ck, hrate, gradient_mean, lvef)
testing_event_0 <- continuous_table_testing[event==0, -1]
testing_event_1 <- continuous_table_testing[event==1, -1]

pvalues_t.test <- data.table(name = character(), p.value = numeric())

for(i in seq(1,15)){
  tmpvar <- var.test(unlist(testing_event_0[,..i]), unlist(testing_event_1[,..i]), alternative = "two.sided")
  #if the p-value is <0.05, we reject the null hypothesis that var(event==0) == var(event==1). 
  #this determines if we either do a 'normal' or a Welch t-test
  if(tmpvar$p.value < 0.05){
    print(paste("Rejected F-test for", names(testing_event_0[,..i]), "at p-value: ", tmpvar$p.value))
    tmpt <- t.test(unlist(testing_event_0[,..i]), unlist(testing_event_1[,..i]), alternative = "two.sided", var.equal = F)
  }else{
    tmpt <- t.test(unlist(testing_event_0[,..i]), unlist(testing_event_1[,..i]), alternative = "two.sided", var.equal = T)
  }
  tmpentry <- data.table(name = names(testing_event_0[,..i]), p.value = tmpt$p.value)
  pvalues_t.test <- rbind(pvalues_t.test, tmpentry)
}

pvalues_t.test$p.adjustBH <- round(p.adjust(pvalues_t.test$p.value, method = "BH"),10)
pvalues_t.test$significantBH <- ifelse(pvalues_t.test$p.adjustBH < 0.05, 1, 0)
pvalues_t.test$p.adjustbonferroni <- round(p.adjust(pvalues_t.test$p.value, method = "bonferroni"),3)
pvalues_t.test$significantbonferroni <- ifelse(pvalues_t.test$p.adjustBH < 0.05, 1, 0)
fwrite(pvalues_t.test, "tables/t_test_continuous_vars.csv")

######### 2. Categorical variables: Fisher's Exact Test #########
categorical_table_testing <- finalTableDT[, -c("Patient", "proc_date", "time", "age", "height", "weight", "bmi", "bsa", "scoreii_log", "calc_sts", "creatinine", "gfr", "hb", "thrombo", "ck", "hrate", "tte_lvef", "tte_gradient_mean", "gradient_mean", "lvef")]

pvalues_fisher.test <- data.table(name = character(), p.value = numeric())

for(i in seq(2,91)){
  count00 <- nrow(categorical_table_testing[categorical_table_testing[, .I[.SD==0], .SDcols = i]][event == 0, ])
  count01 <- nrow(categorical_table_testing[categorical_table_testing[, .I[.SD==0], .SDcols = i]][event == 1, ])
  count10 <- nrow(categorical_table_testing[categorical_table_testing[, .I[.SD==1], .SDcols = i]][event == 0, ])
  count11 <- nrow(categorical_table_testing[categorical_table_testing[, .I[.SD==1], .SDcols = i]][event == 1, ])
  
  tmp <- data.table(value0 = c(count00, count01), value1 = c(count10, count11))
  tmp <- as.matrix(tmp)
  
  tmpfisher <- fisher.test(tmp)
  tmpentry <- data.table(name = names(categorical_table_testing[,..i]), p.value = tmpfisher$p.value)
  pvalues_fisher.test <- rbind(pvalues_fisher.test, tmpentry)
}
#LBBB + RBBB:
count00 <- nrow(categorical_table_testing[event == 0 & (delay1 == 0 & delay2 == 0 ), ])
count01 <- nrow(categorical_table_testing[event == 1 & (delay1 == 0 & delay2 == 0 ), ])
count10 <- nrow(categorical_table_testing[event == 0 & (delay1 == 1 | delay2 == 1 ), ])
count11 <- nrow(categorical_table_testing[event == 1 & (delay1 == 1 | delay2 == 1 ), ])
tmp <- data.table(value0 = c(count00, count01), value1 = c(count10, count11))
tmp <- as.matrix(tmp)
tmpfisher <- fisher.test(tmp)
tmpentry <- data.table(name = "LBBB+RBBB", p.value = tmpfisher$p.value)
pvalues_fisher.test <- rbind(pvalues_fisher.test, tmpentry)

#Left and right femoral access: 
count00 <- nrow(categorical_table_testing[event == 0 & (proc_access1 == 0 & proc_access2 == 0 ), ])
count01 <- nrow(categorical_table_testing[event == 1 & (proc_access1 == 0 & proc_access2 == 0 ), ])
count10 <- nrow(categorical_table_testing[event == 0 & (proc_access1 == 1 | proc_access2 == 1 ), ])
count11 <- nrow(categorical_table_testing[event == 1 & (proc_access1 == 1 | proc_access2 == 1 ), ])
tmp <- data.table(value0 = c(count00, count01), value1 = c(count10, count11))
tmp <- as.matrix(tmp)
tmpfisher <- fisher.test(tmp)
tmpentry <- data.table(name = "Femoral Access", p.value = tmpfisher$p.value)
pvalues_fisher.test <- rbind(pvalues_fisher.test, tmpentry)

pvalues_fisher.test$p.adjustBH <- round(p.adjust(pvalues_fisher.test$p.value, method = "BH"),10)
pvalues_fisher.test$significantBH <- ifelse(pvalues_fisher.test$p.adjustBH < 0.05, 1, 0)
pvalues_fisher.test$p.adjustbonferroni <- round(p.adjust(pvalues_fisher.test$p.value, method = "bonferroni"),3)
pvalues_fisher.test$significantbonferroni <- ifelse(pvalues_fisher.test$p.adjustbonferroni < 0.05, 1, 0)
fwrite(pvalues_fisher.test, "tables/fisher_test_discrete_vars.csv")
