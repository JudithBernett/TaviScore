library(data.table)
library(tidyr)
library(dplyr)
source("02_models/utils.R")

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


finalTableDT <- fread("./table1yrAllCause.csv")
#with BH adjusted p-value
tableBH <- select(finalTableDT, c(time, event, age, creatinine, hb, lvef, gradient_mean, rhythm2, mi, regurg_mitral3, regurg_tricuspid3, dyspnea_nyha4, ad, copd, concom_other, medi_anticoagu, medi_oac_noac, medi_diuretic, medi_combi1))
fBH<- 'Surv(time, event) ~ .'
c <- as.data.table(permutationTest(fBH, as.data.table(tableBH), 100))
#ggplot(c, aes(x = c$V1))+geom_density()+scale_x_continuous(limits = c(0,1))
print(paste("Mean permutation test", mean(c$V1)))
randomTests <- data.table()
randomTests$BH <- c$V1

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

fAllSmaller<- 'Surv(time, event) ~ sex + age + copd + ad + medi_statin + medi_diuretic + hb + block + gradient_mean + medi_combi1 + regurg_mitral3'
c <- as.data.table(permutationTest(fAllSmaller, as.data.table(table1yrAll), 100))
#ggplot(c, aes(x = c$V1))+geom_density()+scale_x_continuous(limits = c(0,1))
print(paste("Mean permutation test", mean(c$V1)))
randomTests$AllSmaller <- c$V1

table1yrIntermediate <- finalTableDT[, -c("Patient", "scoreii_log", "image1", "image3", "proc_access1", "proc_access2", "tte_gradient_mean", "tte_lvef", "tte_regurg_aortic1", "tte_regurg_aortic2", "tte_regurg_aortic3", "tte_regurg_mitral1", "tte_regurg_mitral2", "tte_regurg_mitral3", "tte_regurg_tricuspid1", "tte_regurg_tricuspid2", "tte_regurg_tricuspid3")]
table1yrIntermediate <- table1yrIntermediate[table1yrIntermediate$calc_sts <=8, ]
table1yrIntermediate <- table1yrIntermediate[, -"calc_sts"]
table1yrIntermediate <- table1yrIntermediate[, ccs_stratified := ifelse(ap_ccs0 == 1 | ap_ccs1 == 1, 1, 0)]
fintermed <-
  'Surv(time, event) ~ sex + copd + ad + medi_diuretic + hb + ccs_stratified + regurg_tricuspid34 + regurg_mitral34'
c <- as.data.table(permutationTest(fintermed, as.data.table(table1yrIntermediate), 100))
#ggplot(c, aes(x = c$V1))+geom_density()+scale_x_continuous(limits = c(0,1))
print(paste("Mean permutation test", mean(c$V1)))
randomTests$Intermed <- c$V1

fRandFor <- 'Surv(time, event) ~ creatinine + hb + ck + lvef + age + gradient_mean + ad + copd + medi_diuretic + sex + rhythm2 + medi_steroid + regurg_mitral3 '
c <- as.data.table(permutationTest(fRandFor, as.data.table(table1yrAll), 100))
#ggplot(c, aes(x = c$V1))+geom_density()+scale_x_continuous(limits = c(0,1))
print(paste("Mean permutation test", mean(c$V1)))
randomTests$RandomForest <- c$V1

realMeasurements <- data.table(model = "BH", concordance = 0.727, CrossVal10 = 0.706)
tmp <-
  data.table(model = "All",
             concordance = 0.742,
             CrossVal10 = 0.740)
realMeasurements <- rbind(realMeasurements, tmp)
tmp <- data.table(model = "AllSmaller", concordance = 0.741, CrossVal10 = 0.731)
realMeasurements <- rbind(realMeasurements, tmp)
tmp <- data.table(model = "Intermed", concordance = 0.727, CrossVal10 = 0.693)
realMeasurements <- rbind(realMeasurements, tmp)
tmp <- data.table(model = "RandomForest", concordance = 0.731, CrossVal10 = 0.724)
realMeasurements <- rbind(realMeasurements, tmp)

randomTests2 <- data.table(names = colnames(randomTests), values = t(randomTests))
randomTests2 <- randomTests2 %>% melt( id.vars = "names", variable.name = "permutationNr", value.name = "concordance")
randomTests2 <- merge(randomTests2, realMeasurements, by.x = "names", by.y = "model")
randomTests2$names[randomTests2$names == "All"] <- "LASSO-on-all"
randomTests2$names[randomTests2$names == "AllSmaller"] <- "LASSO-on-all: Smaller [avg p-values]"
randomTests2$names[randomTests2$names == "BH"] <- "Benjamini-Hochberg"
randomTests2$names[randomTests2$names == "Intermed"] <- "LASSO-on-intermed/low risk"
randomTests2$names[randomTests2$names == "RandomForest"] <- "Random Forest"

#Show the concordances and the permutation test's concordances
ggplot(randomTests2, aes(x = names, y = concordance.x))+
  geom_boxplot()+
  geom_point(aes(x = names, y = concordance.y, colour = "Real Concordance"), size = 3, shape = 4)+
  geom_point(aes(x = names, y = CrossVal10, colour = "Real 10-fold CV"), size = 3, shape = 4)+
  theme_bw()+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 345, hjust = 0))+
  scale_color_manual(values = c(cbPalette[c(7,6)]), name = "")+
  scale_fill_manual(values = cbPalette[c(7,6)], name = "")+
  labs(x = "", y = "Concordance")

