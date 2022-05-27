source("02_models/utils.R")
library(ggRandomForests)
library(data.table)
library(ggplot2)
library(survival)
library(survAUC)
finalTableDT <- fread("table1yrAllCause.csv")[, -c("proc_date")]

#RANDOM FOREST
table1yrAll <- finalTableDT[, -c("Patient", "scoreii_log", "calc_sts", "image1", "image3", "proc_access1", "proc_access2", "height", "weight", "tropo_type2", "delay1", "delay2", "delay3")]

#This is also not deterministic
rfsrc_pbc <- rfsrc(Surv(time, event) ~ ., data = table1yrAll, ntree = 1000,
                   nsplit = 10, na.action = "na.impute",
                   tree.err = TRUE,importance = TRUE
)
error <- gg_error(rfsrc_pbc)
error <- error[complete.cases(error),]
plot(error)+
  theme_bw()+
  theme(text = element_text(size=20))

#ggsave("./plots/oob.png", height = 10, width = 15)

#VIMP:
plot(gg_vimp(rfsrc_pbc)) +
  theme(legend.position = c(0.8, 0.2)) +
  labs(fill = "VIMP > 0")+theme_bw()
varImp <- gg_vimp(rfsrc_pbc)

#Minimal Depth:
varsel_pbc <- var.select(rfsrc_pbc)
gg_md <- gg_minimal_depth(varsel_pbc)
plot(gg_md)+theme_bw()

#Comparison:

minimal_vimp_vgl <- gg_minimal_vimp(gg_md)
minimal_vimp_vgl$names <- as.character(minimal_vimp_vgl$names)
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "creatinine"] <- "Creatinine"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "gfr"] <- "GFR"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "hb"] <- "Hb"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "tte_lvef"] <- "LVEF (TTE measurement)"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "ck"] <- "Creatinine Kinase"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "lvef"] <- "LVEF"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "age"] <- "Age"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "bmi"] <- "BMI"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "tte_gradient_mean"] <- "Mean Gradient (TTE measurement)"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "bsa"] <- "BSA"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "thrombo"] <- "Thrombocytes"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "gradient_mean"] <- "Mean Gradient"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "ad"] <- "Peripheral artery disease"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "hrate"] <- "Heart rate"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "copd"] <- "COPD"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "medi_diuretic"] <- "Diuretics"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "tte_regurg_mitral3"] <- "Mitral regurgitation grade 3 (TTE)"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "sex"] <- "Male Gender"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "rhythm2"] <- "Atrial Fibrillation"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "medi_steroid"] <- "Steroid medication"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "regurg_mitral3"] <- "Mitral regurgiation grade 3"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "tte_regurg_tricuspid3"] <- "Tricuspid regurgitation grade 3 (TTE)"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "regurg_tricuspid3"] <- "Tricuspid regurgiation grade 3"
minimal_vimp_vgl$names[minimal_vimp_vgl$names == "medi_statin"] <- "Statin medication"

plot(minimal_vimp_vgl[minimal_vimp_vgl$depth <= 40, ])+
  geom_point(size = 3)+
  theme_bw()+
  theme(text = element_text(size=20))

#ggsave("/Users/judith_bernett/Desktop/Bachelorarbeit/Illustrations_BA/vgl.pdf", height = 20, width = 15)

#Random Forest Model

fRandFor <- 'Surv(time, event) ~ creatinine + hb + ck + lvef + age + gradient_mean + ad + copd + medi_diuretic + sex + rhythm2 + medi_steroid + regurg_mitral3 '
modelRandFor <- coxph(as.formula(fRandFor), table1yrAll)
print(summary(modelRandFor))
residuals <- crossvalidation2(fRandFor, table1yrAll)