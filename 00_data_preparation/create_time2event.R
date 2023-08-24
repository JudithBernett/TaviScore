library(caret)
source("00_data_preparation/load_data.R")
remove(baselinelabelledDT)
remove(baselineNames)
remove(eventsNames)

###### Join FUPs, find longest FUP date, make time-to-event tables ######

#discard all the patients that couldn't be followed up 
fup30days <- all_sheets$fup30days[all_sheets$fup30days$contact_status!=5,]
fup1yr <- all_sheets$fup1yr[all_sheets$fup1yr$contact_status!=5,]
fup2yr <- all_sheets$fup2yr[all_sheets$fup2yr$contact_status!=5,]
fup3yr <- all_sheets$fup3yr[all_sheets$fup3yr$contact_status!=5,]
fup4yr <- all_sheets$fup4yr[all_sheets$fup4yr$contact_status!=5,]
fup5yr <- all_sheets$fup5yr[all_sheets$fup5yr$contact_status!=5,]
fup7yr <- all_sheets$fup7yr[all_sheets$fup7yr$contact_status!=5,]
unscheduled <- all_sheets$unscheduled[all_sheets$unscheduled$contact_status!=5,]
events <- all_sheets$events
discharge <- all_sheets$discharge

timeEvent <- create_time2event(events, fup30days, fup1yr, fup2yr, fup3yr, fup4yr, fup5yr, fup7yr, unscheduled, discharge)
remove(fup30days)
remove(fup1yr)
remove(fup2yr)
remove(fup3yr)
remove(fup4yr)
remove(fup5yr)
remove(fup7yr)
remove(unscheduled)
remove(discharge)
remove(events)

baseline <- as.data.frame(all_sheets$baseline)
# delete dob, angio_tte_tee, proc_date
baseline <- baseline[,-c(3, 69, 86)]

# replace NAs with median
print("Replace NAs with medians...")
for(i in 2:ncol(baseline)){
  baseline[is.na(baseline[,i]), i] <- median(baseline[,i], na.rm = TRUE)
}

# exclude near zero variance
print("Exclude near zero variance columns...")
baseline <- baseline[,-caret::nearZeroVar(baseline)]

#recode 1/2 boolean variables to 0/1: 
boolVars <- c("sex", "diab", "hyper", "dyslip", "copd", "cerebro",
              "cerebro_strokebl", "pacemaker", "valvulo", "cad", "pci",
              "mi", "ad", "csurgery", "dyspnea", "ap", "syncope", "dyspnea_nyha34",
              "medi_asp", "medi_p2y12", "medi_clopido",
              "medi_anticoagu", "medi_noac", "medi_oac_noac", 
              "medi_rivaroxaban", "medi_statin", "medi_ace", "medi_at", "medi_betablock",
              "medi_ca", "medi_diuretic", "medi_steroid", "medi_insulin", 
              "medi_antidiab","block",
              "anesthesia", "concom_balloon", "concom", "concom_pci",
              "concom_other","premeasure",
              "pvalvular")

print("Recode boolean 1/2 boolean variables to 0/1...")
for(col in boolVars){
  baseline[, col][baseline[,col] == 2] <- 0
}

#delete device and device size
baseline <- baseline[, -c(75, 76)]

#factor variables to boolean variables: 
factorVars <- c('dyspnea_nyha', 'ap_ccs', 'ap_ccs34', 'medi_combi', 'tropo_type', 'rhythm', 'delay', 'regurg_aortic',
                'tte_regurg_aortic', 'regurg_mitral', 'tte_regurg_mitral', 'regurg_tricuspid', 'tte_regurg_tricuspid',
                'image', 'proc_access', 'regurgitation')

print("Recode factor variables to booleans ...")
for(col in factorVars){
  baseline[, col] <- factor(baseline[, col])
  f <- as.formula(paste('~', col, '-1'))
  booleanFactor <- model.matrix(f, baseline)
  if(col == 'device'){
    colnames(booleanFactor) <- c('MedtronicCoreValve_1', 'EdwardsSapienXT_2', 'SymetisAcurateNEO_3','SJMPortico_5',
                                 'DirectFlowMedical_7', 'EdwardsSapien3_8', 'BSCLotus_9', 'MedtronicEvolutR_10',
                                 'AllegraNVT_12', 'EvolutPRO_13','EdwardsCentura_14', 'EdwardsSapien3ULTRA_15')
  }
  baseline <- baseline[, -which(names(baseline) == col)]
  baseline <- cbind(baseline, booleanFactor)
}

# summarize regurg_tricuspid 3 + 4 and regurg_mitral 3 + 4 and regurg_aortic 3 + 4
baseline$regurg_aortic34 <- ifelse(baseline$regurg_aortic3 == 1 | baseline$regurg_aortic4 == 1, 1, 0)
baseline$regurg_mitral34 <- ifelse(baseline$regurg_mitral3 == 1 | baseline$regurg_mitral4 == 1, 1, 0)
baseline$regurg_tricuspid34 <- ifelse(baseline$regurg_tricuspid3 == 1 | baseline$regurg_tricuspid4 == 1, 1, 0)

# exclude low variance again:
print("Exclude near zero variances again ...")
baseline <- baseline[,-caret::nearZeroVar(baseline)]

print("Creating final table...")
# final table
finalTable <- merge(timeEvent, baseline, by = 1, all.x = TRUE)
wholeTableLabelled <- merge(as.data.frame(all_sheets$baseline_labelled), timeEvent, by = 1,  all.y = T)
write.csv(finalTable, "./table1yrAllCause.csv", quote = FALSE, row.names = FALSE)
# clean up
remove(all_sheets)
remove(baseline)
remove(booleanFactor)
remove(timeEvent)
remove(tmp)
remove(boolVars)
remove(col)
remove(date_diff)
remove(death_date)
remove(f)
remove(factorVars)
remove(i)
remove(path)
remove(proc_date)
remove(row)

