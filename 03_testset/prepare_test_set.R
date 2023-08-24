source("00_data_preparation/load_data.R")
library(readxl)
library(data.table)
remove(all_sheets)
remove(baselineDT)
remove(baselinelabelledDT)
remove(eventsNames)
remove(baselineNames)
remove(path)

binarize_variable <- function(dt, old_name, new_name){
  dt[, eval(new_name) := ifelse(dt[, old_name, with=FALSE] == 'yes', 1, 0)]
  dt[, eval(old_name) := NULL]
  return(dt)
}

categorize_regurgitation <- function(dt, old_name, new_name){
  levels = c('none', 'mild', 'moderate', 'severe')
  dt[, eval(old_name) := factor(get(old_name), levels=levels)]
  dt[, eval(new_name) := ifelse(get(old_name) == 'none', 1, 
                                ifelse(get(old_name) == 'mild', 2, 
                                       ifelse(get(old_name) == 'moderate', 3, 
                                              ifelse(get(old_name) == 'severe', 4, NA))))]
  dt[, eval(old_name) := NULL]
}

code_factor <- function(dt, variable){
  f <- as.formula(paste('~', variable, '-1'))
  booleanFactor <- model.matrix(f, dt)
  dt[, eval(variable) := NULL]
  dt <- cbind(dt, booleanFactor)
  return(dt)
}

path <- 'SwissTAVI_luzern_für_Philip_Jakob 20230308_FM.xlsx'
#### read in excel sheet
all_sheets <- read_files(path)
baselineDT <- as.data.table(all_sheets$`Lucerne Data cleaned`)
baselineDT <-
  baselineDT[, colnames(baselineDT)[colSums(is.na(baselineDT)) < 0.1 * nrow(baselineDT)], with=FALSE]
baselineDT <- baselineDT[, -c('study_site', 'Date of procedure')]
vars <- colnames(baselineDT)[-c(1,2)]
baselineDT[, (vars) := replace(.SD, .SD == '.m', NA), .SDcols = vars]

# categorize variables
mapping <- c('Peripheral artery disease' = 'ad', 'Diuretics'='diuretics')
lapply(names(mapping), function(x){
  baselineDT <- binarize_variable(baselineDT, x, mapping[x])
})
print("Recode boolean 1/2 boolean variables to 0/1...")
bool_vars <- c('sex', 'copd')
baselineDT[, (bool_vars) := replace(.SD, .SD == 2, 0), .SDcols = bool_vars]

mapping <- c('Mitral regurgitation grade - TTE/TEE' = 'regurg_mitral', 'Tricuspid Regurgitation Grade - TTE/TEE)'='regurg_tricuspid')
lapply(names(mapping), function(x){
  baselineDT <- categorize_regurgitation(baselineDT, x, mapping[x])
})

levels = c('No Angina', 'CCS1', 'CCS2', 'CCS3', 'CCS4')
old_name <- 'CCS Angina Class (No to 4)'
new_name <- 'ap_ccs'
baselineDT[, eval(old_name) := factor(get(old_name), levels=levels)]
baselineDT[, eval(new_name) := ifelse(get(old_name) == 'No Angina', 0,
                                      ifelse(get(old_name) == 'CCS1', 1, 
                                             ifelse(get(old_name) == 'CCS2', 2, 
                                                    ifelse(get(old_name) == 'CCS3', 3, 
                                                           ifelse(get(old_name) == 'CCS4', 4, NA)))))]
baselineDT[, eval(old_name) := NULL]
colnames(baselineDT) <- c('proc_date', 'patient', 'age', 'sex', 'copd', 'scoreii_log', 'calc_sts', 'hb', 'ad', 'medi_diuretic', 'regurg_mitral', 'regurg_tricuspid', 'ap_ccs')

vars <- colnames(baselineDT)[-c(1,2)]
for(col in vars){
  baselineDT[, eval(col) := unlist(replace(.SD, is.na(.SD), lapply(.SD, median, na.rm=TRUE))), .SDcols=col]
}

factor_vars <- c('regurg_mitral', 'regurg_tricuspid', 'ap_ccs')
baselineDT[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]
for(col in factor_vars){
  baselineDT <- code_factor(baselineDT, col)
}

baselineDT[, regurg_mitral34 := ifelse(regurg_mitral3 == 1 | regurg_mitral4 == 1, 1, 0)]
baselineDT[, regurg_tricuspid34 := ifelse(regurg_tricuspid3 == 1 | regurg_tricuspid4 == 1, 1, 0)]
baselineDT[, ccs_stratified := ifelse(ap_ccs0 == 1 | ap_ccs1 == 1, 1, 0)]

fup30days <- all_sheets$FU30days[all_sheets$FU30days$contact_status!=5,]
colnames(fup30days) <- c('patient', 'contact_date', 'contact_status')
fup1yr <- all_sheets$FU1y[all_sheets$FU1y$contact_status!=5,]
fup2yr <- all_sheets$FU2y[all_sheets$FU2y$contact_status!=5,]
fup3yr <- all_sheets$FU3y[all_sheets$FU3y$contact_status!=5,]
fup4yr <- all_sheets$FU4y[all_sheets$FU4y$contact_status!=5,]
fup5yr <- all_sheets$FU5y[all_sheets$FU5y$contact_status!=5,]
fup7yr <- all_sheets$FU7y[all_sheets$FU7y$contact_status!=5,]
unscheduled <- all_sheets$unscheduled[all_sheets$unscheduled$contact_status!=5,]
events <- all_sheets$events
discharge <- all_sheets$discharge

timeEvent <- create_time2event(events, fup30days, fup1yr, fup2yr, fup3yr, fup4yr, fup5yr, fup7yr, unscheduled, discharge)
timeEvent <- as.data.table(timeEvent)
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
timeEvent[, proc_date := NULL]

finalTable <- merge(timeEvent, baselineDT, by.x = 'Patient', by.y = 'patient', all.x = TRUE)
write.csv(finalTable, "./testset_table1yrAllCause.csv", quote = FALSE, row.names = FALSE)


