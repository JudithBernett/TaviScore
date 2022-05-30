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

#outer join the discharge patients with the followed up patients, include death date
mergeall <- merge(all_sheets$events[,c("patient","proc_date", "death_date")], fup30days[, c("patient", "contact_date", "contact_status")], by = "patient", all= TRUE) 
mergeall <- merge(mergeall, fup1yr[,c("patient", "contact_date", "contact_status")], by = "patient", all = TRUE)
mergeall <- merge(mergeall, fup2yr[,c("patient", "contact_date", "contact_status")], by = "patient", all = TRUE)
mergeall <- merge(mergeall, fup3yr[,c("patient", "contact_date", "contact_status")], by = "patient", all = TRUE)
mergeall <- merge(mergeall, fup4yr[,c("patient", "contact_date", "contact_status")], by = "patient", all = TRUE)
mergeall <- merge(mergeall, fup5yr[,c("patient", "contact_date", "contact_status")], by = "patient", all = TRUE)
mergeall <- merge(mergeall, fup7yr[,c("patient", "contact_date", "contact_status")], by = "patient", all = TRUE)
mergeall <- merge(mergeall, unscheduled[,c("patient", "contact_date", "contact_status")], by = "patient", all = TRUE)
colnames(mergeall) <- c("Patient", "proc_date","death_date", 
                        "contact_date_30d", "contact_status_30d",
                        "contact_date_1yr", "contact_status_1yr",
                        "contact_date_2yr", "contact_status_2yr",
                        "contact_date_3yr", "contact_status_3yr",
                        "contact_date_4yr", "contact_status_4yr",
                        "contact_date_5yr", "contact_status_5yr",
                        "contact_date_7yr", "contact_status_7yr",
                        "contact_date_unsc","contact_status_unsc")
remove(fup30days)
remove(fup1yr)
remove(fup2yr)
remove(fup3yr)
remove(fup4yr)
remove(fup5yr)
remove(fup7yr)
remove(unscheduled)

#discard all the patients that have not a single follow up (NA in every contact date column) and no known death date
mergeall <- mergeall[!(is.na(mergeall$death_date) & 
                         is.na(mergeall$contact_date_30d) & 
                         is.na(mergeall$contact_date_1yr) &
                         is.na(mergeall$contact_date_2yr) &
                         is.na(mergeall$contact_date_3yr) &
                         is.na(mergeall$contact_date_4yr) &
                         is.na(mergeall$contact_date_5yr) &
                         is.na(mergeall$contact_date_7yr) &
                         is.na(mergeall$contact_date_unsc)),]

#discard duplicated entries
mergeall <- unique(mergeall)

#there are NA values in proc date. We will discard them
mergeall <- mergeall[!is.na(mergeall$proc_date),]
#create a new dataframe for the biggest dates
timeEvent <- mergeall[FALSE,]
timeEvent <- timeEvent[,c(1,2,3,4)]
colnames(timeEvent) <- c("Patient", "proc_date", "time", "event")
timeTable <- data.table(Patient = character(), time = numeric())
allTimeTable <- data.table(Patient = character(), time = numeric(), type = character())

#find the biggest follow-up date within 365 days
#if the patient survives 365 days -> event = 0
print("Calculating biggest follow-up dates...")

for (row in 1:nrow(mergeall)){
  patient <- mergeall[row, "Patient"]
  proc_date <- mergeall[row, "proc_date"]
  timedeath <- 0
  time30 <- 0
  time1 <- 0
  time2 <- 0
  time3 <- 0
  time4 <- 0
  time5 <-0
  time7 <-0
  timeunsc <- 0
  date <- 0
  time <-0
  tmpTimeEvent <- timeEvent
  time365 <- 0
  #calculate time = date of follow up - proc date
  if(!is.na(mergeall[row, "death_date"])){
    timedeath <- as.numeric(mergeall[row, "death_date"]-proc_date, "days")
  }
  if(!is.na(mergeall[row, "contact_date_30d"])){
    time30 <- as.numeric(mergeall[row, "contact_date_30d"]-proc_date, "days")
    allTimeTable <- rbind(allTimeTable, data.table(Patient = patient, time = time30, type = "30d"))
  }
  if(!is.na(mergeall[row, "contact_date_1yr"])){
    time1 <- as.numeric(mergeall[row, "contact_date_1yr"]-proc_date, "days")
    allTimeTable <- rbind(allTimeTable, data.table(Patient = patient, time = time1, type = "1yr"))
  }
  if(!is.na(mergeall[row, "contact_date_2yr"])){
    time2 <- as.numeric(mergeall[row, "contact_date_2yr"]-proc_date, "days")
    allTimeTable <- rbind(allTimeTable, data.table(Patient = patient, time = time2, type = "2yr"))
  }
  if(!is.na(mergeall[row, "contact_date_3yr"])){
    time3 <- as.numeric(mergeall[row, "contact_date_3yr"]-proc_date, "days")
    allTimeTable <- rbind(allTimeTable, data.table(Patient = patient, time = time3, type = "3yr"))
  }
  if(!is.na(mergeall[row, "contact_date_4yr"])){
    time4 <- as.numeric(mergeall[row, "contact_date_4yr"]-proc_date, "days")
    allTimeTable <- rbind(allTimeTable, data.table(Patient = patient, time = time4, type = "4yr"))
  }
  if(!is.na(mergeall[row, "contact_date_5yr"])){
    time5 <- as.numeric(mergeall[row, "contact_date_5yr"]-proc_date, "days")
    allTimeTable <- rbind(allTimeTable, data.table(Patient = patient, time = time5, type = "5yr"))
  }
  if(!is.na(mergeall[row, "contact_date_7yr"])){
    time7 <- as.numeric(mergeall[row, "contact_date_7yr"]-proc_date, "days")
    allTimeTable <- rbind(allTimeTable, data.table(Patient = patient, time = time7, type = "7yr"))
  }
  if(!is.na(mergeall[row, "contact_date_unsc"])){
    timeunsc <- as.numeric(mergeall[row, "contact_date_unsc"]-proc_date, "days")
    allTimeTable <- rbind(allTimeTable, data.table(Patient = patient, time = timeunsc, type = "unsc"))
  }
  
  time <- max(timedeath,time30, time1, time2,time3,time4,time5,time7,timeunsc) + 1
  timeEntry <- data.table(Patient = patient, time = time)
  timeTable <- rbind(timeTable, timeEntry)
  contact_status <- -1
  
  for( t in c(timedeath, time30, time1, time2, time3, time5, time7, timeunsc) ){
    #find biggest time thats smaller than 365
    if(t > time365 & t <= 365){
      time365 <- t
    }
    #find smallest time thats bigger than 365
    if(t > 365 & t < time){
      time <- t
      if(t == timedeath){
        contact_status <- 4
      }else if(t == time30){
        contact_status <- mergeall[row, "contact_status_30d"]
      }else if(t == time1){
        contact_status <- mergeall[row, "contact_status_1yr"]
      }else if(t == time2){
        contact_status <- mergeall[row, "contact_status_2yr"]
      }else if(t == time3){
        contact_status <- mergeall[row, "contact_status_3yr"]
      }else if(t == time4){
        contact_status <- mergeall[row, "contact_status_4yr"]
      }else if(t == time5){
        contact_status <- mergeall[row, "contact_status_5yr"]
      }else if(t == time7){
        contact_status <- mergeall[row, "contact_status_7yr"]
      }else if(t == timeunsc){
        contact_status <- mergeall[row, "contact_status_unsc"]
      }
    }
  }
  if(time365 == 0 & time > 365){
    #if status is not 4, the patient definitely survived the year. 
    #There is only one case where the patient has a status of 4 and time > 365 (time 1094, id: 113-121-84X). 
    #The rest has status 1.
    #He died in the hospital because of cancer and unrelated to the procedure. Therefore, I will give everyone status 1
    #and cut the time to 365
    
    tmpTimeEvent <- data.frame(patient, as.Date(proc_date), 365, 1)
    colnames(tmpTimeEvent) <- c("Patient", "proc_date", "time", "status")
    timeEvent <- rbind(timeEvent, tmpTimeEvent)
  }else if(time365 == timedeath){
    #contact status is obviously 4: patient dead
    tmpTimeEvent <- data.frame(patient, as.Date(proc_date), time365, 4)
    colnames(tmpTimeEvent) <- c("Patient", "proc_date","time","status")
    timeEvent <- rbind(timeEvent, tmpTimeEvent)
  }
  else if(time365 == time30){
    tmpTimeEvent <- data.frame(patient, as.Date(proc_date), time365, as.numeric(mergeall[row, "contact_status_30d"]))
    colnames(tmpTimeEvent) <- c("Patient", "proc_date", "time", "status")
    timeEvent <- rbind(timeEvent, tmpTimeEvent)
  }else if(time365 == time1){
    tmpTimeEvent <- data.frame(patient, as.Date(proc_date), time365, as.numeric(mergeall[row, "contact_status_1yr"]))
    colnames(tmpTimeEvent) <- c("Patient", "proc_date", "time", "status")
    timeEvent <- rbind(timeEvent, tmpTimeEvent)
  }else if(time365 == time2){
    tmpTimeEvent <- data.frame(patient, as.Date(proc_date), time365, as.numeric(mergeall[row, "contact_status_2yr"]))
    colnames(tmpTimeEvent) <- c("Patient", "proc_date", "time", "status")
    timeEvent <- rbind(timeEvent, tmpTimeEvent)
  }else if(time365 == time3){
    tmpTimeEvent <- data.frame(patient, as.Date(proc_date), time365, as.numeric(mergeall[row, "contact_status_3yr"]))
    colnames(tmpTimeEvent) <- c("Patient", "proc_date", "time", "status")
    timeEvent <- rbind(timeEvent, tmpTimeEvent)
  }else if(time365 == time4){
    tmpTimeEvent <- data.frame(patient, as.Date(proc_date), time365, as.numeric(mergeall[row, "contact_status_4yr"]))
    colnames(tmpTimeEvent) <- c("Patient", "proc_date", "time", "status")
    timeEvent <- rbind(timeEvent, tmpTimeEvent)
  }else if(time365 == time5){
    tmpTimeEvent <- data.frame(patient, as.Date(proc_date), time365, as.numeric(mergeall[row, "contact_status_5yr"]))
    colnames(tmpTimeEvent) <- c("Patient", "proc_date", "time", "status")
    timeEvent <- rbind(timeEvent, tmpTimeEvent)
  }else if(time365 == time7){
    tmpTimeEvent <- data.frame(patient, as.Date(proc_date), time365, as.numeric(mergeall[row, "contact_status_7yr"]))
    colnames(tmpTimeEvent) <- c("Patient", "proc_date", "time", "status")
    timeEvent <- rbind(timeEvent, tmpTimeEvent)
  }else if(time365 == timeunsc){
    tmpTimeEvent <- data.frame(patient, as.Date(proc_date), time365, as.numeric(mergeall[row, "contact_status_unsc"]))
    colnames(tmpTimeEvent) <- c("Patient", "proc_date", "time", "status")
    timeEvent <- rbind(timeEvent, tmpTimeEvent)
  }
}
# clean up
remove(timeEntry)
remove(tmpTimeEvent)
remove(mergeall)
remove(t)
remove(time)
remove(time1)
remove(time2)
remove(time3)
remove(time4)
remove(time5)
remove(time7)
remove(time30)
remove(time365)
remove(timedeath)
remove(timeunsc)
remove(contact_status)
remove(date)
remove(patient)
remove(proc_date)
remove(row)

#Discard duplicated patients, keep the biggest time, merge with event table
timeEvent <- timeEvent[order(-timeEvent$time),]
timeEvent <- timeEvent[!duplicated(timeEvent$Patient),]
#merge timeEvent table with event table
timeEvent <- merge(timeEvent, all_sheets$events[,c("patient","death_date", "death_cause")], by = 1, all.x = TRUE)
timeEvent <- merge(timeEvent, all_sheets$discharge[, c("patient", "dis")], by = 1, all.x = TRUE)

#Because we only want the patients that survived the hospital, we need to delete all patients with a "dis"-code of 88
print("Exclude patients who died in the hospital...")
tmp <- timeEvent[timeEvent$dis == 88, ]
tmp <- data.table(patient = tmp$Patient)
tmp <- unique(tmp)

#If death date is > 365 days far from proc date -> discard
print("Discard all death events that happened after > 365 days...")
for(row in 1: nrow(timeEvent)){
  proc_date <- as.Date(timeEvent[row, "proc_date"])
  death_date <- as.Date(timeEvent[row, "death_date"])
  date_diff <- 0
  if(!is.na(death_date)){
    date_diff <- as.numeric(death_date-proc_date, "days")
  }
  if(date_diff > 365){
    timeEvent[row, "death_date"] <- NA
    timeEvent[row, "death_cause"] <- NA
  }
  
}

timeEvent <- timeEvent[, c(1,2,3,4 )]
timeEvent <- timeEvent[order(-timeEvent$time),]
timeEvent <- timeEvent[!duplicated(timeEvent$Patient),]
timeEvent <- timeEvent[!(timeEvent$Patient %in% tmp$patient), ]
timeEvent$status[timeEvent$status != 4] <- 0
timeEvent$status[timeEvent$status == 4] <- 1
colnames(timeEvent) <- c("Patient","proc_date", "time", "event")
timeEvent <- timeEvent[complete.cases(timeEvent),]


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

