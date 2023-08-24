read_files <- function(path) {
  library(readxl)
  # read files
  sheets <- readxl::excel_sheets(path)
  x <-
    lapply(sheets, function(X)
      readxl::read_excel(path, sheet = X))
  x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
  
}

drop_patients_and_columns <- function(baseline, baselinelabelled) {
  library(data.table)
  # drop all patients without procedure date
  baseline <- baseline[!is.na(baseline$proc_date),]
  baselinelabelled <-
    baselinelabelled[!is.na(baselinelabelled$`Date of procedure`), ]
  
  # drop columns with >= 10% NAs
  baseline <-
    baseline[, colSums(is.na(baseline)) < 0.1 * nrow(baseline)]
  baselinelabelled <-
    baselinelabelled[, colSums(is.na(baselinelabelled)) < 0.1 * nrow(baselinelabelled)]
  
  # convert to data table
  baselineDT <- as.data.table(baseline)
  baselinelabelledDT <- as.data.table(baselinelabelled)
  
  # drop columns we don't want to know about / that are procedure specific / that are dates
  baselineDT <-
    baselineDT[, c(
      "study_site",
      "site",
      "visit",
      "no",
      "ic1",
      "ic1_date",
      "ic2",
      "ic3",
      "ic5",
      "ic4",
      "indication",
      "eligible",
      "fid",
      "eligibility_state",
      "characteristics_state",
      "medication_state",
      "lab_date",
      "tropo_unit",
      "creatinine_unit",
      "hb_unit",
      "thrombo_unit",
      "ck_unit",
      "ckmb_unit",
      "bnp_unit",
      "albumin_unit",
      "lab_state",
      "ecg_date",
      "ecg_state",
      "newonsetafib",
      "tte_date",
      "imaging_state",
      "postmeasure",
      "assessment",
      "reposition",
      "valve",
      "retrieval",
      "disloc",
      "savr",
      "transapical",
      "tamponade",
      "instability",
      "instability_balloon",
      "instability_assist",
      "instability_machine",
      "resuscitation",
      "complication",
      "complication_stent",
      "complication_surgery",
      "complication_stenosis",
      "complication_rupture",
      "complication_fistula",
      "complication_hematoma",
      "complication_embolization",
      "complication_failure",
      "ca_occlusion",
      "rupture",
      "procedure_state",
      "regurgitation_post",
      "post_gradient_mean"
    ) := NULL]
  
  
  baselinelabelledDT <-
    baselinelabelledDT[, c(
      "Hospital",
      "Hospital identifier",
      "Visit identifier",
      "Number",
      "Written informed consent",
      "Date of written informed consent",
      "Age = 18 years",
      "Multidisciplinary decision",
      "Prospective evaluation:",
      "Indication 1",
      "Indication 2",
      "Patient is",
      "eCRF identifier",
      "eCRF state...16",
      "eCRF state...72",
      "eCRF state...98",
      "Date of laboratory assessment",
      "Troponin unit of measurement",
      "Creatinine unit of measurement",
      "Hemoglobin unit of measurement",
      "Thrombocytes unit of measurement",
      "CK unit of measurement",
      "CK-MB unit of measurement",
      "BNP unit of measurement",
      "Albumin unit of measurement",
      "eCRF state...131",
      "ECG date",
      "eCRF state...141",
      "New-Onset of Atrial Fibrillation since baseline" ,
      "Date of TTE",
      "eCRF state...203",
      "Invasive hemodynamic measurements performed...258" ,
      "Assessed by",
      "Reposition with snare",
      "Valve in series",
      "Valve retrieval",
      "Valve dislocation/embolization",
      "Conversion to Surgical Aortic Valve Replacement (SAVR)",
      "Conversion to other access route",
      "Cardiac tamponade/rupture",
      "Hemodynamic instability requiring treatment",
      "Balloon pump",
      "Assist device",
      "Heart lung machine",
      "Resuscitation",
      "Access vessel complication",
      "Was a stent placed",
      "Was vascular surgery required",
      "Stenosis",
      "Rupture",
      "AV-fistula",
      "Hematoma (>2 units transfusion)",
      "Embolization",
      "Failure of percutaneous access site closure",
      "Coronary artery occlusion",
      "Annulus rupture/aortic dissection",
      "eCRF state...297",
      "Aortic regurgitation grade - postmeasure/discharge",
      "Post-procedure mean trans-prosthetic gradient (mmHg) - postmeasure/discharge/fol"
    ) := NULL]
  
  return(list(baselineDT, baselinelabelledDT))
}

load_data <- function(path){
  all_sheets <- read_files(path)
  baseline <- all_sheets$baseline
  baselinelabelled <- all_sheets$baseline_labelled
  print("Dropping patients without procedure dates, columns with >=10% NA's and non-numerical and irrelevant columns...")
  baselinelist <- drop_patients_and_columns(baseline, baselinelabelled)
  all_sheets$baseline <- baselinelist[[1]]
  all_sheets$baseline_labelled <- baselinelist[[2]]
  return(all_sheets)
}

create_time2event <- function(events, fup30days, fup1yr, fup2yr, fup3yr, fup4yr, fup5yr, fup7yr, unscheduled, discharge){
  #outer join the discharge patients with the followed up patients, include death date
  mergeall <- merge(events[,c("patient","proc_date", "death_date")], fup30days[, c("patient", "contact_date", "contact_status")], by = "patient", all= TRUE) 
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
  timeEvent <- merge(timeEvent, events[,c("patient","death_date", "death_cause")], by = 1, all.x = TRUE)
  timeEvent <- merge(timeEvent, discharge[, c("patient", "dis")], by = 1, all.x = TRUE)
  
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
  return(timeEvent)
}

# Training + validation

path <- "./SwissTAVI_zurich_von_CTU_Bern_20191120.xlsx"
all_sheets <- load_data(path)
baselineDT <- all_sheets$baseline
baselinelabelledDT <- all_sheets$baseline_labelled

# look-up for names
baselineNames <- colnames(baselinelabelledDT)
names(baselineNames) <- colnames(baselineDT)
eventsNames <- cbind(colnames(all_sheets$events_labelled), colnames(all_sheets$events))