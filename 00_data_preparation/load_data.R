###Script for the bachelor's thesis###
#library(dplyr)
#library(tidyr)
#library(cowplot)
#library(caret)
#library(glmnet)
#library(survival)
#library(survAUC)
#library(nricens)
#library(ggRandomForests)
#library(pheatmap)
#library(RColorBrewer)
#library(survminer)

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

path <- "./SwissTAVI_zurich_von_CTU_Bern_20191120.xlsx"
all_sheets <- load_data(path)
baselineDT <- all_sheets$baseline
baselinelabelledDT <- all_sheets$baseline_labelled

# look-up for names
baselineNames <- colnames(baselinelabelledDT)
names(baselineNames) <- colnames(baselineDT)
eventsNames <- cbind(colnames(all_sheets$events_labelled), colnames(all_sheets$events))
