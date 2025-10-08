
# initialize list
identify_prev_cases_list <- as.list(seq(1, 29))


# mortality (no subset)
identify_prev_cases_list[[1]] <- function(x) {x <- x}

# colorectal cancer
identify_prev_cases_list[[2]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$colon_cancer_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_colon_cancer <- NA
  x$baseline_colon_cancer[baseline_ncd] <- 1
  x$baseline_colon_cancer[which(x$colon_cancer_baseline_dx == 1)] <- 1
  x$baseline_colon_cancer[which(is.na(x$baseline_colon_cancer))] <- 0
  x$baseline_colon_cancer <- factor(x$baseline_colon_cancer)
  
  x <- x[which(x$baseline_colon_cancer == 0), ]
  return(x)
}

# lung cancer
identify_prev_cases_list[[3]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$lung_cancer_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_lung_cancer <- NA
  x$baseline_lung_cancer[baseline_ncd] <- 1
  x$baseline_lung_cancer[which(x$lung_cancer_baseline_dx == 1)] <- 1
  x$baseline_lung_cancer[which(is.na(x$baseline_lung_cancer))] <- 0
  x$baseline_lung_cancer <- factor(x$baseline_lung_cancer)
  
  x <- x[which(x$baseline_lung_cancer == 0), ]
  return(x)
}

# eso cancer
identify_prev_cases_list[[4]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$eso_cancer_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_eso_cancer <- NA
  x$baseline_eso_cancer[baseline_ncd] <- 1
  x$baseline_eso_cancer[which(x$eso_cancer_baseline_dx == 1)] <- 1
  x$baseline_eso_cancer[which(is.na(x$baseline_eso_cancer))] <- 0
  x$baseline_eso_cancer <- factor(x$baseline_eso_cancer)
  
  x <- x[which(x$baseline_eso_cancer == 0), ]
  return(x)
}

# liver cancer
identify_prev_cases_list[[5]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$liver_cancer_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_liver_cancer <- NA
  x$baseline_liver_cancer[baseline_ncd] <- 1
  x$baseline_liver_cancer[which(x$liver_cancer_baseline_dx == 1)] <- 1
  x$baseline_liver_cancer[which(is.na(x$baseline_liver_cancer))] <- 0
  x$baseline_liver_cancer <- factor(x$baseline_liver_cancer)
  
  x <- x[which(x$baseline_liver_cancer == 0), ]
  return(x)
}

# pancreatic cancer
identify_prev_cases_list[[6]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$pancreatic_cancer_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_pancreatic_cancer <- NA
  x$baseline_pancreatic_cancer[baseline_ncd] <- 1
  x$baseline_pancreatic_cancer[which(x$pancreatic_cancer_baseline_dx == 1)] <- 1
  x$baseline_pancreatic_cancer[which(is.na(x$baseline_pancreatic_cancer))] <- 0
  x$baseline_pancreatic_cancer <- factor(x$baseline_pancreatic_cancer)
  
  x <- x[which(x$baseline_pancreatic_cancer == 0), ]
  return(x)
}

# brain cancer
identify_prev_cases_list[[7]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$brain_cancer_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_brain_cancer <- NA
  x$baseline_brain_cancer[baseline_ncd] <- 1
  x$baseline_brain_cancer[which(x$brain_cancer_baseline_dx == 1)] <- 1
  x$baseline_brain_cancer[which(is.na(x$baseline_brain_cancer))] <- 0
  x$baseline_brain_cancer <- factor(x$baseline_brain_cancer)
  
  x <- x[which(x$baseline_brain_cancer == 0), ]
  return(x)
}

# leukemia
identify_prev_cases_list[[8]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$leukemia_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_leukemia <- NA
  x$baseline_leukemia[baseline_ncd] <- 1
  x$baseline_leukemia[which(x$leukemia_baseline_dx == 1)] <- 1
  x$baseline_leukemia[which(is.na(x$baseline_leukemia))] <- 0
  x$baseline_leukemia <- factor(x$baseline_leukemia)
  
  x <- x[which(x$baseline_leukemia == 0), ]
  return(x)
}

# lymphoma
identify_prev_cases_list[[9]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$lymphoma_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_lymphoma <- NA
  x$baseline_lymphoma[baseline_ncd] <- 1
  x$baseline_lymphoma[which(x$lymphoma_baseline_dx == 1)] <- 1
  x$baseline_lymphoma[which(is.na(x$baseline_lymphoma))] <- 0
  x$baseline_lymphoma <- factor(x$baseline_lymphoma)
  
  x <- x[which(x$baseline_lymphoma == 0), ]
  return(x)
}

# breast cancer
identify_prev_cases_list[[10]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$breast_cancer_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_breast_cancer <- NA
  x$baseline_breast_cancer[baseline_ncd] <- 1
  x$baseline_breast_cancer[which(x$breast_cancer_baseline_dx == 1)] <- 1
  x$baseline_breast_cancer[which(is.na(x$baseline_breast_cancer))] <- 0
  x$baseline_breast_cancer <- factor(x$baseline_breast_cancer)
  
  x <- x[which(x$baseline_breast_cancer == 0), ]
  x <- x[which(x$sex == "Female"), ]
  return(x)
}

# ovarian cancer
identify_prev_cases_list[[11]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$ovarian_cancer_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_ovarian_cancer <- NA
  x$baseline_ovarian_cancer[baseline_ncd] <- 1
  x$baseline_ovarian_cancer[which(x$ovarian_cancer_baseline_dx == 1)] <- 1
  x$baseline_ovarian_cancer[which(is.na(x$baseline_ovarian_cancer))] <- 0
  x$baseline_ovarian_cancer <- factor(x$baseline_ovarian_cancer)
  
  x <- x[which(x$baseline_ovarian_cancer == 0), ]
  x <- x[which(x$sex == "Female"), ]
  return(x)
}

# prostate cancer
identify_prev_cases_list[[12]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$prostate_cancer_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_prostate_cancer <- NA
  x$baseline_prostate_cancer[baseline_ncd] <- 1
  x$baseline_prostate_cancer[which(x$prostate_cancer_baseline_dx == 1)] <- 1
  x$baseline_prostate_cancer[which(is.na(x$baseline_prostate_cancer))] <- 0
  x$baseline_prostate_cancer <- factor(x$baseline_prostate_cancer)
  
  x <- x[which(x$baseline_prostate_cancer == 0), ]
  x <- x[which(x$sex == "Male"), ]
  return(x)
}

# diabetes
identify_prev_cases_list[[13]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$diabetes_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_diabetes <- NA
  x$baseline_diabetes[baseline_ncd] <- 1
  x$baseline_diabetes[which(x$diabetes_diagnosis == "Yes")] <- 1
  x$baseline_diabetes[which(x$diabetes_baseline_dx == 1)] <- 1
  x$baseline_diabetes[which(x$insulin == "Yes")] <- 1
  x$baseline_diabetes[which(x$hbA1c >= 48)] <- 1
  x$baseline_diabetes[which(x$glucose >= 11.1)] <- 1
  x$baseline_diabetes[which(is.na(x$baseline_diabetes))] <- 0
  x$baseline_diabetes <- factor(x$baseline_diabetes)
  
  x <- x[which(x$baseline_diabetes == 0), ]
  return(x)
}

# cvd
identify_prev_cases_list[[14]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$cvd_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_cvd <- NA
  x$baseline_cvd[baseline_ncd] <- 1
  x$baseline_cvd[which(x$heart_attack_diagnosis == "Yes")] <- 1
  x$baseline_cvd[which(x$angina_diagnosis == "Yes")] <- 1
  x$baseline_cvd[which(x$CVD_baseline_dx == 1)] <- 1
  x$baseline_cvd[which(is.na(x$baseline_cvd))] <- 0
  x$baseline_cvd <- factor(x$baseline_cvd)
  
  x <- x[which(x$baseline_cvd == 0), ]
  return(x)
}

# cerebro
identify_prev_cases_list[[15]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$cerebro_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_cerebro <- NA
  x$baseline_cerebro[baseline_ncd] <- 1
  x$baseline_cerebro[which(x$stroke_diagnosis == "Yes")] <- 1
  x$baseline_cerebro[which(x$cerebro_baseline_dx == 1)] <- 1
  x$baseline_cerebro[which(is.na(x$baseline_cerebro))] <- 0
  x$baseline_cerebro <- factor(x$baseline_cerebro)
  
  x <- x[which(x$baseline_cerebro == 0), ]
  return(x)
}

# copd
identify_prev_cases_list[[16]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$copd_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_copd <- NA
  x$baseline_copd[baseline_ncd] <- 1
  x$baseline_copd[which(x$bronchitis_emphysema_diagnosis == "Yes")] <- 1
  x$baseline_copd[which(x$COPD_baseline_dx == 1)] <- 1
  x$baseline_copd[which(is.na(x$baseline_copd))] <- 0
  x$baseline_copd <- factor(x$baseline_copd)
  
  x <- x[which(x$baseline_copd == 0), ]
  return(x)
}

# liver
identify_prev_cases_list[[17]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$liver_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_liver <- NA
  x$baseline_liver[baseline_ncd] <- 1
  x$baseline_liver[which(x$liver_baseline_dx == 1)] <- 1
  x$baseline_liver[which(is.na(x$baseline_liver))] <- 0
  x$baseline_liver <- factor(x$baseline_liver)
  
  x <- x[which(x$baseline_liver == 0), ]
  return(x)
}

# kidney
identify_prev_cases_list[[18]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$kidney_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_kidney <- NA
  x$baseline_kidney[baseline_ncd] <- 1
  x$baseline_kidney[which(x$kidney_baseline_dx == 1)] <- 1
  x$baseline_kidney[which(is.na(x$baseline_kidney))] <- 0
  x$baseline_kidney <- factor(x$baseline_kidney)
  
  x <- x[which(x$baseline_kidney == 0), ]
  return(x)
}

# all_dementia
identify_prev_cases_list[[19]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$all_dementia_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_all_dementia <- NA
  x$baseline_all_dementia[baseline_ncd] <- 1
  x$baseline_all_dementia[which(x$all_dementia_baseline_dx == 1)] <- 1
  x$baseline_all_dementia[which(is.na(x$baseline_all_dementia))] <- 0
  x$baseline_all_dementia <- factor(x$baseline_all_dementia)
  
  x <- x[which(x$baseline_all_dementia == 0), ]
  return(x)
}

# vasc_dementia
identify_prev_cases_list[[20]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$vasc_dementia_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_vasc_dementia <- NA
  x$baseline_vasc_dementia[baseline_ncd] <- 1
  x$baseline_vasc_dementia[which(x$vasc_dementia_baseline_dx == 1)] <- 1
  x$baseline_vasc_dementia[which(is.na(x$baseline_vasc_dementia))] <- 0
  x$baseline_vasc_dementia <- factor(x$baseline_vasc_dementia)
  
  x <- x[which(x$baseline_vasc_dementia == 0), ]
  return(x)
}

# alzheimers
identify_prev_cases_list[[21]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$alzheimers_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_alzheimers <- NA
  x$baseline_alzheimers[baseline_ncd] <- 1
  x$baseline_alzheimers[which(x$alzheimers_baseline_dx == 1)] <- 1
  x$baseline_alzheimers[which(is.na(x$baseline_alzheimers))] <- 0
  x$baseline_alzheimers <- factor(x$baseline_alzheimers)
  
  x <- x[which(x$baseline_alzheimers == 0), ]
  return(x)
}

# parkinsons
identify_prev_cases_list[[22]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$parkinsons_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_parkinsons <- NA
  x$baseline_parkinsons[baseline_ncd] <- 1
  x$baseline_parkinsons[which(x$parkinsons_baseline_dx == 1)] <- 1
  x$baseline_parkinsons[which(is.na(x$baseline_parkinsons))] <- 0
  x$baseline_parkinsons <- factor(x$baseline_parkinsons)
  
  x <- x[which(x$baseline_parkinsons == 0), ]
  return(x)
}

# rheumatoid
identify_prev_cases_list[[23]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$rheumatoid_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_rheumatoid <- NA
  x$baseline_rheumatoid[baseline_ncd] <- 1
  x$baseline_rheumatoid[which(x$rheumatoid_baseline_dx == 1)] <- 1
  x$baseline_rheumatoid[which(is.na(x$baseline_rheumatoid))] <- 0
  x$baseline_rheumatoid <- factor(x$baseline_rheumatoid)
  
  x <- x[which(x$baseline_rheumatoid == 0), ]
  return(x)
}

# macular
identify_prev_cases_list[[24]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$macular_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_macular <- NA
  x$baseline_macular[baseline_ncd] <- 1
  x$baseline_macular[which(x$macular_baseline_dx == 1)] <- 1
  x$baseline_macular[which(is.na(x$baseline_macular))] <- 0
  x$baseline_macular <- factor(x$baseline_macular)
  
  x <- x[which(x$baseline_macular == 0), ]
  return(x)
}

# osteoporosis
identify_prev_cases_list[[25]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$osteoporosis_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_osteoporosis <- NA
  x$baseline_osteoporosis[baseline_ncd] <- 1
  x$baseline_osteoporosis[which(x$osteoporosis_baseline_dx == 1)] <- 1
  x$baseline_osteoporosis[which(is.na(x$baseline_osteoporosis))] <- 0
  x$baseline_osteoporosis <- factor(x$baseline_osteoporosis)
  
  x <- x[which(x$baseline_osteoporosis == 0), ]
  return(x)
}

# osteoarthritis
identify_prev_cases_list[[26]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$osteoarthritis_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_osteoarthritis <- NA
  x$baseline_osteoarthritis[baseline_ncd] <- 1
  x$baseline_osteoarthritis[which(x$osteoarthritis_baseline_dx == 1)] <- 1
  x$baseline_osteoarthritis[which(is.na(x$baseline_osteoarthritis))] <- 0
  x$baseline_osteoarthritis <- factor(x$baseline_osteoarthritis)
  
  x <- x[which(x$baseline_osteoarthritis == 0), ]
  return(x)
}

# hypertension
identify_prev_cases_list[[27]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$hypertension_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_hypertension <- NA
  x$baseline_hypertension[baseline_ncd] <- 1
  x$baseline_hypertension[which(x$hypertension_baseline_dx == 1)] <- 1
  x$baseline_hypertension[which(x$high_blood_pressure_diagnosis == "Yes")] <- 1
  x$baseline_hypertension[which(x$blood_pressure_meds == "Yes")] <- 1
  # x$baseline_hypertension[which((x$systolic_bp/x$diastolic_bp) >= 140/90)] <- 1
  x$baseline_hypertension[which(is.na(x$baseline_hypertension))] <- 0
  x$baseline_hypertension <- factor(x$baseline_hypertension)
  x$baseline_hypertension <- relevel(x$baseline_hypertension, ref = "0")
  
  return(x)
}

# create prevalent obesity variable
identify_prev_cases_list[[28]] <- function(x) {
  
  x$prev_obesity <- ifelse(x$BMI >= 30, 1, 0)
  x$prev_obesity <- factor(x$prev_obesity, ordered = FALSE)
  x$prev_obesity <- relevel(x$prev_obesity, ref = "0")
  
  return(x)
}


# create prevalent dyslipidemia variable
identify_prev_cases_list[[29]] <- function(x) {
  
  # define all participants with NCD diagnosis before or equal to recruitment date
  baseline_ncd <- which(x$dyslipidemia_censor_date <= x$recruitment_date)
  
  # creating health indicator for baseline disease in total population
  x$baseline_dyslipidemia <- NA
  x$baseline_dyslipidemia[baseline_ncd] <- 1
  x$baseline_dyslipidemia[which(x$cholesterol_meds == "Yes")] <- 1
  x$baseline_dyslipidemia[which(x$cholesterol >= (240 / 38.67))] <- 1
  x$baseline_dyslipidemia[which(x$LDL_direct >= (160 / 38.67))] <- 1
  x$baseline_dyslipidemia[which(x$HDL_cholesterol < (40 / 38.67))] <- 1
  x$baseline_dyslipidemia[which(x$triglycerides >= (200 / 88.57))] <- 1
  x$baseline_dyslipidemia[which(is.na(x$baseline_dyslipidemia))] <- 0
  x$baseline_dyslipidemia <- factor(x$baseline_dyslipidemia)
  x$baseline_dyslipidemia <- relevel(x$baseline_dyslipidemia, ref = "0")
  
  return(x)
}


