

score2<-function(sex="D",age=56,smoke="N",sbp=120,dm="N",coltot=230,colhdl=51,risk_region="Low") {
  
  #1a PRUEBA: 
  #  sex=c("D")
  #  age=c(50)
  #  smoke=c("Yes")
  #  sbp=c(140)
  #  dm=c("No")
  #  coltot=c(243)
  #  colhdl=c(54)
  #  risk_region=c("Very high")
  
  # 2a PRUEBA:
  # sex=c("D", "H")
  # age=c(50, 82)
  # smoke=c("N","1")
  # sbp=c(130, 140)
  # dm=c("Yes", "Yes")
  # coltot=c(150, 156)
  # colhdl=c(45,47)
  # risk_region=c("Low", "Low")
  
  # Referencia: SCORE2 risk prediction alghorithms: 
  #new models to estimate 10-year risk of cardiovascular disease in Europe 
  #SCORE2 working group and ESC Cardiovascular risk collaboration 
  #Received 25 January 2021; revised 8 March 2021; editorial decision 4 May 2021; 
  #accepted 5 May 2021; online publish-ahead-of-print 13 June 2021
  #Supplementary methods Table 2: Model coefficients and baseline survival of the SCORE2 algotithm 
  #Supplementary methods Table 4: Illustration of risk estimation for a man or woman with
  #given risk factor values
  
  # Cambiar unidades del colesterol mg/dl a mmol/L 
  coltot = round(0.0259*coltot,4)
  colhdl = round(0.0259*colhdl,4) 
  
  smoke = as.character(smoke) 
  dm = as.character(dm)
  
  # Recode Female, Male 
  
  # 1. Transformation equation 
  cage = (age-60)/5       #Age years
  current = ifelse(smoke%in%c("Yes", "Si","Y","S","1"), 1,0) #current=1/Other=0
  csbp = (sbp-120)/20     #(SBP, mmHg)
  diabetes = ifelse(dm%in%c("Yes", "Y","Si","S","1"), 1,0) #Yes=1/No, other =0
  ctchol = (coltot-6)/1   #(Total cholesterol, mmol/L)
  chdl = (colhdl-1.3)/0.5 #(HDL cholesterol, mmol/L)
  #Interactions
  current_age = cage*current
  sbp_age = cage*csbp
  diabetes_age = cage*diabetes
  coltot_age = cage*ctchol
  colhdl_age = cage*chdl
  
  
  # 1.2 Transformation equation >75 
  # SCORE-OP 
  
  if (age>75) {
    cage =  age-73       #Age years
    current = ifelse(smoke%in%c("Yes", "Si","Y","S","1"), 1,0) #current=1/Other=0
    csbp = (sbp-150)     #(SBP, mmHg)
    diabetes = ifelse(dm%in%c("Yes", "Y","Si","S","1"), 1,0) #Yes=1/No, other =0
    ctchol = (coltot-6)/1   #(Total cholesterol, mmol/L)
    chdl = (colhdl-1.4) #(HDL cholesterol, mmol/L)
    #Interactions
    current_age = cage*current
    diabetes_age = cage*diabetes
    sbp_age = cage*csbp
    coltot_age = cage*ctchol
    colhdl_age = cage*chdl }
  
  # 2. Log HRx transformed value
  
  # Codes Males/ Females -->
  codMale <- c("H","Home","Man","Male","Hombre","Home","M")
  codFemale <- c("D","Dona","Woman","Female","Mujer","W","F")
  
  #For Male / Female-------------
  Age_years=               ifelse     (sex%in%codMale, 0.3742*cage,         ifelse(sex%in%codFemale, 0.4648*cage,NA))
  Smoking=                 ifelse     (sex%in%codMale, 0.6012*current,      ifelse(sex%in%codFemale, 0.7744*current,NA))
  Systolic_blood_pressure= ifelse     (sex%in%codMale, 0.2777*csbp,         ifelse(sex%in%codFemale, 0.3131*csbp,NA))
  Diabetes=                ifelse     (sex%in%codMale, 0.6457*diabetes,     ifelse(sex%in%codFemale, 0.8096*diabetes,NA))
  Total_cholesterol=       ifelse     (sex%in%codMale, 0.1458*ctchol,       ifelse(sex%in%codFemale, 0.1002*ctchol,NA))
  HDL_cholesterol=         ifelse     (sex%in%codMale, -0.2698*chdl,        ifelse(sex%in%codFemale, -0.2606*chdl,NA))
  Smoking_Age=             ifelse     (sex%in%codMale, -0.0755*current_age, ifelse(sex%in%codFemale, -0.1088*current_age,NA))
  SBP_Age=                 ifelse     (sex%in%codMale, -0.0255*sbp_age,     ifelse(sex%in%codFemale, -0.0277*sbp_age,NA))
  Total_Cholesterol_Age=   ifelse     (sex%in%codMale, -0.0281*coltot_age,  ifelse(sex%in%codFemale, -0.0226*coltot_age,NA))
  HDL_Cholesterol_Age=     ifelse     (sex%in%codMale, 0.0426*colhdl_age,   ifelse(sex%in%codFemale, 0.0613*colhdl_age,NA))
  DM_Age=                  ifelse     (sex%in%codMale, -0.0983*diabetes_age,ifelse(sex%in%codFemale, -0.1272*diabetes_age,NA))
  
  
  
  # SCORE-OP
  if (age>75) {
    
    Age_years=               ifelse     (sex%in%codMale, 0.0634*cage,         ifelse(sex%in%codFemale, 0.0789*cage,NA))
    Smoking=                 ifelse     (sex%in%codMale, 0.3524*current,      ifelse(sex%in%codFemale, 0.4921*current,NA))
    Systolic_blood_pressure= ifelse     (sex%in%codMale, 0.0094*csbp,         ifelse(sex%in%codFemale, 0.0102*csbp,NA))
    Diabetes=                ifelse     (sex%in%codMale, 0.4245*diabetes,     ifelse(sex%in%codFemale, 0.6010*diabetes,NA))
    Total_cholesterol=       ifelse     (sex%in%codMale, 0.0850*ctchol,       ifelse(sex%in%codFemale, 0.0605*ctchol,NA))
    HDL_cholesterol=         ifelse     (sex%in%codMale, -0.3564*chdl,        ifelse(sex%in%codFemale, -0.3040*chdl,NA))
    Smoking_Age=             ifelse     (sex%in%codMale, -0.0247*current_age, ifelse(sex%in%codFemale, -0.0255*current_age,NA))
    SBP_Age=                 ifelse     (sex%in%codMale, -0.0005*sbp_age,     ifelse(sex%in%codFemale, -0.0004*sbp_age,NA))
    Total_Cholesterol_Age=   ifelse     (sex%in%codMale, -0.0073*coltot_age,  ifelse(sex%in%codFemale, -0.0009*coltot_age,NA))
    HDL_Cholesterol_Age=     ifelse     (sex%in%codMale, 0.0091*colhdl_age,   ifelse(sex%in%codFemale,  0.0154*colhdl_age,NA))
    DM_Age=                  ifelse     (sex%in%codMale, -0.0174*diabetes_age,ifelse(sex%in%codFemale, -0.0107*diabetes_age,NA))
    
    
  }
  
  
  
  # Linear predictor
  linear_predictor=     rowSums(cbind(Age_years, Smoking, Systolic_blood_pressure, 
                                      Diabetes, Total_cholesterol, HDL_cholesterol, 
                                      Smoking_Age, SBP_Age, DM_Age, Total_Cholesterol_Age, 
                                      HDL_Cholesterol_Age))
  
  # 3. 10-year risk estimation (un-calibrated)
  basesurv=        ifelse(sex%in%codMale, 0.9605, ifelse(sex%in%codFemale,0.9776,NA))
  risk_estimation= round(1-basesurv^exp(linear_predictor),4)
  
  # 4. Calibration of risk estimate according to region specific scaling factors
  score <- NA
  
  score=round(ifelse (risk_region=="Low" & sex%in%codMale,
                      1-exp(-exp(-0.5699+0.7476*log(-log(1-risk_estimation)))),
                      ifelse (risk_region=="Low" & sex%in%codFemale,1-exp(-exp(-0.7380+0.7019*log(-log(1-risk_estimation)))),NA)),4)
  
  score=round(ifelse (risk_region=="Moderate" & sex%in%codMale,
                      1-exp(-exp(-0.1565+0.8009*log(-log(1-risk_estimation)))),
                      ifelse (risk_region=="Moderate" & sex%in%codFemale,1-exp(-exp(-0.3143+0.7701*log(-log(1-risk_estimation)))),score)),4)
  
  score= round(ifelse (risk_region=="High" & sex%in%codMale,
                       1-exp(-exp(0.3207+0.9360*log(-log(1-risk_estimation)))),
                       ifelse (risk_region=="High" & sex%in%codFemale,1-exp(-exp(0.5710+0.9369*log(-log(1-risk_estimation)))),score)),4)
  
  score= round(ifelse (risk_region=="Very high" & sex%in%codMale,
                       1-exp(-exp(0.5836+0.8294*log(-log(1-risk_estimation)))),
                       ifelse(risk_region=="Very high" & sex%in%codFemale,1-exp(-exp(0.9412+0.8329*log(-log(1-risk_estimation)))),score)),4)
  
  return(score*100)
  
}




regicor <- function(age, sex, smoker, diabetes, coltot, colhdl, sbp, dbp, divide = 1){
  n <- length(age)
  diabetes <- as.numeric(diabetes)
  bp_opti <- ifelse(sbp <  120 & dbp < 80, 1, 0)
  bp_high <- ifelse((130 <= sbp & sbp < 140) | (85 <= dbp & dbp < 90), 1, 0)
  bp_i <- ifelse((140 <= sbp & sbp < 160) | (90 <= dbp & dbp < 100), 1, 0)
  bp_ii <- ifelse(160 <= sbp | 100 <= dbp, 1, 0)
  i_bp_ii <- (bp_ii == 1)
  bp_opti[i_bp_ii] <- bp_high[i_bp_ii] <- bp_i[i_bp_ii] <- 0
  i_bp_i <- (bp_i == 1)
  bp_opti[i_bp_i] <- bp_high[i_bp_i] <- 0
  i_bp_high <- (bp_high == 1)
  bp_opti[i_bp_high] <- 0
  
  c_160 <- ifelse(coltot < 160, 1, 0)
  c200_239 <- ifelse(200 <= coltot & coltot < 240, 1, 0)
  c240_279 <- ifelse(240 <= coltot & coltot < 280, 1, 0)
  c280_ <- ifelse(280 <= coltot, 1, 0)
  h_35 <- ifelse(colhdl < 35, 1, 0)
  h35_44 <- ifelse(35 <= colhdl & colhdl < 45, 1, 0)
  h45_49 <- ifelse(45 <= colhdl & colhdl < 50, 1, 0)
  h50_59 <- ifelse(50 <= colhdl & colhdl < 60, 1, 0)
  h60_ <- ifelse(60 <= colhdl, 1, 0)
  
  men <- (sex == 'H')
  l_chol = rep(0, n)
  l_chol[men] <- (0.04826*age - 0.65945*c_160 + 0.17692*c200_239 + 0.50539*c240_279 +
                    0.65713*c280_ + 0.49744*h_35 + 0.24310*h35_44 - 0.05107*h50_59 - 0.48660*h60_ -
                    0.00226*bp_opti + 0.28320*bp_high + 0.52168*bp_i + 0.61859*bp_ii +
                    0.42839*diabetes + 0.52337*smoker)[men]
  l_chol[!men] <- (0.33766*age - 0.00268*(age^2) - 0.26138*c_160 + 0.20771*c200_239 +
                     0.24385*c240_279 + 0.53513*c280_ + 0.84312*h_35 + 0.377096*h35_44 +
                     0.19785*h45_49 - 0.42951*h60_ - 0.53363*bp_opti - 0.06773*bp_high +
                     0.26288*bp_i + 0.46573*bp_ii + 0.59626*diabetes + 0.29246*smoker)[!men]
  g_chol = rep(0, n)
  g_chol[men] <- 3.489
  g_chol[!men] = 10.279
  b_chol <- exp(l_chol - g_chol)
  result <- rep(0,n)
  result[men] <- (1 - (1 -(1 - 0.951)/divide)^b_chol[men])*100 
  result[!men] <- (1 - (1 - (1 - 0.978)/divide)^b_chol[!men])*100
  result
}







