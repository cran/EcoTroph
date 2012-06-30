create.ETdiagnosis <- function (ET_Main, Mul_eff = NULL, Beta = NULL, TopD = NULL, FormD = NULL, TLpred = NULL) 
{
if (is.null(Mul_eff)) 
Mul_eff <- c(0, 0.2, 0.4, 0.7, 1, 1.5, 2, 2.5, 3, 4, 5)
if (is.null(Beta)) 
Beta <- 0.1
if (is.null(TopD)) 
TopD <- 0.2
if (is.null(FormD)) 
FormD <- 0.5
if (is.null(TLpred)) 
TLpred <- 3.5

TL_out <- as.numeric(rownames(ET_Main))

#Initialization
  
TEMP_PB <- array(dim = c(length(TL_out), length(Mul_eff)), dimnames = list(TL_out, Mul_eff))
for (compteur in 1:length(Mul_eff)) TEMP_PB[, compteur] <- ET_Main[, "Fish_mort"]
TEMP_PB <- ET_Main[, "Kin"] - ET_Main[, "Fish_mort"] + sweep(TEMP_PB, 2, Mul_eff, FUN = "*")

TEMP_PB_acc <- array(dim = c(length(TL_out), length(Mul_eff)), dimnames = list(TL_out, Mul_eff))
for (compteur in 1:length(Mul_eff)) TEMP_PB_acc[, compteur] <- ET_Main[, "Fish_mort_acc"]
TEMP_PB_acc <- ET_Main[, "Kin_acc"] - ET_Main[, "Fish_mort_acc"] + sweep(TEMP_PB_acc, 2, Mul_eff, FUN = "*")
  
FLOW_MF <- array(dim = c(length(TL_out), length(Mul_eff)), dimnames = list(TL_out, Mul_eff))
for (compteur in 1:length(Mul_eff)) FLOW_MF[, compteur] <- ET_Main[, "FL_P"]
  
FLOW_MF_acc <- array(dim = c(length(TL_out), length(Mul_eff)), dimnames = list(TL_out, Mul_eff))
for (compteur in 1:length(Mul_eff)) FLOW_MF_acc[, compteur] <- ET_Main[, "FL_P_acc"]

BIOM_MF <- array(dim = c(length(TL_out), length(Mul_eff)), dimnames = list(TL_out, Mul_eff))
for (compteur in 1:length(Mul_eff)) BIOM_MF[, compteur] <- ET_Main[, "B"]
  
BIOM_MF_acc <- array(dim = c(length(TL_out), length(Mul_eff)), dimnames = list(TL_out, Mul_eff))
for (compteur in 1:length(Mul_eff)) BIOM_MF_acc[, compteur] <- ET_Main[, "B_acc"]
  
PB_MF <- TEMP_PB
PB_MF_acc <- TEMP_PB_acc

SC <- 3
SC2 <- 3

#Iteration
 
while (SC2 != 0) {

BIOM_MF <- FLOW_MF/PB_MF
BIOM_MF <- BIOM_MF[-1, ] ##sum of BIOM_MF for TL>=2 in the biomass input control equation

BIOM_MF_acc <- FLOW_MF_acc/PB_MF_acc

if (ET_Main[1, "B_acc"]==0){
cas <- 1
FLOW_MF_TMP <- FLOW_MF[1, ]

FLOW_MF[1, ] <- (1 - Beta) * ET_Main[1, "FL_P"] + Beta * ET_Main[1, "FL_P"] * (apply(BIOM_MF[, ], 2, sum)/sum(BIOM_MF[, "1"]))
for (compteur in 2:length(rownames(FLOW_MF))) {
for (compteur2 in 1:length(colnames(FLOW_MF))) {        
FLOW_MF[compteur, compteur2] <- FLOW_MF[(compteur - 1), compteur2] * exp(-(ET_Main[(compteur - 1), "N_loss"] + as.numeric(colnames(FLOW_MF))[compteur2] * ET_Main[compteur - 1, "Fish_mort"]/PB_MF[compteur - 1, compteur2]) * (as.numeric(rownames(FLOW_MF)[compteur]) - as.numeric(rownames(FLOW_MF)[compteur - 1])))
}
}

FLOW_MF_acc[2, ] <- ET_Main[2, "FL_P_acc"] * FLOW_MF[2,]/FLOW_MF[2,"1"]
for (compteur in 3:length(rownames(FLOW_MF))) {
for (compteur2 in 1:length(colnames(FLOW_MF))) {
FLOW_MF_acc[compteur, compteur2] <- FLOW_MF_acc[(compteur - 1), compteur2] * exp(-(ET_Main[(compteur - 1), "N_loss_acc"] + as.numeric(colnames(FLOW_MF_acc))[compteur2] * ET_Main[compteur - 1, "Fish_mort"]/PB_MF_acc[compteur - 1, compteur2]/ET_Main[(compteur - 1), "Selec"]) * (as.numeric(rownames(FLOW_MF_acc)[compteur]) - as.numeric(rownames(FLOW_MF_acc)[compteur - 1])))
}}
}

else {
cas <- 2   
FLOW_MF_TMP <- FLOW_MF[1, ]

FLOW_MF[1, ] <- (1 - Beta) * ET_Main[1, "FL_P"] + Beta * ET_Main[1, "FL_P"] * (apply(BIOM_MF[, ], 2, sum)/sum(BIOM_MF[, "1"]))
FLOW_MF_acc[1, ] <- ET_Main[1, "FL_P_acc"] * FLOW_MF[1, ]/FLOW_MF[1,"1"]
for (compteur in 2:length(rownames(FLOW_MF))) {
for (compteur2 in 1:length(colnames(FLOW_MF))) {
FLOW_MF[compteur, compteur2] <- FLOW_MF[(compteur - 1), compteur2] * exp(-(ET_Main[(compteur - 1), "N_loss"] + as.numeric(colnames(FLOW_MF))[compteur2] * ET_Main[compteur - 1, "Fish_mort"]/PB_MF[compteur - 1, compteur2]) * (as.numeric(rownames(FLOW_MF)[compteur]) - as.numeric(rownames(FLOW_MF)[compteur - 1])))
FLOW_MF_acc[compteur, compteur2] <- FLOW_MF_acc[(compteur - 1), compteur2] * exp(-(ET_Main[(compteur - 1), "N_loss_acc"] + as.numeric(colnames(FLOW_MF_acc))[compteur2] * ET_Main[compteur - 1, "Fish_mort"]/PB_MF_acc[compteur - 1, compteur2]/ET_Main[(compteur - 1), "Selec"]) * (as.numeric(rownames(FLOW_MF_acc)[compteur]) - as.numeric(rownames(FLOW_MF_acc)[compteur - 1])))
}}
}

BIOM_MF <- FLOW_MF/PB_MF
BIOM_MF_acc <- FLOW_MF_acc/PB_MF_acc

PB_MF[1, ] <- (ET_Main[1, "Kin"] - ET_Main[1, "Fish_mort"]) * (1 + TopD * (apply(BIOM_MF[2:5, ], 2, sum)^FormD - sum(BIOM_MF[2:5, "1"])^FormD)/(sum(BIOM_MF[2:5, "1"])^FormD)) + Mul_eff * ET_Main[1, "Fish_mort"]

if (cas==2){
PB_MF_acc[1, ] <- (ET_Main[1, "Kin_acc"] - ET_Main[1, "Fish_mort_acc"]) * (1 + TopD * (apply(BIOM_MF[2:5, ], 2, sum)^FormD - sum(BIOM_MF[2:5, "1"])^FormD)/(sum(BIOM_MF[2:5, "1"])^FormD)) + Mul_eff * ET_Main[1, "Fish_mort_acc"]
}

for (compteur in 2:38) {
PB_MF[compteur, ] <- (ET_Main[compteur, "Kin"] - ET_Main[compteur, "Fish_mort"]) * (1 + TopD * (apply(BIOM_MF[(compteur + 8):(compteur + 13), ], 2, sum)^FormD - sum(BIOM_MF[(compteur + 8):(compteur + 13), "1"])^FormD)/(sum(BIOM_MF[(compteur + 8):(compteur + 13), "1"])^FormD)) + Mul_eff * ET_Main[compteur, "Fish_mort"]
PB_MF_acc[compteur, ] <- (ET_Main[compteur, "Kin_acc"] - ET_Main[compteur, "Fish_mort_acc"]) * (1 + TopD * (apply(BIOM_MF[(compteur + 8):(compteur + 13), ], 2, sum)^FormD - sum(BIOM_MF[(compteur + 8):(compteur + 13), "1"])^FormD)/(sum(BIOM_MF[(compteur + 8):(compteur + 13), "1"])^FormD)) + Mul_eff * ET_Main[compteur, "Fish_mort_acc"]
}
    
for (compteur in 38:length(rownames(PB_MF))) {
for (compteur2 in 1:length(Mul_eff)) {
x <- as.numeric(rownames(PB_MF)[(compteur - 9):(compteur - 2)])
y <- log(PB_MF[(compteur - 9):(compteur - 2), compteur2])
reg <- coef(lm(y ~ x))
PB_MF[compteur, compteur2] <- exp(reg[1] + reg[2] * as.numeric(rownames(PB_MF)[compteur]))
y <- log(PB_MF_acc[(compteur - 9):(compteur - 2), compteur2])
reg <- coef(lm(y ~ x))
PB_MF_acc[compteur, compteur2] <- exp(reg[1] + reg[2] * as.numeric(rownames(PB_MF_acc)[compteur]))
}
}

SC <- round(sum(PB_MF)/sum(TEMP_PB), 4)
SC21 <- sum(FLOW_MF[1, ])/sum(FLOW_MF_TMP)
SC2 <- round((sum(FLOW_MF[1, ]) - sum(FLOW_MF_TMP)) * 1000)
cat("##")
TEMP_PB <- PB_MF
TEMP_PB_acc <- PB_MF_acc
}

#Indices calculation

TOT_biomass <- apply(BIOM_MF[-1, ], 2, sum) #without TL=1 
TOT_biomass_acc <- apply(BIOM_MF_acc[-1, ], 2, sum) #without TL=1
Predat_biom <- apply(BIOM_MF[(which(rownames(BIOM_MF)==TLpred)):(length(BIOM_MF[, 1])),], 2, sum) 
TOT_FL <- apply(FLOW_MF, 2, sum) 
TOT_FL_acc <- apply(FLOW_MF_acc, 2, sum) 
Predat_FL <- apply(FLOW_MF[(which(rownames(BIOM_MF)==TLpred)):(length(BIOM_MF[, 1])), ], 2, sum) 

if (cas==1){
Catches <- BIOM_MF_acc[-1,]
Catches <- sweep(BIOM_MF_acc[-1,], 1, ET_Main[-1, "Fish_mort_acc"], FUN = "*")
for (compteur in 1:length(Mul_eff)) {
Catches[, compteur] <- Catches[, compteur] * Mul_eff[compteur]
}
}

if (cas==2){
Catches <- BIOM_MF_acc
Catches <- sweep(BIOM_MF_acc, 1, ET_Main[, "Fish_mort_acc"], FUN = "*")
for (compteur in 1:length(Mul_eff)) {
Catches[, compteur] <- Catches[, compteur] * Mul_eff[compteur]
}
}

Y <- apply(Catches, 2, sum) 

if (cas==1){
Pred_Y <- apply(Catches[(which(rownames(Catches)==TLpred)):(length(BIOM_MF[-1, 1])), ], 2, sum)
}

if (cas==2){
Pred_Y <- apply(Catches[(which(rownames(Catches)==TLpred)):(length(BIOM_MF[, 1])), ], 2, sum)
}

R_TOT_biomass <- TOT_biomass/TOT_biomass["1"]
R_TOT_biomass_acc <- TOT_biomass_acc/TOT_biomass_acc["1"]
R_Predat_biom <- Predat_biom/Predat_biom["1"]
R_TOT_FL <- TOT_FL/TOT_FL["1"]
R_TOT_FL_acc <- TOT_FL_acc/TOT_FL_acc["1"]
R_Predat_FL <- Predat_FL/Predat_FL["1"]
R_Y <- Y/Y["1"]
R_Pred_Y <- Pred_Y/Pred_Y["1"] 
TL_TOT_biomass <- apply(sweep(BIOM_MF[-1, ], 1, TL_out[-1], FUN = "*"), 2, sum)/TOT_biomass
TL_TOT_biomass_acc <- apply(sweep(BIOM_MF_acc[-1, ], 1, TL_out[-1], FUN = "*"), 2, sum)/TOT_biomass_acc

if (cas==1){
TL_Catches <- apply(sweep(Catches, 1, TL_out[-1], FUN = "*"), 2, sum)/Y
}
if (cas==2){
TL_Catches <- apply(sweep(Catches[-1, ], 1, TL_out[-1], FUN = "*"), 2, sum)/Y
}

ET_Main_diagnose <- cbind(TOT_biomass, TOT_biomass_acc, Predat_biom, TOT_FL, TOT_FL_acc, Predat_FL, Y, Pred_Y, R_TOT_biomass, R_TOT_biomass_acc, R_Predat_biom, R_TOT_FL, R_TOT_FL_acc, R_Predat_FL, R_Y, R_Pred_Y, TL_TOT_biomass, TL_TOT_biomass_acc, TL_Catches)
return(list(ET_Main_diagnose = ET_Main_diagnose, BIOM_MF = BIOM_MF, BIOM_MF_acc = BIOM_MF_acc, Catches = Catches, FLOW_MF = FLOW_MF, FLOW_MF_acc = FLOW_MF_acc))
}
