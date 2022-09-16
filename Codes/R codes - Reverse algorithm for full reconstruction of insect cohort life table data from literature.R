#install.packages("devRate")
library(readxl)
library(devRate)
###################################################################################################################################
## Estimation of oviposition for a female cohort
# model used (fecundity = a + bT + bT^2) by # actual fecudity
## (1)...Estimating Oviposition model parameters for Spodoptera frugiperda..........
# Temperatures and mean fecundity (from literature) used in the estimation of model parameters
temp_faw<-c(18, 26, 32)
mean_ovi_faw <- c(335.28, 484.3, 235) 

# Estimating Parameters 
one_faw<-c(1,temp_faw[1],(temp_faw[1]^2))
two_faw<-c(1,temp_faw[2],(temp_faw[2]^2))
three_faw<-c(1,temp_faw[3],(temp_faw[3]^2))
LHS_faw<-c(mean_ovi_faw[1],  mean_ovi_faw[2],  mean_ovi_faw[3]) 
RHS_faw <-rbind(c(one_faw),c(two_faw),c(three_faw))
parms_faw <-solve(RHS_faw,LHS_faw)
b1_faw<-parms_faw[1]; b2_faw<-parms_faw[2]; b3_faw<-parms_faw[3] # Estimated of model parameters

# Determining the mean eggs laid by cohort of females at specified temperatures
faw_ovip <- read_excel("D:/Eric/Manuscripts/Lifetable reconstruction/life table recostruction data.xlsx", sheet = "oviposition faw"); attach(faw_ovip) # from experimental study
temp_const_faw <- faw_ovip$temp # vector of specified temperature)
estimated_ovip_faw <-b1_faw+(b2_faw*temp_const_faw)+(b3_faw*(temp_const_faw^2))
faw_ovip$actual_fecundity_faw # actual fecundity
estimated_ovip_faw # estimated fecundity

# comparison with the published or actual
t.test(faw_ovip$actual_fecundity_faw, estimated_ovip_faw, alternative = "two.sided", paired=TRUE)

## (2)...Estimating Oviposition model parameters for Chilo partellus..........
# Temperatures and mean fecundity (from literature) used in the estimation of model parameters
temp_chilo<-c(18,	25,	35)
mean_ovi_chilo <- c(205, 375.3, 46) 

# Estimating oviposition model Parameters for cohort (using model proposed by Deevey (1947)) 
one_chilo<-c(1,temp_chilo[1],(temp_chilo[1]^2))
two_chilo<-c(1,temp_chilo[2],(temp_chilo[2]^2))
three_chilo<-c(1,temp_chilo[3],(temp_chilo[3]^2))
LHS_chilo<-c(mean_ovi_chilo[1],  mean_ovi_chilo[2],  mean_ovi_chilo[2]) 
RHS_chilo <-rbind(c(one_chilo),c(two_chilo),c(three_chilo))
parms_chilo <-solve(RHS_chilo,LHS_chilo)
b1_chilo<-parms_chilo[1]; b2_chilo<-parms_chilo[2]; b3_chilo<-parms_chilo[3] # Estimated of model parameters

# Determining the mean eggs laid by females cohort at specified temperatures using the resulting model
chilo_ovip <- read_excel("D:/Eric/Manuscripts/Lifetable reconstruction/life table recostruction data.xlsx", sheet = "oviposition chilo"); attach(chilo_ovip) # from experimental study
temp_const_chilo <- chilo_ovip$temp # vector of specified temperature
estimated_ovip_chilo <-b1_chilo+(b2_chilo*temp_const_chilo)+(b3_chilo*(temp_const_chilo^2)) 
chilo_ovip$actual_fecundity_chilo # actual fecundity
estimated_ovip_chilo # estimated fecundity

# comparison of actual and estimates of fecundity
t.test(actual_fecundity_chilo, estimated_ovip_chilo, alternative = "two.sided", paired=TRUE)


## (3)...Estimating Oviposition model parameters for Busseola fusca..........
# Temperatures and mean fecundity (from literature) used in the estimation of model parameters
temp <- c(15,	25,	30) 
mean_ovi <- c(102.75, 252.21,	155.50) 

# Estimating oviposition model Parameters for cohort (using model proposed by Deevey (1947)) 
one<-c(1,temp[1],(temp[1]^2))
two<-c(1,temp[2],(temp[2]^2))
three<-c(1,temp[3],(temp[3]^2))
LHS<-c(mean_ovi[1],  mean_ovi[2], mean_ovi[3]) 
RHS <-rbind(c(one),c(two),c(three))
parms <-solve(RHS,LHS) 
b1<-parms[1]; b2<-parms[2]; b3<-parms[3] # parameters estimates for the model

# Determining the mean eggs laid by females cohort at specified temperatures using the resulting model
fusca_ovip <- read_excel("D:/Eric/Manuscripts/Lifetable reconstruction/life table recostruction data.xlsx", sheet = "oviposition fusca") # data for model validation (actual life table data)
temp_const_fusca <- fusca_ovip$temp # vector of specified temperature
estimated_ovip_fusca <- b1+(b2*temp_const_fusca)+(b3*(temp_const_fusca^2))
fusca_ovip$actual_fecundity_fusca # actual fecundity
estimated_ovip_fusca # estimated fecundity

# comparison of actual and estimates of fecundity
t.test(fusca_ovip$actual_fecundity_fusca, estimated_ovip_fusca, alternative = "two.sided", paired=TRUE)


############################## MORTALITY RATES ###########################################################################

## (1) ............ Estimating mortality model parameters for Spodoptera frugiperda......................
#set 1: Estimating Parameters  ( MT = Exp( a + bT + cT^2))
faw_mort <- read_excel("D:/Eric/Manuscripts/Lifetable reconstruction/life table recostruction data.xlsx", sheet = "mortality faw"); attach(faw_mort) 
faw_mort$stage <- factor(stage); str(faw_mort)

temp_mort_faw<-c(15, 17, 18, 20, 22, 25, 26, 27, 28, 30, 32, 35, 38)
faw_mort_eggs <- faw_mort[stage == "egg", ]   
faw_mort_larva <- faw_mort[stage == "larva", ]   
faw_mort_pupa <- faw_mort[stage == "pupa", ]

MORT_E_faw_ln <- log(faw_mort_eggs$published_to_estimate) 
MORT_larva_faw_ln <- log(faw_mort_larva$published_to_estimate) 
MORT_pupa_faw_ln <- log(faw_mort_pupa$published_to_estimate) 

# i) Estimating parameters for mortality-eggs 
LHS_egg_faw  <- c(MORT_E_faw_ln[1], MORT_E_faw_ln[10], MORT_E_faw_ln[12])
one_egg_faw  <- c(1, temp_mort_faw[1],(temp_mort_faw[1]^2))
two_egg_faw <- c(1,temp_mort_faw[8],(temp_mort_faw[8]^2))
three_egg_faw <- c(1,temp_mort_faw[12],(temp_mort_faw[12]^2))

RHS_egg_faw <-rbind(c(one_egg_faw), c(two_egg_faw), c(three_egg_faw))
parms_egg_faw <-solve(RHS_egg_faw, LHS_egg_faw)

b1_E_faw <-parms_egg_faw[1]
b2_E_faw <-parms_egg_faw[2]
b3_E_faw <-parms_egg_faw[3]
b1_E_faw; b2_E_faw; b3_E_faw;

# ii) Estimating parameters for mortality-larva 
LHS_larva_faw <- c(MORT_larva_faw_ln[1], MORT_larva_faw_ln[8], MORT_larva_faw_ln[11])
one_larva_faw <- c(1, temp_mort_faw[1], (temp_mort_faw[1]^2))
two_larva_faw <- c(1, temp_mort_faw[9], (temp_mort_faw[9]^2))
three_larva_faw <- c(1, temp_mort_faw[12], (temp_mort_faw[12]^2))
RHS_larva_faw <-rbind(c(one_larva_faw), c(two_larva_faw),c(three_larva_faw))

parms_larva_faw <-solve(RHS_larva_faw, LHS_larva_faw)
b1_larva_faw <-parms_larva_faw[1] 
b2_larva_faw <-parms_larva_faw[2]
b3_larva_faw <-parms_larva_faw[3]
b1_larva_faw;  b2_larva_faw; b3_larva_faw 

# iii) Estimating parameters for mortality-pupa 
LHS_pupa_faw <- c(MORT_pupa_faw_ln[4], MORT_pupa_faw_ln[6], MORT_pupa_faw_ln[10])
one_pupa_faw <- c(1, temp_mort_faw[4], (temp_mort_faw[4]^2))
two_pupa_faw <- c(1, temp_mort_faw[6], (temp_mort_faw[6]^2))
three_pupa_faw <- c(1, temp_mort_faw[11], (temp_mort_faw[11]^2))
RHS_pupa_faw <-rbind(c(one_pupa_faw), c(two_pupa_faw),c(three_pupa_faw))

parms_pupa_faw <-solve(RHS_pupa_faw, LHS_pupa_faw)
b1_pupa_faw <-parms_pupa_faw[1] 
b2_pupa_faw <-parms_pupa_faw[2]
b3_pupa_faw <-parms_pupa_faw[3]
parms_pupa_faw

#set 2: Comparison between estimated and actual mortality rates
# 1. eggs 
temp_faw_MT <- c(20,25, 28, 30, 32) 
MT_eggs_faw_actual <- na.omit(faw_mort_eggs$actual_s); round(MT_eggs_faw_actual, digits = 4)
MT_eggs_faw_estimate <- exp(b1_E_faw +(b2_E_faw*temp_faw_MT)+(b3_E_faw*(temp_faw_MT^2))); round(MT_eggs_faw_estimate, digits = 4)
t.test(MT_eggs_faw_actual, MT_eggs_faw_estimate, alternative = "two.sided", paired=TRUE)

# 2: larva
MT_larva_faw_actual <- na.omit(faw_mort_larva$actual_s); round(MT_larva_faw_actual, digits = 4)
MT_larva_faw_estimate <- exp(b1_larva_faw +(b2_larva_faw*temp_faw_MT)+(b3_larva_faw*(temp_faw_MT^2))); round(MT_larva_faw_estimate, digits = 4)
t.test(MT_larva_faw_actual, MT_larva_faw_estimate, alternative = "two.sided", paired=TRUE)

# 3: pupa
MT_pupa_faw_actual <- na.omit(faw_mort_pupa$actual_s); round(MT_pupa_faw_actual, digits = 4)
MT_pupa_faw_estimate <- exp(b1_pupa_faw +(b2_pupa_faw*temp_faw_MT)+(b3_pupa_faw*(temp_faw_MT^2))); round(MT_pupa_faw_estimate, digits = 4)
t.test(MT_pupa_faw_actual, MT_pupa_faw_estimate, alternative = "two.sided", paired=TRUE)

## (2)............ Estimating Oviposition model parameters for Chilo partellus ......................
chilo_mort <- read_excel("D:/Eric/Manuscripts/Lifetable reconstruction/life table recostruction data.xlsx", sheet = "mortality chilo"); attach(chilo_mort)
chilo_mort$stage <- factor(stage); str(chilo_mort)
chilo_mort$published<- as.numeric(published); str(chilo_mort)

temp_mort_chilo <-c(15, 17, 18, 20,22, 25, 27, 28, 30, 32, 35, 38)
chilo_mort_larva <- chilo_mort[stage == "larva", ]   
chilo_mort_pupa <- chilo_mort[stage == "pupa", ]

MORT_larva_chilo_ln <- log(chilo_mort_larva$published) 
MORT_pupa_chilo_ln <- log(chilo_mort_pupa$published) 

# i) Estimating parameters for mortality-larva 
LHS_larva_chilo  <- c(MORT_larva_chilo_ln[3], MORT_larva_chilo_ln[7], MORT_larva_chilo_ln[12])
one_larva_chilo  <- c(1, temp_mort_chilo[3],(temp_mort_chilo[3]^2))
two_larva_chilo <- c(1,temp_mort_chilo[7],(temp_mort_chilo[7]^2))
three_larva_chilo <- c(1,temp_mort_chilo[12],(temp_mort_chilo[12]^2))

RHS_larva_chilo <-rbind(c(one_larva_chilo), c(two_larva_chilo), c(three_larva_chilo))
parms_larva_chilo <-solve(RHS_larva_chilo, LHS_larva_chilo)

b1_larva_chilo <-parms_larva_chilo[1]
b2_larva_chilo <-parms_larva_chilo[2]
b3_larva_chilo <-parms_larva_chilo[3]

# ii) Estimating parameters for mortality-pupa 
LHS_pupa_chilo  <- c(MORT_pupa_chilo_ln[3], MORT_pupa_chilo_ln[8], MORT_pupa_chilo_ln[10])
one_pupa_chilo  <- c(1, temp_mort_chilo[3],(temp_mort_chilo[3]^2))
two_pupa_chilo <- c(1,temp_mort_chilo[8],(temp_mort_chilo[8]^2))
three_pupa_chilo <- c(1,temp_mort_chilo[10],(temp_mort_chilo[10]^2))

RHS_pupa_chilo <-rbind(c(one_pupa_chilo), c(two_pupa_chilo), c(three_pupa_chilo))
parms_pupa_chilo <-solve(RHS_pupa_chilo, LHS_pupa_chilo)

b1_pupa_chilo <-parms_pupa_chilo[1]
b2_pupa_chilo <-parms_pupa_chilo[2]
b3_pupa_chilo <-parms_pupa_chilo[3]


# Comparison with the published or actual 
# 1. larva
temp_chilo_MT <- c(18, 20, 25, 30,32,35)
MT_larva_chilo_actual <- na.omit(chilo_mort_larva$actual_s_or); round(MT_larva_chilo_actual, digits = 4)

MT_larva_chilo_estimate <- exp(b1_larva_chilo +(b2_larva_chilo*temp_chilo_MT)+(b3_larva_chilo*(temp_chilo_MT^2))); round(MT_larva_chilo_estimate, digits = 4)
t.test(MT_larva_chilo_actual, MT_larva_chilo_estimate, alternative = "two.sided", paired=TRUE)

# 1. pupa
temp_chilo_MT <- c(15, 18, 20, 25, 30, 32, 35) 
MT_pupa_chilo_actual <- na.omit(chilo_mort_pupa$actual_s_or); round(MT_pupa_chilo_actual, digits = 4)

MT_pupa_chilo_estimate <- exp(b1_pupa_chilo +(b2_pupa_chilo*temp_chilo_MT)+(b3_pupa_chilo*(temp_chilo_MT^2))); round(MT_pupa_chilo_estimate, digits = 4)
t.test(MT_pupa_chilo_actual, MT_pupa_chilo_estimate, alternative = "two.sided", paired=TRUE)


## (c)............ Estimating mortality model parameters for fusca ......................
fusca_mort <- read_excel("D:/Eric/Manuscripts/Lifetable reconstruction/life table recostruction data.xlsx", sheet = "mortality fusca"); attach(fusca_mort)
fusca_mort$stage <- factor(stage); str(fusca_mort)
temp_mort_fusca<-c(15, 18, 20, 25, 26, 28, 30, 32, 35, 38)

fusca_mort_larva <- fusca_mort[stage == "larva", ]   
fusca_mort_pupa <- fusca_mort[stage == "pupa", ]

MORT_larva_fusca_ln <- log(fusca_mort_larva$published) 
MORT_pupa_fusca_ln <- log(fusca_mort_pupa$published) 

# i) Estimating parameters for mortality-larva 
LHS_larva_fusca <- c(MORT_larva_fusca_ln[1], MORT_larva_fusca_ln[5], MORT_larva_fusca_ln[7])
one_larva_fusca <- c(1, temp_mort_fusca[1], (temp_mort_fusca[1]^2))
two_larva_fusca <- c(1, temp_mort_fusca[5], (temp_mort_fusca[5]^2))
three_larva_fusca <- c(1, temp_mort_fusca[7], (temp_mort_fusca[7]^2))
RHS_larva_fusca <-rbind(c(one_larva_fusca), c(two_larva_fusca),c(three_larva_fusca))

parms_larva_fusca <-solve(RHS_larva_fusca, LHS_larva_fusca)
b1_larva_fusca <-parms_larva_fusca[1] 
b2_larva_fusca <-parms_larva_fusca[2]
b3_larva_fusca <-parms_larva_fusca[3]


# ii) Estimating parameters for mortality-pupa 
LHS_pupa_fusca <- c(MORT_pupa_fusca_ln[1], MORT_pupa_fusca_ln[4], MORT_pupa_fusca_ln[6])
one_pupa_fusca <- c(1, temp_mort_fusca[1], (temp_mort_fusca[1]^2))
two_pupa_fusca <- c(1, temp_mort_fusca[4], (temp_mort_fusca[4]^2))
three_pupa_fusca <- c(1, temp_mort_fusca[7], (temp_mort_fusca[7]^2))
RHS_pupa_fusca <-rbind(c(one_pupa_fusca), c(two_pupa_fusca),c(three_pupa_fusca))

parms_pupa_fusca <-solve(RHS_pupa_fusca, LHS_pupa_fusca)
b1_pupa_fusca <-parms_pupa_fusca[1] 
b2_pupa_fusca <-parms_pupa_fusca[2]
b3_pupa_fusca <-parms_pupa_fusca[3]

#set 2: Comparison with the published or actual 
# 1: larva
temp_fusca_MT <- c(15, 18, 20, 25, 30) 
MT_larva_fusca_actual <- na.omit(fusca_mort_larva$actual_s); round(MT_larva_fusca_actual, digits = 4)
MT_larva_fusca_estimate <- exp(b1_larva_fusca +(b2_larva_fusca*temp_fusca_MT)+(b3_larva_fusca*(temp_fusca_MT^2))); round(MT_larva_fusca_estimate, digits = 4)
t.test(MT_larva_fusca_actual, MT_larva_fusca_estimate, alternative = "two.sided", paired=TRUE)

# 2: pupa
MT_pupa_fusca_actual <- na.omit(fusca_mort_pupa$actual_s); round(MT_pupa_fusca_actual, digits = 4)
MT_pupa_fusca_estimate <- exp(b1_pupa_fusca +(b2_pupa_fusca*temp_fusca_MT)+(b3_pupa_fusca*(temp_fusca_MT^2))); round(MT_pupa_fusca_estimate, digits = 4)
t.test(MT_pupa_fusca_actual, MT_pupa_fusca_estimate, alternative = "two.sided", paired=TRUE)


###################################################################################################################################

## (a)............ Estimating development time ......................

#1.Zone of linearity
#S.frugiperda
#Egg
faw_egg_temp <- matrix(c(22,1,26,1), nrow=2, byrow = 2)# published values
faw_egg_DR_lit <- matrix(c((1/4),(1/3)))
faw_egg_coeff <- solve(faw_egg_temp,faw_egg_DR_lit)

faw_egg_coeff1 <- faw_egg_coeff[1,1]
faw_egg_coeff1 <- round(faw_egg_coeff1, digits=4)
faw_egg_coeff2 <- faw_egg_coeff[2,1]
faw_egg_coeff2 <- round(faw_egg_coeff2,digits = 4)

faw_egg_x <- c(20,22,24,25,26)
faw_egg_DR <- faw_egg_coeff1*faw_egg_x+faw_egg_coeff2
faw_egg_DR <- round(faw_egg_DR, digits = 4)
# faw_egg_DT <- round((1/faw_egg_DR),digits = 2)# faw development times

faw_egg_DR_r <- c(0.1567,faw_egg_DR[1],faw_egg_DR[2],faw_egg_DR[3],faw_egg_DR[4],faw_egg_DR[5],0.5,0.5)# Reconstructed faw egg development rates
faw_etemp_r <- c(18,20,22,24,25,26,30,32) #Reconstructed faw egg development rates temperatures

faw_egg<- data.frame(
  temp = faw_etemp_r, 
  devRate = (faw_egg_DR_r))

#Larvae
faw_larvae_temp <- matrix(c(22,1,26,1), nrow=2, byrow = 2)#published values
faw_larvae_DR_lit <- matrix(c((1/20.58),(1/14.86)))
faw_larvae_coeff <- solve(faw_larvae_temp,faw_larvae_DR_lit)

faw_larvae_coeff1 <- faw_larvae_coeff[1,1]
faw_larvae_coeff1 <- round(faw_larvae_coeff1, digits=4)
faw_larvae_coeff2 <- faw_larvae_coeff[2,1]
faw_larvae_coeff2 <- round(faw_larvae_coeff2, digits=4)

faw_larvae_x <- c(20,21,22,23,24,25,26)
faw_larvae_DR <- faw_larvae_coeff1*faw_larvae_x+faw_larvae_coeff2
faw_larvae_DR <- round(faw_larvae_DR, digits=4)
#faw_larvae_DT <- round((1/faw_larvae_DR),digits = 2)# Development times

faw_larvae_DR_r <- c(0.0291,faw_larvae_DR[1],faw_larvae_DR[2],faw_larvae_DR[3],faw_larvae_DR[4],faw_larvae_DR[5],faw_larvae_DR[6],faw_larvae_DR[7],0.0879,0.0957)#,e_dt[4],e_dt[5],5.9)
faw_ltemp_r <- c(18,20,21,22,23,24,25,26,30,32)

faw_larvae<- data.frame(
  temp = faw_ltemp_r, 
  devRate = (faw_larvae_DR_r))

#Pupae
faw_pupa_temp <- matrix(c(22,1,26,1), nrow=2, byrow = 2)#published values
faw_pupa_DR_lit <- matrix(c((1/17.06),(1/11.43)))
faw_pupa_coeff <- solve(faw_pupa_temp,faw_pupa_DR_lit)

faw_pupa_coeff1 <- faw_pupa_coeff[1,1]
faw_pupa_coeff1 <- round(faw_pupa_coeff1, digits=4)
faw_pupa_coeff2 <- faw_pupa_coeff[2,1]
faw_pupa_coeff2 <- round(faw_pupa_coeff2, digits=4)

faw_pupa_x <- c(20,21,22,23,24,25,26)
faw_pupa_DR <- faw_pupa_coeff1*faw_pupa_x+faw_pupa_coeff2
faw_pupa_DR <- round(faw_pupa_DR, digits = 4)
#faw_pupa_DT <- round((1/faw_pupa_DR),digits = 2)#Development time

faw_pupa_DR_r <- c(0.0326,faw_pupa_DR[1],faw_pupa_DR[2],faw_pupa_DR[3],faw_pupa_DR[4],faw_pupa_DR[5],faw_pupa_DR[6],faw_pupa_DR[7],0.1111,0.1550,0.1279)
faw_ptemp_r <- c(18,20,21,22,23,24,25,26,30,31,32)

faw_pupa<- data.frame(
  temp = faw_ptemp_r, 
  devRate = (faw_pupa_DR_r))

# C. partellus
#Larvae
chilo_larvae_temp <- matrix(c(20,1,25,1), nrow=2, byrow = 2)#published values
chilo_larvae_DR_lit <- matrix(c((1/57.4),(1/33.2)))
chilo_larvae_coeff <- solve(chilo_larvae_temp,chilo_larvae_DR_lit)

chilo_larvae_coeff1 <- chilo_larvae_coeff[1,1]
chilo_larvae_coeff1 <- round(chilo_larvae_coeff1, digits = 4)
chilo_larvae_coeff2 <- chilo_larvae_coeff[2,1]
chilo_larvae_coeff2 <- round(chilo_larvae_coeff2, digits = 4)

chilo_larvae_x <- c(18,20,22,24,25,26)
chilo_larvae_DR <- chilo_larvae_coeff1*chilo_larvae_x+chilo_larvae_coeff2
chilo_larvae_DR <- round(chilo_larvae_DR, digits = 4)
#chilo_larvae_DT <- round((1/chilo_larvae_DR),digits = 2)#Development times

chilo_larvae_DR_r<- c(chilo_larvae_DR[1],chilo_larvae_DR[2],chilo_larvae_DR[3],chilo_larvae_DR[4],chilo_larvae_DR[5],chilo_larvae_DR[6],0.0412,0.0450)#,e_dt[4],e_dt[5],5.9)
chilo_ltemp_r <- c(18,20,22,24,25,26,28,30)#,27,30)

chilo_larvae <- data.frame(
  temp = chilo_ltemp_r, 
  devRate = (chilo_larvae_DR_r))

#Pupae
chilo_pupa_temp <- matrix(c(20,1,25,1), nrow=2, byrow = 2)#published values
chilo_pupae_DR_lit <- matrix(c((1/16.7),(1/9.3)))
chilo_pupa_coeff <- solve(chilo_pupa_temp,chilo_pupae_DR_lit)

chilo_pupa_coeff1 <- chilo_pupa_coeff[1,1]
chilo_pupa_coeff1 <- round(chilo_pupa_coeff1, digits = 4)
chilo_pupa_coeff2 <- chilo_pupa_coeff[2,1]
chilo_pupa_coeff2 <- round(chilo_pupa_coeff2, digits = 4)

chilo_pupa_x <- c(18,20,22,24,25,26)
chilo_pupa_DR <- chilo_pupa_coeff1*chilo_pupa_x+chilo_pupa_coeff2
chilo_pupa_DR <- round(chilo_pupa_DR, digits=4)
#chilo_pupa_DT <- round((1/chilo_pupa_DR),digits = 2) #Development times

chilo_pupa_DR_r <- c(chilo_pupa_DR[1],chilo_pupa_DR[2],chilo_pupa_DR[3],chilo_pupa_DR[4],chilo_pupa_DR[5],chilo_pupa_DR[6],0.1099,0.1449)
chilo_ptemp_r <- c(18,20,22,24,25,26,28,30)

chilo_pupa <- data.frame(
  temp = chilo_ptemp_r, 
  devRate = (chilo_pupa_DR_r))

#B.fusca
#Larvae
fusca_larvae_temp <- matrix(c(15,1,25,1), nrow=2, byrow = 2)#published values
fusca_larvae_DR_lit <- matrix(c((1/90.2),(1/48.9)))
fusca_larvae_coeff <- solve(fusca_larvae_temp,fusca_larvae_DR_lit)

fusca_larvae_coeff1 <- fusca_larvae_coeff[1,1]
fusca_larvae_coeff1 <- round(fusca_larvae_coeff1,digits = 4)
fusca_larvae_coeff2 <- fusca_larvae_coeff[2,1]
fusca_larvae_coeff2<- round(fusca_larvae_coeff2,digits = 4)

fusca_larvae_x <- c(18,20,22,24,25,26)
fusca_larvae_DR <- fusca_larvae_coeff1*fusca_larvae_x+fusca_larvae_coeff2
fusca_larvae_DR <- round(fusca_larvae_DR, digits = 4)
#fusca_larvae_DT <- round((1/fusca_larvae_DR),digits = 2) Development times

fusca_larvae_DR_r <- c(0.0111,fusca_larvae_DR[1],fusca_larvae_DR[2],fusca_larvae_DR[3],fusca_larvae_DR[4],fusca_larvae_DR[5],fusca_larvae_DR[6],0.0299)#,e_dt[4],e_dt[5],5.9)
fusca_ltemp_r <- c(15,18,20,22,24,25,26,30)#,27,30)

fusca_larvae <- data.frame(
  temp = fusca_ltemp_r, 
  devRate = (fusca_larvae_DR_r))

#Pupae
fusca_pupa_temp <- matrix(c(15,1,25,1), nrow=2, byrow = 2)#published values
fusca_pupa_DR_lit <- matrix(c((1/40),(1/14.79)))
fusca_pupa_coeff <- solve(fusca_pupa_temp,fusca_pupa_DR_lit)

fusca_pupa_coeff1 <- fusca_pupa_coeff[1,1]
fusca_pupa_coeff1 <- round(fusca_pupa_coeff1, digits = 4)
fusca_pupa_coeff2 <- fusca_pupa_coeff[2,1]
fusca_pupa_coeff2 <- round(fusca_pupa_coeff2, digits = 4)

fusca_pupa_x <- c(18,20,22,24,25,26)
fusca_pupa_DR <- fusca_pupa_coeff1*fusca_pupa_x+fusca_pupa_coeff2
fusca_pupa_DR <- round(fusca_pupa_DR, digits = 4)
#fusca_pupa_DT <- round((1/fusca_pupa_DR),digits = 2)#Development times

fusca_pupa_DR_r <- c(0.025,fusca_pupa_DR[1],fusca_pupa_DR[2],fusca_pupa_DR[3],fusca_pupa_DR[4],fusca_pupa_DR[5],fusca_pupa_DR[6],0.0752)#,e_dt[4],e_dt[5],5.9)
fusca_ptemp_r <- c(15,18,20,22,24,25,26,30)#,27,30)

fusca_pupa <- data.frame(
  temp = fusca_ptemp_r, 
  devRate = (fusca_pupa_DR_r))


#2. Development Rate at Higher Temperatures
set.seed(1234)
faw_egg_logan6_76 <- devRateModel(eq = logan6_76, 
                                  dfData = faw_egg,
                                  startValues = list(phi = 0, deltaT = 1,Tmax = 40, bb = 0),algo  = "LM", control = list(maxiter = 1000))

faw_larvae_briere2_99 <- devRateModel(eq = briere2_99,
                                      dfData = faw_larvae,
                                      startValues = list(aa = 0, Tmin = 8, Tmax = 40, bb = 3),
                                      algo  = "LM",
                                      control = list(maxiter = 500))


faw_pupa_logan6_76_t2 <- devRateModel(eq = logan6_76, 
                                      dfData = faw_pupa,
                                      startValues = list(phi = 0, deltaT = 0.5,Tmax = 39, bb = 1),
                                      algo  = "LM", control = list(maxiter = 1000))

chilo_larvae_logan6_76 <- devRateModel(eq = logan6_76, 
                                       dfData = chilo_larvae,
                                       startValues = list(phi = 0, deltaT = 2,Tmax = 40, bb = 0.001),
                                       algo  = "LM", control = list(maxiter = 1000))

chilo_pupa_briere2_99 <- devRateModel(eq = briere2_99, 
                                      dfData = chilo_pupa,
                                      startValues = list(aa = 0, Tmin = 12, Tmax = 32, bb = 1),
                                      algo  = "LM",
                                      control = list(maxiter = 1000))

fusca_larvae_logan6_76 <- devRateModel(eq = logan6_76, 
                                       dfData = fusca_larvae,
                                       startValues = list(phi = 0, deltaT = 5,Tmax = 35, bb = 1),
                                       algo  = "LM", control = list(maxiter = 1000))

fusca_pupa_briere2_99 <- devRateModel(eq = briere2_99, 
                                      dfData = fusca_pupa,
                                      startValues = list(aa = 0.01, Tmin = 12, Tmax = 35, bb = 2),
                                      algo  = "LM",control = list(maxiter = 500))


###################################################################################################################################

# Oviposition 
#install.packages("Runuran")
library(Runuran)

# summary of dataset: 
estimated_ovip_faw # fecundity means estimated at temperatures: 20, 25, 30 and 32 respectively
# if 100 females results in aching stage then
females_20 = 100; mean_20 = estimated_ovip_faw[1]; total_20 = mean_20*females_20
females_25 = 100; mean_25 = estimated_ovip_faw[2]; total_25 = mean_25*females_25
females_30 = 100; mean_30 = estimated_ovip_faw[4]; total_30 = mean_30*females_30
females_32 = 100; mean_32 = estimated_ovip_faw[5]; total_32 = mean_32*females_32



# step (1): DISTRIBUTION OF TOTAL EGGS AMONG THE NO. OF INSECTS ATE EACH TEMPERATURE
#.............. Normal distribution parameters................................ 
# mean = mean @ each temp; std = from literature, lower bound(lb) & upper bound(lb) from literature corresponding to the std used
mean_20; std_20 <- 57.1 *sqrt(29); lb_20 <- 233;  ub_20 <- 1674
mean_25; std_25 <- 65.2 *sqrt(25); lb_25 <- 72;  ub_25 <- 1438
mean_30; std_30 <-  51.4 *sqrt(32); lb_30 <- 96;  ub_30 <- 1198
mean_32; std_32 <- 56.8 *sqrt(8); lb_32 <- 39;  ub_32 <- 584

#...function to distribute the total eggs among each individual, using normal distribution...
rand_no_gen <- function(no_of_females, mean, sd=s, lb, ub) {
  vector <- urnorm(no_of_females, mean/no_of_females, sd, lb, ub)
  vector / sum(vector) * mean}

#.............................temp 20........................................
set.seed(33)
indiv_ovip_20 <- rand_no_gen(females_20, total_20, std_20, lb_20, ub_20); 
indiv_ovip_20_r <- round(indiv_ovip_20, digit=0); indiv_ovip_20_r
sum(indiv_ovip_20_r)

#.............................temp 25.........................................
set.seed(25)
indiv_ovip_25 <- rand_no_gen(females_25, total_25, std_25, lb_25, ub_25); 
indiv_ovip_25_r <- round(indiv_ovip_25, 0);indiv_ovip_25_r
sum(indiv_ovip_25_r)

#.............................temp 30.........................................
set.seed(27)
indiv_ovip_30 <- rand_no_gen(females_30, total_30, std_30, lb_30, ub_30); 
indiv_ovip_30_r <- round(indiv_ovip_30, 0)
sum(indiv_ovip_30_r)

#.............................temp 32.........................................
set.seed(21)
indiv_ovip_32 <- round(rand_no_gen(females_32, total_32, std_32, lb_32, ub_32), digit=0); 
indiv_ovip_32_r <- round(indiv_ovip_32, 0)
sum(indiv_ovip_32_r)

#.. Outcome of the distribution of egg per individual in at each temperature
ovi_distr <- seq(max(length(indiv_ovip_18_r), length(indiv_ovip_20_r), length(indiv_ovip_25_r), length(indiv_ovip_30_r), length(indiv_ovip_32_r)))

ovi_distr_df <- data.frame(indiv_ovip_18_r[ovi_distr], 
                           indiv_ovip_20_r[ovi_distr], 
                           indiv_ovip_25_r[ovi_distr], 
                           indiv_ovip_30_r[ovi_distr], 
                           indiv_ovip_32_r[ovi_distr]) 

colnames(ovi_distr_df) <- c('indiv_pest_ovip_18','indiv_pest_ovip_20','indiv_pest_ovip_25', 'indiv_pest_ovip_30', 'indiv_pest_ovip_32')
ovi_distr_df

# step (2): DISTRIBUTION OF TOTAL EGGS/INDIVIDUAL PEST ACROSS OVIPOSITION PERIOD AT EACH TEMPERATURE
#.............. Gamma distribution parameters................................  
# Distribution use justified by Prasad, T., Srinivasa Rao, M., Rao, K., Bal, S., Muttapa, Y., Choudhary, J., & Singh, V. (2022)
# parameters to determine cumulative oviposition rate (shape =  9.043 +- 4.41, scale = 13.79+-2.19 # 
alpha = 4.633 # min of shape parameter 

# oviposition periods # obtained from literature
oviposition_period_t20 = 15 
oviposition_period_t25 = 13 
oviposition_period_t30 = 11 
oviposition_period_t32 = 9 

#.............................temp 20.........................................
set.seed(201)
t20_pgamma <- seq(1, oviposition_period_t20, by = 1)
cum_ovip_rate20 <- pgamma(t20_pgamma, shape = alpha) 
plot(cum_ovip_rate20)
A20 <- matrix(replicate(1, cum_ovip_rate20),ncol=1)
B20 <- matrix(replicate(1,indiv_ovip_20_r), nrow=1) # 8 but 16 
table_20 <- t(A20%*%B20)
table_20_A <- round(table_20, digits =0)

table_20_AA <- data.frame(table_20_A)
table_20_AA3 <- table_20_AA[,2:15] - table_20_AA[,1:14]
table_20_final_lf <- cbind(table_20_AA$X1,table_20_AA3)
colnames(table_20_final_lf) <- c("d1","d2","d3","d4","d5","d6","d7","d8","d9","d10","d11","d12","d13","d14","d15")
table_20_final_lf
sum(table_20_final_lf)
options(max.print=10000)
table_20_final_lf
sum(table_18_final_lf)

#.............................temp 25.........................................
set.seed(251)
t25_pgamma <- seq(1, oviposition_period_t25, by = 1)
cum_ovip_rate25 <- pgamma(t25_pgamma, shape = alpha) 
cum_ovip_rate25_adj <- cum_ovip_rate25 * 1/cum_ovip_rate25[oviposition_period_t25]
plot(cum_ovip_rate25_adj)
A25 <- matrix(replicate(1, cum_ovip_rate25_adj),ncol=1)
B25 <- matrix(replicate(1,indiv_ovip_25_r), nrow=1) # 8 but 16 
table_25 <- t(A25%*%B25)
table_25_A <- round(table_25, digits =0)
table_25_AA <- data.frame(table_25_A)

table_25_AA3 = table_25_AA[,2:oviposition_period_t25] - table_25_AA[, 1:(oviposition_period_t25-1)]
table_25_final_lf <- cbind(table_25_AA$X1, table_25_AA3)
colnames(table_25_final_lf) <- c("d1","d2","d3","d4","d5","d6","d7","d8","d9","d10","d11","d12","d13")
table_25_final_lf
sum(table_25_final_lf)

#.............................temp 30.........................................
set.seed(301)
t30_pgamma <- seq(1, oviposition_period_t30, by = 1)
cum_ovip_rate30 <- pgamma(t30_pgamma, shape = alpha) 
plot(cum_ovip_rate30)
cum_ovip_rate30_adj <- cum_ovip_rate30 * 1/cum_ovip_rate30[oviposition_period_t30]
A30 <- matrix(replicate(1, cum_ovip_rate30_adj),ncol=1)
B30 <- matrix(replicate(1,indiv_ovip_30_r), nrow=1) # 
table_30 <- t(A30%*%B30)
table_30_A <- round(table_30, digits = 0)
table_30_AA <- data.frame(table_30_A)
table_30_AA3 <- table_30_AA[,2:11] - table_30_AA[,1:10]
table_30_final_lf <- cbind(table_30_AA$X1,table_30_AA3)
colnames(table_30_final_lf) <- c("d1","d2","d3","d4","d5","d6","d7","d8","d9","d10","d11")
table_30_final_lf
sum(table_30_final_lf)

#.............................temp 32.........................................
set.seed(321)
t32_pgamma <- seq(1, oviposition_period_t32, by = 1)
cum_ovip_rate32 <- pgamma(t32_pgamma, shape = alpha) 
plot(cum_ovip_rate32)
cum_ovip_rate32_adj <- cum_ovip_rate32 * 1/cum_ovip_rate32[oviposition_period_t32]
A32 <- matrix(replicate(1, cum_ovip_rate32_adj), ncol=1)
B32 <- matrix(replicate(1,indiv_ovip_32_r), nrow=1) # 8 but 16 

table_32 <- t(A32%*%B32)
table_32_A <- round(table_32, digits =0)


table_32_AA <- data.frame(table_32_A)
table_32_AA3 <- table_32_AA[,2:9] - table_32_AA[,1:8]

table_32_final_lf <- cbind(table_32_AA$X1,table_32_AA3)
colnames(table_32_final_lf) <- c("d1","d2","d3","d4","d5","d6","d7","d8","d9")
table_32_final_lf
sum(table_32_final_lf)

#Summary Tables 
ovi_distr_df

table_18_final_lf
table_20_final_lf
table_25_final_lf
table_30_final_lf
table_32_final_lf

