####Libraries####
library(easypackages)
libs<-c("tidyverse", "ggplot2", "car", "psych", "haven", "naniar", "plm")
libraries(libs)
####Import the Data####
dat.orig.4 <- read_sav("Data/s7991_UPenn_LongPollW4_FINAL_WEIGHTED_client_7.11.2019.sav")
dat.orig.5 <- read_sav("Data/s7991_UPenn_LongPollW5_prelim_3.23.2020_client.sav")

# Add longitudinal indicator
dat.orig.4$wave_W4 <- 1
dat.orig.5$wave_W5 <- 1

# Append wave 5 data with _W5 (unnecessary with final data)
cols.5<-colnames(dat.orig.5)
cols.5<-c(cols.5[1], paste(cols.5[-1], "_W5", sep=""))
colnames(dat.orig.5)<-cols.5

####Merge Datasets####
dat <- dat.orig.4 %>%
  full_join(dat.orig.5, by='CaseId')

####Cleaning####
#Set up values for NA
na.values.2<-c(77, 98, 99)
na.values.3<- c(777, 998, 999)
#Get indices for Therms
therms<-grep("THERM", colnames(dat), fixed=T)
#vector of columns with 2-digit NAs
clean.length2<-colnames(dat[c(-1,-therms)])
#vector of columns with 3-digit NAs
clean.length3<- colnames(dat[therms])
#Remove NAs
dat<- dat %>%
  replace_with_na_at(.vars=clean.length2, condition = ~.x %in% na.values.2)
dat <-dat %>%
  replace_with_na_at(.vars=clean.length3, condition = ~.x %in% na.values.3)

####Variable Manipulations and Recodes####

# Demographic Dummies

##Wave 4
dat$male_4 <- ifelse(dat$GENDER_W4==1, 1, 0)
dat$white_4 <- ifelse(dat$RACETHNICITY_W4==1, 1, 0)

##Wave 5
dat$male_5 <- ifelse(dat$GENDER_W4==1, 1, 0)
dat$white_5 <- ifelse(dat$RACETHNICITY_W4==1, 1, 0)

# Dummies for SM use

##dummy for social media platform use in wave 4
dat$fb.use.dummy_4<-ifelse(dat$SOCIAL1_1_W4>1, 1, 0)
dat$yt.use.dummy_4<-ifelse(dat$SOCIAL1_2_W4>1, 1, 0)
dat$tw.use.dummy_4<-ifelse(dat$SOCIAL1_3_W4>1, 1, 0)
dat$ig.use.dummy_4<-ifelse(dat$SOCIAL1_4_W4>1, 1, 0) 

##dummy for social media platform use in wave 5
dat$fb.use.dummy_5<-ifelse(dat$SOCIAL1_1_W4>1, 1, 0)
dat$yt.use.dummy_5<-ifelse(dat$SOCIAL1_2_W4>1, 1, 0)
dat$tw.use.dummy_5<-ifelse(dat$SOCIAL1_3_W4>1, 1, 0)
dat$ig.use.dummy_5<-ifelse(dat$SOCIAL1_4_W4>1, 1, 0) 

# Longitudinal SM use (0=never; 1=about once a week or less; 2=few times weekly; 3=daily; 4= many times daily)

## Wave 4
dat$fb.use5_4 <- car::recode(dat$SOCIAL1_1_W4, "1=0; 2=1; 3=1; 4=2; 5=3; 6=4")
dat$yt.use5_4 <- car::recode(dat$SOCIAL1_2_W4, "1=0; 2=1; 3=1; 4=2; 5=3; 6=4")
dat$tw.use5_4 <- car::recode(dat$SOCIAL1_3_W4, "1=0; 2=1; 3=1; 4=2; 5=3; 6=4")
dat$ig.use5_4 <- car::recode(dat$SOCIAL1_4_W4, "1=0; 2=1; 3=1; 4=2; 5=3; 6=4")

## Wave 5
dat$fb.use5_5 <- car::recode(dat$SOCIAL1_1_W5, "1=0; 2=1; 3=1; 4=1; 5=2; 6=3; 7=4")
dat$yt.use5_5 <- car::recode(dat$SOCIAL1_2_W5, "1=0; 2=1; 3=1; 4=1; 5=2; 6=3; 7=4")
dat$tw.use5_5 <- car::recode(dat$SOCIAL1_3_W5, "1=0; 2=1; 3=1; 4=1; 5=2; 6=3; 7=4")
dat$ig.use5_5 <- car::recode(dat$SOCIAL1_4_W5, "1=0; 2=1; 3=1; 4=1; 5=2; 6=3; 7=4")

# Party recodes and dummies

#Wave 4 Dummies
dat$dem_4 <- ifelse(dat$P_PARTYID7_W4<=3, 1, 0)
dat$rep_4 <- ifelse(dat$P_PARTYID7_W4>=5, 1, 0)
dat$ind_4 <- ifelse(dat$P_PARTYID7_W4==4, 1, 0)

#Wave 5 Dummies
dat$dem_5 <- ifelse(dat$P_PARTYID7_W5<=3, 1, 0)
dat$rep_5<- ifelse(dat$P_PARTYID7_W5>=5, 1, 0)
dat$ind_5 <- ifelse(dat$P_PARTYID7_W5==4, 1, 0)

##Party3 (-1=dems, 0=Independents (no leaners), 1=reps)
dat$party3_4 <- ifelse(dat$dem_4==1, -1,
                       ifelse(dat$rep_4==1, 1,
                              ifelse(dat$ind_4==1, 0, NA)))

dat$party3_5 <- ifelse(dat$dem_5==1, -1,
                       ifelse(dat$rep_5==1, 1,
                              ifelse(dat$ind_5==1, 0, NA)))

# Affective Polarization (wave 5 only)
dat$therm.rep_5<-NA
dat$therm.dem_5<-NA
for (x in 1:nrow(dat)){
  if(is.na(dat$RND_03_W5[x])) {
    dat$therm.rep_5[x]<-NA 
    dat$therm.dem_5[x]<-NA
  } else if(dat$RND_03_W5[x]==0) {
      dat$therm.rep_5[x]<-dat$THERM3_W5[x] 
      dat$therm.dem_5[x]<-dat$THERM4_W5[x]
  } else if (dat$RND_03_W5[x]==1) {
        dat$therm.rep_5[x]<-dat$THERM4_W5[x] 
        dat$therm.dem_5[x]<-dat$THERM3_W5[x]}
}

dat <- dat %>%
  mutate(aff.pol_5=abs(therm.rep_5-therm.dem_5))

# Perceived Polarization

## Wave 4
dat$ppol1_4 <- dat$PP1_W4-1
dat$ppol2_4 <- dat$PP2_W4-1
dat$ppol3_4 <- 4-dat$PP3_W4
dat$ppol4_4 <- 4-dat$PP4_W4

###Index and reliability
index.ppol_4 <- which(colnames(dat) %in% c("ppol1_4", "ppol2_4", "ppol3_4", "ppol4_4"))
alpha.ppol_4<-psych::alpha(dat[,index.ppol_4])
dat$ppol.ind_4 <- rowMeans(dat[,index.ppol_4])

## Wave 5
dat$ppol1_5 <- dat$PP1_W5-1
dat$ppol2_5 <- dat$PP2_W5-1
dat$ppol3_5 <- 4-dat$PP3_W5
dat$ppol4_5 <- 4-dat$PP4_W5

###Index and reliability
index.ppol_5 <- which(colnames(dat) %in% c("ppol1_5", "ppol2_5", "ppol3_5", "ppol4_5"))
alpha.ppol_5<-psych::alpha(dat[,index.ppol_5])
dat$ppol.ind_5 <- rowMeans(dat[,index.ppol_5])

####Quick analyses####
dat.p <- dat %>%
  select(CaseId, ppol.ind_4, ppol.ind_5, fb.use5_4, fb.use5_5, yt.use5_4, yt.use5_5, tw.use5_4, tw.use5_5,
         ig.use5_4, ig.use5_5) %>%
  pivot_longer(-CaseId, names_to = c(".value", "wave"), names_sep = "_")

model.q1<-plm(formula=ppol.ind~fb.use5, data=dat.p, model='within', index='CaseId', type='individual')
model.q2<-plm(formula=ppol.ind~yt.use5, data=dat.p, model='within', index='CaseId', type='individual')
model.q3<-plm(formula=ppol.ind~tw.use5, data=dat.p, model='within', index='CaseId', type='individual')
model.q4<-plm(formula=ppol.ind~ig.use5, data=dat.p, model='within', index='CaseId', type='individual')

lm.q1 <- lm(aff.pol_5~fb.use5_5+AGE7_W5+party3_5+male_5+white_5+INCOME_W5+EDUC4_W5, data=dat)
lm.q2 <- lm(aff.pol_5~yt.use5_5+AGE7_W5+party3_5+male_5+white_5+INCOME_W5+EDUC4_W5, data=dat)
lm.q3 <- lm(aff.pol_5~tw.use5_5+AGE7_W5+party3_5+male_5+white_5+INCOME_W5+EDUC4_W5, data=dat)
lm.q4 <- lm(aff.pol_5~ig.use5_5+AGE7_W5+party3_5+male_5+white_5+INCOME_W5+EDUC4_W5, data=dat)



