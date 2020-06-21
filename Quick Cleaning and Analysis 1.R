####Libraries####
library(easypackages)
libs<-c("tidyverse", "ggplot2", "car", "psych", "haven", "naniar", "plm", "stargazer")
libraries(libs)
####Import the Data####
dat.orig.4 <- read_sav("Data/s7991_UPenn_LongPollW4_FINAL_WEIGHTED_client_7.11.2019.sav")
dat.orig.5 <- read_sav("Data/7991_UPenn_LongPoll W5_client_04.13.2020.sav")

# Add longitudinal indicator
dat.orig.4$wave_W4 <- 1
dat.orig.5$wave_W5 <- 1

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
dat$fb.use.dummy_5<-ifelse(dat$SOCIAL1_1_W5>1, 1, 0)
dat$yt.use.dummy_5<-ifelse(dat$SOCIAL1_2_W5>1, 1, 0)
dat$tw.use.dummy_5<-ifelse(dat$SOCIAL1_3_W5>1, 1, 0)
dat$ig.use.dummy_5<-ifelse(dat$SOCIAL1_4_W5>1, 1, 0) 

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

#Party Extremity
dat$party.x_4<-(car::recode(dat$P_PARTYID7_W4, "1=3; 7=3; 2=2; 6=2; 3=1; 5=1; 4=0; else=NA"))/3
dat$party.x_5<-(car::recode(dat$P_PARTYID7_W5, "1=3; 7=3; 2=2; 6=2; 3=1; 5=1; 4=0; else=NA"))/3

# Affective Polarization (wave 5 only); (0,1) 1= highly affectively polarized
dat$therm.rep_5<-NA
dat$therm.dem_5<-NA
for (x in 1:nrow(dat)){
  if(is.na(dat$RND_03[x])) {
    dat$therm.rep_5[x]<-NA 
    dat$therm.dem_5[x]<-NA
  } else if(dat$RND_03[x]==0) {
      dat$therm.rep_5[x]<-dat$THERM3_W5[x] 
      dat$therm.dem_5[x]<-dat$THERM4_W5[x]
  } else if (dat$RND_03[x]==1) {
        dat$therm.rep_5[x]<-dat$THERM4_W5[x] 
        dat$therm.dem_5[x]<-dat$THERM3_W5[x]}
}

dat <- dat %>%
  mutate(aff.pol_5=abs(therm.rep_5-therm.dem_5)/100)

# Perceived Polarization (0,1); 1=high perceived polarization

## Wave 4
dat$ppol1_4 <- dat$PP1_W4-1
dat$ppol2_4 <- dat$PP2_W4-1
dat$ppol3_4 <- 4-dat$PP3_W4
dat$ppol4_4 <- 4-dat$PP4_W4

###Index and reliability
index.ppol_4 <- which(colnames(dat) %in% c("ppol1_4", "ppol2_4", "ppol3_4", "ppol4_4"))
alpha.ppol_4<-psych::alpha(dat[,index.ppol_4])
dat$ppol.ind_4 <- rowMeans(dat[,index.ppol_4])/3

## Wave 5
dat$ppol1_5 <- dat$PP1_W5-1
dat$ppol2_5 <- dat$PP2_W5-1
dat$ppol3_5 <- 4-dat$PP3_W5
dat$ppol4_5 <- 4-dat$PP4_W5

###Index and reliability
index.ppol_5 <- which(colnames(dat) %in% c("ppol1_5", "ppol2_5", "ppol3_5", "ppol4_5"))
alpha.ppol_5<-psych::alpha(dat[,index.ppol_5])
dat$ppol.ind_5 <- rowMeans(dat[,index.ppol_5])/3

#Civility Index; (0,1), high=civil

dat$civil.1_4<-dat$CIVIL1_W4
dat$civil.2_4<-recode(dat$CIVIL2_W4, "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
dat$civil.3_4<-dat$CIVIL3_W4
dat$civil.4_4<-recode(dat$CIVIL4_W4, "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
dat$civil.5_4<-dat$CIVIL5_W4

dat$civil.1_5<-dat$CIVIL1_W5
dat$civil.2_5<-recode(dat$CIVIL2_W5, "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
dat$civil.3_5<-dat$CIVIL3_W5
dat$civil.4_5<-recode(dat$CIVIL4_W5, "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
dat$civil.5_5<-dat$CIVIL5_W5

##Create scale, standardized (0,1), and alpha

civil.index_4<-which(colnames(dat) %in% c("civil.1_4", "civil.2_4", "civil.3_4", "civil.4_4", "civil.5_4"))
dat$civil.ind_4 <- (rowMeans(dat[,civil.index_4])-1)/9
alpha.civil_4<-psych::alpha(dat[civil.index_4])

civil.index_5<-which(colnames(dat) %in% c("civil.1_5", "civil.2_5", "civil.3_5", "civil.4_5", "civil.5_5"))
dat$civil.ind_5 <- (rowMeans(dat[,civil.index_5])-1)/9
alpha.civil_5<-psych::alpha(dat[civil.index_5])

#Interest (0= no interest; 3=very interested)
dat$interest_4<-(4-dat$Q16_W4)/3
dat$interest_5 <- (4-dat$Q16_W5)/3

#Create measure of passive political info exposure per platform; [0,3] 3= high exposure
dat <- dat %>%
  mutate(fb.pass.exp_4=SOCIAL3_1_W4-1, yt.pass.exp_4=SOCIAL3_2_W4-1, tw.pass.exp_4=SOCIAL3_3_W4-1,
         ig.pass.exp_4=SOCIAL3_4_W4-1)

dat <- dat %>%
  mutate(fb.pass.exp_5=SOCIAL3_1_W5-1, yt.pass.exp_5=SOCIAL3_2_W5-1, tw.pass.exp_5=SOCIAL3_3_W5-1,
         ig.pass.exp_5=SOCIAL3_4_W5-1)

# Immigration Index (0=Anti-immigration; 4=pro-immigration)

dat$imm1_4<-recode(dat$IM1_W4, "5=1;4=2;3=3;2=4;1=5")
dat$imm2_4<-dat$IM2_W4
dat$imm3_4<-dat$IM3_W4

dat$imm1_5<-recode(dat$IM1_W5, "5=1;4=2;3=3;2=4;1=5")
dat$imm2_5<-dat$IM2_W5
dat$imm3_5<-dat$IM3_W5

##Collapsed into Scale and Standardized. Alpha reported

imm.index_4<-which(colnames(dat) %in% c("imm1_4", "imm2_4", "imm3_4"))
dat$imm.ind_4 <- rowMeans(dat[,imm.index_4])-1
alpha.imm_4<-psych::alpha(dat[imm.index_4])

imm.index_5<-which(colnames(dat) %in% c("imm1_5", "imm2_5", "imm3_5"))
dat$imm.ind_5 <- rowMeans(dat[,imm.index_5])-1
alpha.imm_5<-psych::alpha(dat[imm.index_5])

#Issue Polarization Scale; (0,1), 1=highly polarized, 0= middle of road/no polarization

dat$iss1_4<-recode(dat$GUNS_W4, "3=0;1=1;5=1;2=.5;4=.5")
dat$iss2_4<-recode(dat$TSN1_W4, "3=0;1=1;5=1;2=.5;4=.5")
dat$iss3_4<-recode(dat$TSN2_W4, "3=0;1=1;5=1;2=.5;4=.5")
dat$iss4_4<-recode(dat$RUS1_W4, "3=0;1=1;5=1;2=.5;4=.5")

dat$iss1_5<-recode(dat$GUNS_W5, "3=0;1=1;5=1;2=.5;4=.5")
dat$iss2_5<-recode(dat$TSN1_W5, "3=0;1=1;5=1;2=.5;4=.5")
dat$iss3_5<-recode(dat$TSN2_W5, "3=0;1=1;5=1;2=.5;4=.5")
dat$iss4_5<-recode(dat$RUS1_W5, "3=0;1=1;5=1;2=.5;4=.5")

##Transformation of Immigration index

dat <- dat %>%
  mutate(iss5_4=abs(imm.ind_4-2)/2, iss5_5=abs(imm.ind_5-2)/2)

##Index construction and alpha

iss.pol.index_4<-which(colnames(dat) %in% c("iss1_4", "iss2_4", "iss3_4", "iss4_4", "iss5_4"))
alpha.iss.pol_4<-psych::alpha(dat[iss.pol.index_4])
dat$iss.pol_4 <- rowMeans(dat[,iss.pol.index_4])

iss.pol.index_5<-which(colnames(dat) %in% c("iss1_5", "iss2_5", "iss3_5", "iss4_5", "iss5_5"))
alpha.iss.pol_5<-psych::alpha(dat[iss.pol.index_5])
dat$iss.pol_5 <- rowMeans(dat[,iss.pol.index_5])

#Extreme Ideology; (0,3) 3=extreme, 0=moderate

dat$ideo.x_4<-(recode(dat$P_IDEO_W4, "1=3;7=3;2=2;6=2;3=1;5=1;4=0;8=0;else=NA"))/3
dat$ideo.x_5<-(recode(dat$P_IDEO_W5, "1=3;7=3;2=2;6=2;3=1;5=1;4=0;8=0;else=NA"))/3

# Perceived Elite Polarization (0,1); 1=high perceived elite polarization
dat <- dat %>%
  mutate(imm.diff_5=abs(IM1A_W5-IM1B_W5)/4, tr.diff_5=abs(TR1A_W5-TR1B_W5)/4, 
         chi.diff_5=abs(CHI3_W5-CHI4_W5)/4, rs.diff_5=abs(RS1A_W5-RS1B_W5)/3,
         hc.diff_5=abs(HC5A_W5-HC5B_W5)/3, guns.diff_5=abs(GUNSREP_W5-GUNSDEM_W5)/4)

##Index and reliability
index.diff<-grep("\\.diff_5", colnames(dat))
alpha.diff<-psych::alpha(dat[,index.diff])
dat$elite.ppolar_5 <- rowMeans(dat[,index.diff])

# Combined Social Media Use Index (0,16); 16=high use
dat <- dat %>%
  mutate(sm.ind_4=fb.use5_4+ig.use5_4+tw.use5_4+yt.use5_4, sm.ind_5=fb.use5_5+ig.use5_5+tw.use5_5+yt.use5_5)

##Alphas
alpha.sm_4<-psych::alpha(dat[,grep("use5_4", colnames(dat))])
alpha.sm_5<-psych::alpha(dat[,grep("use5_5", colnames(dat))])

# Sum of number of social media platforms used
dat <- dat %>%
  mutate(platform.sum_4=fb.use.dummy_4+ig.use.dummy_4+tw.use.dummy_4+yt.use.dummy_4,
         platform.sum_5=fb.use.dummy_5+ig.use.dummy_5+tw.use.dummy_5+yt.use.dummy_5)

# Code non-users across the 4 platforms and rescale SM Use index to exclude non-users
dat$nonuse_4<-ifelse(dat$sm.ind_4==0, 1, 0)
dat$nonuse_5<-ifelse(dat$sm.ind_5==0, 1, 0)

## Recode SM use
dat$sm.ind.r_4<-ifelse(dat$sm.ind_4==0, NA, (dat$sm.ind_4-1)/15)
dat$sm.ind.r_5<-ifelse(dat$sm.ind_5==0, NA, (dat$sm.ind_5-1)/15)

# Rescale EDUC4 (0,1; 1=BA+)
dat$edu4_4<-(dat$EDUC4_W4-1)/3
dat$edu4_5<-(dat$EDUC4_W5-1)/3

#Rescale INCOME (0,1; 1=200k+)
dat$inc_4<-(dat$INCOME_W4-1)/17
dat$inc_5<-(dat$INCOME_W5-1)/17

# Rescale AGE7 (0,1; 1=75+)
dat$age7_4<-(dat$AGE7_W4-1)/6
dat$age7_5<-(dat$AGE7_W5-1)/6
####Detailed Analyses####
# Average each polarization by each platform

##Get the data 
fb.users.polar <- dat %>%
  select(fb.use.dummy_4, fb.use.dummy_5, yt.use.dummy_4, yt.use.dummy_5, tw.use.dummy_4, tw.use.dummy_5, 
         ig.use.dummy_4, ig.use.dummy_5, iss.pol_4, iss.pol_5, ppol.ind_4, ppol.ind_5, aff.pol_5, 
         elite.ppolar_5) %>%
  pivot_longer(everything(), names_to = c(".value", "Wave"), names_sep = "_") %>%
  group_by(fb.use.dummy, Wave) %>%
  summarise(mean.ppol=mean(ppol.ind, na.rm=T), mean.iss=mean(iss.pol, na.rm=T), 
            mean.aff=mean(aff.pol, na.rm=T), mean.elite=mean(elite.ppolar, na.rm=T),
            lwr.ppol=mean.ppol-(1.96*sd(ppol.ind, na.rm=T)/sqrt(n())), 
            upr.ppol=mean.ppol+(1.96*sd(ppol.ind, na.rm=T)/sqrt(n())), 
            lwr.iss=mean.iss-(1.96*sd(iss.pol, na.rm=T)/sqrt(n())), 
            upr.iss=mean.iss+(1.96*sd(iss.pol, na.rm=T)/sqrt(n())),
            lwr.aff=mean.aff-(1.96*sd(aff.pol, na.rm=T)/sqrt(n())), 
            upr.aff=mean.aff+(1.96*sd(aff.pol, na.rm=T)/sqrt(n())),
            lwr.elite=mean.elite-(1.96*sd(elite.ppolar, na.rm=T)/sqrt(n())), 
            upr.elite=mean.elite+(1.96*sd(elite.ppolar, na.rm=T)/sqrt(n())))
fb.users.polar$platform<-"Facebook"

yt.users.polar <- dat %>%
  select(fb.use.dummy_4, fb.use.dummy_5, yt.use.dummy_4, yt.use.dummy_5, tw.use.dummy_4, tw.use.dummy_5, 
         ig.use.dummy_4, ig.use.dummy_5, iss.pol_4, iss.pol_5, ppol.ind_4, ppol.ind_5, aff.pol_5, 
         elite.ppolar_5) %>%
  pivot_longer(everything(), names_to = c(".value", "Wave"), names_sep = "_") %>%
  group_by(yt.use.dummy, Wave) %>%
  summarise(mean.ppol=mean(ppol.ind, na.rm=T), mean.iss=mean(iss.pol, na.rm=T), 
            mean.aff=mean(aff.pol, na.rm=T), mean.elite=mean(elite.ppolar, na.rm=T),
            lwr.ppol=mean.ppol-(1.96*sd(ppol.ind, na.rm=T)/sqrt(n())), 
            upr.ppol=mean.ppol+(1.96*sd(ppol.ind, na.rm=T)/sqrt(n())), 
            lwr.iss=mean.iss-(1.96*sd(iss.pol, na.rm=T)/sqrt(n())), 
            upr.iss=mean.iss+(1.96*sd(iss.pol, na.rm=T)/sqrt(n())),
            lwr.aff=mean.aff-(1.96*sd(aff.pol, na.rm=T)/sqrt(n())), 
            upr.aff=mean.aff+(1.96*sd(aff.pol, na.rm=T)/sqrt(n())),
            lwr.elite=mean.elite-(1.96*sd(elite.ppolar, na.rm=T)/sqrt(n())), 
            upr.elite=mean.elite+(1.96*sd(elite.ppolar, na.rm=T)/sqrt(n())))
yt.users.polar$platform<-"YouTube"

ig.users.polar <- dat %>%
  select(fb.use.dummy_4, fb.use.dummy_5, yt.use.dummy_4, yt.use.dummy_5, tw.use.dummy_4, tw.use.dummy_5, 
         ig.use.dummy_4, ig.use.dummy_5, iss.pol_4, iss.pol_5, ppol.ind_4, ppol.ind_5, aff.pol_5, 
         elite.ppolar_5) %>%
  pivot_longer(everything(), names_to = c(".value", "Wave"), names_sep = "_") %>%
  group_by(ig.use.dummy, Wave) %>%
  summarise(mean.ppol=mean(ppol.ind, na.rm=T), mean.iss=mean(iss.pol, na.rm=T), 
            mean.aff=mean(aff.pol, na.rm=T), mean.elite=mean(elite.ppolar, na.rm=T),
            lwr.ppol=mean.ppol-(1.96*sd(ppol.ind, na.rm=T)/sqrt(n())), 
            upr.ppol=mean.ppol+(1.96*sd(ppol.ind, na.rm=T)/sqrt(n())), 
            lwr.iss=mean.iss-(1.96*sd(iss.pol, na.rm=T)/sqrt(n())), 
            upr.iss=mean.iss+(1.96*sd(iss.pol, na.rm=T)/sqrt(n())),
            lwr.aff=mean.aff-(1.96*sd(aff.pol, na.rm=T)/sqrt(n())), 
            upr.aff=mean.aff+(1.96*sd(aff.pol, na.rm=T)/sqrt(n())),
            lwr.elite=mean.elite-(1.96*sd(elite.ppolar, na.rm=T)/sqrt(n())), 
            upr.elite=mean.elite+(1.96*sd(elite.ppolar, na.rm=T)/sqrt(n())))
ig.users.polar$platform<-"Instagram"

tw.users.polar <- dat %>%
  select(fb.use.dummy_4, fb.use.dummy_5, yt.use.dummy_4, yt.use.dummy_5, tw.use.dummy_4, tw.use.dummy_5, 
         ig.use.dummy_4, ig.use.dummy_5, iss.pol_4, iss.pol_5, ppol.ind_4, ppol.ind_5, aff.pol_5, 
         elite.ppolar_5) %>%
  pivot_longer(everything(), names_to = c(".value", "Wave"), names_sep = "_") %>%
  group_by(tw.use.dummy, Wave) %>%
  summarise(mean.ppol=mean(ppol.ind, na.rm=T), mean.iss=mean(iss.pol, na.rm=T), 
            mean.aff=mean(aff.pol, na.rm=T), mean.elite=mean(elite.ppolar, na.rm=T),
            lwr.ppol=mean.ppol-(1.96*sd(ppol.ind, na.rm=T)/sqrt(n())), 
            upr.ppol=mean.ppol+(1.96*sd(ppol.ind, na.rm=T)/sqrt(n())), 
            lwr.iss=mean.iss-(1.96*sd(iss.pol, na.rm=T)/sqrt(n())), 
            upr.iss=mean.iss+(1.96*sd(iss.pol, na.rm=T)/sqrt(n())),
            lwr.aff=mean.aff-(1.96*sd(aff.pol, na.rm=T)/sqrt(n())), 
            upr.aff=mean.aff+(1.96*sd(aff.pol, na.rm=T)/sqrt(n())),
            lwr.elite=mean.elite-(1.96*sd(elite.ppolar, na.rm=T)/sqrt(n())), 
            upr.elite=mean.elite+(1.96*sd(elite.ppolar, na.rm=T)/sqrt(n())))
tw.users.polar$platform<-"Twitter"

dat.polar <- rbind(fb.users.polar[fb.users.polar$fb.use.dummy==1,-1], 
                   ig.users.polar[ig.users.polar$ig.use.dummy==1,-1],
                   tw.users.polar[tw.users.polar$tw.use.dummy==1,-1], 
                   yt.users.polar[yt.users.polar$yt.use.dummy==1,-1])
dat.polar <- dat.polar[dat.polar$platform %in% c("Facebook", "Instagram", "YouTube", "Twitter"),]


## Make the figure
fig.issue.pol.platform<-ggplot(aes(x=platform, group=Wave), data=dat.polar)+
  geom_point(aes(y=mean.iss, shape=Wave), position=position_dodge(width=.5))+
  geom_errorbar(aes(ymin=lwr.iss, ymax=upr.iss), position=position_dodge(width=.5), width=.3)+
  theme_bw()+
  #ggtitle("Average Issue Extremity among Users of Each Platform")+
  xlab("")+
  ylab("Issue Extremity (0,1)")+
  theme(plot.title = element_text(size=18, hjust=0.5))
  
fig.perc.pol.platform<-ggplot(aes(x=platform, group=Wave), data=dat.polar)+
  geom_point(aes(y=mean.ppol, shape=Wave), position=position_dodge(width=.5))+
  geom_errorbar(aes(ymin=lwr.ppol, ymax=upr.ppol), position=position_dodge(width=.5), width=.3)+
  theme_bw()+
  #ggtitle("Average Perceived Polarization among Users of Each Platform")+
  xlab("")+
  ylab("Perceived Polarization (0,1)")+
  theme(plot.title = element_text(size=18, hjust=0.5))  

fig.aff.pol.platform<-ggplot(aes(x=platform, group=Wave), data=dat.polar)+
  geom_point(aes(y=mean.aff, shape=Wave))+
  geom_errorbar(aes(ymin=lwr.aff, ymax=upr.aff), width=.3)+
  theme_bw()+
  #ggtitle("Average Affective Polarization among Users of Each Platform")+
  xlab("")+
  ylab("Affective Polarization (0,1)")+
  theme(plot.title = element_text(size=18, hjust=0.5))

fig.elite.pol.platform<-ggplot(aes(x=platform, group=Wave), data=dat.polar)+
  geom_point(aes(y=mean.elite, shape=Wave))+
  geom_errorbar(aes(ymin=lwr.elite, ymax=upr.elite), width=.3)+
  theme_bw()+
  #ggtitle("Average Perceived Elite Polarization among Users of Each Platform")+
  xlab("")+
  ylab("Perceived Elite Polarization (0,1)")+
  theme(plot.title = element_text(size=18, hjust=0.5))

fig.polar.comb<-ggpubr::ggarrange(fig.issue.pol.platform, fig.perc.pol.platform, fig.aff.pol.platform, 
                  fig.elite.pol.platform, legend='right', common.legend = T)

#Cross-sectional effects of social media use (wave 5)

## Issue Extremity
lm.iss.fb<-lm(iss.pol_5~fb.use5_5+fb.use.dummy_5+ig.use.dummy_5+tw.use.dummy_5+yt.use.dummy_5+white_5+
                EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)
lm.iss.ig<-lm(iss.pol_5~ig.use5_5+ig.use.dummy_5+fb.use.dummy_5+tw.use.dummy_5+yt.use.dummy_5+white_5+
                EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)
lm.iss.tw<-lm(iss.pol_5~tw.use5_5+tw.use.dummy_5+ig.use.dummy_5+fb.use.dummy_5+yt.use.dummy_5+white_5+
                EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)
lm.iss.yt<-lm(iss.pol_5~yt.use5_5+yt.use.dummy_5+ig.use.dummy_5+tw.use.dummy_5+fb.use.dummy_5+white_5+
                EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)

lm.iss.all1<-lm(iss.pol_5~sm.ind.r_5+white_5+edu4_5+inc_5+male_5+rep_5+dem_5+party.x_5+age7_5+interest_5, data=dat[dat$nonuse_5==0,])

lm.iss.all2<-lm(iss.pol_5~sm.ind_5+white_5+EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+yt.use.dummy_5+
                  ig.use.dummy_5+tw.use.dummy_5+fb.use.dummy_5+civil.ind_5+interest_5, data=dat)

lm.iss.platform<-lm(iss.pol_5~platform.sum_5+white_5+EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+interest_5, data=dat)

##Perceived Polarization
lm.ppol.fb<-lm(ppol.ind_5~fb.use5_5+fb.use.dummy_5+ig.use.dummy_5+tw.use.dummy_5+yt.use.dummy_5+white_5+
                EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)
lm.ppol.ig<-lm(ppol.ind_5~ig.use5_5+ig.use.dummy_5+fb.use.dummy_5+tw.use.dummy_5+yt.use.dummy_5+white_5+
                EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)
lm.ppol.tw<-lm(ppol.ind_5~tw.use5_5+tw.use.dummy_5+ig.use.dummy_5+fb.use.dummy_5+yt.use.dummy_5+white_5+
                EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)
lm.ppol.yt<-lm(ppol.ind_5~yt.use5_5+yt.use.dummy_5+ig.use.dummy_5+tw.use.dummy_5+fb.use.dummy_5+white_5+
                EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)

lm.ppol.all1<-lm(ppol.ind_5~sm.ind.r_5+white_5+edu4_5+inc_5+male_5+rep_5+dem_5+party.x_5+age7_5+interest_5, data=dat[dat$nonuse_5==0,])

lm.ppol.all2<-lm(ppol.ind_5~sm.ind_5+white_5+EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+yt.use.dummy_5+
                  ig.use.dummy_5+tw.use.dummy_5+fb.use.dummy_5+civil.ind_5+interest_5, data=dat)

lm.ppol.platform<-lm(ppol.ind_5~platform.sum_5+white_5+EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+interest_5, data=dat)


##Affective Polarization
lm.aff.fb<-lm(aff.pol_5~fb.use5_5+fb.use.dummy_5+ig.use.dummy_5+tw.use.dummy_5+yt.use.dummy_5+white_5+
                 EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)
lm.aff.ig<-lm(aff.pol_5~ig.use5_5+ig.use.dummy_5+fb.use.dummy_5+tw.use.dummy_5+yt.use.dummy_5+white_5+
                 EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)
lm.aff.tw<-lm(aff.pol_5~tw.use5_5+tw.use.dummy_5+ig.use.dummy_5+fb.use.dummy_5+yt.use.dummy_5+white_5+
                 EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)
lm.aff.yt<-lm(aff.pol_5~yt.use5_5+yt.use.dummy_5+ig.use.dummy_5+tw.use.dummy_5+fb.use.dummy_5+white_5+
                 EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)

lm.aff.all1<-lm(aff.pol_5~sm.ind.r_5+white_5+edu4_5+inc_5+male_5+rep_5+dem_5+party.x_5+age7_5+interest_5, data=dat[dat$nonuse_5==0,])

lm.aff.all2<-lm(aff.pol_5~sm.ind_5+white_5+EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+yt.use.dummy_5+
                   ig.use.dummy_5+tw.use.dummy_5+fb.use.dummy_5+civil.ind_5+interest_5, data=dat)

lm.aff.platform<-lm(aff.pol_5~platform.sum_5+white_5+EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+interest_5, data=dat)


##Perceived Elite Polarization
lm.elite.fb<-lm(elite.ppolar_5~fb.use5_5+fb.use.dummy_5+ig.use.dummy_5+tw.use.dummy_5+yt.use.dummy_5+white_5+
                EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)
lm.elite.ig<-lm(elite.ppolar_5~ig.use5_5+ig.use.dummy_5+fb.use.dummy_5+tw.use.dummy_5+yt.use.dummy_5+white_5+
                EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)
lm.elite.tw<-lm(elite.ppolar_5~tw.use5_5+tw.use.dummy_5+ig.use.dummy_5+fb.use.dummy_5+yt.use.dummy_5+white_5+
                EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)
lm.elite.yt<-lm(elite.ppolar_5~yt.use5_5+yt.use.dummy_5+ig.use.dummy_5+tw.use.dummy_5+fb.use.dummy_5+white_5+
                EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+civil.ind_5+interest_5, data=dat)

lm.elite.all1<-lm(elite.ppolar_5~sm.ind.r_5+white_5+edu4_5+inc_5+male_5+rep_5+dem_5+party.x_5+age7_5+interest_5, data=dat[dat$nonuse_5==0,])

lm.elite.all2<-lm(elite.ppolar_5~sm.ind_5+white_5+EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+yt.use.dummy_5+
                  ig.use.dummy_5+tw.use.dummy_5+fb.use.dummy_5+civil.ind_5+interest_5, data=dat)

lm.elite.platform<-lm(elite.ppolar_5~platform.sum_5+white_5+EDUC4_W5+INCOME_W5+male_5+rep_5+dem_5+party.x_5+AGE7_W5+interest_5, data=dat)


##Compile models
stargazer(lm.iss.all1, lm.ppol.all1, lm.aff.all1, lm.elite.all1, star.cutoffs = c(.05, .01, .001), 
          covariate.labels = c("Social Media Use", "White", "Education", "Income", "Male", "Republican", "Democrat", 
                               "Party Extremity", "Age", "Interest"))
stargazer(lm.iss.all2, lm.ppol.all2, lm.aff.all2, lm.elite.all2, star.cutoffs = c(.05, .01, .001))
stargazer(lm.iss.fb, lm.iss.ig, lm.iss.tw, lm.iss.yt, star.cutoffs = c(.05, .01, .001))
stargazer(lm.ppol.fb, lm.ppol.ig, lm.ppol.tw, lm.ppol.yt, star.cutoffs = c(.05, .01, .001))
stargazer(lm.aff.fb, lm.aff.ig, lm.aff.tw, lm.aff.yt, star.cutoffs = c(.05, .01, .001))
stargazer(lm.elite.fb, lm.elite.ig, lm.elite.tw, lm.elite.yt, star.cutoffs = c(.05, .01, .001))

# Change over time analyses
dat.p2 <- dat %>%
  select(CaseId, ppol.ind_4, ppol.ind_5, fb.use5_4, fb.use5_5, yt.use5_4, yt.use5_5, tw.use5_4, tw.use5_5,
         ig.use5_4, ig.use5_5, fb.use.dummy_4, fb.use.dummy_5, yt.use.dummy_4, yt.use.dummy_5, 
         tw.use.dummy_4, tw.use.dummy_5, ig.use.dummy_4, ig.use.dummy_5, iss.pol_4, iss.pol_5, civil.ind_4,
         civil.ind_5, interest_4, interest_5, sm.ind.r_4, sm.ind.r_5, platform.sum_4, platform.sum_5) %>%
  pivot_longer(-CaseId, names_to = c(".value", "wave"), names_sep = "_")

##Perceived Polarization
m.ppol.fb <- plm(ppol.ind~fb.use5+fb.use.dummy+ig.use.dummy+tw.use.dummy+yt.use.dummy+civil.ind+interest, 
                 data=dat.p2, model='within', index='CaseId')

m.ppol.ig <- plm(ppol.ind~ig.use5+fb.use.dummy+ig.use.dummy+tw.use.dummy+yt.use.dummy+civil.ind+interest, 
                 data=dat.p2, model='within', index='CaseId')

m.ppol.tw <- plm(ppol.ind~tw.use5+fb.use.dummy+ig.use.dummy+tw.use.dummy+yt.use.dummy+civil.ind+interest, 
                 data=dat.p2, model='within', index='CaseId')

m.ppol.yt <- plm(ppol.ind~yt.use5+fb.use.dummy+ig.use.dummy+tw.use.dummy+yt.use.dummy+civil.ind+interest, 
                 data=dat.p2, model='within', index='CaseId')

m.ppol.all<-plm(ppol.ind~sm.ind+fb.use.dummy+ig.use.dummy+tw.use.dummy+yt.use.dummy+interest, 
                data=dat.p2, model='within', index='CaseId')

m.ppol.all2<-plm(ppol.ind~sm.ind.r+interest, 
                data=dat.p2, model='within', index='CaseId')

m.ppol.platform<-plm(ppol.ind~platform.sum+interest, data=dat.p2, model='within', index='CaseId')


##Issue Polarization
m.iss.fb <- plm(iss.pol~fb.use5+fb.use.dummy+ig.use.dummy+tw.use.dummy+yt.use.dummy+civil.ind+interest, 
                 data=dat.p2, model='within', index='CaseId')

m.iss.ig <- plm(iss.pol~ig.use5+fb.use.dummy+ig.use.dummy+tw.use.dummy+yt.use.dummy+civil.ind+interest, 
                 data=dat.p2, model='within', index='CaseId')

m.iss.tw <- plm(iss.pol~tw.use5+fb.use.dummy+ig.use.dummy+tw.use.dummy+yt.use.dummy+civil.ind+interest, 
                 data=dat.p2, model='within', index='CaseId')

m.iss.yt <- plm(iss.pol~yt.use5+fb.use.dummy+ig.use.dummy+tw.use.dummy+yt.use.dummy+civil.ind+interest, 
                 data=dat.p2, model='within', index='CaseId')

m.iss.all <- plm(iss.pol~sm.ind+fb.use.dummy+ig.use.dummy+tw.use.dummy+yt.use.dummy+interest, 
                data=dat.p2, model='within', index='CaseId')

m.iss.all2 <- plm(iss.pol~sm.ind.r+interest, 
                 data=dat.p2, model='within', index='CaseId')

m.iss.platform <- plm(iss.pol~platform.sum+interest, data=dat.p2, model='within', index='CaseId')


stargazer(m.iss.all, m.ppol.all, star.cutoffs = c(.05, .01, .001))
stargazer(m.iss.fb, m.iss.ig, m.iss.tw, m.iss.yt, star.cutoffs = c(.05, .01, .001))
stargazer(m.ppol.fb, m.ppol.ig, m.ppol.tw, m.ppol.yt, star.cutoffs = c(.05, .01, .001))
stargazer(m.iss.all2, m.ppol.all2, star.cutoffs = c(.05, .01, .001))

##############################################
##############################################
####Not used####
#Cross-sectional effects of social media use (wave 4)

## Issue Extremity
lm.iss.fb_4<-lm(iss.pol_4~fb.use5_4+fb.use.dummy_4+ig.use.dummy_4+tw.use.dummy_4+yt.use.dummy_4+white_4+
                EDUC4_W4+INCOME_W4+male_4+party3_4+AGE7_W4, data=dat)
lm.iss.ig_4<-lm(iss.pol_4~ig.use5_4+ig.use.dummy_4+fb.use.dummy_4+tw.use.dummy_4+yt.use.dummy_4+white_4+
                EDUC4_W4+INCOME_W4+male_4+party3_4+AGE7_W4, data=dat)
lm.iss.tw_4<-lm(iss.pol_4~tw.use5_4+tw.use.dummy_4+ig.use.dummy_4+fb.use.dummy_4+yt.use.dummy_4+white_4+
                EDUC4_W4+INCOME_W4+male_4+party3_4+AGE7_W4, data=dat)
lm.iss.yt_4<-lm(iss.pol_4~yt.use5_4+yt.use.dummy_4+ig.use.dummy_4+tw.use.dummy_4+fb.use.dummy_4+white_4+
                EDUC4_W4+INCOME_W4+male_4+party3_4+AGE7_W4, data=dat)

##Perceived Polarization
lm.ppol.fb_4<-lm(ppol.ind_4~fb.use5_4+fb.use.dummy_4+ig.use.dummy_4+tw.use.dummy_4+yt.use.dummy_4+white_4+
                 EDUC4_W4+INCOME_W4+male_4+party3_4+AGE7_W4, data=dat)
lm.ppol.ig_4<-lm(ppol.ind_4~ig.use5_4+ig.use.dummy_4+fb.use.dummy_4+tw.use.dummy_4+yt.use.dummy_4+white_4+
                 EDUC4_W4+INCOME_W4+male_4+party3_4+AGE7_W4, data=dat)
lm.ppol.tw_4<-lm(ppol.ind_4~tw.use5_4+tw.use.dummy_4+ig.use.dummy_4+fb.use.dummy_4+yt.use.dummy_4+white_4+
                 EDUC4_W4+INCOME_W4+male_4+party3_4+AGE7_W4, data=dat)
lm.ppol.yt_4<-lm(ppol.ind_4~yt.use5_4+yt.use.dummy_4+ig.use.dummy_4+tw.use.dummy_4+fb.use.dummy_4+white_4+
                 EDUC4_W4+INCOME_W4+male_4+party3_4+AGE7_W4, data=dat)

#Compare Coefficients from models above

# Perceived Polarization
fb.ppol<-rnorm(10000, mean=summary(lm.ppol.fb)$coef[1,2], sd=summary(lm.ppol.fb)$coef[2,2])
ig.ppol<-rnorm(10000, mean=summary(lm.ppol.ig)$coef[1,2], sd=summary(lm.ppol.ig)$coef[2,2])
tw.ppol<-rnorm(10000, mean=summary(lm.ppol.tw)$coef[1,2], sd=summary(lm.ppol.tw)$coef[2,2])
yt.ppol<-rnorm(10000, mean=summary(lm.ppol.yt)$coef[1,2], sd=summary(lm.ppol.yt)$coef[2,2])

## Comparisons
###Facebook has a larger effect on ppol than twitter use
fb.tw.diff<-fb.ppol-tw.ppol
p.fb.tw.diff<-sum(fb.tw.diff>0)/10000
p.fb.tw.diff

###Facebook has a smaller effect on ppol than Insta use
fb.ig.diff<-fb.ppol-ig.ppol
p.fb.ig.diff<-sum(fb.ig.diff>0)/10000
p.fb.ig.diff

###Facebook has a smaller effect on ppol than YT use
fb.yt.diff<-fb.ppol-yt.ppol
p.fb.yt.diff<-sum(fb.yt.diff>0)/10000
p.fb.yt.diff

###Twitter has a smaller effect on ppol than Insta use
tw.ig.diff<-tw.ppol-ig.ppol
p.tw.ig.diff<-sum(tw.ig.diff>0)/10000
p.tw.ig.diff

###Twitter has a smaller effect on ppol than YouTube use
tw.yt.diff<-tw.ppol-yt.ppol
p.tw.yt.diff<-sum(tw.yt.diff>0)/10000
p.tw.yt.diff

###YouTube has a larger effect than Instagram
yt.ig.diff<-yt.ppol-ig.ppol
p.yt.ig.diff<-sum(yt.ig.diff>0)/10000
p.yt.ig.diff

# Issue Polarization
fb.ppol<-rnorm(10000, mean=summary(lm.iss.fb)$coef[1,2], sd=summary(lm.iss.fb)$coef[2,2])
ig.ppol<-rnorm(10000, mean=summary(lm.iss.ig)$coef[1,2], sd=summary(lm.iss.ig)$coef[2,2])
tw.ppol<-rnorm(10000, mean=summary(lm.iss.tw)$coef[1,2], sd=summary(lm.iss.tw)$coef[2,2])
yt.ppol<-rnorm(10000, mean=summary(lm.iss.yt)$coef[1,2], sd=summary(lm.iss.yt)$coef[2,2])

## Comparisons
###Facebook has a larger effect on ppol than twitter use
fb.tw.diff<-fb.ppol-tw.ppol
p.fb.tw.diff<-sum(fb.tw.diff>0)/10000
p.fb.tw.diff

###Facebook has a smaller effect on ppol than Insta use
fb.ig.diff<-fb.ppol-ig.ppol
p.fb.ig.diff<-sum(fb.ig.diff>0)/10000
p.fb.ig.diff

###Facebook has a smaller effect on ppol than YT use
fb.yt.diff<-fb.ppol-yt.ppol
p.fb.yt.diff<-sum(fb.yt.diff>0)/10000
p.fb.yt.diff

###Twitter has a smaller effect on ppol than Insta use
tw.ig.diff<-tw.ppol-ig.ppol
p.tw.ig.diff<-sum(tw.ig.diff>0)/10000

###Twitter has a smaller effect on ppol than YouTube use
tw.yt.diff<-tw.ppol-yt.ppol
p.tw.yt.diff<-sum(tw.yt.diff>0)/10000

###YouTube has a larger effect than Instagram
yt.ig.diff<-yt.ppol-ig.ppol
p.yt.ig.diff<-sum(yt.ig.diff>0)/10000

# Affective Polarization
fb.ppol<-rnorm(10000, mean=summary(lm.aff.fb)$coef[1,2], sd=summary(lm.aff.fb)$coef[2,2])
ig.ppol<-rnorm(10000, mean=summary(lm.aff.ig)$coef[1,2], sd=summary(lm.aff.ig)$coef[2,2])
tw.ppol<-rnorm(10000, mean=summary(lm.aff.tw)$coef[1,2], sd=summary(lm.aff.tw)$coef[2,2])
yt.ppol<-rnorm(10000, mean=summary(lm.aff.yt)$coef[1,2], sd=summary(lm.aff.yt)$coef[2,2])

## Comparisons
###Facebook has a larger effect on ppol than twitter use
fb.tw.diff<-fb.ppol-tw.ppol
p.fb.tw.diff<-sum(fb.tw.diff>0)/10000

###Facebook has a smaller effect on ppol than Insta use
fb.ig.diff<-fb.ppol-ig.ppol
p.fb.ig.diff<-sum(fb.ig.diff>0)/10000

###Facebook has a smaller effect on ppol than YT use
fb.yt.diff<-fb.ppol-yt.ppol
p.fb.yt.diff<-sum(fb.yt.diff>0)/10000

###Twitter has a smaller effect on ppol than Insta use
tw.ig.diff<-tw.ppol-ig.ppol
p.tw.ig.diff<-sum(tw.ig.diff>0)/10000

###Twitter has a smaller effect on ppol than YouTube use
tw.yt.diff<-tw.ppol-yt.ppol
p.tw.yt.diff<-sum(tw.yt.diff>0)/10000

###YouTube has a larger effect than Instagram
yt.ig.diff<-yt.ppol-ig.ppol
p.yt.ig.diff<-sum(yt.ig.diff>0)/10000



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




####Increasers vs Decreasers####

# Look at SM Increasers
dat <- dat %>%
  mutate(fb.diff=fb.use5_5-fb.use5_4, ig.diff=ig.use5_5-ig.use5_4,
         tw.diff=tw.use5_5-tw.use5_4, yt.diff=yt.use5_5-yt.use5_4, 
         iss.pol.diff=iss.pol_5-iss.pol_4, ppol.diff=ppol.ind_5-ppol.ind_4, 
         interest.diff=interest_5-interest_4, sm.diff=sm.ind.r_5-sm.ind.r_4)

dat$fb.inc<-ifelse(dat$fb.diff>0, dat$fb.diff, 0)
dat$fb.dec<-ifelse(dat$fb.diff<0, abs(dat$fb.diff), 0)
dat$ig.inc<-ifelse(dat$ig.diff>0, dat$ig.diff, 0)
dat$ig.dec<-ifelse(dat$ig.diff<0, abs(dat$ig.diff), 0)
dat$tw.inc<-ifelse(dat$tw.diff>0, dat$tw.diff, 0)
dat$tw.dec<-ifelse(dat$tw.diff<0, abs(dat$tw.diff), 0)
dat$yt.inc<-ifelse(dat$yt.diff>0, dat$yt.diff, 0)
dat$yt.dec<-ifelse(dat$yt.diff<0, abs(dat$yt.diff), 0)

dat$sm.inc<-ifelse(dat$sm.diff>0, dat$sm.diff, 0)
dat$sm.dec<-ifelse(dat$sm.diff<0, abs(dat$sm.diff), 0)

##Models

###Issue Polarization
m.fb.iss.inc<-lm(iss.pol.diff~fb.inc, data=dat)
m.fb.iss.dec<-lm(iss.pol.diff~fb.dec, data=dat)

m.ig.iss.inc<-lm(iss.pol.diff~ig.inc, data=dat)
m.ig.iss.dec<-lm(iss.pol.diff~ig.dec, data=dat)

m.tw.iss.inc<-lm(iss.pol.diff~tw.inc, data=dat)
m.tw.iss.dec<-lm(iss.pol.diff~tw.dec, data=dat)

m.yt.iss.inc<-lm(iss.pol.diff~yt.inc, data=dat)
m.yt.iss.dec<-lm(iss.pol.diff~yt.dec, data=dat)

m.sm.iss<-lm(iss.pol.diff~sm.inc+sm.dec+interest.diff, data=dat)
linearHypothesis(m.sm.iss, c("sm.inc=sm.dec"))

###Perceived Polarization
m.fb.ppol.inc<-lm(ppol.diff~fb.inc, data=dat)
m.fb.ppol.dec<-lm(ppol.diff~fb.dec, data=dat)

m.ig.ppol.inc<-lm(ppol.diff~ig.inc, data=dat)
m.ig.ppol.dec<-lm(ppol.diff~ig.dec, data=dat)

m.tw.ppol.inc<-lm(ppol.diff~tw.inc, data=dat)
m.tw.ppol.dec<-lm(ppol.diff~tw.dec, data=dat)

m.yt.ppol.inc<-lm(ppol.diff~yt.inc, data=dat)
m.yt.ppol.dec<-lm(ppol.diff~yt.dec, data=dat)

m.sm.ppol<-lm(ppol.diff~sm.inc+sm.dec+interest.diff, data=dat)
linearHypothesis(m.sm.ppol, c("sm.inc=sm.dec"))
## Compile Models
stargazer(m.sm.iss, m.sm.ppol, star.cutoffs = c(.05, .01, .001))

# Output it all to Stata for double-checking
dat.out<- dat %>%
  select(sm.inc, sm.dec, ppol.diff, iss.pol.diff, interest.diff, CaseId)

colnames(dat.out) <- gsub("\\.", "_", colnames(dat.out))
foreign::write.dta(dat.out, file="Asymmetrical Data.dta")







