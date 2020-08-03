####Libraries####
library(easypackages)
libs<-c("tidyverse", "ggplot2", "car", "psych", "haven", "naniar", "plm", "stargazer")
libraries(libs)

####Import the Data####
dat.orig.4 <- read_sav("Data/s7991_UPenn_LongPollW4_FINAL_WEIGHTED_client_7.11.2019.sav")
dat.orig.5 <- read_sav("Data/7991_UPenn_LongPoll W5_client_04.13.2020.sav")
dat.orig.3 <- read_sav("Data/Upenn_LongPollW3_FINAL_WEIGHTED_2018-11-20.sav")

# Add longitudinal indicator
dat.orig.3$wave_W3 <- 1
dat.orig.4$wave_W4 <- 1
dat.orig.5$wave_W5 <- 1

####Merge Datasets####
dat <- dat.orig.4 %>%
  full_join(dat.orig.5, by='CaseId')

dat <- dat.orig.3 %>%
  select(CaseId, THERM3_W3, THERM4_W3, IM1A_W3, IM1B_W3, TR1A_W3, TR1B_W3, CHI3_W3, CHI4_W3) %>%
  right_join(dat, by='CaseId')

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
dat <- dat %>%
  replace_with_na_at(.vars=clean.length2[1:100], condition = ~.x %in% na.values.2)
dat <- dat %>%
  replace_with_na_at(.vars=clean.length2[101:200], condition = ~.x %in% na.values.2)
dat <- dat %>%
  replace_with_na_at(.vars=clean.length2[201:300], condition = ~.x %in% na.values.2)
dat <- dat %>%
  replace_with_na_at(.vars=clean.length2[301:400], condition = ~.x %in% na.values.2)
dat <- dat %>%
  replace_with_na_at(.vars=clean.length2[401:500], condition = ~.x %in% na.values.2)
dat <- dat %>%
  replace_with_na_at(.vars=clean.length2[501:length(clean.length2)], condition = ~.x %in% na.values.2)

dat <-dat %>%
  replace_with_na_at(.vars=clean.length3, condition = ~.x %in% na.values.3)

#Clean data backup
save(dat, file = "Analysis Dataset.RData")