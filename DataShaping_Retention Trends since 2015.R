### Retention Trends by Fall to Fall from cohort 2015
setwd("G:/Shared drives/HMCSE-PAM Lab/Jay's Space/Jay's space_2021/Retention Trends")
# improt data
library(dplyr)
library(readr)
STU_ENROLLMENT_SPRING21 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/DATA 202101/202101/STU_ENROLLMENT_SPRING21.csv") %>% 
   filter(Stu_DEMO_DATA_SOURCE =="SIF") 

STU_ENROLLMENT_SPRING21 <- STU_ENROLLMENT_SPRING21  %>% 
  mutate(Stu_ClassificationCode = as.double(Stu_ClassificationCode)) %>% filter(Stu_ClassificationCode <= 4)
#match ID for 2020
#CSE_MATCH_IDS_2020 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/Matt's Space/CSE_MATCH_COURSE_DATA/CSE_MATCH_IDS_2020.csv")
#write.csv(CSE_MATCH_IDS_2020,"CSE_COHORT_2020_ID.csv")
CSE_MATCH_IDS_2020 <- read.csv("CSE_COHORT_2020_ID.csv")
library(readr)
CSE_ALL_ENR_up2020 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/Jay's Space/DATA/CSE_ALL_ENR_up2020.csv", 
                                    col_types = cols(X1 = col_skip())) %>% 
  select(STU_ID,UNIV_ROW_ID,COHORT_YEAR,APPLICANT_TIER,GPA_HIGHSCHOOL,FIRST_GENERATION_STUDENT,COUNTY,COUNTY_GROUP,
         HIGH_SCHOOL_NAME,FIRST_FALL_PELL, FIRST_FALL_BRIGHT_FUTURES,STATE_GROUP,GPA_ENTERING_SECOND_FALL,ATTENDED_SECOND_FALL_TERM,
         APR,FIRST_FALL_GPA,GPA_ENTERING_SECOND_FALL )

CSE_ALL_Cohort15_20 <- merge(CSE_ALL_ENR_up2020, CSE_MATCH_IDS_2020, by="UNIV_ROW_ID", all.x = T) %>% 
  select(-STU_ID.x, "STU_ID"=STU_ID.y) %>% 
  select(16,1:15)

colnames(CSE_ALL_Cohort15_20)
colSums(is.na(CSE_ALL_Cohort15_20))


# select FTIC

library(dplyr)
library(stringr)
#Fall FTIC only
FTICFALL <- STU_ENROLLMENT_SPRING21 %>% 
  filter(Stu_StudentTypeCode == "B" & # for FTIC
           Stu_LoadIPEDSFTPT=="Full Time" & # full time
           Stu_AdmissionRecentTypeCode=="B" & # FTIC
           Stu_DEMO_TIME_FRAME >= 201505 ) %>%  # since cochort 2015
  group_by(STU_ID) %>% 
  mutate(ID_index = row_number()) %>% 
  filter(str_detect(Stu_AdmissionTerm, "Fall...")) %>% # for fall semester starters
  filter(ID_index== 1)  # index for beginner summer and came back fall are duplicated

#check Fall FTIC2017 1018
xtabs(~FTICFALL$Stu_Department + FTICFALL$Stu_StudentTypeCode)

#Summer FTIC only
FTICSUMMER <- STU_ENROLLMENT_SPRING21 %>% 
  filter(Stu_StudentTypeCode == "B" &
           Stu_LoadIPEDSFTPT=="Full Time" & 
           Stu_AdmissionRecentTypeCode=="B" & 
           Stu_DEMO_TIME_FRAME >= 201505 ) %>% 
  group_by(STU_ID) %>% 
  mutate(ID_index = row_number()) %>% 
  filter(ID_index== 1) %>% 
  filter(str_detect(Stu_AdmissionTerm, "Summer...")) # summer starters

# check Summer FTIC 2017 58
xtabs(~FTICSUMMER$Stu_AdmissionTerm+   FTICSUMMER$Stu_StudentTypeCode)
library(tidyr)

# combine Fall and summer
FTIC_All <- rbind(FTICSUMMER,FTICFALL)
FTIC_All <- FTIC_All %>%  tidyr::separate( col = Stu_AdmissionTerm, into = c("Term", "Cohort"), sep = " ") # create new column for Cohort
FTIC_ALL <- FTIC_All[!duplicated(FTIC_All$STU_ID),]



FTIC_All_cohort_ID <- FTIC_ALL %>% select("STU_ID","Cohort")
FTIC_All_cohort_all_terms <- inner_join(FTIC_All_cohort_ID, STU_ENROLLMENT_SPRING21, by="STU_ID")  # all terms
table(FTIC_All_cohort_all_terms$Cohort, FTIC_All_cohort_all_terms$Stu_DEMO_TIME_FRAME)  
  
# select columns
FTIC_Fall_only<-  FTIC_All_cohort_all_terms %>% 
  filter(str_detect(Stu_Term, "Fall...")) %>%  #choose falls
  group_by(STU_ID) %>% 
  arrange(Stu_DEMO_TIME_FRAME, .by_group=TRUE) %>% 
  select(1:5,"Cohort", contains("age"), contains("FTPT"), contains("College"), contains("Department"), contains("gender"), contains("ethnicity"), contains("GPA")) %>% 
  mutate(NumFallTerms = row_number())

addmargins(table(FTIC_Fall_only$Cohort, FTIC_Fall_only$Stu_DEMO_TIME_FRAME))

FALL_GPAHS <- FTIC_Fall_only  %>% group_by(Cohort) %>%  filter(NumFallTerms ==1) %>% 
  filter(Stu_GPAHighSchool != 0) %>% 
  dplyr::summarise(meanHSGPA=mean(Stu_GPAHighSchool, na.rm = TRUE), Counts=n())


# export data
write.csv(FTIC_Fall_only, "FTIC_2015_2020_retention_DF_HSGPA.csv")
NA_FALLS <- FTIC_Fall_only[which(is.na(FTIC_Fall_only$Stu_GPAHighSchool)),] 
addmargins(table(NA_FALLS$Cohort))


### for all semester
FTIC_all_terms <-  FTIC_All_cohort_all_terms %>% 
  group_by(STU_ID) %>% 
  arrange(Stu_DEMO_TIME_FRAME, .by_group=TRUE) %>% 
  filter(ifelse(Cohort == 2015, Stu_DEMO_TIME_FRAME>= 201508,
                ifelse(Cohort == 2016, Stu_DEMO_TIME_FRAME>=201608,
                       ifelse(Cohort == 2017, Stu_DEMO_TIME_FRAME>= 201708,
                              ifelse(Cohort == 2018, Stu_DEMO_TIME_FRAME>= 201808,
                                     ifelse(Cohort == 2019, Stu_DEMO_TIME_FRAME>= 201908,
                                            ifelse(Cohort == 2020, Stu_DEMO_TIME_FRAME>= 202008))))))) %>% 
  select(1:5,"Cohort", contains("age"), contains("FTPT"), contains("College"), contains("Department"), contains("gender"),
         contains("ethnicity"), contains("GPA"), -Stu_GPAHighSchool, contains("Total"), contains("termload")) %>% 
  mutate(NumTerms = row_number())

addmargins(table(FTIC_all_terms$Cohort, FTIC_all_terms$Stu_DEMO_TIME_FRAME))

colnames(FTIC_all_terms)

# combined with CSE and Stu_data

retention_YOY_DF <- merge(FTIC_all_terms, CSE_ALL_Cohort15_20, by="STU_ID", all.x = T)
addmargins(table(retention_YOY_DF$Cohort, retention_YOY_DF$NumTerms))

# export data
write.csv(retention_YOY_DF, "V2_retention_YOY_DF.csv")






