library(tidyverse)
library(tidyr)
library(haven)
library(readr)
library(stargazer)
library(ggplot2)
library(xtable)
library(pollster)
library(knitr)
library(naniar)
library(scales)
library(gmodels)
library(questionr)


data <- read_sav("Y1104 UMD CA Voter ID Study Final Weighted Data 121225 - Confidential (1).SAV")

# Demographics

race <- NA
race[data$ER2 == 1 & data$ER1 == 2] <- 1
race[data$ER2 == 2 & data$ER1 == 2] <- 2
race[data$ER1 == 1] <- 3
race[data$ER2 == 3 | data$ER2 == 5] <- 4
race[data$ER2 == 4 | data$ER2 == 6 | data$ER2 == 7] <- 5


data$race_and_ethnicity <- NA
data$race_and_ethnicity[race == 1] <- "White Non-Hispanic"
data$race_and_ethnicity[race == 2] <- "Black Non-Hispanic"
data$race_and_ethnicity[race == 3] <- "Hispanic"
data$race_and_ethnicity[race == 4] <- "AAPI"
data$race_and_ethnicity[race == 5] <- "Other Race"


pid <- NA
pid[data$PID_STEM == 1] <- 1
pid[data$PID_LEAN == 1] <- 1
pid[data$PID_STEM == 2] <- 2
pid[data$PID_LEAN == 2] <- 2
pid[data$PID_LEAN == 3] <- 3

data$pid3cat <- factor(pid, levels = c(1, 2, 3), labels = c("Democrat", "Republican", "Independent"))

age18_29 <- NA
age18_29[data$AGE1_1 < 30] <- 0
age18_29[data$AGE1_1 > 29] <- 1

data$age18_29 <- factor(age18_29, levels = c(0,1), labels = c("Age 18-29", "Age 30+"))

inc4cat <- NA
inc4cat[data$INC1 == 1] <- 0
inc4cat[data$INC1 == 2] <- 0
inc4cat[data$INC1 == 3] <- 0
inc4cat[data$INC1 == 4] <- 0
inc4cat[data$INC1 == 5] <- 1
inc4cat[data$INC1 == 6] <- 1
inc4cat[data$INC1 == 7] <- 2
inc4cat[data$INC1 == 8] <- 2
inc4cat[data$INC1 == 9] <- 3

data$inc4cat <- factor(inc4cat, levels = c(0, 1, 2, 3), 
                       labels = c("Less than $30,000", 
                                  "$30,000 - $50,000",
                                  "$50,000 - $100,000",
                                  "Over $100,000"))

gender_2cat <- NA
gender_2cat[data$GENDER == 1] <- 0
gender_2cat[data$GENDER == 2] <- 1

data$gender_2cat <- factor(gender_2cat,
                           levels = c(0,1),
                           labels = c("Male", "Female"))


# Full Driver's License

license <- NA
license[data$DRIVERS_LICENSE == 5] <- 0
license[data$DRIVERS_LICENSE == 4 & data$LICENSE_NAME == 2] <- 1 # Expired OOS License, Incorrect Name
license[data$DRIVERS_LICENSE == 4 & data$LICENSE_NAME == 1] <- 2 # Expired OOS License, Correct Name
license[data$DRIVERS_LICENSE == 2 & data$LICENSE_NAME == 2] <- 3 # Current OOS License, Incorrect Name
license[data$DRIVERS_LICENSE == 2 & data$LICENSE_NAME == 1] <- 4 # Current OOS License, Correct Name
license[data$DRIVERS_LICENSE == 3 & data$LICENSE_ADDRESS1 == 2 & data$LICENSE_NAME == 2] <- 5 # Expired CA License, Incorrect Address & Name
license[data$DRIVERS_LICENSE == 3 & data$LICENSE_ADDRESS1 == 1 & data$LICENSE_NAME == 2] <- 6 # Expired CA License, Correct Address, Incorrect Name
license[data$DRIVERS_LICENSE == 3 & data$LICENSE_ADDRESS1 == 2 & data$LICENSE_NAME == 1] <- 7 # Expired CA License, Incorrect Address, Correct Name
license[data$DRIVERS_LICENSE == 3 & data$LICENSE_ADDRESS1 == 1 & data$LICENSE_NAME == 1] <- 8 # Expired CA License, Correct Address & Name
license[data$DRIVERS_LICENSE == 1 & data$LICENSE_ADDRESS1 == 2 & data$LICENSE_NAME == 2] <- 9 # Current CA License, Incorrect Address & Name
license[data$DRIVERS_LICENSE == 1 & data$LICENSE_ADDRESS1 == 1 & data$LICENSE_NAME == 2] <- 10 # Current CA License, Correct Address, Incorrect Name
license[data$DRIVERS_LICENSE == 1 & data$LICENSE_ADDRESS1 == 2 & data$LICENSE_NAME == 1] <- 11 # Current CA License, Incorrect Address, Correct Name
license[data$DRIVERS_LICENSE == 1 & data$LICENSE_ADDRESS1 == 1 & data$LICENSE_NAME == 1] <- 12 # Current CA License, Correct Address & Name

data$license <- factor(license,
                       levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("Do Not Have One",
                                  "Expired OOS License, Incorrect Name",
                                  "Expired OOS License, Correct Name",
                                  "Current OOS License, Incorrect Name",
                                  "Current OOS License, Correct Name",
                                  "Expired CA License, Incorrect Address & Name",
                                  "Expired CA License, Correct Address, Incorrect Name",
                                  "Expired CA License, Incorrect Address, Correct Name",
                                  "Expired CA License, Correct Address & Name",
                                  "Current CA License, Incorrect Address & Name",
                                  "Current CA License, Correct Address, Incorrect Name",
                                  "Current CA License, Incorrect Address, Correct Name",
                                  "Current CA License, Correct Address & Name"
                                  ))


license_details <- NA
license_details[data$DRIVERS_LICENSE == 5] <- 0
license_details[data$DRIVERS_LICENSE == 4] <- 1 # Expired OOS License
license_details[data$DRIVERS_LICENSE == 2] <- 2 # Current OOS License
license_details[data$DRIVERS_LICENSE == 3 & data$LICENSE_ADDRESS1 == 2 & data$LICENSE_NAME == 2] <- 3 # Expired CA License, Incorrect Address & Name
license_details[data$DRIVERS_LICENSE == 3 & data$LICENSE_ADDRESS1 == 1 & data$LICENSE_NAME == 2] <- 3 # Expired CA License, Correct Address, Incorrect Name
license_details[data$DRIVERS_LICENSE == 3 & data$LICENSE_ADDRESS1 == 2 & data$LICENSE_NAME == 1] <- 3 # Expired CA License, Incorrect Address, Correct Name
license_details[data$DRIVERS_LICENSE == 3 & data$LICENSE_ADDRESS1 == 1 & data$LICENSE_NAME == 1] <- 4 # Expired CA License, Correct Address & Name
license_details[data$DRIVERS_LICENSE == 1 & data$LICENSE_ADDRESS1 == 2 & data$LICENSE_NAME == 2] <- 5 # Current CA License, Incorrect Address & Name
license_details[data$DRIVERS_LICENSE == 1 & data$LICENSE_ADDRESS1 == 1 & data$LICENSE_NAME == 2] <- 5 # Current CA License, Correct Address, Incorrect Name
license_details[data$DRIVERS_LICENSE == 1 & data$LICENSE_ADDRESS1 == 2 & data$LICENSE_NAME == 1] <- 5 # Current CA License, Incorrect Address, Correct Name
license_details[data$DRIVERS_LICENSE == 1 & data$LICENSE_ADDRESS1 == 1 & data$LICENSE_NAME == 1] <- 6 # Current CA License, Correct Address & Name



data$license_details <- factor(license_details,
                               levels = c(0,1,2,3,4,5,6),
                               labels = c("Do Not Have One",
                                          "Expired OOS License",
                                          "Current OOS License",
                                          "Expired CA License, Incorrect Details",
                                          "Expired CA License, Correct Details",
                                          "Current CA License, Incorrect Details",
                                          "Current CA License, Correct Details"))


ca_license_dummy <- NA
ca_license_dummy[license_details == 0] <- 0
ca_license_dummy[license_details == 1] <- 0
ca_license_dummy[license_details == 2] <- 0
ca_license_dummy[license_details == 3] <- 0
ca_license_dummy[license_details == 4] <- 0
ca_license_dummy[license_details == 5] <- 0
ca_license_dummy[license_details == 6] <- 1

data$ca_license_dummy <- factor(ca_license_dummy,
                                levels = c(0,1),
                                labels = c("Does not have one",
                                           "Has a current, correct CA License"))


# CA State ID



ca_stateID <- NA
ca_stateID[data$STATE_ID == 5] <- 0
ca_stateID[data$STATE_ID == 2 & data$STATE_ID_NAME == 2] <- 1 # Expired OOS ID, Incorrect Name
ca_stateID[data$STATE_ID == 2 & data$STATE_ID_NAME == 1] <- 2 # Expired OOS ID, Correct Name
ca_stateID[data$STATE_ID == 4 & data$STATE_ID_NAME == 2] <- 3 # Current OOS ID, Incorrect Name
ca_stateID[data$STATE_ID == 4 & data$STATE_ID_NAME == 1] <- 4 # Current OOS ID, Correct Name
ca_stateID[data$STATE_ID == 3 & data$STATE_ID_ADDRESS1 == 2 & data$STATE_ID_NAME == 2] <- 5
ca_stateID[data$STATE_ID == 3 & data$STATE_ID_ADDRESS1 == 1 & data$STATE_ID_NAME == 2] <- 6
ca_stateID[data$STATE_ID == 3 & data$STATE_ID_ADDRESS1 == 2 & data$STATE_ID_NAME == 1] <- 7
ca_stateID[data$STATE_ID == 3 & data$STATE_ID_ADDRESS1 == 1 & data$STATE_ID_NAME == 1] <- 8
ca_stateID[data$STATE_ID == 1 & data$STATE_ID_ADDRESS1 == 2 & data$STATE_ID_NAME == 2] <- 9
ca_stateID[data$STATE_ID == 1 & data$STATE_ID_ADDRESS1 == 1 & data$STATE_ID_NAME == 2] <- 10
ca_stateID[data$STATE_ID == 1 & data$STATE_ID_ADDRESS1 == 2 & data$STATE_ID_NAME == 1] <- 11
ca_stateID[data$STATE_ID == 1 & data$STATE_ID_ADDRESS1 == 1 & data$STATE_ID_NAME == 1] <- 12

freq(ca_stateID)


data$ca_stateID <- factor(ca_stateID,
                          levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                          labels = c("No State ID",
                                     "Expired OOS ID, Incorrect Name",
                                     "Expired OOS ID, Correct Name",
                                     "Current OOS ID, Incorrect Name",
                                     "Current OOS ID, Correct Name",
                                     "Expired CA State ID, Incorrect Address & Name",
                                     "Expired CA State ID, Correct Address, Incorrect Name",
                                     "Expired CA State ID, Incorrect Address, Correct Name",
                                     "Expired CA State ID, Correct Address & Name",
                                     "Current CA State ID, Incorrect Address & Name",
                                     "Current CA State ID, Correct Address, Incorrect Name",
                                     "Current CA State ID, Incorrect Address, Correct Name",
                                     "Current CA State ID, Correct Adress & Name"))


ca_stateID_details <- NA
ca_stateID_details[ca_stateID == 0] <- 0
ca_stateID_details[ca_stateID == 1] <- 1 # Expired OOS ID
ca_stateID_details[ca_stateID == 2] <- 1 # Expired OOS ID
ca_stateID_details[ca_stateID == 3] <- 2 # Current OOS ID
ca_stateID_details[ca_stateID == 4] <- 2 # Current OOS ID
ca_stateID_details[ca_stateID == 5] <- 3 # Expired CA State ID, Incorrect Details
ca_stateID_details[ca_stateID == 6] <- 3 # Expired CA State ID, Incorrect Details
ca_stateID_details[ca_stateID == 7] <- 3 # Expired CA State ID, Incorrect Details
ca_stateID_details[ca_stateID == 8] <- 4 # Expired CA State ID, Correct Details
ca_stateID_details[ca_stateID == 9] <- 5 # Current CA State ID, Incorrect Details
ca_stateID_details[ca_stateID == 10] <- 5 # Current CA State ID, Incorrect Details
ca_stateID_details[ca_stateID == 11] <- 5 # Current CA State ID, Incorrect Details
ca_stateID_details[ca_stateID == 12] <- 6 # Current CA State ID, Correct Details

data$ca_stateID_details <- factor(ca_stateID_details, 
                                  levels = c(0,1,2,3,4,5,6),
                                  labels = c("No CA State ID",
                                             "Expired OOS ID",
                                             "Current OOS ID",
                                             "Expired CA State ID, Incorrect Details",
                                             "Expired CA State ID, Correct Details",
                                             "Current CA State ID, Incorrect Details",
                                             "Current CA State ID, Correct Details"))



ca_stateID_dummy <- NA
ca_stateID_dummy[ca_stateID_details == 0] <- 0
ca_stateID_dummy[ca_stateID_details == 1] <- 0
ca_stateID_dummy[ca_stateID_details == 2] <- 0
ca_stateID_dummy[ca_stateID_details == 3] <- 0
ca_stateID_dummy[ca_stateID_details == 4] <- 0
ca_stateID_dummy[ca_stateID_details == 5] <- 0
ca_stateID_dummy[ca_stateID_details == 6] <- 1

data$ca_stateID_dummy <- factor(ca_stateID_dummy,
                                levels = c(0,1),
                                labels = c("Does not have one",
                                           "Has a current, correct CA State ID"))



# Passport

id_passport <- NA
id_passport[data$PASSPORT == 2] <- 0
id_passport[data$EXPIRATION_PASSPORT == 2] <- 0
id_passport[data$EXPIRATION_PASSPORT == 1 & data$PASSPORT_NAME == 2] <- 1
id_passport[data$EXPIRATION_PASSPORT == 1 & data$PASSPORT_NAME == 1] <- 2
id_passport[data$PASSPORT == 1 & data$PASSPORT_NAME == 2] <- 3
id_passport[data$PASSPORT == 1 & data$PASSPORT_NAME == 1] <- 4

data$id_passport <- factor(id_passport,
                           levels = c(0,1,2,3,4),
                           labels = c("No Passport",
                                      "Expired Passport, Incorrect Name",
                                      "Expired Passport, Correct Name",
                                      "Current Passport, Incorrect Name",
                                      "Current Passport, Correct Name"))

id_passport_dummy <- NA
id_passport_dummy[id_passport == 0] <- 0
id_passport_dummy[id_passport == 1] <- 0
id_passport_dummy[id_passport == 2] <- 0
id_passport_dummy[id_passport == 3] <- 0
id_passport_dummy[id_passport == 4] <- 1

data$id_passport_dummy <- factor(id_passport_dummy,
                                 levels = c(0,1),
                                 labels = c("Does not have one",
                                            "Has a current, correct passport"))


# Military ID

id_military <- NA
id_military[data$EXPIRATION_MILITARY == 2] <- 0
id_military[data$EXPIRATION_MILITARY == 1] <- 1
id_military[data$MILITARY_ID == 1] <- 2

data$id_military <- factor(id_military,
                           levels = c(0,1,2),
                           labels = c("Does not have one",
                                      "Expired Military ID",
                                      "Current Military ID"))

# Veteran ID


id_veterans <- NA
id_veterans[data$EXPIRATION_VETERANS == 2] <- 0
id_veterans[data$EXPIRATION_VETERANS == 1] <- 1
id_veterans[data$VETERANS_ID == 1] <- 2

data$id_veterans <- factor(id_veterans,
                           levels = c(0,1,2),
                           labels = c("Does not have one",
                                      "Expired Veterans ID",
                                      "Current Veterans ID"))

# Student ID

studentID <- NA
studentID[data$STUDENT_ID_8 == 1] <- 0 # none
studentID[data$STUDENT_ID_7 == 1] <- 0 # other, only 6 people
studentID[data$STUDENT_ID_2 == 1] <- 1 # private HS
studentID[data$STUDENT_ID_4 == 1] <- 2 # OOS public college
studentID[data$STUDENT_ID_6 == 1] <- 3 # OOS private college
studentID[data$STUDENT_ID_5 == 1] <- 4 #CA private college
studentID[data$STUDENT_ID_1 == 1] <- 5 # public HS
studentID[data$STUDENT_ID_3 == 1] <- 6 # CA public college



data$studentID <- factor(studentID,
                         levels = c(0,1,2,3,4,5,6),
                         labels = c("No Student ID",
                                    "Private HS ID",
                                    "OOS Public College ID",
                                    "OOS Private College ID",
                                    "CA Private College ID",
                                    "Public HS ID",
                                    "CA Public College ID"))


studentID2 <- NA
studentID2[data$STUDENT_ID_8 == 1] <- 0 # none
studentID2[data$STUDENT_ID_7 == 1] <- 0 # other, only 6 people
studentID2[data$STUDENT_ID_2 == 1] <- 1 # private HS
studentID2[data$STUDENT_ID_4 == 1] <- 2 # OOS public college
studentID2[data$STUDENT_ID_6 == 1] <- 3 # OOS private college
studentID2[data$STUDENT_ID_5 == 1] <- 4 #CA private college
studentID2[data$STUDENT_ID_1 == 1] <- 5 # public HS
studentID2[data$STUDENT_ID_3 == 1] <- 5 # CA public college



data$studentID2 <- factor(studentID2,
                         levels = c(0,1,2,3,4,5),
                         labels = c("No Student ID",
                                    "Private HS ID",
                                    "OOS Public College ID",
                                    "OOS Private College ID",
                                    "CA Private College ID",
                                    "CA Public HS or CA Public College"))


studentID3 <- NA
studentID3[studentID2 == 0] <- 0
studentID3[studentID2 == 1] <- 1
studentID3[studentID2 == 2] <- 1
studentID3[studentID2 == 3] <- 1
studentID3[studentID2 == 4] <- 1
studentID3[studentID2 == 5] <- 2

data$studentID3 <- factor(studentID3,
                         levels = c(0,1,2),
                         labels = c("No student ID",
                                    "Other Student ID",
                                    "CA Public HS or CA Public College"))



# Tribal ID

id_tribal <- NA
id_tribal[data$EXPIRATION_TRIBAL == 2] <- 0
id_tribal[data$EXPIRATION_TRIBAL == 1] <- 1
id_tribal[data$TRIBAL_ID == 1] <- 2

data$id_tribal <- factor(id_tribal,
                           levels = c(0,1,2),
                           labels = c("Does not have one",
                                      "Expired Tribal ID",
                                      "Current Tribal ID"))

# Hunting License

id_hunting <- NA
id_hunting[data$EXPIRATION_HUNTING_LICENSE == 3] <- 0
id_hunting[data$EXPIRATION_HUNTING_LICENSE == 2] <- 1
id_hunting[data$EXPIRATION_HUNTING_LICENSE == 1] <- 2
id_hunting[data$HUNTING_LICENSE_ID == 2] <- 3
id_hunting[data$HUNTING_LICENSE_ID == 1] <- 4

data$id_hunting <- factor(id_hunting,
                          levels = c(0,1,2,3,4),
                          labels = c("Does not have hunting license",
                                     "Expired OOS Hunting License",
                                     "Expired CA Hunting License",
                                     "Current OOS Hunting License",
                                     "Current CA Hunting License"))

id_hunting_dummy <- NA
id_hunting_dummy[id_hunting == 0] <- 0
id_hunting_dummy[id_hunting == 1] <- 0
id_hunting_dummy[id_hunting == 2] <- 0
id_hunting_dummy[id_hunting == 3] <- 0
id_hunting_dummy[id_hunting == 4] <- 1

data$id_hunting_dummy <- factor(id_hunting_dummy,
                                levels = c(0,1),
                                labels = c("Does not have one",
                                           "Current CA Hunting License"))

# Weapons Permit ID

id_weapons <- NA
id_weapons[data$EXPIRATION_WEAPONS_PERMIT == 3] <- 0
id_weapons[data$EXPIRATION_WEAPONS_PERMIT == 2] <- 1
id_weapons[data$EXPIRATION_WEAPONS_PERMIT == 1] <- 2
id_weapons[data$WEAPONS_PERMIT_ID == 2] <- 3
id_weapons[data$WEAPONS_PERMIT_ID == 1] <- 4

data$id_weapons <- factor(id_weapons,
                          levels = c(0,1,2,3,4),
                          labels = c("Does not have Weapons Permit",
                                     "Expired OOS Weapons Permit",
                                     "Expired CA Weapons Permit",
                                     "Current OOS Weapons Permit",
                                     "Current CA Weapons Permit"))

id_weapons_dummy <- NA
id_weapons_dummy[id_weapons == 0] <- 0
id_weapons_dummy[id_weapons == 1] <- 0
id_weapons_dummy[id_weapons == 2] <- 0
id_weapons_dummy[id_weapons == 3] <- 0
id_weapons_dummy[id_weapons == 4] <- 1

data$id_weapons_dummy <- factor(id_weapons_dummy,
                                levels = c(0,1),
                                labels = c("Does not have one",
                                           "Current CA Weapons Permit"))

# Employee ID

## Federal

fedgov_id <- NA
fedgov_id[data$EXPIRATION_EMPLOYEE_ID_1 == 0] <- 0
fedgov_id[data$EMPLOYEE_ID_1 == 0] <- 0
fedgov_id[data$EXPIRATION_EMPLOYEE_ID_1 == 1] <- 1
fedgov_id[data$EMPLOYEE_ID_1 == 1] <- 2

data$fedgov_id <- factor(fedgov_id,
                         levels = c(0,1,2),
                         labels = c("Does not have one",
                                    "Expired federal gov employee ID",
                                    "Current federal gov employee ID"))

## CA State Gov

ca_gov_id <- NA
ca_gov_id[data$EXPIRATION_EMPLOYEE_ID_2 == 0] <- 0
ca_gov_id[data$EMPLOYEE_ID_2 == 0] <- 0
ca_gov_id[data$EXPIRATION_EMPLOYEE_ID_2 == 1] <- 1
ca_gov_id[data$EMPLOYEE_ID_2 == 1] <- 2

data$ca_gov_id <- factor(ca_gov_id,
                         levels = c(0,1,2),
                         labels = c("Does not have one",
                                    "Expired CA gov employee ID",
                                    "Current CA gov employee ID"))

## Local Gov in CA

local_ca_gov_id <- NA
local_ca_gov_id[data$EXPIRATION_EMPLOYEE_ID_3 == 0] <- 0
local_ca_gov_id[data$EMPLOYEE_ID_3 == 0] <- 0
local_ca_gov_id[data$EXPIRATION_EMPLOYEE_ID_3 == 1] <- 1
local_ca_gov_id[data$EMPLOYEE_ID_3 == 1] <- 2

data$local_ca_gov_id <- factor(local_ca_gov_id,
                         levels = c(0,1,2),
                         labels = c("Does not have one",
                                    "Expired local gov employee ID, CA",
                                    "Current local gov employee ID, CA"))

## Local Gov OOS

local_oos_gov_id <- NA
local_oos_gov_id[data$EXPIRATION_EMPLOYEE_ID_4 == 0] <- 0
local_oos_gov_id[data$EMPLOYEE_ID_4 == 0] <- 0
local_oos_gov_id[data$EXPIRATION_EMPLOYEE_ID_4 == 1] <- 1
local_oos_gov_id[data$EMPLOYEE_ID_4 == 1] <- 2

data$local_oos_gov_id <- factor(local_oos_gov_id,
                         levels = c(0,1,2),
                         labels = c("Does not have one",
                                    "Expired local gov employee ID, OOS",
                                    "Current local gov employee ID, OOS"))


# Employee ID Summaries

employee_summary <- NA
employee_summary[fedgov_id == 0] <- 0 # No Employee ID
employee_summary[ca_gov_id == 0] <- 0 # No Employee ID
employee_summary[local_ca_gov_id == 0] <- 0 # No Employee
employee_summary[fedgov_id == 1] <- 1 # Expired Fed, CA State, or CA Local ID
employee_summary[ca_gov_id == 1] <- 1 # Expired Fed, CA State, or CA Local ID
employee_summary[local_ca_gov_id == 1] <- 1 # Expired Fed, CA State, or CA Local ID
employee_summary[fedgov_id == 2] <- 2 # Current Fed, CA State, or CA Local ID
employee_summary[ca_gov_id == 2] <- 2 # Current Fed, CA State, or CA Local ID
employee_summary[local_ca_gov_id == 2] <- 2 # Current Fed, CA State, or CA Local ID

data$employee_summary <- factor(employee_summary,
                                levels = c(0,1,2),
                                labels = c("No Gov Employee ID",
                                           "Expired Fed, CA State, or CA Local ID",
                                           "Current Fed, CA State, or CA Local ID"))

# Name Recodes


pp_name_recode <- NA
pp_name_recode[data$PASSPORT_NAME_REASON == 4] <- 4
pp_name_recode[data$PASSPORT_NAME_REASON_4_OTHER == "In the process of changing my maiden name to my married name."] <- 1
pp_name_recode[data$PASSPORT_NAME_REASON == 1] <- 1
pp_name_recode[data$PASSPORT_NAME_REASON == 2] <- 2
pp_name_recode[data$PASSPORT_NAME_REASON == 3] <- 3

data$pp_name_recode <- factor(pp_name_recode,
                              levels = c(1,2,3,4),
                              labels = c("Getting married",
                                         "Getting separated/divorced",
                                         "Using a name that corresponds with my gender identity",
                                         "Other (please explain)"))

bc_name_recode <- NA
bc_name_recode[data$BIRTH_CERTIFICATE_NAME_REASON == 4] <- 4
bc_name_recode[data$BIRTH_CERTIFICATE_NAME_REASON_4_OTHER == "Got Married"] <- 1
bc_name_recode[data$BIRTH_CERTIFICATE_NAME_REASON_4_OTHER == "Got married and changed last name"] <- 1
bc_name_recode[data$BIRTH_CERTIFICATE_NAME_REASON_4_OTHER == "Got married. Past tense"] <- 1
bc_name_recode[data$BIRTH_CERTIFICATE_NAME_REASON_4_OTHER == "Has my maiden name on it"] <- 1
bc_name_recode[data$BIRTH_CERTIFICATE_NAME_REASON_4_OTHER == "I have been married 21 years"] <- 1
bc_name_recode[data$BIRTH_CERTIFICATE_NAME_REASON_4_OTHER == "It has my birth name, not my marriage name"] <- 1
bc_name_recode[data$BIRTH_CERTIFICATE_NAME_REASON_4_OTHER == "Maiden name is on birth certificate"] <- 1
bc_name_recode[data$BIRTH_CERTIFICATE_NAME_REASON == 1] <- 1
bc_name_recode[data$BIRTH_CERTIFICATE_NAME_REASON == 2] <- 2
bc_name_recode[data$BIRTH_CERTIFICATE_NAME_REASON == 3] <- 3

data$bc_name_recode <- factor(bc_name_recode,
                              levels = c(1,2,3,4),
                              labels = c("Getting married",
                                         "Getting separated/divorced",
                                         "Using a name that corresponds with my gender identity",
                                         "Other (please explain)"))


mc_name_recode <- NA
mc_name_recode[data$MARRIAGE_CERTIFICATE_NAME_REASON == 4] <- 4
mc_name_recode[data$MARRIAGE_CERTIFICATE_NAME_REASON_4_OTHER == "Changed after I got married"] <- 1
mc_name_recode[data$MARRIAGE_CERTIFICATE_NAME_REASON_4_OTHER == "It has my maiden name on it"] <- 1
mc_name_recode[data$MARRIAGE_CERTIFICATE_NAME_REASON_4_OTHER == "maiden name"] <- 1
mc_name_recode[data$MARRIAGE_CERTIFICATE_NAME_REASON == 1] <- 1
mc_name_recode[data$MARRIAGE_CERTIFICATE_NAME_REASON == 2] <- 2
mc_name_recode[data$MARRIAGE_CERTIFICATE_NAME_REASON == 3] <- 3

data$mc_name_recode <- factor(mc_name_recode,
                              levels = c(1,2,3,4),
                              labels = c("Getting married",
                                         "Getting separated/divorced",
                                         "Using a name that corresponds with my gender identity",
                                         "Other (please explain)"))


# Passport

#id_passport
#id_passport_dummy


passport_full_summary <- NA
passport_full_summary[id_passport == 0] <- 0
passport_full_summary[id_passport == 1 & data$EXPIRED_PASSPORT_ACCESSIBLE == 2] <- 1 # Inacc. Ex. w/ wrong name
passport_full_summary[id_passport == 1 & data$EXPIRED_PASSPORT_ACCESSIBLE == 1] <- 2 # Acc. Ex. w/ wrong name
passport_full_summary[id_passport == 2 & data$EXPIRED_PASSPORT_ACCESSIBLE == 2] <- 3 # Inacc. Ex. w/ right name
passport_full_summary[id_passport == 2 & data$EXPIRED_PASSPORT_ACCESSIBLE == 1] <- 4 # Acc. Ex. w/ right name
passport_full_summary[id_passport == 3 & data$PASSPORT_ACCESSIBLE == 2] <- 5 # Inacc. Current w/ wrong name
passport_full_summary[id_passport == 3 & data$PASSPORT_ACCESSIBLE == 1] <- 6 # Acc. Current w/ wrong name
passport_full_summary[id_passport == 4 & data$PASSPORT_ACCESSIBLE == 2] <- 7 # Inacc. Current w/ right name
passport_full_summary[id_passport == 4 & data$PASSPORT_ACCESSIBLE == 1] <- 8 # Acc. Current w/ right name

data$passport_full_summary <- factor(passport_full_summary,
                                     levels = c(0,1,2,3,4,5,6,7,8),
                                     labels = c("No Passport",
                                                "Inacc. Ex. w/ wrong name",
                                                "Acc. Ex. w/ wrong name",
                                                "Inacc. Ex. w/ right name",
                                                "Acc. Ex. w/ right name",
                                                "Inacc. Current w/ wrong name",
                                                "Acc. Current w/ wrong name",
                                                "Inacc. Current w/ right name",
                                                "Acc. Current w/ right name"))




passport_3cat_unexpired <- NA
passport_3cat_unexpired[passport_full_summary == 0] <- 0
passport_3cat_unexpired[passport_full_summary == 1] <- 0
passport_3cat_unexpired[passport_full_summary == 2] <- 0
passport_3cat_unexpired[passport_full_summary == 3] <- 0
passport_3cat_unexpired[passport_full_summary == 4] <- 0
passport_3cat_unexpired[passport_full_summary == 5] <- 1
passport_3cat_unexpired[passport_full_summary == 6] <- 2
passport_3cat_unexpired[passport_full_summary == 7] <- 1
passport_3cat_unexpired[passport_full_summary == 8] <- 2

data$passport_3cat_unexpired <- factor(passport_3cat_unexpired, levels = c(0,1,2),
                        labels = c("No Passport.",
                                   "Innacc. Passport.",
                                   "Acc. Passport"))


passport_3cat_with_expired <- NA
passport_3cat_with_expired[passport_full_summary == 0] <- 0
passport_3cat_with_expired[passport_full_summary == 1] <- 1
passport_3cat_with_expired[passport_full_summary == 2] <- 2
passport_3cat_with_expired[passport_full_summary == 3] <- 1
passport_3cat_with_expired[passport_full_summary == 4] <- 2
passport_3cat_with_expired[passport_full_summary == 5] <- 1
passport_3cat_with_expired[passport_full_summary == 6] <- 2
passport_3cat_with_expired[passport_full_summary == 7] <- 1
passport_3cat_with_expired[passport_full_summary == 8] <- 2

data$passport_3cat_with_expired <- factor(passport_3cat_with_expired, levels = c(0,1,2),
                             labels = c("No Passport.",
                                        "Innacc. Passport.",
                                        "Acc. Passport"))


# 2 NA's, a "don't know" in PASSPORT and a "don't know" in PASSPORT_ACCESSIBLE

pp_name <- NA
pp_name[passport_3cat_unexpired == 0] <- 0
pp_name[passport_3cat_unexpired == 1 & data$PASSPORT_NAME == 2] <- 1
pp_name[passport_3cat_unexpired == 1 & data$PASSPORT_NAME == 1] <- 2
pp_name[passport_3cat_unexpired == 2 & data$PASSPORT_NAME == 2] <- 3
pp_name[passport_3cat_unexpired == 2 & data$PASSPORT_NAME == 1] <- 4

data$pp_name <- factor(pp_name, 
                             levels = c(0,1,2,3,4),
                             labels = c("No Passport",
                                        "Inacc. Passport w/ wrong name",
                                        "Inacc. Passport w/ right name",
                                        "Acc. Passport w/ wrong name",
                                        "Acc. Passport w/ right name"))

# NAs because of 3 "don't knows" in PASSPORT_NAME



passport_name_dummy <- NA
passport_name_dummy[pp_name == 0] <- 0
passport_name_dummy[pp_name == 1] <- 0
passport_name_dummy[pp_name == 2] <- 0
passport_name_dummy[pp_name == 3] <- 0
passport_name_dummy[pp_name == 4] <- 1

data$passport_name_dummy <- factor(passport_name_dummy,
                                   levels = c(0,1),
                                   labels = c("Does not",
                                              "Has Acc. Passport w/ right name"))



# Birth Certificate 



birth_cert <- NA
birth_cert[data$BIRTH_CERTIFICATE == 2] <- 0
birth_cert[data$BIRTH_CERTIFICATE_ACCESSIBLE == 2] <- 1
birth_cert[data$BIRTH_CERTIFICATE_ACCESSIBLE == 1] <- 2

data$birth_cert <- factor(birth_cert, levels = c(0,1,2),
                          labels = c("No Birth Certificate",
                                     "Inaccessible Birth Certificate",
                                     "Accessible Birth Certificate"))

# 2 NAs because 2 "don't know" in BIRTH_CERTIFICATE_ACCESSIBLE


birth_cert_name <- NA
birth_cert_name[birth_cert == 0] <- 0
birth_cert_name[birth_cert == 1 & data$BIRTH_CERTIFICATE_NAME == 2] <- 1
birth_cert_name[birth_cert == 1 & data$BIRTH_CERTIFICATE_NAME == 1] <- 2
birth_cert_name[birth_cert == 2 & data$BIRTH_CERTIFICATE_NAME == 2] <- 3
birth_cert_name[birth_cert == 2 & data$BIRTH_CERTIFICATE_NAME == 1] <- 4

data$birth_cert_name <- factor(birth_cert_name, 
                               levels = c(0,1,2,3,4),
                               labels = c("No Birth Certificate",
                                          "Inacc. Birth Cert. w/ wrong name",
                                          "Inacc. Birth Cert. w/ right name",
                                          "Acc. Birth Cert. w/ wrong name",
                                          "Acc. Birth Cert. w/ right name"))


birth_cert_name_dummy <- NA
birth_cert_name_dummy[birth_cert_name == 0] <- 0
birth_cert_name_dummy[birth_cert_name == 1] <- 0
birth_cert_name_dummy[birth_cert_name == 2] <- 0
birth_cert_name_dummy[birth_cert_name == 3] <- 0
birth_cert_name_dummy[birth_cert_name == 4] <- 1

data$birth_cert_name_dummy <- factor(birth_cert_name_dummy,
                                     levels = c(0,1),
                                     labels = c("Does not",
                                                "Has Acc. Birth Cert w/ right name"))

# Naturalization Certificate

nat_cert <- NA
nat_cert[data$NATURALIZATION_CERTIFICATE == 2] <- 0
nat_cert[data$NATURALIZATION_CERTIFICATE_ACCESSIBLE == 2] <- 1
nat_cert[data$NATURALIZATION_CERTIFICATE_ACCESSIBLE == 1] <- 2

data$nat_cert <- factor(nat_cert, levels = c(0,1,2),
                        labels = c("No Nat. Cert.",
                                   "Innacc. Nat. Cert.",
                                   "Acc. Nat. Cert"))

nat_cert_name <- NA
nat_cert_name[nat_cert == 0] <- 0
nat_cert_name[nat_cert == 1 & data$NATURALIZATION_CERTIFICATE_NAME == 2] <- 1
nat_cert_name[nat_cert == 1 & data$NATURALIZATION_CERTIFICATE_NAME == 1] <- 2
nat_cert_name[nat_cert == 2 & data$NATURALIZATION_CERTIFICATE_NAME == 2] <- 3
nat_cert_name[nat_cert == 2 & data$NATURALIZATION_CERTIFICATE_NAME == 1] <- 4

data$nat_cert_name <- factor(nat_cert_name, 
                             levels = c(0,1,2,3,4),
                             labels = c("No Nat. Cert.",
                                        "Inacc. Nat. Cert. w/ wrong name",
                                        "Inacc. Nat. Cert. w/ right name",
                                        "Acc. Nat. Cert. w/ wrong name",
                                        "Acc. Nat. Cert. w/ right name"))
# 1239 NAs because of citizens


nat_cert_name_dummy <- NA
nat_cert_name_dummy[nat_cert_name == 0] <- 0
nat_cert_name_dummy[nat_cert_name == 1] <- 0
nat_cert_name_dummy[nat_cert_name == 2] <- 0
nat_cert_name_dummy[nat_cert_name == 3] <- 0
nat_cert_name_dummy[nat_cert_name == 4] <- 1

data$nat_cert_name_dummy <- factor(nat_cert_name_dummy,
                                   levels = c(0,1),
                                   labels = c("Does not",
                                              "Has Acc. Nat. Cert w/ right name"))

# Certificate of Citizenship 


citi_cert <- NA
citi_cert[data$CERTIFICATE_OF_CITIZENSHIP == 2] <- 0
citi_cert[data$CERTIFICATE_OF_CITIZENSHIP_ACCESSIBLE == 2] <- 1
citi_cert[data$CERTIFICATE_OF_CITIZENSHIP_ACCESSIBLE == 1] <- 2

data$citi_cert <- factor(citi_cert, levels = c(0,1,2),
                         labels = c("No Cert. of Citizenship",
                                    "Innacc.Cert. of Citizenship",
                                    "Acc. Cert. of Citizenship"))

citi_cert_name <- NA
citi_cert_name[citi_cert == 0] <- 0
citi_cert_name[citi_cert == 1 & data$CERTIFICATE_OF_CITIZENSHIP_NAME == 2] <- 1
citi_cert_name[citi_cert == 1 & data$CERTIFICATE_OF_CITIZENSHIP_NAME == 1] <- 2
citi_cert_name[citi_cert == 2 & data$CERTIFICATE_OF_CITIZENSHIP_NAME == 2] <- 3
citi_cert_name[citi_cert == 2 & data$CERTIFICATE_OF_CITIZENSHIP_NAME == 1] <- 4

data$citi_cert_name <- factor(citi_cert_name, 
                              levels = c(0,1,2,3,4),
                              labels = c("No Cert. of Citizenship",
                                         "Inacc. Cert. of Citizenship w/ wrong name",
                                         "Inacc. Cert. of Citizenship w/ right name",
                                         "Acc. Cert. of Citizenship w/ wrong name",
                                         "Acc. Cert. of Citizenship w/ right name"))



citi_cert_name_dummy <- NA
citi_cert_name_dummy[citi_cert_name == 0] <- 0
citi_cert_name_dummy[citi_cert_name == 1] <- 0
citi_cert_name_dummy[citi_cert_name == 2] <- 0
citi_cert_name_dummy[citi_cert_name == 3] <- 0
citi_cert_name_dummy[citi_cert_name == 4] <- 1

data$citi_cert_name_dummy <- factor(citi_cert_name_dummy,
                                    levels = c(0,1),
                                    labels = c("Does not",
                                               "Has Acc. Cert. of Citizenship w/ right name"))
# 1494 NAs because question doesn't apply



# DPOC Summary

dpoc_overall_dummy <- NA
dpoc_overall_dummy[passport_name_dummy == 0] <- 0 
dpoc_overall_dummy[birth_cert_name_dummy == 0] <- 0 
dpoc_overall_dummy[nat_cert_name_dummy == 0] <- 0 
dpoc_overall_dummy[citi_cert_name_dummy == 0] <- 0 
dpoc_overall_dummy[passport_name_dummy == 1] <- 1 
dpoc_overall_dummy[birth_cert_name_dummy == 1] <- 1 
dpoc_overall_dummy[nat_cert_name_dummy == 1] <- 1 
dpoc_overall_dummy[citi_cert_name_dummy == 1] <- 1

data$dpoc_overall_dummy <- factor(dpoc_overall_dummy,
                                  levels = c(0,1),
                                  labels = c("No DPOC",
                                             "Accessible DPOC w/ right name"))

# No NA's!


dpoc_summary <- NA
dpoc_summary[pp_name == 0] <- 0 # Does not have one
dpoc_summary[birth_cert_name == 0] <- 0 
dpoc_summary[nat_cert_name == 0] <- 0
dpoc_summary[citi_cert_name == 0] <- 0
dpoc_summary[pp_name == 1] <- 1 # Inacc. wrong name
dpoc_summary[birth_cert_name == 1] <- 1
dpoc_summary[nat_cert_name == 1] <- 1
dpoc_summary[citi_cert_name == 1] <- 1
dpoc_summary[pp_name == 2] <- 2 # Inacc. right name
dpoc_summary[birth_cert_name == 2] <- 2
dpoc_summary[nat_cert_name == 2] <- 2
dpoc_summary[citi_cert_name == 2] <- 2
dpoc_summary[pp_name == 3] <- 3 # Acc. wrong name
dpoc_summary[birth_cert_name == 3] <- 3
dpoc_summary[nat_cert_name == 3] <- 3
dpoc_summary[citi_cert_name == 3] <- 3
dpoc_summary[pp_name == 4] <- 4 # Acc. right name
dpoc_summary[birth_cert_name == 4] <- 4
dpoc_summary[nat_cert_name == 4] <- 4
dpoc_summary[citi_cert_name == 4] <- 4

data$dpoc_summary <- factor(dpoc_summary,
                            levels = c(0,1,2,3,4),
                            labels = c("No DPOC",
                                       "Inacc. DPOC w/ wrong name",
                                       "Inacc. DPOC w/ right name",
                                       "Acc. DPOC w/ wrong name",
                                       "Acc. DPOC w/ right name"))


dpoc_name <- NA
dpoc_name[pp_name == 0] <- 0 # None
dpoc_name[birth_cert_name == 0] <- 0
dpoc_name[nat_cert_name == 0] <- 0
dpoc_name[citi_cert_name == 0] <- 0
dpoc_name[pp_name == 1] <- 1 # Wrong name
dpoc_name[birth_cert_name == 1] <- 1
dpoc_name[nat_cert_name == 1] <- 1
dpoc_name[citi_cert_name == 1] <- 1
dpoc_name[pp_name == 2] <- 2 # Right name
dpoc_name[birth_cert_name == 2] <- 2
dpoc_name[nat_cert_name == 2] <- 2
dpoc_name[citi_cert_name == 2] <- 2
dpoc_name[pp_name == 3] <- 1 # Wrong name
dpoc_name[birth_cert_name == 3] <- 1
dpoc_name[nat_cert_name == 3] <- 1
dpoc_name[citi_cert_name == 3] <- 1
dpoc_name[pp_name == 4] <- 2 # Right name
dpoc_name[birth_cert_name == 4] <- 2
dpoc_name[nat_cert_name == 4] <- 2
dpoc_name[citi_cert_name == 4] <- 2

data$dpoc_name <- factor(dpoc_name,
                         levels = c(0,1,2),
                         labels = c("No DPOC",
                                    "DPOC with wrong name",
                                    "DPOC with right name"))


dpoc_acc <- NA
dpoc_acc[pp_name == 0] <- 0 # None
dpoc_acc[birth_cert_name == 0] <- 0
dpoc_acc[nat_cert_name == 0] <- 0
dpoc_acc[citi_cert_name == 0] <- 0
dpoc_acc[pp_name == 1] <- 1 # Inacc
dpoc_acc[birth_cert_name == 1] <- 1
dpoc_acc[nat_cert_name == 1] <- 1
dpoc_acc[citi_cert_name == 1] <- 1
dpoc_acc[pp_name == 2] <- 1 # Inacc
dpoc_acc[birth_cert_name == 2] <- 1
dpoc_acc[nat_cert_name == 2] <- 1
dpoc_acc[citi_cert_name == 2] <- 1
dpoc_acc[pp_name == 3] <- 2 # Acc
dpoc_acc[birth_cert_name == 3] <- 2
dpoc_acc[nat_cert_name == 3] <- 2
dpoc_acc[citi_cert_name == 3] <- 2
dpoc_acc[pp_name == 4] <- 2 # Acc
dpoc_acc[birth_cert_name == 4] <- 2
dpoc_acc[nat_cert_name == 4] <- 2
dpoc_acc[citi_cert_name == 4] <- 2


data$dpoc_acc <- factor(dpoc_acc,
                        levels = c(0,1,2),
                        labels = c("No DPOC",
                                   "Inacc. DPOC",
                                   "Acc. DPOC"))

dpoc_problems <- NA
dpoc_problems[pp_name == 0] <- 0 # None
dpoc_problems[birth_cert_name == 0] <- 0
dpoc_problems[nat_cert_name == 0] <- 0
dpoc_problems[citi_cert_name == 0] <- 0
dpoc_problems[pp_name == 1] <- 1 # problem
dpoc_problems[birth_cert_name == 1] <- 1
dpoc_problems[nat_cert_name == 1] <- 1
dpoc_problems[citi_cert_name == 1] <- 1
dpoc_problems[pp_name == 2] <- 1 # problem
dpoc_problems[birth_cert_name == 2] <- 1
dpoc_problems[nat_cert_name == 2] <- 1
dpoc_problems[citi_cert_name == 2] <- 1
dpoc_problems[pp_name == 3] <- 1 # problem
dpoc_problems[birth_cert_name == 3] <- 1
dpoc_problems[nat_cert_name == 3] <- 1
dpoc_problems[citi_cert_name == 3] <- 1
dpoc_problems[pp_name == 4] <- 2 # Acc
dpoc_problems[birth_cert_name == 4] <- 2
dpoc_problems[nat_cert_name == 4] <- 2
dpoc_problems[citi_cert_name == 4] <- 2

data$dpoc_problems <- factor(dpoc_problems,
                             levels = c(0,1,2),
                             labels = c("No DPOC",
                                        "Problematic DPOC",
                                        "DPOC"))


# Counting Expired Passports

pp_name_with_expired <- NA
pp_name_with_expired[passport_3cat_with_expired == 0] <- 0
pp_name_with_expired[passport_3cat_with_expired == 1 & data$PASSPORT_NAME == 2] <- 1
pp_name_with_expired[passport_3cat_with_expired == 1 & data$PASSPORT_NAME == 1] <- 2
pp_name_with_expired[passport_3cat_with_expired == 2 & data$PASSPORT_NAME == 2] <- 3
pp_name_with_expired[passport_3cat_with_expired == 2 & data$PASSPORT_NAME == 1] <- 4

data$pp_name_with_expired <- factor(pp_name_with_expired, 
                       levels = c(0,1,2,3,4),
                       labels = c("No Passport",
                                  "Inacc. Passport w/ wrong name",
                                  "Inacc. Passport w/ right name",
                                  "Acc. Passport w/ wrong name",
                                  "Acc. Passport w/ right name"))

# NAs because of 3 "don't knows" in PASSPORT_NAME



passport_name_dummy_with_expired <- NA
passport_name_dummy_with_expired[pp_name_with_expired == 0] <- 0
passport_name_dummy_with_expired[pp_name_with_expired == 1] <- 0
passport_name_dummy_with_expired[pp_name_with_expired == 2] <- 0
passport_name_dummy_with_expired[pp_name_with_expired == 3] <- 0
passport_name_dummy_with_expired[pp_name_with_expired == 4] <- 1

data$passport_name_dummy_with_expired <- factor(passport_name_dummy_with_expired,
                                   levels = c(0,1),
                                   labels = c("Does not",
                                              "Has Acc. Passport w/ right name"))

# DPOC Summary

dpoc_overall_dummy_with_expired <- NA
dpoc_overall_dummy_with_expired[passport_name_dummy_with_expired == 0] <- 0 
dpoc_overall_dummy_with_expired[birth_cert_name_dummy == 0] <- 0 
dpoc_overall_dummy_with_expired[nat_cert_name_dummy == 0] <- 0 
dpoc_overall_dummy_with_expired[citi_cert_name_dummy == 0] <- 0 
dpoc_overall_dummy_with_expired[passport_name_dummy_with_expired == 1] <- 1 
dpoc_overall_dummy_with_expired[birth_cert_name_dummy == 1] <- 1 
dpoc_overall_dummy_with_expired[nat_cert_name_dummy == 1] <- 1 
dpoc_overall_dummy_with_expired[citi_cert_name_dummy == 1] <- 1

data$dpoc_overall_dummy_with_expired <- factor(dpoc_overall_dummy_with_expired,
                                  levels = c(0,1),
                                  labels = c("No DPOC",
                                             "Accessible DPOC w/ right name"))

# No NA's!


dpoc_summary_with_expired <- NA
dpoc_summary_with_expired[pp_name_with_expired == 0] <- 0 # Does not have one
dpoc_summary_with_expired[birth_cert_name == 0] <- 0 
dpoc_summary_with_expired[nat_cert_name == 0] <- 0
dpoc_summary_with_expired[citi_cert_name == 0] <- 0
dpoc_summary_with_expired[pp_name_with_expired == 1] <- 1 # Inacc. wrong name
dpoc_summary_with_expired[birth_cert_name == 1] <- 1
dpoc_summary_with_expired[nat_cert_name == 1] <- 1
dpoc_summary_with_expired[citi_cert_name == 1] <- 1
dpoc_summary_with_expired[pp_name_with_expired == 2] <- 2 # Inacc. right name
dpoc_summary_with_expired[birth_cert_name == 2] <- 2
dpoc_summary_with_expired[nat_cert_name == 2] <- 2
dpoc_summary_with_expired[citi_cert_name == 2] <- 2
dpoc_summary_with_expired[pp_name_with_expired == 3] <- 3 # Acc. wrong name
dpoc_summary_with_expired[birth_cert_name == 3] <- 3
dpoc_summary_with_expired[nat_cert_name == 3] <- 3
dpoc_summary_with_expired[citi_cert_name == 3] <- 3
dpoc_summary_with_expired[pp_name_with_expired == 4] <- 4 # Acc. right name
dpoc_summary_with_expired[birth_cert_name == 4] <- 4
dpoc_summary_with_expired[nat_cert_name == 4] <- 4
dpoc_summary_with_expired[citi_cert_name == 4] <- 4

data$dpoc_summary_with_expired <- factor(dpoc_summary_with_expired,
                            levels = c(0,1,2,3,4),
                            labels = c("No DPOC",
                                       "Inacc. DPOC w/ wrong name",
                                       "Inacc. DPOC w/ right name",
                                       "Acc. DPOC w/ wrong name",
                                       "Acc. DPOC w/ right name"))


dpoc_name_with_expired <- NA
dpoc_name_with_expired[pp_name_with_expired == 0] <- 0 # None
dpoc_name_with_expired[birth_cert_name == 0] <- 0
dpoc_name_with_expired[nat_cert_name == 0] <- 0
dpoc_name_with_expired[citi_cert_name == 0] <- 0
dpoc_name_with_expired[pp_name_with_expired == 1] <- 1 # Wrong name
dpoc_name_with_expired[birth_cert_name == 1] <- 1
dpoc_name_with_expired[nat_cert_name == 1] <- 1
dpoc_name_with_expired[citi_cert_name == 1] <- 1
dpoc_name_with_expired[pp_name_with_expired == 2] <- 2 # Right name
dpoc_name_with_expired[birth_cert_name == 2] <- 2
dpoc_name_with_expired[nat_cert_name == 2] <- 2
dpoc_name_with_expired[citi_cert_name == 2] <- 2
dpoc_name_with_expired[pp_name_with_expired == 3] <- 1 # Wrong name
dpoc_name_with_expired[birth_cert_name == 3] <- 1
dpoc_name_with_expired[nat_cert_name == 3] <- 1
dpoc_name_with_expired[citi_cert_name == 3] <- 1
dpoc_name_with_expired[pp_name_with_expired == 4] <- 2 # Right name
dpoc_name_with_expired[birth_cert_name == 4] <- 2
dpoc_name_with_expired[nat_cert_name == 4] <- 2
dpoc_name_with_expired[citi_cert_name == 4] <- 2

data$dpoc_name_with_expired <- factor(dpoc_name_with_expired,
                         levels = c(0,1,2),
                         labels = c("No DPOC",
                                    "DPOC with wrong name",
                                    "DPOC with right name"))


dpoc_acc_with_expired <- NA
dpoc_acc_with_expired[pp_name_with_expired == 0] <- 0 # None
dpoc_acc_with_expired[birth_cert_name == 0] <- 0
dpoc_acc_with_expired[nat_cert_name == 0] <- 0
dpoc_acc_with_expired[citi_cert_name == 0] <- 0
dpoc_acc_with_expired[pp_name_with_expired == 1] <- 1 # Inacc
dpoc_acc_with_expired[birth_cert_name == 1] <- 1
dpoc_acc_with_expired[nat_cert_name == 1] <- 1
dpoc_acc_with_expired[citi_cert_name == 1] <- 1
dpoc_acc_with_expired[pp_name_with_expired == 2] <- 1 # Inacc
dpoc_acc_with_expired[birth_cert_name == 2] <- 1
dpoc_acc_with_expired[nat_cert_name == 2] <- 1
dpoc_acc_with_expired[citi_cert_name == 2] <- 1
dpoc_acc_with_expired[pp_name_with_expired == 3] <- 2 # Acc
dpoc_acc_with_expired[birth_cert_name == 3] <- 2
dpoc_acc_with_expired[nat_cert_name == 3] <- 2
dpoc_acc_with_expired[citi_cert_name == 3] <- 2
dpoc_acc_with_expired[pp_name_with_expired == 4] <- 2 # Acc
dpoc_acc_with_expired[birth_cert_name == 4] <- 2
dpoc_acc_with_expired[nat_cert_name == 4] <- 2
dpoc_acc_with_expired[citi_cert_name == 4] <- 2


data$dpoc_acc_with_expired <- factor(dpoc_acc_with_expired,
                        levels = c(0,1,2),
                        labels = c("No DPOC",
                                   "Inacc. DPOC",
                                   "Acc. DPOC"))

dpoc_problems_with_expired <- NA
dpoc_problems_with_expired[pp_name == 0] <- 0 # None
dpoc_problems_with_expired[birth_cert_name == 0] <- 0
dpoc_problems_with_expired[nat_cert_name == 0] <- 0
dpoc_problems_with_expired[citi_cert_name == 0] <- 0
dpoc_problems_with_expired[pp_name == 1] <- 1 # problem
dpoc_problems_with_expired[birth_cert_name == 1] <- 1
dpoc_problems_with_expired[nat_cert_name == 1] <- 1
dpoc_problems_with_expired[citi_cert_name == 1] <- 1
dpoc_problems_with_expired[pp_name == 2] <- 1 # problem
dpoc_problems_with_expired[birth_cert_name == 2] <- 1
dpoc_problems_with_expired[nat_cert_name == 2] <- 1
dpoc_problems_with_expired[citi_cert_name == 2] <- 1
dpoc_problems_with_expired[pp_name == 3] <- 1 # problem
dpoc_problems_with_expired[birth_cert_name == 3] <- 1
dpoc_problems_with_expired[nat_cert_name == 3] <- 1
dpoc_problems_with_expired[citi_cert_name == 3] <- 1
dpoc_problems_with_expired[pp_name == 4] <- 2 # Acc
dpoc_problems_with_expired[birth_cert_name == 4] <- 2
dpoc_problems_with_expired[nat_cert_name == 4] <- 2
dpoc_problems_with_expired[citi_cert_name == 4] <- 2

data$dpoc_problems_with_expired <- factor(dpoc_problems_with_expired,
                             levels = c(0,1,2),
                             labels = c("No DPOC",
                                        "Problematic DPOC",
                                        "DPOC"))

no_photoID <- NA
no_photoID[data$DRIVERS_LICENSE == 5] <- 0
no_photoID[data$STATE_ID == 5] <- 0
no_photoID[data$PASSPORT == 2] <- 0
no_photoID[data$EXPIRATION_PASSPORT == 2] <- 0
no_photoID[id_military == 0] <- 0
no_photoID[id_veterans == 0] <- 0
no_photoID[studentID3 == 0] <- 0
no_photoID[id_tribal == 0] <- 0
no_photoID[id_hunting == 0] <- 0
no_photoID[id_weapons == 0] <- 0
no_photoID[employee_summary == 0] <- 0
no_photoID[data$DRIVERS_LICENSE != 5] <- 1
no_photoID[data$STATE_ID != 5] <- 1
no_photoID[data$PASSPORT != 2] <- 1
no_photoID[data$EXPIRATION_PASSPORT != 2] <- 1
no_photoID[id_military != 0] <- 1
no_photoID[id_veterans != 0] <- 1
no_photoID[studentID3 != 0] <- 1
no_photoID[id_tribal != 0] <- 1
no_photoID[id_hunting != 0] <- 1
no_photoID[id_weapons != 0] <- 1
no_photoID[employee_summary != 0] <- 1



table(no_photoID, useNA = "ifany")

write_sav(data, "ca photo id variables and dpoc v3.sav")


no_photoID_old <- NA
no_photoID_old[data$DRIVERS_LICENSE == 5] <- 0
no_photoID_old[data$STATE_ID == 5] <- 0
no_photoID_old[data$PASSPORT == 2] <- 0
no_photoID_old[data$EXPIRATION_PASSPORT == 2] <- 0
no_photoID_old[id_military == 0] <- 0
no_photoID_old[id_veterans == 0] <- 0
no_photoID_old[studentID3 == 0] <- 0
no_photoID_old[id_tribal == 0] <- 0
no_photoID_old[id_hunting == 0] <- 0
no_photoID_old[id_weapons == 0] <- 0
no_photoID_old[employee_summary == 0] <- 0

no_photoID_old[data$DRIVERS_LICENSE != 5] <- 1
no_photoID_old[data$STATE_ID != 5] <- 1
no_photoID_old[data$PASSPORT != 2] <- 1
no_photoID_old[data$EXPIRATION_PASSPORT != 2] <- 1
no_photoID_old[id_military != 0] <- 1
no_photoID_old[id_veterans != 0] <- 1
no_photoID_old[studentID3 != 0] <- 1
no_photoID_old[id_tribal != 0] <- 1
no_photoID_old[id_hunting != 0] <- 1
no_photoID_old[id_weapons != 0] <- 1
no_photoID_old[employee_summary != 0] <- 1







# Step 1: create logical indicators for each ID type
has_dl        <- ifelse(is.na(data$DRIVERS_LICENSE), NA, data$DRIVERS_LICENSE != 5)
has_stateid   <- ifelse(is.na(data$STATE_ID), NA, data$STATE_ID != 5)
has_passport  <- ifelse(is.na(data$PASSPORT), NA, data$PASSPORT != 2)
has_military  <- ifelse(is.na(id_military), NA, id_military != 0)
has_veterans  <- ifelse(is.na(id_veterans), NA, id_veterans != 0)
has_student   <- ifelse(is.na(studentID3), NA, studentID3 != 0)
has_tribal    <- ifelse(is.na(id_tribal), NA, id_tribal != 0)
has_hunting   <- ifelse(is.na(id_hunting), NA, id_hunting != 0)
has_weapons   <- ifelse(is.na(id_weapons), NA, id_weapons != 0)
has_employee  <- ifelse(is.na(employee_summary), NA, employee_summary != 0)

# Step 2: combine them
has_any_photoID <- has_dl |
  has_stateid |
  has_passport |
  has_military |
  has_veterans |
  has_student |
  has_tribal |
  has_hunting |
  has_weapons |
  has_employee

# Step 3: convert to final variable
no_photoID <- ifelse(is.na(has_any_photoID), NA,
                     ifelse(has_any_photoID, 1, 0))

data$no_photoID <- no_photoID


table(data$no_photoID, useNA = "ifany")
prop.table(table(data$no_photoID, useNA = "ifany"))




table(data$DRIVERS_LICENSE, useNA="ifany")
table(data$STATE_ID, useNA="ifany")
table(data$PASSPORT, useNA="ifany")
table(data$EXPIRATION_PASSPORT, useNA="ifany")


data$no_photoID_old <- no_photoID_old
table(data$no_photoID_old, data$no_photoID, useNA="ifany")


# table(data$no_photoID, useNA = "ifany")
#0    1 
#7 1554 
#I think i picked up two more due to passport categorization






# Valid IDs (current)
# Valid IDs = any current/non-expired ID
has_valid_id <-
  license_details %in% c(2, 5, 6) |
  ca_stateID_details %in% c(2, 5, 6) |
  id_passport %in% c(3, 4) |
  id_military == 2 |
  id_veterans == 2 |
  id_tribal == 2 |
  id_hunting %in% c(3, 4) |
  id_weapons %in% c(3, 4) |
  employee_summary == 2 |
  local_oos_gov_id == 2 |
  studentID3 != 0

# Expired IDs = has ID, but only expired categories
has_expired_id <-
  license_details %in% c(1, 3, 4) |
  ca_stateID_details %in% c(1, 3, 4) |
  id_passport %in% c(1, 2) |
  id_military == 1 |
  id_veterans == 1 |
  id_tribal == 1 |
  id_hunting %in% c(1, 2) |
  id_weapons %in% c(1, 2) |
  employee_summary == 1 |
  local_oos_gov_id == 1

id_status <- NA

id_status[has_valid_id] <- 2
id_status[!has_valid_id & has_expired_id] <- 1
id_status[!has_valid_id & !has_expired_id] <- 0

data$id_status <- factor(
  id_status,
  levels = c(0, 1, 2),
  labels = c("No ID", "Only expired ID", "Has valid ID")
)

data %>%
  filter(id_status == "No ID") %>%
  count(license_details, ca_stateID_details, id_passport,
        id_hunting, id_weapons, studentID3)

table(data$id_status, useNA = "ifany")
prop.table(table(data$id_status))



