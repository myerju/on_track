#Load/Install Packages
#install.packages("janitor")
#install.packages('plyr')
library(tidyr)
library(janitor)
library(dplyr)
library(plyr)

#The line below reads the data, and changes empty values to NA
data <- read.csv("/Users/alexsalem/Documents/ontrack/ontrack.csv", na.strings = c(NA, ""))

#These lines change the data type of the mother and middle columns to character--necessary for
#some of the later functions
data$mother <- as.character(data$mother)
data$sp_mother <- as.character(data$sp_mother)
data$middle <- as.character(data$middle)
data$sp_middle <- as.character(data$sp_middle)

#These lines separate the data into arms by the survey--i.e. beginning of the year, end of the year, etc
arm_1 <- data %>% 
  filter(redcap_event_name == "beginning_of_the_y_arm_1") 
arm_2 <- data %>%
  filter(redcap_event_name == "end_of_the_year_arm_2")
arm_3 <- data %>%
  filter(redcap_event_name == "end_of_visit_arm_3")
arm_4 <- data %>%
 filter(redcap_event_name == "end_of_middle_scho_arm_4")

#This takes the Spanish data for mother, middle, dob and adds it to corr_mother, corr_middle, and corr_dob columns
combine_spanish_data <- function(arm){
  arm <- arm %>%
    mutate(corr_mother = ifelse(spanish==1, sp_mother, mother),
           corr_middle = ifelse(spanish==1, sp_middle, middle),
           corr_dob = ifelse(spanish==1, sp_dob, dob))
}

#Changing lowercase to uppercase function:
lower_to_upper <- function(arm) {
  arm <- arm %>%
    mutate(corr_mother = toupper(mother)) %>%
    mutate(corr_middle = toupper(middle))
}

#The below function corrects the mother and middle ids. If they are longer than 2, we just 
#take the first two letters. If they are only 1 character, we append an X to the value.
#If they are NA, we make it an xx. 
full_name_correction <- function(arm) {
  arm <- arm %>%
    mutate(corr_mother = ifelse(nchar(corr_mother) >2, substr(corr_mother, 1, 2), corr_mother)) %>%
    mutate(corr_mother = ifelse(nchar(corr_mother) ==1, paste(corr_mother, "X", sep = ""), corr_mother)) %>%
    mutate(corr_mother = ifelse(is.na(corr_mother), "XX", corr_mother)) %>%
    mutate(corr_middle = ifelse(nchar(corr_middle) >2, substr(corr_middle, 1, 2), corr_middle)) %>%
    mutate(corr_middle = ifelse(nchar(corr_middle) ==1, paste(corr_middle, "X", sep = ""), corr_middle)) %>%
    mutate(corr_middle = ifelse(is.na(corr_middle), "XX", corr_middle))
}

#Fixing 'XX' to XX:
xx_fix <- function(arm) {
  arm <- arm %>%
    mutate(corr_mother = ifelse(corr_mother == "'XX'", "XX", corr_mother)) %>%
    mutate(corr_middle = ifelse(corr_middle == "'XX'", "XX", corr_middle)) 
}

#Changing DOB like 9 to 09:
fix_dob <- function(arm) {
  arm <- arm %>%
    mutate(corr_dob = toString(dob)) %>%
    mutate(corr_dob = ifelse(nchar(dob) == 1, paste("0", dob, sep = ""), dob)) 
}

#Combine main three identifiers to create study ID
make_id <- function(arm) {
  arm <- arm %>%
    mutate(id = ifelse(is.na(dob) & is.na(mother) & is.na(middle), NA, paste(corr_dob, corr_mother, corr_middle, sep = "")))
}

#Create community data from school data
community <- function(arm) {
  arm <- arm %>%
    mutate(community = ifelse((ms_school %in% c(1, 4,5,7:10) | hs_school %in% c(1, 4,5,7:10)), 1,
                              ifelse((ms_school %in% c(3, 12) | hs_school %in% c(3, 12)), 2,
                                     ifelse((ms_school %in% c(2, 6, 11) | hs_school %in% c(2, 6, 11)), 3, NA))))
}

#Did we correct any of the information for main identifier columns?
corrected <- function(arm) {
  arm <- arm %>%
    mutate(mother_corrected = ifelse(mother==corr_mother, 0, 1)) %>%
    mutate(middle_corrected = ifelse(middle==corr_middle, 0, 1)) %>%
    mutate(dob_corrected = ifelse(dob==corr_dob, 0, 1))
}

#Executing fuctions for each arm:

#arm_1:
tidy_arm_1 <- fix_dob(arm_1)
tidy_arm_1<- combine_spanish_data(tidy_arm_1)
tidy_arm_1 <- lower_to_upper(tidy_arm_1)
tidy_arm_1 <- xx_fix(tidy_arm_1)
tidy_arm_1 <- community(tidy_arm_1)
tidy_arm_1<- full_name_correction(tidy_arm_1)
tidy_arm_1 <- make_id(tidy_arm_1)
tidy_arm_1 <- corrected(tidy_arm_1)


#arm_2:
tidy_arm_2 <- fix_dob(arm_2)
tidy_arm_2<- combine_spanish_data(tidy_arm_2)
tidy_arm_2 <- lower_to_upper(tidy_arm_2)
tidy_arm_2 <- xx_fix(tidy_arm_2)
tidy_arm_2 <- community(tidy_arm_2)
tidy_arm_2<- full_name_correction(tidy_arm_2)
tidy_arm_2 <- make_id(tidy_arm_2)
tidy_arm_2 <- corrected(tidy_arm_2)

#arm_3:
tidy_arm_3 <- fix_dob(arm_3)
tidy_arm_3<- combine_spanish_data(tidy_arm_3)
tidy_arm_3 <- lower_to_upper(tidy_arm_3)
tidy_arm_3 <- xx_fix(tidy_arm_3)
tidy_arm_3 <- community(tidy_arm_3)
tidy_arm_3<- full_name_correction(tidy_arm_3)
tidy_arm_3 <- make_id(tidy_arm_3)
tidy_arm_3 <- corrected(tidy_arm_3)

#arm_4:
tidy_arm_4 <- fix_dob(arm_4)
tidy_arm_4<- combine_spanish_data(tidy_arm_4)
tidy_arm_4 <- lower_to_upper(tidy_arm_4)
tidy_arm_4 <- xx_fix(tidy_arm_4)
tidy_arm_4 <- community(tidy_arm_4)
tidy_arm_4<- full_name_correction(tidy_arm_4)
tidy_arm_4 <- make_id(tidy_arm_4)
tidy_arm_4 <- corrected(tidy_arm_4)

