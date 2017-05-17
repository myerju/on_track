#Load/Install Packages
#install.packages("janitor")
#install.packages('plyr')
library(tidyr)
library(janitor)
library(dplyr)
library(plyr)

#REDCap to R

data <- read.csv("../Desktop/ontrack/ontrack.csv", na.strings = c(NA, ""))

#Cleaning and Tidying


#1. change 'XX' to NA
#2. fix dob to be 2 numbers
#3. pass to next stage if id has correct format
#4. add a flag column to indicate data was changed in an above correction.
#5. Create problem data set-- all problem rows so we identify what to look for. 
  #starting with XX, 'xx', NA, missing, Hh-Hh, full name (centrell), repeated rows... 


#pre_survey <- data %>%
#  select(contains("pre"))

#male <- data %>%
#  filter(gender == 1)

summary(data$redcap_event_name) #This is taking a summary of the redcap_event_name data)

#Below is a bunch of messing around with functions stuff
#number_rows <- data %>%
#  select(redcap_event_name) %>% 
#  nrow() 
  #summary()

#changeXX <- function(data, column_name){
#  for (i in 1:number_rows){
#    if (data[i, column_name] == "xx") {
#      data[i, column_name] <- NA
#    }
#  }  
#}

#trial <-changeXX(data, "beginning_of_the_y_arm_1")



arm_1 <- data %>% 
  filter(redcap_event_name == "beginning_of_the_y_arm_1") %>%
  select(dob, mother, middle, gender, grade, ms_school, hs_school)
  
arm_2 <- data %>%
  filter(redcap_event_name == "end_of_the_year_arm_2") %>%
  select(dob, mother, middle, gender, grade, ms_school, hs_school)
  
#arm_3 <- data %>%
 # filter(redcap_event_name == "end_of_visit_arm_3") %>%
 # select(dob, mother, middle)

#arm_4 <- data %>%
# filter(redcap_event_name == "end_of_middle_scho_arm_4") %>%
# select(dob, mother, middle)

#Making changing factor to character function:
change_character <- function(arm){
  arm$mother <- as.character(arm$mother)
  arm$middle <- as.character(arm$middle)
}

#Change mother and middle values from factor to character so we can use ifelse
#arm_1$mother <- as.character(arm_1$mother)
#arm_1$middle <- as.character(arm_1$middle)


#Making changing factor to lowercase to uppercase function:
lower_to_upper <- function(arm) {
  arm <- arm %>%
    mutate(corr_mother = toupper(mother)) %>%
    mutate(corr_middle = toupper(middle))
}


#Change lowercase to uppercase:
#tidy_arm_1<- arm_1 %>%
#  mutate(corr_mother = toupper(mother)) %>%
#  mutate(corr_middle = toupper(middle))

#full_name_correction <- function(arm) {
#  arm <- arm %>%
#    mutate(corr_mother = ifelse(nchar(corr_mother)!=2, NA, corr_mother)) %>%
#    mutate(corr_middle = ifelse(nchar(corr_middle) !=2, NA, corr_middle))
#}

#The below function corrects the mother and middle ids. If they are longer than 2, we just 
#take the first two letters. If they are only 1 character, we append an X to the value.
#If they are na, we make it an xx. (ask Julianne about this)
full_name_correction2 <- function(arm) {
  arm <- arm %>%
    mutate(corr_mother = ifelse(nchar(corr_mother) >2, substr(corr_mother, 1, 2), corr_mother)) %>%
    mutate(corr_mother = ifelse(nchar(corr_mother) ==1, paste(corr_mother, "X", sep = ""), corr_mother)) %>%
    mutate(corr_mother = ifelse(is.na(corr_mother), "XX", corr_mother)) %>%
    mutate(corr_middle = ifelse(nchar(corr_middle) >2, substr(corr_middle, 1, 2), corr_middle)) %>%
    mutate(corr_middle = ifelse(nchar(corr_middle) ==1, paste(corr_middle, "X", sep = ""), corr_middle)) %>%
    mutate(corr_middle = ifelse(is.na(corr_middle), "XX", corr_middle))
}


#Changing values longer than 2 letters to NA:
#tidy_arm_1 <- tidy_arm_1 %>%
#  mutate(corr_mother = ifelse(nchar(corr_mother)!=2, NA, corr_mother)) %>%
#  mutate(corr_middle = ifelse(nchar(corr_middle) !=2, NA, corr_middle))


#Julianne's code:
#tidy_arm_1 <- arm_1 %>%
#  mutate_each(funs(corr = toupper), c(mother, middle))

#xx_to_na <- function(arm) {
#  arm <- arm %>%
#    mutate(corr_mother = ifelse(corr_mother == "XX", NA, corr_mother)) %>%
#    mutate(corr_middle = ifelse(corr_middle == "XX", NA, corr_middle))
#}


#Fixing 'XX' to XX:
xx_to_na <- function(arm) {
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
 

make_id <- function(arm) {
  arm <- arm %>%
    mutate(id = ifelse(is.na(corr_dob) | is.na(corr_mother) | is.na(corr_middle), NA, paste(corr_dob, corr_mother, corr_middle, sep = "")))
}


corrected <- function(arm) {
  arm <- arm %>%
    mutate(mother_corrected = ifelse(mother==corr_mother, 0, 1)) %>%
    mutate(middle_corrected = ifelse(middle==corr_middle, 0, 1)) %>%
    mutate(dob_corrected = ifelse(dob==corr_dob, 0, 1))
}
  


#doing this stuff for arm_1:
tidy_arm_1 <- fix_dob(arm_1)
change_character(tidy_arm_1)
tidy_arm_1 <- lower_to_upper(tidy_arm_1)
tidy_arm_1 <- xx_to_na(tidy_arm_1)
#tidy_arm_1<- full_name_correction(tidy_arm_1)
tidy_arm_1<- full_name_correction2(tidy_arm_1)
tidy_arm_1 <- make_id(tidy_arm_1)
tidy_arm_1 <- corrected(tidy_arm_1)

#doing this stuff for arm_4:
tidy_arm_4 <- fix_dob(arm_4)
change_character(tidy_arm_4)
tidy_arm_4 <- lower_to_upper(tidy_arm_4)
tidy_arm_4<- full_name_correction(tidy_arm_4)
tidy_arm_4 <- xx_to_na(tidy_arm_4)
tidy_arm_4 <- make_id(tidy_arm_4)
tidy_arm_4 <- corrected(tidy_arm_4)

#For arm_3:
tidy_arm_3 <- fix_dob(arm_3)
change_character(tidy_arm_3)
tidy_arm_3 <- lower_to_upper(tidy_arm_3)
tidy_arm_3<- full_name_correction(tidy_arm_3)
tidy_arm_3 <- xx_to_na(tidy_arm_3)
tidy_arm_3 <- make_id(tidy_arm_3)
tidy_arm_3 <- corrected(tidy_arm_3)

#For arm_2:
tidy_arm_2 <- fix_dob(arm_2)
change_character(tidy_arm_2)
tidy_arm_2 <- lower_to_upper(tidy_arm_2)
tidy_arm_2 <- xx_to_na(tidy_arm_2)
#tidy_arm_2<- full_name_correction(tidy_arm_2)
tidy_arm_2<- full_name_correction2(tidy_arm_2)
tidy_arm_2 <- make_id(tidy_arm_2)
tidy_arm_2 <- corrected(tidy_arm_2)


#Now we have our four arms. Time to combine and match.

#Getting rid of NAs:
delete_rows_with_na_id <- function(arm) {
  arm <- arm %>%
    filter(!is.na(id))
}
  
new_tidy_arm_1 <- delete_rows_with_na_id(tidy_arm_1)
new_tidy_arm_2 <- delete_rows_with_na_id(tidy_arm_2)

#try and match complete matching IDs
#create vectors of ids
arm1_id <- new_tidy_arm_1$id
arm2_id <- new_tidy_arm_2$id

#try an ifelse to match from two data frames (This will only work if our two data frames match in length.)
arm_1$true_id <- ifelse((new_tidy_arm_1$id %in% arm2_id),
         new_tidy_arm_1$id, NA)

#try a merge just to see what actually matches
arm1_2_merge <- merge(new_tidy_arm_1, new_tidy_arm_2, by='id', all=T)

#a double check to see if any of the following identifiers match
arm1_2_merge <- arm1_2_merge %>%
  mutate(match = ifelse(((corr_dob.x == corr_dob.y) 
                         & (corr_middle.x == corr_middle.y) 
                         & (corr_mother.x == corr_mother.y)
                         & (gender.x == gender.y) 
                         #& (grade.x == grade.y) 
                         #& (ms_school.x == ms_school.y)
                         #& (hs_school.x == hs_school.y)
                         ), 1, 0))
count(arm1_2_merge$match)

#just main 3 id components match
arm_1_2_idmatch <- arm1_2_merge %>%
  filter(match == 1)

#main 3 id components and gender match
arm_1_2_fullmatch <- arm1_2_merge %>%
  filter(match %in% c(0,1))
