#Load/Install Packages
#install.packages("janitor")
#install.packages('plyr')
library(tidyr)
library(janitor)
library(dplyr)
library(plyr)

#REDCap to R

data <- read.csv("./Desktop/ontrack/ontrack.csv", na.strings = c(NA, ""))

#Cleaning and Tidying


#1. change 'XX' to NA
#2. fix dob to be 2 numbers
#3. pass to next stage if id has correct format
#4. add a flag column to indicate data was changed in an above correction.
#5. Create problem data set-- all problem rows so we identify what to look for. 
  #starting with XX, 'xx', NA, missing, Hh-Hh, full name (centrell), repeated rows... 


summary(data$redcap_event_name) #This is taking a summary of the redcap_event_name data)


arm_1 <- data %>% 
  filter(redcap_event_name == "beginning_of_the_y_arm_1") %>%
  select(dob, mother, middle, gender, grade, ms_school, hs_school)
  
arm_2 <- data %>%
  filter(redcap_event_name == "end_of_the_year_arm_2") %>%
  select(dob, mother, middle, gender, grade, ms_school, hs_school)
  
#arm_3 <- data %>%
 # filter(redcap_event_name == "end_of_visit_arm_3") %>%
 # select(dob, mother, middle, gender, grade, ms_school, hs_school)

#arm_4 <- data %>%
# filter(redcap_event_name == "end_of_middle_scho_arm_4") %>%
# select(dob, mother, middle, gender, grade, ms_school, hs_school)

#Making changing factor to character function:
change_character <- function(arm){
  arm$mother <- as.character(arm$mother)
  arm$middle <- as.character(arm$middle)
}


#Making changing factor to lowercase to uppercase function:
lower_to_upper <- function(arm) {
  arm <- arm %>%
    mutate(corr_mother = toupper(mother)) %>%
    mutate(corr_middle = toupper(middle))
}


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
change_character(tidy_arm_1)
tidy_arm_1 <- lower_to_upper(tidy_arm_1)
tidy_arm_1 <- xx_fix(tidy_arm_1)
tidy_arm_1 <- community(tidy_arm_1)
#tidy_arm_1<- full_name_correction(tidy_arm_1)
tidy_arm_1<- full_name_correction2(tidy_arm_1)
tidy_arm_1 <- make_id(tidy_arm_1)
tidy_arm_1 <- corrected(tidy_arm_1)

#arm_2:
tidy_arm_2 <- fix_dob(arm_2)
change_character(tidy_arm_2)
tidy_arm_2 <- lower_to_upper(tidy_arm_2)
tidy_arm_2 <- xx_fix(tidy_arm_2)
tidy_arm_2 <- community(tidy_arm_2)
#tidy_arm_2<- full_name_correction(tidy_arm_2)
tidy_arm_2<- full_name_correction2(tidy_arm_2)
tidy_arm_2 <- make_id(tidy_arm_2)
tidy_arm_2 <- corrected(tidy_arm_2)

#arm_3:
#tidy_arm_3 <- fix_dob(arm_3)
#change_character(tidy_arm_3)
#tidy_arm_3 <- lower_to_upper(tidy_arm_3)
#tidy_arm_3 <- xx_fix(tidy_arm_3)
#tidy_arm_3 <- community(tidy_arm_3)
#tidy_arm_3<- full_name_correction(tidy_arm_3)
#tidy_arm_3<- full_name_correction2(tidy_arm_3)
#tidy_arm_3 <- make_id(tidy_arm_3)
#tidy_arm_3 <- corrected(tidy_arm_3)

#arm_4:
#tidy_arm_4 <- fix_dob(arm_4)
#change_character(tidy_arm_4)
#tidy_arm_4 <- lower_to_upper(tidy_arm_4)
#tidy_arm_4 <- xx_fix(tidy_arm_4)
#tidy_arm_4 <- community(tidy_arm_4)
#tidy_arm_4<- full_name_correction(tidy_arm_4)
#tidy_arm_4<- full_name_correction2(tidy_arm_4)
#tidy_arm_4 <- make_id(tidy_arm_4)
#tidy_arm_4 <- corrected(tidy_arm_4)



#Now we have our four arms. Time to combine and match.

#Getting rid of full NAs:
delete_rows_with_na_id <- function(arm) {
  arm <- arm %>%
    filter(!is.na(id))
}
  
new_tidy_arm_1 <- delete_rows_with_na_id(tidy_arm_1)
new_tidy_arm_2 <- delete_rows_with_na_id(tidy_arm_2)

#only select columns that we actually care about for matching; we can go back and get rid of this later if we want more data included
new_tidy_arm_1 <- new_tidy_arm_1 %>%
  select(one_of(c("id", "corr_dob","corr_mother", "corr_middle", "gender", "grade", "ms_school", "hs_school")))

new_tidy_arm_2 <- new_tidy_arm_2 %>%
  select(one_of(c("id", "corr_dob","corr_mother", "corr_middle", "gender", "grade", "ms_school", "hs_school")))
  

#try and match complete matching IDs
#create vectors of ids
arm1_id <- new_tidy_arm_1$id
arm2_id <- new_tidy_arm_2$id

#raw count of ids that match without merging
arm1_id %in% arm2_id %>%
  count()


#try an ifelse to match from two data frames (This will only work if our two data frames match in length.)
#arm_1$true_id <- ifelse((new_tidy_arm_1$id %in% arm2_id),
#        new_tidy_arm_1$id, NA)

#try a merge just to see what actually matches
arm1_2_merge <- merge(new_tidy_arm_1, new_tidy_arm_2, by=c('id'), all=T)

#a double check to see if any of the following identifiers match
arm1_2_merge <- arm1_2_merge %>%
  mutate(true_id = ifelse(((corr_dob.x == corr_dob.y) 
                         & (corr_middle.x == corr_middle.y) 
                         & (corr_mother.x == corr_mother.y)
                         #& (gender.x == gender.y) 
                         #& (grade.x == grade.y) 
                         #& (ms_school.x == ms_school.y)
                         #& (hs_school.x == hs_school.y)
                         ), id, NA))
count(!is.na(arm1_2_merge$true_id))



#matching 6/6

names_x <- c("gender.x", "grade.x", "ms_school.x", "hs_school.x", "corr_dob.x", "corr_mother.x", "corr_middle.x")
names_y <- c("gender.y", "grade.y", "ms_school.y", "hs_school.y", "corr_dob.y", "corr_mother.y", "corr_middle.y")

combinations <- combn(names_x, 5)

new_tidy_arm_1 <- new_tidy_arm_1 %>%
  mutate(true_id = ifelse((new_tidy_arm_1$id %in% new_tidy_arm_2$id), 
                          id, NA))

new_tidy_arm_2 <- new_tidy_arm_2 %>%
  mutate(true_id = ifelse((new_tidy_arm_2$id %in% new_tidy_arm_1$id), #no middle
  id, NA))


#matching 5/6
new_tidy_arm_1 <- new_tidy_arm_1 %>%
  #filter(is.na(true_id)) %>%
  mutate(partial_id = ifelse(((new_tidy_arm_1$corr_dob == new_tidy_arm_2$corr_dob) 
                              & (new_tidy_arm_1$corr_mother == new_tidy_arm_2$corr_mother) 
                              & (new_tidy_arm_1$corr_middle == new_tidy_arm_2$corr_middle)
                              & (new_tidy_arm_1$gender == new_tidy_arm_2$gender)) 
                              #& (new_tidy_arm_1$grade == new_tidy_arm_2$grade))
                                      , id, NA))


new_tidy_arm_2 <- new_tidy_arm_2 %>%
  #filter(is.na(true_id)) %>%
  mutate(partial_id = ifelse(((new_tidy_arm_2$corr_dob %in% new_tidy_arm_1$corr_dob),
                              ifelse((new_tidy_arm_2$corr_mother %in% new_tidy_arm_1$corr_mother) 
                              & (new_tidy_arm_2$corr_middle == new_tidy_arm_1$corr_middle)
                              & (new_tidy_arm_2$gender == new_tidy_arm_1$gender) 
                              & (new_tidy_arm_2$grade == new_tidy_arm_1$grade))
                                      , id, NA)))

#Merge 7 varibles (dob, mother, middle, gender, grade, ms_school, hs_school)          
merge.7  <- merge(new_tidy_arm_1, new_tidy_arm_2, by=c('corr_dob','corr_mother', 'corr_middle', 'gender', 'grade', 'ms_school', 'hs_school'), all=T)
merge.7 <- merge.7 %>%
  mutate(id_match = ifelse((id.x == id.y), 1, NA)) 
count(merge.7$id_match) #How many have matching IDs? 9

#Merge 6 varibles (dob, mother, middle, gender, grade, ms_school, hs_school)  
merge.6  <- merge(new_tidy_arm_1, new_tidy_arm_2, by=c('corr_dob','corr_mother', 'corr_middle', 'gender', 'grade', 'ms_school'), all=T)

merge.6 <- merge.6 %>%
  mutate(id_match = ifelse((id.x == id.y), 1, NA), 
         hs_school_match = ifelse((hs_school.x == hs_school.y), 1, NA)) 
count(merge.6$id_match)#How many have matching IDs? 9
count(merge.6$hs_school_match)#How many have matching hs_school? 0

#Merge 5 varibles (dob, mother, middle, gender, grade, ms_school, hs_school)  
merge.5  <- merge(new_tidy_arm_1, new_tidy_arm_2, by=c('corr_dob','corr_mother', 'corr_middle', 'gender', 'grade'), all=T)

merge.5 <- merge.5 %>%
  mutate(id_match = ifelse((id.x == id.y), 1, NA), 
         hs_school_match = ifelse((hs_school.x == hs_school.y), 1, NA),
         ms_school_match = ifelse((ms_school.x == ms_school.y), 1, NA)) 
count(merge.5$id_match)#How many have matching IDs? 9
count(merge.5$hs_school_match)#How many have matching hs_school? 0
count(merge.5$ms_school_match)#How many have matching ms_school? 9


#Merge 4 varibles (dob, mother, middle, gender, grade, ms_school, hs_school)  
merge.4  <- merge(new_tidy_arm_1, new_tidy_arm_2, by=c('corr_dob','corr_mother', 'corr_middle', 'gender'), all=T)

merge.4 <- merge.4 %>%
  mutate(id_match = ifelse((id.x == id.y), 1, NA), 
         hs_school_match = ifelse((hs_school.x == hs_school.y), 1, NA),
         ms_school_match = ifelse((ms_school.x == ms_school.y), 1, NA),
         grade_match = ifelse((grade.x == grade.y), 1, NA)) 
count(merge.4$id_match)#How many have matching IDs? 9
count(merge.4$hs_school_match)#How many have matching hs_school? 0
count(merge.4$ms_school_match)#How many have matching ms_school? 9
count(merge.4$grade_match)#How many have matching grade? 9



#Merge 3 varibles (dob, mother, middle, gender, grade, ms_school, hs_school)  
merge.3  <- merge(new_tidy_arm_1, new_tidy_arm_2, by=c('corr_dob','corr_mother', 'corr_middle'), all=T)

merge.3 <- merge.3 %>%
  mutate(id_match = ifelse((id.x == id.y), 1, NA), 
         hs_school_match = ifelse((hs_school.x == hs_school.y), 1, NA),
         ms_school_match = ifelse((ms_school.x == ms_school.y), 1, NA),
         grade_match = ifelse((grade.x == grade.y), 1, NA),
         gender_match = ifelse((gender.x == gender.x), 1, NA)) 
count(merge.3$id_match)#How many have matching IDs? 20
count(merge.3$hs_school_match)#How many have matching hs_school? 0
count(merge.3$ms_school_match)#How many have matching ms_school? 15
count(merge.3$grade_match)#How many have matching grade? 16
count(merge.3$gender_match)#How many have matching gender? 1404

#all possible options
#(new_tidy_arm_1$corr_dob %in% new_tidy_arm_2$corr_dob) & (new_tidy_arm_1$corr_mother %in% new_tidy_arm_2$corr_mother) & (new_tidy_arm_1$corr_middle %in% new_tidy_arm_2$corr_middle)
#(new_tidy_arm_1$gender %in% new_tidy_arm_2$gender) & (new_tidy_arm_1$grade %in% new_tidy_arm_2$grade) & (new_tidy_arm_1$ms_school %in% new_tidy_arm_2$ms_school) &


#could be a solution? https://stackoverflow.com/questions/10617377/merge-data-with-partial-match-in-r