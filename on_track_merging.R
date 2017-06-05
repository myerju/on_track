#Load/Install Packages
#install.packages("janitor")
#install.packages('plyr')
library(tidyr)
library(janitor)
library(dplyr)
library(plyr)

tidy_arm_1 <- read.csv("/Users/alexsalem/Documents/ontrack/tidy_arm_1.csv")
tidy_arm_2 <- read.csv("/Users/alexsalem/Documents/ontrack/tidy_arm_2.csv")
tidy_arm_3 <- read.csv("/Users/alexsalem/Documents/ontrack/tidy_arm_3.csv")
tidy_arm_4 <- read.csv("/Users/alexsalem/Documents/ontrack/tidy_arm_4.csv")


#Here is a function that gets rid of rows with ids that are just NAs:
delete_rows_with_na_id <- function(arm) {
  arm <- arm %>%
    filter(!is.na(id))
}

#Applying that function to the four data frames:
new_tidy_arm_1 <- delete_rows_with_na_id(tidy_arm_1)
new_tidy_arm_2 <- delete_rows_with_na_id(tidy_arm_2)
new_tidy_arm_3 <- delete_rows_with_na_id(tidy_arm_3)
new_tidy_arm_4 <- delete_rows_with_na_id(tidy_arm_4)


#From now on, we're just working with arm_1 and arm_2


#Here we only select columns that we actually care about for matching; we can go back and get rid of this later if we want more data included
new_tidy_arm_1 <- new_tidy_arm_1 %>%
  select(one_of(c("id", "corr_dob","corr_mother", "corr_middle", "gender", "grade", "ms_school", "hs_school")))
new_tidy_arm_2 <- new_tidy_arm_2 %>%
  select(one_of(c("id", "corr_dob","corr_mother", "corr_middle", "gender", "grade", "ms_school", "hs_school")))


#Here are lines to do a count of exact matching ids in arm1 and arm2:
#try and match complete matching IDs
#create vectors of ids
arm1_id <- new_tidy_arm_1$id
arm2_id <- new_tidy_arm_2$id
#raw count of ids that match without merging
arm1_id %in% arm2_id %>%
  count()
#count in the other direction
arm2_id %in% arm1_id %>%
  count()


#Here was a different attempt--didn't work because different length data frames
#try an ifelse to match from two data frames (This will only work if our two data frames match in length.)
arm_1$true_id <- ifelse((new_tidy_arm_1$id %in% arm2_id),
        new_tidy_arm_1$id, NA)


#Here is a basic merge by id
arm1_2_merge <- merge(new_tidy_arm_1, new_tidy_arm_2, by=c('id'), all=T)
arm2_1_merge <- merge(new_tidy_arm_2, new_tidy_arm_1, by=c('id'), all=T)

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

arm2_1_merge <- arm2_1_merge %>%
  mutate(true_id = ifelse(((corr_dob.x == corr_dob.y) 
                           & (corr_middle.x == corr_middle.y) 
                           & (corr_mother.x == corr_mother.y)
                           #& (gender.x == gender.y) 
                           #& (grade.x == grade.y) 
                           #& (ms_school.x == ms_school.y)
                           #& (hs_school.x == hs_school.y)
  ), id, NA))
count(!is.na(arm2_1_merge$true_id))




#Below is an attempt to merge based on sets of 5 matching values.

names_x <- c("gender.x", "grade.x", "ms_school.x", "hs_school.x", "corr_dob.x", "corr_mother.x", "corr_middle.x")
names_y <- c("gender.y", "grade.y", "ms_school.y", "hs_school.y", "corr_dob.y", "corr_mother.y", "corr_middle.y")
names <- c("gender", "grade", "ms_school", "hs_school", "corr_dob", "corr_mother", "corr_middle")

combinations <- combn(names_x, 6)
combinations.y <- combn(names_y, 6)

#create all combinations of 5
combinations <- combn(names, 5)

#append a row to add id to each combinations
id.row <- replicate(21, "id")
combinations <- rbind(combinations,id.row)

#select columns in dataframe that match the 1st combination
combinations_x1 <- new_tidy_arm_1 %>%
  select(one_of (combinations[,1]))

#Function that returns TRUE if the two data frame rows have the same value for each column in set_of_columns, 
#returns FALSE otherwise.
#Takes as input two data frame rows from two data frames, and a set of column names
helper_function <- function(data_frame1_row, data_frame2_row, set_of_columns) {
  x <- TRUE
  for (i in seq(set_of_columns)) {
    ifelse(data_frame1_row[set_of_columns[i]] != data_frame2_row[set_of_columns[i]], x <-FALSE, x<- TRUE)
  }
}

#Theoretically, this should loop through each row in data frame 1, and then data frame 2, and run the 
#helper function. Then, it creates a true_id column that has the matching id.
trial.merge <- new_tidy_arm_1 %>%
  for (i in seq(new_tidy_arm_1$id)) {
    for (j in seq(new_tidy_arm_2$id)) {
      mutate(true_id = ifelse((helper_function(new_tidy_arm_1[i,], new_tidy_arm_2[j,], names))==TRUE, new_tidy_arm_1$id,
                              ifelse(helper_function(new_tidy_arm_1[i,], new_tidy_arm_2[j,], combinations[,1])==TRUE, new_tidy_arm_1$id,
                                     NA)))
    }
  }

#Here was another attempt--making a true_id column:
new_tidy_arm_1 <- new_tidy_arm_1 %>%
  mutate(true_id = ifelse((new_tidy_arm_1$id %in% new_tidy_arm_2$id), 
                          id, NA))
new_tidy_arm_2 <- new_tidy_arm_2 %>%
  mutate(true_id = ifelse((new_tidy_arm_2$id %in% new_tidy_arm_1$id), #no middle
                          id, NA))


#And below are some more ideas:
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


