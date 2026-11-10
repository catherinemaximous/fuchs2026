# Replication Project For Fuchs 2026

This is a GitHub template to use for replications projects in PSYCH 201a at UCSD. Create a new repo using this as a template in order to use it.


This experiment replicates the findings in the single-linguistic picture-matching task.


# Data Dicitionary 

subject_id <- assigned participant ID by JsPsych 
participant_id <- assigned participant ID by researcher (first three letters of their first name, first three letters of their last name, and last two digits of their phone number)
sound_check_passed <- 1 = their audio worked successfully, 0 = audio failed 
Age <- age of participant
Gender <- gender of participant
trial <- trial number
block <- block number consisting of 4 blocks each with 30 audio stimuli 
structure <- sentence type, whether participants saw Reduced Relative (RR), Subject Relative (SR), Object Relative (OR) in Spanish
pair <- refers to the number of image stimuli in the images folder
audio <- audio they heard, the type of sentence
pic_left <- image appeared on the left
pic_right <- image appeared on the right
correct_response <- the correct arrow (left or right) for the trial
correct <- 1 if participant got it correct and 0 if they got it wrong
