# ===== Introduction =====
# Project: Learning R by Doing - HarvardX-MITx Analysis
# By: Matthew Hendrickson
# What: Importing & Merging HxMITx Data Sources

# -----Set up environment -----
library(tidyverse)



# ===== Importing & Reviewing the Data =====
# ----- a. Import HXMITX course data into RStudio -----
HXMITX <- read_csv("HMXPC13_DI_v2_5-14-14.csv")

# View & get summary of HXMITX course data
View(HXMITX) # all rows
head(HXMITX) # first few rows
tail(HXMITX) # last few rows
summary(HXMITX) # basic summary info

# Drop roles as all records are NULL
HXMITX$roles <- NULL



# ----- b. Import reference data into RStudio -----
courses <- read_csv("hxmitx_course_reference.csv")

# View & get summary of reference data
View(courses) # all rows
head(courses) # first few rows
tail(courses) # last few rows
summary(courses) # basic summary info

# Drop institution to avoid duplication after join
courses$institution <- NULL



# ===== Add new fields =====
# For more on regex construction: https://regex101.com/

# ----- a. Exploration -----
# Gets every instance of HarvardX or MITx and prints it
str_extract_all(HXMITX$course_id, 'HarvardX')
str_extract_all(HXMITX$course_id, 'MITx')

# Sets True/False for each case where it matches/does not match HarvardX or MITx
str_detect(HXMITX$course_id, 'HarvardX')
str_detect(HXMITX$course_id, 'MITx')



# ----- b. Needed fields -----
# Create field "institution"
HXMITX$institution <-       str_extract(HXMITX$course_id,"^\\w+(?=/)") ## Returns all before first /

# Create field "course_code"
HXMITX$course_code <-       str_extract(HXMITX$course_id,"(?<=/).+(?=/)") ## Returns all between first and second /

# Create field "year_term"
HXMITX$year_term <-         str_extract(HXMITX$course_id,"(?<=/)\\w+$") ## Returns all after second /

# Create field "year"
HXMITX$year <-              str_extract(HXMITX$course_id,"(?<=/)\\d{4}") ## Returns all after second / and before _

# Create field "start_time_ym"
HXMITX$start_time_ym <-     str_extract(HXMITX$start_time_DI,"^\\w+(?=-).+(?=-)") ## Returns all before second -

# Create field "last_event_ym"
HXMITX$last_event_ym <-     str_extract(HXMITX$last_event_DI,"^\\w+(?=-).+(?=-)") ## Returns all before second -

# Create field "term"
HXMITX$term <-              str_extract(HXMITX$course_id,"(?<=/\\d{4}_)\\w+$") ## Retuns all after _

# Create Letter Grade
HXMITX$letter_grade <-  cut(HXMITX$grade,
                            breaks = c(0,.6,.7,.8,.9, 1.01), 
                            labels = c("F", "D", "C", "B", "A"),
                            right = FALSE,
                            include.highest = TRUE,
                            include.lowest = TRUE)

# Create Indicator: nevents_ind
HXMITX$nevents_ind <-       ifelse(HXMITX$nevents >= 1, 1,0)

# Create Indicator: ndays_act_ind
HXMITX$ndays_act_ind <-     ifelse(HXMITX$ndays_act >= 1, 1,0)

# Create Indicator: nplay_video_ind
HXMITX$nplay_video_ind <-   ifelse(HXMITX$nplay_video >= 1, 1,0)

# Create Indicator: nchapters_ind
HXMITX$nchapters_ind <-     ifelse(HXMITX$nchapters >= 1, 1,0)

# Create Indicator: nforum_posts_ind
HXMITX$nforum_posts_ind <-  ifelse(HXMITX$nforum_posts >= 1, 1,0)

# Create binary indicator for future modeling
HXMITX$treatment <-         rbinom(n = 641138, size = 1, prob = 0.5)

# CONSIDER BINS FOR nevents, ndays_act, nplay_video, nchapters



# ===== Appending course summary data from second dataset =====
# Join datasets
HxMx <- left_join(HXMITX, courses, by = "course_code", copy = FALSE)
View(HxMx) # all rows
head(HxMx) # first few rows
tail(HxMx) # last few rows
summary(HxMx) # basic summary info

# Remove HXMITX and courses dataframes
rm(HXMITX)
rm(courses)



# ===== Write HxMx to .csv for recall after refreshing session =====
write_csv(HxMx, "HxMx.csv")



# ===== Drop test and created fields =====
#HXMITX$institution <- NULL
#HXMITX$course <- NULL
#HXMITX$year_term <- NULL
#HXMITX$year <- NULL
#HXMITX$term <- NULL
#HXMITX$HarvardX <- NULL
#HXMITX$MITx <- NULL
#HXMITX$harvardx <- NULL
#HXMITX$mitx <- NULL
#HXMITX$letter_grade <- NULL
#HXMITX$nevents_ind <- NULL
#HXMITX$ndays_act_ind <- NULL
#HXMITX$nplay_video_ind <- NULL
#HXMITX$nchapters_ind <- NULL
#HXMITX$nforum_posts_ind <- NULL
