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
set.seed(123)
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

# Drop roles as all records are NULL
HxMx$roles <- NULL



# ----- Set Characters as Factors -----
# course_id -----
# Review distinct values for setting factors
unique(HxMx$course_id)
# Check if field is a factor
levels(HxMx$course_id)
# Change field to a factor
HxMx$course_id <- as.factor(HxMx$course_id) %>% 
  factor(levels = c(
    "HarvardX/CB22x/2013_Spring",
    "HarvardX/CS50x/2012",
    "HarvardX/ER22x/2013_Spring",
    "HarvardX/PH207x/2012_Fall",
    "HarvardX/PH278x/2013_Spring",
    "MITx/6.002x/2012_Fall",
    "MITx/6.002x/2013_Spring",
    "MITx/14.73x/2013_Spring",
    "MITx/2.01x/2013_Spring",
    "MITx/3.091x/2012_Fall",
    "MITx/3.091x/2013_Spring",
    "MITx/6.00x/2012_Fall",
    "MITx/6.00x/2013_Spring",
    "MITx/7.00x/2013_Spring",
    "MITx/8.02x/2013_Spring",
    "MITx/8.MReV/2013_Summer"
  ),
  ordered = FALSE
  )
# Ensure levels took and are accurate
levels(HxMx$course_id)



# final_cc_cname_DI -----
# Review distinct values for setting factors
unique(HxMx$final_cc_cname_DI)
# Check if field is a factor
levels(HxMx$final_cc_cname_DI)
# Change field to a factor
HxMx$final_cc_cname_DI <- as.factor(HxMx$final_cc_cname_DI) %>% 
  factor(levels = c(
    "United States",
    "France",
    "Unknown/Other",
    "Mexico",
    "Australia",
    "India",
    "Canada",
    "Russian Federation",
    "Other South Asia",
    "Other North & Central Amer., Caribbean",
    "Other Europe",
    "Other Oceania",
    "Japan",
    "Other Africa",
    "Colombia",
    "Germany",
    "Other Middle East/Central Asia",
    "Poland",
    "Indonesia",
    "Other East Asia",
    "Bangladesh",
    "China",
    "United Kingdom",
    "Ukraine",
    "Spain",
    "Greece",
    "Pakistan",
    "Brazil",
    "Nigeria",
    "Egypt",
    "Other South America",
    "Portugal",
    "Philippines",
    "Morocco"
  ),
  ordered = FALSE
  )
# Ensure levels took and are accurate
levels(HxMx$final_cc_cname_DI)



# LoE_DI -----
# Review distinct values for setting factors
unique(HxMx$LoE_DI)
# Check if field is a factor
levels(HxMx$LoE_DI)
# Change field to a factor
HxMx$LoE_DI <- as.factor(HxMx$LoE_DI) %>% 
  factor(levels = c(
    "Less than Secondary",
    "Secondary",
    "Bachelor's",
    "Master's",
    "Doctorate"
  ),
  ordered = TRUE
  )
# Ensure levels took and are accurate
levels(HxMx$LoE_DI)



# Gender -----
# Review distinct values for setting factors
unique(HxMx$gender)
# Check if field is a factor
levels(HxMx$gender)
# Change field to a factor
HxMx$gender <- as.factor(HxMx$gender) %>% 
  factor(levels = c(
    "m",
    "f",
    "o"
  ),
  ordered = FALSE
  )
# Ensure levels took and are accurate
levels(HxMx$gender)



# institution -----
# Review distinct values for setting factors
unique(HxMx$institution)
# Check if field is a factor
levels(HxMx$institution)
# Change field to a factor
HxMx$institution <- as.factor(HxMx$institution) %>% 
  factor(levels = c(
    "HarvardX",
    "MITx"
  ),
  ordered = FALSE
  )
# Ensure levels took and are accurate
levels(HxMx$institution)



# course_code -----
# Review distinct values for setting factors
unique(HxMx$course_code)
# Check if field is a factor
levels(HxMx$course_code)
# Change field to a factor
HxMx$course_code <- as.factor(HxMx$course_code) %>% 
  factor(levels = c(
    "CB22x",
    "CS50x",
    "ER22x",
    "PH207x",
    "PH278x",
    "6.002x",
    "14.73x",
    "2.01x",
    "3.091x",
    "6.00x",
    "7.00x",
    "8.02x",
    "8.MReV"
  ),
  ordered = FALSE
  )
# Ensure levels took and are accurate
levels(HxMx$course_code)



# term -----
# Review distinct values for setting factors
unique(HxMx$term)
# Check if field is a factor
levels(HxMx$term)
# Change field to a factor
HxMx$term <- as.factor(HxMx$term) %>% 
  factor(levels = c(
    "Fall",
    "Spring",
    "Summer"
  ),
  ordered = FALSE
  )
# Ensure levels took and are accurate
levels(HxMx$term)



# short_title -----
# Review distinct values for setting factors
unique(HxMx$short_title)
# Check if field is a factor
levels(HxMx$short_title)
# Change field to a factor
HxMx$short_title <- as.factor(HxMx$short_title) %>% 
  factor(levels = c(
    "HeroesX",
    "-",
    "JusticeX",
    "HealthStat",
    "HealthEnv",
    "Circuits",
    "Poverty",
    "Structures",
    "SSChem",
    "CS",
    "Biology",
    "E&M",
    "MechRev"
  ),
  ordered = FALSE
  )
# Ensure levels took and are accurate
levels(HxMx$short_title)



# full_title -----
# Review distinct values for setting factors
unique(HxMx$full_title)
# Check if field is a factor
levels(HxMx$full_title)
# Change field to a factor
HxMx$full_title <- as.factor(HxMx$full_title) %>% 
  factor(levels = c(
    "The Ancient Greek Hero",
    "Introduction to Computer Science I",
    "Justice",
    "Health in Numbers: Quantitative Methods in Clinical & Public Health Research",
    "Human Health and Global Environmental Change",
    "Circuits and Electronics",
    "The Challenges of Global Poverty",
    "Elements of Structures",
    "Introduction to Solid State Chemistry",
    "Introduction to Computer Science and Programming",
    "Introduction to Biology - The Secret of Life",
    "Electricity and Magnetism",
    "Mechanics Review"
  ),
  ordered = FALSE
  )
# Ensure levels took and are accurate
levels(HxMx$full_title)



# semester -----
# Review distinct values for setting factors
unique(HxMx$semester)
# Check if field is a factor
levels(HxMx$semester)
# Change field to a factor
HxMx$semester <- as.factor(HxMx$semester) %>% 
  factor(levels = c(
    "Spring-Summer 2013",
    "Fall 2012 - Spring 2013",
    "Fall 2012",
    "Summer 2013",
    "Fall 2012 and Spring 2013",
    "Spring 2013"
  ),
  ordered = FALSE
  )
# Ensure levels took and are accurate
levels(HxMx$semester)



# ----- Set Characters as Numeric -----
class(HxMx$YoB) # Check class
HxMx$YoB <- as.integer(HxMx$YoB)
class(HxMx$YoB) # Check new class



# ===== Write HxMx to .rds for recall after refreshing session =====
# write_csv(HxMx, "HxMx.csv") -- removed to save as .rds instead
saveRDS(HxMx, "HxMx.rds") # saves factors in file



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
