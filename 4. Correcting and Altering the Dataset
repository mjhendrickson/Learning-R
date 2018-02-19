## For more on regex construction: https://regex101.com/

## Load needed packages
library(tidyverse)
library(dplyr)
library(stringr)

## After discussing with a colleague, updated code to use stringr str_extract() and regex
## For more on regex construction: https://regex101.com/

library(stringr) ## Load stringr

## Exploration
## Gets every instance of HarvardX or MITx and prints it
str_extract_all(HXMITX$course_id, 'HarvardX')
str_extract_all(HXMITX$course_id, 'MITx')

## Sets True/False for each case where it matches/does not match HarvardX or MITx
str_detect(HXMITX$course_id, 'HarvardX')
str_detect(HXMITX$course_id, 'MITx')


## Needed fields
## Create field "institution"
HXMITX$institution <- str_extract(HXMITX$course_id,"^\\w+(?=/)") ## Returns all before first /

## Create field "course_code"
HXMITX$course_code <- str_extract(HXMITX$course_id,"(?<=/).+(?=/)") ## Returns all between first and second /

## Create field "year_term"
HXMITX$year_term <- str_extract(HXMITX$course_id,"(?<=/)\\w+$") ## Returns all after second /

## Create field "year"
HXMITX$year <- str_extract(HXMITX$course_id,"(?<=/)\\d{4}") ## Returns all after second / and before _

## Create field "term"
HXMITX$term <- str_extract(HXMITX$course_id,"(?<=/\\d{4}_)\\w+$") ## Retuns all after _

## Create Letter Grade
## DOES NOT WORK
HXMITX$letter_grade <- switch (HXMITX$grade,
                               A = list(grade.min = 90.00, grade.max = 100),
                               B = list(grade.min = 80.00, grade.max = 89.99),
                               C = list(grade.min = 70.00, grade.max = 79.99),
                               D = list(grade.min = 60.00, grade.max = 69.99),
                               F = list(grade.min = 0, grade.max = 59.99)
)

#-------------------------------------------------
## Drop test and created fields
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
