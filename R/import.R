# In order to access this dataset, we've had to agree to not redistribute any
# copies of it whether here in github or elsewhere. 
# Data may be downloaded from this link: 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/26147
# Place dataset in a data folder within this R project. 
# Do not stage or commit the data folder and its contents

########## Importing & Reviewing the Data
## Import main data into RStudio
library(readr)
HXMITX <- read_csv("data/HMXPC13_DI_v2_5-14-14.csv")

## View main data
View(HXMITX) # all rows
head(HXMITX) # first few rows
tail(HXMITX) # last few rows

## Get basic summary information about the main dataset
summary(HXMITX)



## Import reference data into RStudio
library(readr)
courses <- read_csv("data/hxmitx_course_reference.csv")

## View reference data
View(courses) # all rows

## Get basic summary information about the reference dataset
summary(courses)
