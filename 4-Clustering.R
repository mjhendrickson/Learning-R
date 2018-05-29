# ===== Introduction =====
# Project: Learning R by Doing - HarvardX-MITx Analysis
# By: Matthew Hendrickson
# What: Clustering

# ----- Set up environment -----
library(tidyverse)
#library(cluster)
#library(fpc)
#library(factoextra)

# ----- Open HxMx after session refresh -----
HxMx <- read_csv("HxMx.csv")
glimpse(HxMx)

# ----- Next Steps -----
# 1. Finish Characters as Factors
# 2. Characters as Numeric



# ===== Hierarchical Clustering =====
### Still in early exploration

# ----- Set Characters as Factors -----
# course_id -----
# Review distinct values for setting factors
unique(HxMx$course_id)
# Check if field is a factor
levels(HxMx$course_id)
# Change field to a factor
HxMx$course_id_f <- as.factor(HxMx$course_id) %>% 
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
levels(HxMx$course_id_f)



# final_cc_cname_DI -----
# Review distinct values for setting factors
unique(HxMx$final_cc_cname_DI)
# Check if field is a factor
levels(HxMx$final_cc_cname_DI)
# Change field to a factor
HxMx$final_cc_cname_DI_f <- as.factor(HxMx$final_cc_cname_DI) %>% 
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
levels(HxMx$final_cc_cname_DI_f)



# LoE_DI -----
# Review distinct values for setting factors
unique(HxMx$LoE_DI)
# Check if field is a factor
levels(HxMx$LoE_DI)
# Change field to a factor
HxMx$LoE_DI_f <- as.factor(HxMx$LoE_DI) %>% 
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
levels(HxMx$LoE_DI_f)



# Gender -----
# Review distinct values for setting factors
unique(HxMx$gender)
# Check if field is a factor
levels(HxMx$gender)
# Change field to a factor
HxMx$gender_f <- as.factor(HxMx$gender) %>% 
  factor(levels = c(
    "m",
    "f",
    "o"
  ),
  ordered = FALSE
  )
# Ensure levels took and are accurate
levels(HxMx$gender_f)



# institution -----
# Review distinct values for setting factors
unique(HxMx$institution)
# Check if field is a factor
levels(HxMx$institution)
# Change field to a factor
HxMx$institution_f <- as.factor(HxMx$institution) %>% 
  factor(levels = c(
    "HarvardX",
    "MITx"
  ),
  ordered = FALSE
  )
# Ensure levels took and are accurate
levels(HxMx$institution_f)



# course_code -----
# Review distinct values for setting factors
unique(HxMx$course_code)
# Check if field is a factor
levels(HxMx$course_code)
# Change field to a factor
HxMx$course_code_f <- as.factor(HxMx$course_code) %>% 
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
levels(HxMx$course_code_f)



# term -----
# Review distinct values for setting factors
unique(HxMx$term)
# Check if field is a factor
levels(HxMx$term)
# Change field to a factor
HxMx$term_f <- as.factor(HxMx$term) %>% 
  factor(levels = c(
    "Fall",
    "Spring",
    "Summer"
  ),
  ordered = FALSE
  )
# Ensure levels took and are accurate
levels(HxMx$term_f)



# short_title -----
# Review distinct values for setting factors
unique(HxMx$short_title)
# Check if field is a factor
levels(HxMx$short_title)
# Change field to a factor
HxMx$short_title_f <- as.factor(HxMx$short_title) %>% 
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
levels(HxMx$short_title_f)



# full_title -----
# Review distinct values for setting factors
unique(HxMx$full_title)
# Check if field is a factor
levels(HxMx$full_title)
# Change field to a factor
HxMx$full_title_f <- as.factor(HxMx$full_title) %>% 
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
levels(HxMx$full_title_f)



# semester -----
# Review distinct values for setting factors
unique(HxMx$semester)
# Check if field is a factor
levels(HxMx$semester)
# Change field to a factor
HxMx$semester_f <- as.factor(HxMx$semester) %>% 
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
levels(HxMx$semester_f)



# ===== Write HxMx to .csv for recall after refreshing session =====
> write_csv(HxMx, "HxMx.csv")



# ----- Set Characters as Numeric -----
#HxMx$YoB_n <- as.numeric(HxMx$YoB)


# ----- Scale Data -----
#scale(HxMx)


# ----- Distance Matrix -----
