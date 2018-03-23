# ===== Introduction =====
# Project: Learning R by Doing - HarvardX -MITx Analysis
# By: Matthew Hendrickson
# What: Exploratory Data Analysis

# -----Set up environment -----
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(DataExplorer)



# ----- Next Steps -----
# 1. User ID - too many records to show cleanly
# 2. Edit scales / limits for plots



# ===== Exploratory Data Analysis with DataExplorer =====

plot_str(HxMx)
plot_missing(HxMx)
plot_histogram(HxMx)
plot_density(HxMx)
plot_bar(HxMx)
create_report(HxMx)

# ===== EDA Pt.1 Single Variable =====

# Course ID
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = course_id)) +
  scale_x_discrete(name = " ") +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Course ID")

# Institution
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = institution)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Institution") 

# Course code
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = course_code, fill = institution)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Course Code") 

# Year & Term
#ggplot(data = HxMx) +
#  geom_bar(mapping = aes(x = year_term)) +
#  scale_y_continuous(name=" ", labels = comma) +
#  theme(axis.title.x = element_blank()) +
#  labs(title = "Year & Term") 
## Repetitive and not as clean as Year and Term below

# Year
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = year)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Year") 

# Term
HxMx %>%
  subset(!is.na(term)) %>%
ggplot() +
  geom_bar(mapping = aes(x = term)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Term") 

# Semester
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = semester)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Semester") 

# User ID
### Too many records to show cleanly
#ggplot(data = HxMx) +
#  geom_bar(mapping = aes(x = userid_DI)) +
#  theme(axis.title.x = element_blank()) +
#  labs(title = "User Id")

# Registered
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = registered)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Registered")

# Viewed
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = viewed)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Viewed") 

# Explored
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = explored)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Explored") 

# Certified
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = certified)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Certified") 

# Country code
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = final_cc_cname_DI)) +
  scale_x_discrete(name = " ") +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Country Code")

# Level of education
HxMx %>%
  subset(!is.na(LoE_DI)) %>%
ggplot() +
  geom_bar(mapping = aes(x = LoE_DI)) +
  scale_x_discrete(name = " ", limits = c("Less than Secondary", "Secondary", "Bachelor's", "Master's", "Doctorate")) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Level of Education")

# Year of birth
HxMx %>%
  subset(!is.na(YoB)) %>%
ggplot() +
  geom_bar(mapping = aes(x = YoB)) +
  scale_x_discrete(name = " ") +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Year of Birth")

# Gender
HxMx %>%
  subset(!is.na(gender)) %>%
ggplot() +
  geom_bar(mapping = aes(x = gender)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Gender") 

# Grade
HxMx %>%
  subset(!is.na(grade)) %>%
ggplot() +
  geom_bar(mapping = aes(x = grade)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade") 

# Letter grade
HxMx %>%
  subset(!is.na(letter_grade)) %>%
ggplot() +
  geom_bar(mapping = aes(x = letter_grade)) +
  scale_x_discrete(limits = c("A","B","C","D","F")) + #,NA)) + inclue for NA, also remove subset
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Letter Grade") 

# Start time
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = start_time_DI)) +
  scale_x_date (name = " ") +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Start Time")

# Last event
HxMx %>%
  summarize(
    mean = mean(last_event_DI, na.rm=TRUE),
    med = median(last_event_DI, na.rm=TRUE),
    sd = sd(last_event_DI, na.rm=TRUE),
    iqr = IQR(last_event_DI, na.rm=TRUE),
    mad = mad(last_event_DI, na.rm=TRUE),
    min = min(last_event_DI, na.rm=TRUE),
    max = max(last_event_DI, na.rm=TRUE),
    n = n()
  )

#HxMx %>%
#  subset(!is.na(last_event_DI)) %>%
#ggplot() +
#  geom_bar(mapping = aes(x = last_event_DI)) +
#  scale_x_date (name = " ") +
#  scale_y_continuous(name=" ", labels = comma) +
#  theme(axis.title.x = element_blank(),
#        axis.text.x = element_text(angle=90, vjust=0.5)) +
#  labs(title = "Last Event")  

HxMx %>%
  subset(!is.na(last_event_ym)) %>%
ggplot() +
  geom_bar(mapping = aes(x = last_event_ym)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Last Event")

# Number of events
HxMx %>%
  summarize(
    mean = mean(nevents, na.rm=TRUE),
    med = median(nevents, na.rm=TRUE),
    sd = sd(nevents, na.rm=TRUE),
    iqr = IQR(nevents, na.rm=TRUE),
    mad = mad(nevents, na.rm=TRUE),
    min = min(nevents, na.rm=TRUE),
    max = max(nevents, na.rm=TRUE),
    n = n()
  )

HxMx %>%
  subset(!is.na(nevents)) %>%
ggplot() +
  geom_bar(mapping = aes(x = nevents)) +
  scale_y_continuous(name=" ", labels = comma) + #, limits=c(0,150)) + to change Y axis
  theme(axis.title.x = element_blank()) +
  labs(title = "Number of Events") 

# Events indicator
HxMx %>%
  subset(!is.na(nevents_ind)) %>%
ggplot() +
  geom_bar(mapping = aes(x = nevents_ind)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Participated in an Event") 

# Number of days active
HxMx %>%
  subset(!is.na(ndays_act)) %>%
ggplot() +
  geom_bar(mapping = aes(x = ndays_act)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Number of Days") 

# Days active indicator
HxMx %>%
  subset(!is.na(ndays_act_ind)) %>%
ggplot() +
  geom_bar(mapping = aes(x = ndays_act_ind)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Was Active") 

# Number of video plays
HxMx %>%
  subset(!is.na(nplay_video)) %>%
ggplot() +
  geom_bar(mapping = aes(x = nplay_video)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Number of Video Plays") 

# Video plays indicator
HxMx %>%
  subset(!is.na(nplay_video_ind)) %>%
ggplot() +
  geom_bar(mapping = aes(x = nplay_video_ind)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Played Video") 

# Number of chapters
HxMx %>%
  subset(!is.na(nchapters)) %>%
ggplot() +
  geom_bar(mapping = aes(x = nchapters)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Number of Chapters") 

# Chapters indicator
HxMx %>%
  subset(!is.na(nchapters_ind)) %>%
ggplot() +
  geom_bar(mapping = aes(x = nchapters_ind)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Read a Chapter") 

# Number of forum posts
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = nforum_posts)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Number of Forum Posts") 

# Forum posts indicator
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = nforum_posts_ind)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Posted in Forum") 

# Roles
# All values are NULL - remove
#ggplot(data = HxMx) +
#  geom_bar(mapping = aes(x = roles)) +
#  scale_y_continuous(name=" ", labels = comma) +
#  theme(axis.title.x = element_blank()) +
#  labs(title = "Roles") 

# Inconsistent Flag
HxMx %>%
  subset(!is.na(incomplete_flag)) %>%
ggplot() +
  geom_bar(mapping = aes(x = incomplete_flag)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Incomplete Flag") 

# Short title
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = short_title)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Course Short Title") 

# Full title
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = full_title)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Course Full Title") 



# ===== EDA Pt.2 Multi-Variable =====

# First attempt at Institution x Grade (colored by letter_grade)
### Still a work in progress
ggplot(data = HxMx) +
  geom_point(mapping = aes(x = institution, y=grade, color=letter_grade)) +
  scale_y_continuous(name=" ", labels = comma) +
  labs(title = "Grades by Institution")

# First attempt at Grade x Institution
### Still a work in progress
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = grade)) +
  facet_grid(. ~HxMx$institution) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade") 

ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = grade)) +
  facet_grid(institution ~ short_title) +
  scale_y_continuous(name=" ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Grade")
