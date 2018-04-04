# ===== Introduction =====
# Project: Learning R by Doing - HarvardX-MITx Analysis
# By: Matthew Hendrickson
# What: Exploratory Data Analysis

# ----- Set up environment -----
library(tidyverse)
library(forcats)
library(scales)

# ----- Open HxMx after session refresh -----
HxMx <- read_csv("HxMx.csv")
glimpse(HxMx)

# ----- Next Steps -----
# 1. 



# ===== EDA Pt.1 Single Variable =====

# ----- Course ID (character) -----
# Distribution of course_id, colored by institution
# More useful if split out by institution, course_code, and term
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = fct_infreq(course_id), fill = institution)) +
  scale_fill_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  scale_x_discrete(name = " ") +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Course ID")



# ----- Institution (character) -----
# Distribution of institution, colored by institution
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = institution, fill = institution)) +
  scale_fill_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Institution") 

# Counts by institution
HxMx %>%
  group_by(institution) %>%
  summarize(n = n())



# ----- Course code (character) -----
# Distribution of course_code, colored by institution
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = fct_infreq(course_code), fill = institution)) +
  scale_fill_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Course Code")

# Counts by course_code
HxMx %>%
  group_by(course_code) %>%
  summarize(n = n())



# ----- Year (integer) -----
# Distribution of year
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = year)) +
  scale_x_continuous(name = " ", breaks = c(2012, 2013)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Year") 

# Counts by year
HxMx %>%
  group_by(year) %>%
  summarize(n = n())



# ----- Term (character) -----
# Distribution of term
HxMx %>%
  subset(!is.na(term)) %>%
ggplot() +
  geom_bar(mapping = aes(x = term)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Term") 

# Counts by term
HxMx %>%
  group_by(term) %>%
  summarize(n = n())



# ----- Semester (character) -----
# Distribution of semester
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = semester)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Semester") 

# Counts by semester
HxMx %>%
  group_by(semester) %>%
  summarize(n = n())



# ----- User ID (character) -- too many records to show cleanly -----
# Distribution of userid_DI
# Some users took multiple courses
#ggplot(data = HxMx) +
#  geom_bar(mapping = aes(x = userid_DI)) +
#  theme(axis.title.x = element_blank()) +
#  labs(title = "User ID")



# ----- Registered (integer) -----
# Distribution of registered
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = registered)) +
  scale_x_discrete(name = " ", limits = c("Yes")) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Registered")

# Counts by registered
HxMx %>%
  group_by(registered) %>%
  summarize(n = n())



# ----- Viewed (integer) -----
# Distribution of viewed
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = viewed)) +
  scale_x_continuous(name = " ", breaks = c(0, 1)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Viewed") 

# Counts by viewed
HxMx %>%
  group_by(viewed) %>%
  summarize(n = n())



# ----- Explored (integer) -----
# Distribution of explored
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = explored)) +
  scale_x_continuous(name = " ", breaks = c(0, 1)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Explored") 

# Counts by explored
HxMx %>%
  group_by(explored) %>%
  summarize(n = n())



# ----- Certified (integer) -----
# Distribution of certified
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = certified)) +
  scale_x_continuous(name = " ", breaks = c(0, 1)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Certified") 

# Counts by certified
HxMx %>%
  group_by(certified) %>%
  summarize(n = n())



# ----- Country code (character) -----
# Distribution of final_cc_cname_DI
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = fct_infreq(final_cc_cname_DI))) +
  scale_x_discrete(name = " ") +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Country Code")

# Counts by final_cc_cname_DI, sorted desc
HxMx %>%
  filter(!is.na(final_cc_cname_DI)) %>% 
  group_by(final_cc_cname_DI) %>% 
  tally(sort = T) %>% 
  arrange(desc(n))


# ----- Level of education (character) -----
# Distribution of LoE_DI
HxMx %>%
  subset(!is.na(LoE_DI)) %>%
ggplot() +
  geom_bar(mapping = aes(x = LoE_DI)) +
  scale_x_discrete(name = " ", limits = c("Less than Secondary", 
                                          "Secondary", 
                                          "Bachelor's", 
                                          "Master's", 
                                          "Doctorate")) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Level of Education")

# Counts by LoE_DI, sorted desc
HxMx %>%
  filter(!is.na(LoE_DI)) %>% 
  group_by(LoE_DI) %>% 
  tally(sort = T) %>% 
  arrange(desc(n))



# ----- Year of birth (character) -----
# Distribution of YoB
HxMx %>%
  subset(!is.na(YoB)) %>%
ggplot() +
  geom_bar(mapping = aes(x = YoB)) +
  scale_x_discrete(name = " ") +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Year of Birth")

# Counts by YoB, sorted desc
HxMx %>%
  filter(!is.na(YoB)) %>% 
  group_by(YoB) %>% 
  tally(sort = T) %>% 
  arrange(desc(n))



# ----- Gender (character) -----
# Distribution of gender
# Keep 'o' as a value - 17 observations
HxMx %>%
  subset(!is.na(gender)) %>%
ggplot() +
  geom_bar(mapping = aes(x = gender)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Gender") 

# Counts by gender
HxMx %>%
  group_by(gender) %>%
  summarize(n = n())



# ----- Grade (double) -----
# Distribution of grade
HxMx %>%
  subset(!is.na(grade)) %>%
ggplot() +
  geom_bar(mapping = aes(x = grade)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade") 

# Distribution of grade, remove grade of "0"
HxMx %>%
  subset(!is.na(grade) & grade != 0) %>%
ggplot() +
  geom_bar(mapping = aes(x = grade)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade") 

# Distribution of grade, binning results
HxMx %>%
  subset(!is.na(grade)) %>%
ggplot() +
  geom_histogram(mapping = aes(x = grade)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade") 

# Distribution of grade, binning results, remove grade of 0
HxMx %>% 
  subset(!is.na(grade) & grade != 0) %>% 
ggplot() +
  geom_histogram(mapping = aes(x = grade), binwidth = 0.1) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade") 



# ----- Letter grade (character) -----
# Distribution of letter_grade
HxMx %>%
  subset(!is.na(letter_grade)) %>%
ggplot() +
  geom_bar(mapping = aes(x = letter_grade)) +
  scale_x_discrete(limits = c("A", "B", "C", "D", "F")) + #,NA)) + inclue for NA, also remove subset
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Letter Grade") 

# Distribution of letter_grade, remove "F"
HxMx %>%
  subset(!is.na(letter_grade)) %>%
ggplot() +
  geom_bar(mapping = aes(x = letter_grade)) +
  scale_x_discrete(limits = c("A", "B", "C", "D")) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Letter Grade") 

# Counts by letter_grade
HxMx %>%
  group_by(letter_grade) %>%
  summarize(n = n())



# ----- Start time (date) -----
# Summary stats of start_time_DI
HxMx %>%
  summarize(
    mean  = mean(start_time_DI, na.rm = TRUE),
    med   = median(start_time_DI, na.rm = TRUE),
    iqr   = IQR(start_time_DI, na.rm = TRUE),
    mad   = mad(start_time_DI, na.rm = TRUE),
    min   = min(start_time_DI, na.rm = TRUE),
    max   = max(start_time_DI, na.rm = TRUE),
    n     = n()
  )

# Distribution of start_time_DI
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = start_time_DI)) +
  scale_x_date(name = " ") +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Start Time")

# Distribution start_time_ym by month
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = start_time_ym)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Start Month")

# Counts by start_time_ym
HxMx %>%
  group_by(start_time_ym) %>%
  summarize(n = n())



# ----- Last event (date) -----
# Summary stats of last_event_DI
HxMx %>%
  summarize(
    mean  = mean(last_event_DI, na.rm = TRUE),
    med   = median(last_event_DI, na.rm = TRUE),
    sd    = sd(last_event_DI, na.rm = TRUE),
    mad   = mad(last_event_DI, na.rm = TRUE),
    min   = min(last_event_DI, na.rm = TRUE),
    max   = max(last_event_DI, na.rm = TRUE),
    n     = n()
  )

# Distribution of last_event_DI
HxMx %>%
  subset(!is.na(last_event_DI)) %>%
ggplot() +
  geom_bar(mapping = aes(x = last_event_DI)) +
  scale_x_date(name = " ") +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Last Event")  

# Investigate date outliers
# Find course(s) with extreme dates
filter(HxMx, last_event_DI > "2013-09-06") %>% 
  group_by(last_event_DI, course_code) %>% 
  summarize(count = n())

# Isolate semester
filter(HxMx, course_code == "CB22x") %>% 
  group_by(course_code, semester) %>% 
  summarize(count = n())

# Plot isolated course(s)
filter(HxMx, course_code == "CB22x") %>% 
  subset(!is.na(last_event_DI)) %>% 
ggplot() +
  geom_histogram(mapping = aes(x = last_event_DI)) +
  scale_x_date(name = " ") +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Last Event") 

# Distribution of last_event_ym
HxMx %>%
  subset(!is.na(last_event_ym)) %>%
ggplot() +
  geom_bar(mapping = aes(x = last_event_ym)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Last Event Month")

# Counts by last_event_ym
HxMx %>%
  group_by(last_event_ym) %>%
  summarize(n = n())



# ----- Number of events (integer) -----
# Summary stats of nevents
HxMx %>%
  summarize(
    mean  = mean(nevents, na.rm = TRUE),
    med   = median(nevents, na.rm = TRUE),
    sd    = sd(nevents, na.rm = TRUE),
    iqr   = IQR(nevents, na.rm = TRUE),
    mad   = mad(nevents, na.rm = TRUE),
    min   = min(nevents, na.rm = TRUE),
    max   = max(nevents, na.rm = TRUE),
    n     = n()
  )

# Distribution of nevents
# Not useful without modification - check nevents_ind
# Reduce to a reasonable range
HxMx %>%
  subset(!is.na(nevents) & nevents <= 250) %>%
ggplot() +
  geom_bar(mapping = aes(x = nevents)) +
  scale_y_continuous(name = " ", labels = comma) + #, limits=c(0,150)) + to change Y axis
  theme(axis.title.x = element_blank()) +
  labs(title = "Number of Events") 

# Counts by nevents, sorted desc
HxMx %>%
  filter(!is.na(nevents)) %>% 
  group_by(nevents) %>% 
  tally(sort = T) %>% 
  arrange(desc(n))

# Counts by nevents, sorted asc
HxMx %>%
  filter(!is.na(nevents) & nevents >= 5000) %>% 
  group_by(nevents) %>% 
  tally(sort = T) %>% 
  arrange(n)



# ----- Events indicator (integer) -----
# Distribution of nevents_ind
HxMx %>%
  subset(!is.na(nevents_ind)) %>%
ggplot() +
  geom_bar(mapping = aes(x = nevents_ind)) +
  scale_x_discrete(name = " ", limits = c("Yes")) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Participated in an Event") 

# Counts by nevents_ind
HxMx %>%
  group_by(nevents_ind) %>%
  summarize(n = n())



# ----- Number of days active (integer) -----
# Distribution of ndays_act
# Not useful without modification - check ndays_act_ind
# Reduce to a reasonable range
HxMx %>%
  subset(!is.na(ndays_act) & ndays_act <= 40) %>%
ggplot() +
  geom_bar(mapping = aes(x = ndays_act)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Number of Days") 

# Counts by nevents, sorted desc
HxMx %>%
  filter(!is.na(ndays_act)) %>% 
  group_by(ndays_act) %>% 
  tally(sort = T) %>% 
  arrange(desc(n))

# Counts by ndays_act, sorted asc
HxMx %>%
  filter(!is.na(ndays_act) & ndays_act >= 150) %>% 
  group_by(ndays_act) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))



# ----- Days active indicator (integer) -----
# Distribution of ndays_act_ind
HxMx %>%
  subset(!is.na(ndays_act_ind)) %>%
ggplot() +
  geom_bar(mapping = aes(x = ndays_act_ind)) +
  scale_x_discrete(name = " ", limits = c("Yes")) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Was Active") 

# Counts by ndays_act_ind
HxMx %>%
  group_by(ndays_act_ind) %>%
  summarize(n = n())



# ----- Number of video plays (integer) -----
# Distribution of nplay_video
# Not useful without modification - check nplay_video_ind
# Reduce to a reasonable range
HxMx %>%
  subset(!is.na(nplay_video)) %>%
ggplot() +
  geom_bar(mapping = aes(x = nplay_video) & nplay_video <= 350) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Number of Video Plays") 

# Counts by nplay_video, sorted desc
HxMx %>%
  filter(!is.na(nplay_video)) %>% 
  group_by(nplay_video) %>% 
  tally(sort = T) %>% 
  arrange(desc(n))

# Counts by nplay_video, sorted asc
HxMx %>%
  filter(!is.na(nplay_video) & nplay_video >= 10000) %>% 
  group_by(nplay_video) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))



# ----- Video plays indicator (integer) -----
# Distribution of nplay_video_ind
HxMx %>%
  subset(!is.na(nplay_video_ind)) %>%
ggplot() +
  geom_bar(mapping = aes(x = nplay_video_ind)) +
  scale_x_continuous(name = " ", breaks = c(1)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Played Video") 

# Counts by nplay_video_ind
HxMx %>%
  group_by(nplay_video_ind) %>%
  summarize(n = n())



# ----- Number of chapters (integer) -----
# Distribution of nchapters
HxMx %>%
  subset(!is.na(nchapters)) %>%
ggplot() +
  geom_bar(mapping = aes(x = nchapters)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Number of Chapters") 

# Counts by nchapters
HxMx %>%
  group_by(nchapters) %>%
  summarize(n = n())



# ----- Chapters indicator (integer) -----
# Distribution of nchapters_ind
HxMx %>%
  subset(!is.na(nchapters_ind)) %>%
ggplot() +
  geom_bar(mapping = aes(x = nchapters_ind)) +
  scale_x_discrete(name = " ", limits = c("Yes")) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Read a Chapter") 

# Counts by nchapters_ind
HxMx %>%
  group_by(nchapters_ind) %>%
  summarize(n = n())



# ----- Number of forum posts (integer) -----
# Distribution of nforum_posts
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = nforum_posts)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Number of Forum Posts") 

# Counts by nforum_posts
HxMx %>%
  group_by(nforum_posts) %>%
  summarize(n = n())



# ----- Forum posts indicator (integer) -----
# Distribution of nforum_posts_ind
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = nforum_posts_ind)) +
  scale_x_continuous(name = " ", breaks = c(0, 1)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Posted in Forum") 

# Counts by nforum_posts_ind
HxMx %>%
  group_by(nforum_posts_ind) %>%
  summarize(n = n())



# ----- Inconsistent Flag (integer) -----
# Distribution of incomplete_flag
HxMx %>%
  subset(!is.na(incomplete_flag)) %>%
ggplot() +
  geom_bar(mapping = aes(x = incomplete_flag)) +
  scale_x_discrete(name = " ", limits = c("Yes")) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Incomplete Flag") 

# Counts by incomplete_flag
HxMx %>%
  group_by(incomplete_flag) %>%
  summarize(n = n())



# ----- Short title (character) -----
# Distribution of short_title, colored by institution
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = fct_infreq(short_title), fill = institution)) +
  scale_fill_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Course Short Title") 

# Counts by short_title
HxMx %>%
  group_by(short_title) %>%
  summarize(n = n())



# ----- Full title (character) -----
# Distribution of full_title, colored by institution
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = fct_infreq(full_title), fill = institution)) +
  scale_fill_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Course Full Title") 

# Counts by full_title
HxMx %>%
  group_by(full_title) %>%
  summarize(n = n())
