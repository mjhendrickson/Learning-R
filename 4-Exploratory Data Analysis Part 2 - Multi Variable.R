# ===== Introduction =====
# Project: Learning R by Doing - HarvardX-MITx Analysis
# By: Matthew Hendrickson
# What: Exploratory Data Analysis Part 2 - Multi-Variable

# ----- Set up environment -----
library(tidyverse)
library(forcats)
library(scales)
library(binr)

# ----- Open HxMx after session refresh -----
HxMx <- read_csv("HxMx.csv")
glimpse(HxMx)

# ----- Next Steps -----
# 1. 

# ===== EDA Pt.2 Multi-Variable =====
### Still in early exploration

# ----- Institution x Grade (colored by letter_grade)
ggplot(data = HxMx) +
  geom_point(mapping = aes(x = institution, y = grade, color = letter_grade)) +
  scale_y_continuous(name = " ", labels = comma) +
  labs(title = "Grades by Institution")

# Remove NA and 0
HxMx %>% 
  subset(!is.na(grade) & grade != 0) %>%
  ggplot() +
  geom_point(mapping = aes(x = institution, y = grade, color = letter_grade)) +
  scale_y_continuous(name = " ", labels = comma) +
  labs(title = "Grades by Institution")



# ----- Grade x Institution
ggplot(data = HxMx) +
  geom_bar(mapping = aes(x = grade), binwidth = 0.05) +
  facet_grid(. ~institution) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade") 

# Remove NA and 0
HxMx %>% 
  subset(!is.na(grade) & grade != 0) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = grade), binwidth = 0.05) +
  facet_grid(. ~institution) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade") 


# ----- Grade x Institution x Course
HxMx %>% 
  subset(short_title != "-" &
           !is.na(letter_grade)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = letter_grade)) +
  facet_grid(institution ~ short_title) +
  scale_y_continuous(name = " ", labels = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Grade")



# ----- Grade x Letter Grade
HxMx %>% 
  subset(!is.na(grade) & grade != 0) %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = letter_grade, y = grade)) +
  scale_y_continuous() +
  labs(title = "Grade") 



# ----- First attempt at 2 layer
HxMx %>% 
  subset(!is.na(nchapters) & !is.na(ndays_act)) %>% 
  ggplot(data = HxMx, mapping = aes(x = nchapters, y = ndays_act)) +
  geom_point() +
  geom_smooth()
