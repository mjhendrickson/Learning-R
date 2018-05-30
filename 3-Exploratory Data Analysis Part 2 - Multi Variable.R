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
# HxMx <- read_csv("HxMx.csv") # update to .rds
HxMx <- readRDS("HxMx.rds")
glimpse(HxMx)

# ----- Next Steps -----
# 1. 



# ===== EDA Pt.2 Multi-Variable =====
### Still in early exploration

# ----- Institution x Grade (colored by letter_grade) -----
# Comparison of grade distribution by institution
HxMx %>% 
  subset(!is.na(grade) & grade != 0) %>% # Remove NA and 0
ggplot() +
  geom_point(mapping = aes(x = institution, y = grade, color = letter_grade)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Grades by Institution")

# Alternate comparison
HxMx %>% 
  subset(!is.na(grade) & grade != 0) %>% # Remove NA and 0
ggplot() +
  geom_count(mapping = aes(x = letter_grade, y = institution, color = institution)) +
  scale_color_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  labs(title = "Letter Grade by Institution")

# Remove F's for additional clarity
HxMx %>% 
  subset(!is.na(grade) & grade != 0 & letter_grade != "F") %>%
ggplot() +
  geom_count(mapping = aes(x = letter_grade, y = institution, color = institution)) +
  scale_color_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  labs(title = "Letter Grade by Institution")



# ----- Grade x Institution -----
HxMx %>% 
ggplot() +
  geom_histogram(mapping = aes(x = grade), binwidth = 0.05) +
  facet_grid(. ~institution) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade by Institution") 

# Remove NA and 0
HxMx %>% 
  subset(!is.na(grade) & grade != 0) %>%
ggplot() +
  geom_histogram(mapping = aes(x = grade), binwidth = 0.05) +
  facet_grid(. ~institution) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade by Institution") 

# Alternate view - frequency
HxMx %>% 
  subset(!is.na(grade) & grade != 0) %>%
ggplot(mapping = aes(x = grade)) +
  geom_freqpoly(mapping = aes(color = institution)) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade by Institution") 

# Alternate view - density, shows standardized counts
  # Appears much more similar
HxMx %>% 
  subset(!is.na(grade) & grade != 0) %>%
ggplot(mapping = aes(x = grade, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = institution)) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade by Institution - Standardized by Density") 

# Alternate view - boxplot
HxMx %>% 
  subset(!is.na(grade) & grade != 0) %>%
ggplot(mapping = aes(x = institution, y = grade, fill = institution)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  scale_y_continuous(labels = percent) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade Distribution by Institution - Removing '0's") 

# Remove F's for additional clarity
HxMx %>% 
  subset(!is.na(grade) & letter_grade != "F") %>%
ggplot(mapping = aes(x = institution, y = grade, fill = institution)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  scale_y_continuous(labels = percent) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Grade Distribution by Institution - Removing 'F's") 



# ----- Grade x Institution x Course -----
HxMx %>% 
  subset(short_title != "-" & !is.na(letter_grade)) %>% 
ggplot() +
  geom_bar(mapping = aes(x = letter_grade)) +
  facet_grid(institution ~ short_title) +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Letter Grade by Course by Institution")

# Alternate view - color by institution
HxMx %>% 
  subset(short_title != "-" & !is.na(letter_grade)) %>% 
ggplot() +
  geom_bar(mapping = aes(x = letter_grade, fill = institution)) +
  scale_fill_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  facet_grid(. ~ short_title) +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Letter Grade by Course by Institution")

# Alternate view - color by institution, remove 'F's
HxMx %>% 
  subset(short_title != "-" & !is.na(letter_grade) & letter_grade != "F") %>% 
ggplot() +
  geom_bar(mapping = aes(x = letter_grade, fill = institution)) +
  scale_fill_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  facet_grid(. ~ short_title) +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Letter Grade by Course by Institution - Removing 'F's")

# Alternate view - color by institution, remove 'D's and 'F's
HxMx %>% 
  subset(short_title != "-" & !is.na(letter_grade) & letter_grade != "D" & letter_grade != "F") %>% 
ggplot() +
  geom_bar(mapping = aes(x = letter_grade, fill = institution)) +
  scale_fill_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  facet_grid(. ~ short_title) +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Letter Grade by Course by Institution - Removing 'D's & 'F's")



# ----- Grade x Letter Grade -----
HxMx %>% 
  subset(!is.na(grade) & grade != 0) %>% 
ggplot() +
  geom_boxplot(mapping = aes(x = letter_grade, y = grade)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Grade by Letter Grade Distribution") 

# Alternate view - facet by institution
HxMx %>% 
  subset(!is.na(grade) & grade != 0) %>% 
ggplot() +
  geom_boxplot(mapping = aes(x = letter_grade, y = grade, fill = institution)) +
  scale_fill_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  facet_grid(. ~ institution) +
  scale_y_continuous(labels = percent) +
  labs(title = "Grade by Letter Grade Distribution by Institution")

# Alternate view - institutions side x side
HxMx %>% 
  subset(!is.na(grade) & grade != 0 & letter_grade != "F") %>% 
ggplot() +
  geom_boxplot(mapping = aes(x = letter_grade, y = grade, fill = institution)) +
  scale_fill_manual(values = c("#C90016", "#8A8B8C")) + # hex colors matching institution
  scale_y_continuous(labels = percent) +
  labs(title = "Grade by Letter Grade Distribution by Institution - Removing 'F's")



# ----- Registered, Viewed, Explored, Certified -----
# First Attempt
HxMx %>% 
  subset(!is.na(viewed) & !is.na(explored)) %>% # Remove NA and 0
ggplot() +
  geom_count(mapping = aes(x = viewed, y = explored)) +
  scale_x_continuous(breaks = c(0, 1), labels = comma) +
  scale_y_continuous(breaks = c(0, 1), labels = comma) +
  labs(title = "Viewed by Explored")

HxMx %>% 
  subset(!is.na(viewed) & !is.na(explored)) %>% # Remove NA and 0
ggplot() +
  geom_count(mapping = aes(x = viewed, y = explored)) +
  scale_x_discrete(breaks = c(0, 1), labels = comma) +
  scale_y_discrete(breaks = c(0, 1), labels = comma) +
  labs(title = "Viewed by Explored")



# ----- First attempt at 2 layer -----
HxMx %>% 
  subset(!is.na(nchapters) & !is.na(ndays_act)) %>% 
ggplot(data = HxMx, mapping = aes(x = nchapters, y = ndays_act)) +
  geom_point() +
  geom_smooth()
