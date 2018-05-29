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
# 1. 



# ===== Hierarchical Clustering =====
### Still in early exploration



# ----- Set Characters as Numeric -----
class(HxMx$YoB) # Check class
HxMx$YoB_i <- as.integer(HxMx$YoB)
class(HxMx$YoB_i) # Check new class



# ===== Write HxMx to .csv for recall after refreshing session =====
#write_csv(HxMx, "HxMx.csv")



# ----- Scale Data -----
#scale(HxMx)


# ----- Distance Matrix -----
