getwd()
library(readxl)
raw_test <- read_excel("C:/Users/ASUS/OneDrive/Desktop/STA330-Group07-CommuterStress/data/raw/commuter-survey-complete-2026-11-22.xlsx", sheet = "Data (labels)")
colnames(raw_test)

# note that relative paths actually works here
library(readxl)
raw_test <- read_excel("data/raw/commuter-survey-complete-2026-11-22.xlsx", sheet = "Data (codes)")
colnames(raw_test)
source("scripts/00_config.R")
