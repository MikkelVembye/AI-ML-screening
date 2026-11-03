# Data inspection

library(readr)
library(tidyverse)
library(striprtf)


Endedijk2022 <- 
  read.csv(file = "Data/uyb7x-osfstorage-Data-archive/04_Endedijk et al. (2022).csv")


Fong <- read.csv("Data/uyb7x-osfstorage-Data-archive/07_Fong et al. (2021).csv")

Fong_all <- read.csv("Data/Fong_with_final_included.csv")
