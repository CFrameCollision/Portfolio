library(tidyverse)
library(readxl)
library(shinyBS)
library(sdcMicro)

# You must set your wd before running this script.
setwd(
  "/home/cframecollisions/Desktop/repositories/Portfolio/Killian-Research-Example"
)

set.seed(1234)
dataMain <- as.data.frame(read_xlsx("data/WSWBS Combined.xlsx"))

sdcMicro::sdcApp()
