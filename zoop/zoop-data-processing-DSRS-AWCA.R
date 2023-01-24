# Processing zooplankton data for DSRS-AWCA project
# Ted Flynn
# 1/24/2023

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")

# Set working directory
setwd("./zoop/")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

# Importing Raw Data -----------------------------------------------------------

# Import data files
zoop_files <- dir(path = "data/CSVs", pattern = "\\.csv", full.names = T)

df_zoop <- map_dfr(zoop_files, ~read_csv(.x))
