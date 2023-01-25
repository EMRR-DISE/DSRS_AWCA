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

# Import Raw Data --------------------------------------------------------------

# Import data files
zoop_files <- dir(path = "data/CSVs", pattern = "\\.csv", full.names = T)

df_zoop <- map_dfr(zoop_files, ~read_csv(.x))

# Clean up column names
df_zoop <- df_zoop %>% clean_names(case = "big_camel")

# Combine date and time column -------------------------------------------------
df_zoop <- df_zoop %>% unite(DateTime, c("Date","Time"), sep = " ")

df_zoop$DateTime <- as_datetime(df_zoop$DateTime, 
                                 tz = "US/Pacific",
                                 format = c("%Y-%m-%d %H:%M:%OS"))

# Check for missing dates
df_zoop %>% filter(is.na(DateTime)) # No missing dates

# Change case of category names ------------------------------------------------
df_zoop <- df_zoop %>% mutate(Category = str_to_sentence(Category))

df_zoop$Taxon <- gsub("HARPACTICOIDS","Harpacticoids",df_zoop$Taxon)

# Check that all categories are unique with no duplicates/typos
sort(unique(df_zoop$Category))

taxa <- as_tibble(sort(unique(df_zoop$Taxon)))

taxa <- df_zoop %>%
  select(Category, Taxon)

write_csv(taxa, file = "unique_zoop_taxa.csv")
