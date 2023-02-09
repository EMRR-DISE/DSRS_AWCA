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
zoop_files <- dir(path = "data/Data_CSV", pattern = "\\.csv", full.names = T)

df_zoop <- map_dfr(zoop_files, ~read_csv(.x, col_types = "cctccdddddd")) %>%
  mutate(date = parse_date_time(date, c("Ymd", "mdY")))

zoop <- map_dfr(zooplankton, ~ read_csv(.x, col_types = "cctccdddddd")) %>% 
  # Parse date_time variable from character to date_time format
  #also had to deal with fact that dates were in two different formats
  mutate(date = parse_date_time(date, c("Ymd", "mdY")))
str(df_zoop)

# Clean up column names
df_zoop <- df_zoop %>% clean_names(case = "big_camel")

# Import tow data
df_tow <- read_csv("data/zoop_tow_data.csv") 

#calculate volumes for each tow
#net mouth area based on net radius of 0.15 m
df_tow <- df_tow %>%
  mutate(tow1_vol_m3 = 0.15^2*pi * tow1_sub_area_prop * tow1_length_m) %>%
  mutate(tow2_vol_m3 = 0.15^2*pi * tow2_sub_area_prop * tow2_length_m) %>%
  mutate(tow3_vol_m3 = 0.15^2*pi * tow3_sub_area_prop * tow3_length_m) %>%
  mutate(tow4_vol_m3 = 0.15^2*pi * tow4_sub_area_prop * tow4_length_m)

#sum the four volumes for a given sampling location
df_tow <- df_tow %>%
  rowwise %>%
  mutate(tow_vol_m3 = sum(c_across(tow1_vol_m3:tow4_vol_m3), 
                          na.rm = TRUE))

df_tow <- df_tow %>%
  select(sample, date, time_pdt, tow_vol_m3)

# Replace underscores with hyphens in sample names to match tow datasheet ------
df_zoop$Sample <- gsub("_","-",df_zoop$Sample)

# Clean up column names
df_tow <- df_tow %>% clean_names(case = "big_camel")

# Combine date and time column -------------------------------------------------
df_zoop <- df_zoop %>% unite(DateTime, c("Date","Time"), sep = " ")

df_zoop$DateTime <- as_datetime(df_zoop$DateTime, 
                                 tz = "US/Pacific",
                                 format = c("%Y-%m-%d %H:%M:%OS"))

df_tow <- df_tow %>% unite(DateTime, c("Date","TimePdt"), sep = " ")

df_tow$DateTime <- as_datetime(df_tow$DateTime, 
                                tz = "US/Pacific",
                                format = c("%m/%d/%Y %H:%M:%OS"))

# Check for missing dates
df_zoop %>% filter(is.na(DateTime)) # No missing dates

df_tow %>% filter(is.na(DateTime)) # No missing dates

# Check for unique sample events
unique(df_zoop$DateTime)
unique(df_tow$DateTime)

range(df_zoop$DateTime)
range(df_tow$DateTime)

# Join tow data with biological data -------------------------------------------

test <- left_join(df_zoop, df_tow)

test <- test %>%
  filter(is.na(test$TowVolM3)) %>% 
  select(Sample, DateTime)

unique(test)

# Six sample datetimes did not join, assume that tow time is incorrect

df_tow <- df_tow %>%
  mutate(DateTime = case_when(DateTime == as.POSIXct('2018-02-06 09:55:00') 
                              & Sample == "DI-SAV-2" ~ 
                                as.POSIXct('2018-02-06 10:10:00'), 
                                      TRUE ~ DateTime)) %>%
  mutate(DateTime = case_when(DateTime == as.POSIXct('2017-06-07 11:10:00') 
                              & Sample == "DI-SAV-2" ~ 
                                as.POSIXct('2017-06-07 11:06:00'), 
                              TRUE ~ DateTime)) %>%
  mutate(DateTime = case_when(DateTime == as.POSIXct('2017-05-25 08:14:00') 
                              & Sample == "DI-WAT-1" ~ 
                                as.POSIXct('2017-05-25 08:15:00'), 
                              TRUE ~ DateTime)) %>%
  mutate(DateTime = case_when(DateTime == as.POSIXct('2017-06-07 11:35:00') 
                              & Sample == "DI-WAT-2" ~ 
                                as.POSIXct('2017-06-07 11:55:00'), 
                              TRUE ~ DateTime)) %>%
  mutate(DateTime = case_when(DateTime == as.POSIXct('2017-05-23 14:53:00') 
                              & Sample == "FI-SAV-1" ~ 
                                as.POSIXct('2017-05-23 14:30:00'), 
                              TRUE ~ DateTime)) %>%
  mutate(DateTime = case_when(DateTime == as.POSIXct('2017-05-23 13:40:00') 
                              & Sample == "LH-WAT-1" ~ 
                                as.POSIXct('2017-05-23 13:32:00'), 
                              TRUE ~ DateTime))

# Join tow data with zoop data
df_zoop <- left_join(df_zoop, df_tow)

# Change case of category names ------------------------------------------------
df_zoop <- df_zoop %>% mutate(Category = str_to_sentence(Category))

df_zoop$Taxon <- gsub("HARPACTICOIDS","Harpacticoids",df_zoop$Taxon)

# Check that all categories are unique with no duplicates/typos
#sort(unique(df_zoop$Category))

#taxa <- df_zoop %>% select(Category, Taxon)

#taxa <- unique(taxa)

#write_csv(taxa, file = "unique_zoop_taxa.csv")

# Calculate CPUE ---------------------------------------------------------------

# look at range of volumes
range(df_zoop$TowVolM3) #0.4594579 1.8378317 m^3
# NOTE: these are in m^3 instead of mL

# quick look at preserved sample volumes
unique(df_zoop$V1Ml)
range(df_zoop$V1Ml) # from 31 to 465 mL
# comparison of V1 vs V2; should be the same number
# for two samples, there was a mismatch, but I corrected this
sum(df_zoop$V1Ml-df_zoop$V2Ml) #value is zero, as it should be

# quick look at number of subsamples
unique(df_zoop$Subsamples) #always just one subsample

# quick look at subsamples
range(df_zoop$Sub1Ml) #0.1 75.0
range(df_zoop$Sub2Ml) #0.09 69.00

# calculate the number of organisms per unit volume

# (subsample count / (subsample volume/bottle volume))/tow volume
# e.g. (72 rotifers/(0.25 mL subsample/78 mL bottle vol))/1414000 ml tow vol = 
# 0.01588685 rotifers/mL
# make sure to convert tow volume from m^3 to mL for calculations
# note that there is different subsampling for meso/macrozoop vs micro zoop

# meso/macrozooplankton
# figure out how to apply this to specific categories (ie, all but "MICROZOOPLANKTON")
# this is a bit of a hack-y approach but will work in a pinch
# subset final df to exclude microzooplankton
df_zoop <- df_zoop %>%
  mutate(IndivPermL = case_when(Category == "Microzooplankton" ~ (Count/(Sub1Ml/V1Ml))/(TowVolM3*1000000),
                                TRUE ~ (Count/(Sub2Ml/V2Ml))/(TowVolM3*1000000)))

#remove columns that won't be needed in subsequent analysis (ie, stuff about various sample volumes)
df_zoop_final <- df_zoop %>%
  filter(IndivPermL != 0) %>%
  select(Sample:Taxon, IndivPermL)


test <- df_zoop_final %>%
  select(Sample:DateTime)

unique(test)

# Import taxonomy data from WoRMS and add to main df ---------------------------

df_genera <- read_csv("unique_zoop_taxa.csv")

df_taxa <- read_csv("data/zoop_genera_matched.csv")

df_zoop <- left_join(df_zoop, df_genera)
df_zoop <- left_join(df_zoop, df_taxa)

df_zoop <- df_zoop %>% 
  relocate(Genus, .after = Taxon) %>%
  relocate(Phylum, .before = Genus) %>%
  relocate(Class, .before = Genus) %>%
  relocate(Order, .before = Genus) %>%
  relocate(Family, .before = Genus)
  
test <- df_zoop %>%
  filter(Order == "Amphipoda")
