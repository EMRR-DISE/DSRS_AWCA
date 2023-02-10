#Delta Smelt Resiliency Strategy 
#Aquatic Weed Control Action
#Phytoplankton Data 2017-2018
#Format raw data in preparation for visualization and analysis

#required packages
library(tidyverse) #suite of data science tools
library(janitor) #functions for cleaning up data sets
library(hms) #working with date/time
library(lubridate) #working with dates
library(readxl) #importing data from excel files

#Notes
#For all BSA files from 2013 to 2021, the column "Number of cells per unit" really means "Total cells", 
#which is the total number of cells counted for that taxon in a particular sample
#calculations in this script were corrected accordingly on 2/10/2022

#To do list------------------
#is time in PST or PDT? I think the latter but need to check; it might even have changed over time
#also figure out why some date-time failed to parse; probably missing times
#should check to see if organisms per ml and cells per ml are equal for cases where phyto_form = individual
#prep data set for publishing on EDI
#move analysis and most plotting to a different script
#use biovolume to calculate biomass
#then calculate fatty acid content
#try to match plankton samples with veg rake samples
#look at all comments from taxonomists for red flags; most probably just note high sediment levels

# Read in and combine phyto abundance data----------------------------------------------

#Create character vectors of all files (n = 5) 
phyto_files <- dir(path = "./phyto/data_input/abundances",pattern = ".xlsx|.XLSX", full.names = T)

#Combine all of the data files into a single df
#column names and order are identical for the first three files
#But the last two files have additional columns

phytoplankton <- phyto_files %>% 
  #set_names() grabs the file names
  set_names() %>%  
  #reads in the files, .id adds the file name column
  #also imports all data as text because date and time columns don't import correctly otherwise
  #and the number of columns varies among the five data files
  map_dfr(~read_excel(.x, col_types = "text"), .id = "source") %>% 
  #reduce file name to just the needed info (ie, file name)
  mutate(file_name = as.factor(str_sub(source,-12,-6))) %>% 
  #clean up formatting of column names
  clean_names() %>% 
  glimpse()
#succeeded in combining all the sample files
#but date and time are in weird format

# Read in the other files----------------

#read in taxonomy data
#update this file with the updates/corrections I got from AlgaeBase 2/24/2022
taxonomy <- read_csv("phyto/data_input/other/phyto_2018-12_taxonomy_complete.csv")

#read in sampling month data
#use this to check for typos in sampling dates and also to assign survey months (samples aren't always taken with sampling month)
sample_month <- read_csv("phyto/data_input/other/survey_months_complete.csv")

#format the sample data set------------
#NOTE: leave out derived columns (ie, calculated from other columns)
#then do calculations myself to minimize risk of errors

phyto_clean <- phytoplankton %>% 
  #remove empty rows created by linear cell measurement rows (length, width, depth)  
  #a little tricky just because the survey name appears in every row including the otherwise empty ones
  #chose the taxon column as the ones to check for missing data
  drop_na(taxon) %>%
  #rename the confusingly incorrectly name column
  rename(total_cells=number_of_cells_per_unit) %>% 
  #subset to just the needed columns
  select(file_name
         ,sample_date
         ,sample_time
         ,station = station_code
         ,volume_received_m_l #probably optional
         ,volume_analyzed_m_l #needed
         ,field_of_view_mm2 #needed
         ,slide_chamber_area_mm2 #needed
         ,area_counted #derived column?
         ,number_of_fields_counted #needed
         ,factor #derived column we can drop
         ,bsa_tin #keep for now; will make filtering by taxa a little easier 
         ,taxon
         ,diatom_soft_body #will drop this later; check against my taxonomy first
         ,genus
         ,species
         ,synonym
         ,unit_abundance #needed
         ,total_cells #needed
         ,gald #probably optional
         ,phyto_form = colony_filament_individual_group_code
         ,taxonomist #useful for look at potential ID biases
         ,comments #need to look at these before consider dropping this column
         ,biovolume_1:biovolume_10 #needed
         ) %>% 
  mutate(
    #change some columns from text to numeric
    across(c(volume_received_m_l:bsa_tin,unit_abundance:gald,biovolume_1:biovolume_10), as.numeric)
    #change some columns from character to factor
    ,across(c(station,phyto_form,taxonomist),as.factor)
    #format date; check date and time against input files to see if formatting worked correctly
    ,date1 = as.Date(as.numeric(sample_date),origin = "1899-12-30")
    #format time
    ,time1 = as_hms(as.numeric(sample_time)*60*60*24)
    #calculate percent of sample volume analyzed
    ,volume_analyzed_prop = volume_received_m_l/volume_analyzed_m_l
    #create new column that calculates mean biovolume per cell
    #add units to biovolume
    ,mean_cell_biovolume = mean(c_across(biovolume_1:biovolume_10),na.rm=T)
    #create new column that calculates organisms per mL; round number to nearest tenth
    #different from cells per mL because some organisms are multicellular
    ,organisms_per_ml = round((unit_abundance*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
    #,organisms_per_ml_easy = (unit_abundance*factor)
    #create new column that calculates cells per mL; round number to nearest tenth
    ,cells_per_ml = round((total_cells*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
    #,cells_per_ml_easy = (total_cells*factor)
    #create a column that calculates biovolume per mL
    #units for biovolume are cubic microns; old version is incorrect calculations; round number to nearest tenth
    #,biovolume_per_ml_old = organisms_per_ml * total_cells * mean_cell_biovolume
    ,biovolume_per_ml = round((total_cells* mean_cell_biovolume*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
    #,biovolume_per_ml_easy = factor * total_cells * mean_cell_biovolume
  )  %>% 
  select(file_name
         ,date1
         ,time1
         ,station 
         ,volume_analyzed_m_l #needed
         ,volume_analyzed_prop
         ,field_of_view_mm2 #needed
         ,slide_chamber_area_mm2 #needed
         #,area_counted #derived column
         ,number_of_fields_counted #needed
         ,bsa_tin #keep for now; will make filtering by taxa a little easier 
         ,taxon
         ,diatom_soft_body #will drop this later; check against my taxonomy first
         ,genus
         ,species
         ,synonym
         ,unit_abundance #needed
         ,total_cells #needed
         ,gald #possibly useful to some users
         ,phyto_form 
         ,taxonomist #useful for look at potential ID biases
         ,comments #need to look at these before consider dropping this column
         ,organisms_per_ml
         ,cells_per_ml
         ,biovolume_per_ml
         ,mean_cell_biovolume #decided this is useful to users
  ) %>% 
  glimpse()
#I prefer to use the formulas based on the more raw version of the data 
#rather than the ones based on the factor column
#which is a derived column and therefore more prone to errors
#NOTE: 27 failed to parse for date_time_pst; figure out why; NAs?

#look for typos in station names
unique(phyto_clean$station)
#33 unique names; should only be 16 names (4 locations x 2 habitats x 2 reps)
#inconsistent use of "-" and "_" in site names
#Replace all occurrences of "-" with "_" in site names
#also DI_SAV_1 was incorrectly called DI_SAV_18 in Dec. 2018

phyto_cleaner <- phyto_clean %>% 
  mutate(
    #replace "-" with "_", which fixes most station name typos
    station2 = str_replace_all(station, pattern = "-", replacement = "_")) %>% 
  #drop old station column
  select(-station) %>% 
  #fix DI_SAV_18 typo
  mutate(station = case_when(station2 == "DI_SAV_18"~ "DI_SAV_1",TRUE~station2)) %>% 
  #drop unneeded station column
  select(-station2) %>% 
  #move up new station column
  relocate(station,.after = time1) %>% 
  glimpse()

#check for typos in station names again
unique(phyto_cleaner$station)
#now 16 station names as expected

#investigate missing times
dt_parse <- phyto_cleaner%>% 
  filter(is.na(time1)) %>% 
  distinct(date1,station) %>%
  arrange(date1,station) %>% 
  glimpse()
#5 missing times

#create small dataframe that adds missing times from companion zoop data files
times_corr <- dt_parse %>% 
  distinct(date1,station) %>%
  add_column(time = c("8:40:00","13:52:00","14:15:00","14:15:00","14:30:00")) %>% 
  mutate(time2=as_hms(time))%>% 
  select(-time) %>% 
  glimpse()

#also look for typos in sampling date and add column for sampling month

#first format the sample month df
smonth <- sample_month %>% 
  rename(station = site
         ,date1 = date) %>% 
  glimpse()

#now look for date typos
dtypo <- anti_join(phyto_cleaner,smonth) %>% 
  distinct(date1,station) %>% 
  arrange(date1,station)
#6 non-matching station-date combos
#but just three incorrect dates

#fill in missing times and fix date typos
phyto_cleanest <- phyto_cleaner %>%
  #add times as a new column
  left_join(times_corr) %>% 
  mutate(
    #create new time column with all times present
    time = case_when(is.na(time1)~time2,TRUE~time1)
    #correct date typos
    ,date = case_when(date1 == as_date("2018-03-13") ~ as_date("2018-03-14")
                       ,date1 == as_date("2018-04-16") ~ as_date("2018-04-17")
                       ,date1 == as_date("2018-10-10") ~ as_date("2018-10-16")
                      ,TRUE~date1)
    #add date-time column; not sure this is pst; might be pdt
  ,date_time_pst = ymd_hms(as.character(paste(date, time)),tz="Etc/GMT+8")
  ) %>% 
  #move up new time column
  relocate(c(date,time,date_time_pst),.after = date1) %>% 
  #drop old time and date-time columns
  select(-c(time1,time2,date1)) %>% 
  glimpse()

#look for date typos again
dtypo2 <- anti_join(phyto_cleanest,smonth) %>% 
  distinct(date,station) %>% 
  arrange(date,station)
#all are now corrected

#check a specific date example
dypos3 <- phyto_cleanest %>% 
  filter(date==as_date("2018-03-14")) %>% 
  distinct(date,station)
#incorrect date was replaced with correct date

#look for missing times again
dt_parse2 <- phyto_cleanest%>% 
  filter(is.na(time)) %>% 
  distinct(date,station) %>%
  arrange(date,station) %>% 
  glimpse()
#no missing times now

#check a specific time example
dt_parse3 <- phyto_cleanest %>% 
  filter(station =="LH_WAT_2" & date==as_date("2017-06-29")) %>% 
  distinct(date,station,time)
#missing time was replaced with correct time

#check for NAs in date-time
dt_parse4 <- phyto_cleanest %>% 
  filter(is.na(date_time_pst)) %>% 
  distinct(date,station)
#no NAs for date-time

#look at number of samples per station
samp_count<-phyto_cleanest %>% 
  distinct(station, date) %>% 
  group_by(station) %>% 
  summarize(count = n())
#need to look into this some more
#one case of NA for station which shouldn't be

#look at ranges of some of the raw data columns to decide 
#whether to retain in dataset or just describe in metadata

range(phyto_cleanest$volume_analyzed_m_l,na.rm = T) #1-10; pretty wide range
range(phyto_cleanest$field_of_view_mm2,na.rm = T) #0.0683000 0.0697465; pretty narrow
range(phyto_cleanest$slide_chamber_area_mm2,na.rm = T) #314.159 314.159
range(phyto_cleanest$area_counted,na.rm = T) #0.3415 1.5709, somewhat large range

hist(phyto_cleanest$volume_analyzed_m_l)




#Add higher level taxonomic information using the algaeClassify package--------
#as of 3/4/2022 this package wasn't working

#started by trying out examples from documentation. they didn't work
#algae_search(genus='Anabaena',species='flos-aquae',long=FALSE)

#data(lakegeneva)
#lakegeneva=lakegeneva[1,] ##use 1 row for testing
#lakegeneva.algaebase<-
#  spp_list_algaebase(lakegeneva,phyto.name='phyto_name',long=FALSE,write=FALSE)

#Add higher level taxonomic information and habitat information manually-------------

#names(taxonomy)

#subset to just the needed columns
#will match genus name between the two data frames
taxon_high<-taxonomy %>% 
  clean_names() %>% 
  select(kingdom
  ,phylum
  ,class
  ,genus
  ,algal_type) %>%  #confirmed that there is just one type per genus
#this approach is very simple because it removes the need for exact matches in the taxon names
#part of the difficulty in matching taxon names is presence of "cf." for many taxa
#unfortunately by just matching by genus and not taxon, we lose the habitat type, and salinity range info
#also with species level data, there's a chance the taxonomy dataset doesn't include every species in the samples
  #remove some (likely incorrect) combinations that are creating duplicates for genus
  #check AlgaeBase to see which taxonomic info is correct
  filter(!(genus == "Achnanthidium" & class =="Fragilariophyceae") & 
           !(genus == "Elakatothrix" & class =="Chlorophyceae") &
           !(genus == "Leptocylindrus" & algal_type =="Centric diatom"))%>% 
  #condense taxonomy data set to just the unique combinations
  distinct(kingdom,phylum,class,genus)  
#initially some genera appeared more than once in this taxonomy data set
#but this has been corrected

#investigating duplicates for genus---------------

#count number of times each genus appears
#ideally this would be once
tax_gen_sum<-data.frame(table(taxon_high$genus)) 

#look at repeat genera
tax_gen_sum_sub<-filter(tax_gen_sum, Freq >1)
#first time doing this there were three genera plus "unknown"
#Achnanthidium    n=2, Elakatothrix    n=2, Leptocylindrus   n=2
#fixed this so that only "unknown" has duplicates

#now go back to taxonomy data set and look at these three genera
#gen_dup<-taxon_high_rn %>% 
#  filter(genus == "Achnanthidium" | genus =="Elakatothrix" | genus=="Leptocylindrus" )
#remove the combos that are duplicates from the main data set

#combine sample data and high level taxonomy by genus----------
names(phyto_cleanest)
names(taxon_high)
phyto_tax<-left_join(phyto_cleanest,taxon_high) %>% 
  glimpse()

#final edits to complete final data version of data set
phyto_final<-phyto_tax %>% 
  #order by date and time
  arrange(date, time) %>%
  mutate(
    #fix one case of phyto_form from "f." to "f"
    phyto_form = case_when(phyto_form =="f." ~ "f",TRUE ~ phyto_form)
    #make time a character column for export; otherwise will automatically convert to UTC
    ,time_pst = as.character(time)
                 ) %>% 
  #reorder columns data frame export
  select(station
         ,station_comb
         ,region
         ,collected_by
         ,date
         ,time_pst
         ,kingdom
         ,phylum
         ,class
         ,genus
         ,taxon
         ,phyto_form
         ,organisms_per_ml
         ,cells_per_ml
         #,biovolume_per_ml_old
         ,biovolume_per_ml
  ) %>%
  glimpse()

#check for NAs
#check_na2 <- phyto_final[rowSums(is.na(phyto_final)) > 0,]

#write the formatted data as csv 
#write_csv(phyto_final,file = "EDI/data_output/SMSCG_phytoplankton_formatted_2020-2021.csv")

#write version of data set that includes only the samples collected by DFW--------
#this is for the phytoplankton synthesis effort

#look at number of rows associated with DFW vs EMP
phyto_surv_sum <- phyto_tax %>% 
  group_by(survey) %>% 
  summarize(n = n())

#look at number of samples associated with DFW vs EMP
phyto_surv_sum2 <- phyto_tax %>%
  distinct(survey,station,date) %>% 
  group_by(survey) %>% 
  summarize(n = n())

phyto_dfw<-phyto_tax %>% 
  filter(survey=="DFW") %>% 
  select(-station) %>% 
  rename(station=station_clean) %>% 
  select(station
         ,date
         ,time
         ,kingdom
         ,phylum
         ,class
         ,phyto_form
         ,genus
         ,taxon
         ,organisms_per_ml
         ,cells_per_ml
         #,biovolume_per_ml_old
         ,biovolume_per_ml
  ) %>%
  glimpse()
#write_csv(phyto_dfw,file = "EDI/data_output/SMSCG_phytoplankton_formatted_DFW_only_2020-2021.csv")

#look at list of station names in DFW samples data set
unique(phyto_dfw$station)

#look for NAs
check_na <- as.data.frame(colSums(is.na(phyto_dfw))) 
#only time column has NAs
check_time <- phyto_dfw %>% 
  distinct(station,date,time) %>% 
  filter(is.na(time))
#three samples are missing times which makes sense


#exploring taxonomy data set--------------------

#how many genera in samples not in taxonomy data?
#look for NA in Class column
sum(is.na(phyto_tax$class))
#31 cases in which a genus in a sample didn't match the taxonomy data

#look at set of samples without matching taxonomy
misfits <- phyto_tax %>% 
  filter(is.na(class)) %>% 
  distinct(genus)
#12 new genera plus NA

#how often do genera have multiple algae types? if always just one, then can just add the algae type column
#if more than one, then we have to match up taxa to use the algae type
tax_at<-unique(taxonomy[,c('Genus','Algal Type')])

#also just how many algal types in total
unique(taxonomy$'Algal Type')
#n = 32 but some of these are just duplicates created by formatting differences and also unknown categories
#but I bet many of these are uncommon and can be lumped into an "Other" category

#look at algal types and Class together to see if that helps
tax_atc<-unique(taxonomy[,c('Phylum','Class','Algal Type')])
sorted<-tax_atc[order(tax_atc$Phylum, tax_atc$Class),]
  
#count number of unique algal types within Genus
#ideally this is one for every genus
tax_at_sum<-data.frame(table(tax_at$Genus)) 
#looks like there are some genera with multiple algal types

#look at genera with multiple algal types
tax_at_sum_sub<-filter(tax_at_sum, Freq >1)
#most of these are associated with "Unknown" genus
#there are two associated with genus Leptocylindrus

#look at Leptocylindrus in main taxonomy data set
lept<-taxonomy %>% 
  filter(Genus =="Leptocylindrus")
#the two algal types are simply different forms of the same name "Centric diatom" and "Centric Diatom"

#create a secondary data set that has the same information but 
#but based on synonyms of the current names
#subset this data set to exclude rows with "Synonym(s)" = "None" or "Unknown"

#create columns that separates the genus and species of the synonyms
#this way, any out of date names in the sample data can be matched by genus



            
  
  
  
  
  
  
  
  
