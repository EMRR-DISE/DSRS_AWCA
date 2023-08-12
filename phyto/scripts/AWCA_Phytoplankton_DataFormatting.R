#Delta Smelt Resiliency Strategy 
#Aquatic Weed Control Action
#Phytoplankton Data 2017-2018
#Format raw data in preparation for visualization and analysis

#required packages
library(tidyverse) #suite of data science tools
library(janitor) #functions for cleaning up data sets
library(hms) #working with date/time
library(lubridate) #working with dates
library(readxl) #importing data from excel file

#Notes
#For all BSA files from 2013 to 2021, the column "Number of cells per unit" really means "Total cells", 
#which is the total number of cells counted for that taxon in a particular sample
#calculations in this script were corrected accordingly on 2/10/2022

#To do list------------------
#look at Sarah Perry's code for assembling and publishing phyto data set
#should check to see if organisms per ml and cells per ml are equal for cases where phyto_form = individual
#move analysis and most plotting to a different script
#use biovolume to calculate biomass
#then calculate fatty acid content
#try to match plankton samples with veg rake samples

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

#read in EMP data from EDI
emp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1320.5&entityid=67b9d4ee30d5eee6e74b2300426471f9") %>% 
  clean_names()

#read in taxonomy data from PESP GitHub repo
taxonomy <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/PESP/main/admin/global_data/phyto_classification.csv") %>% 
  clean_names()

#read in supplementary taxonomy info that fills gaps in PESP list
taxonomy_fix <- read_csv("phyto/data_input/other/phyto_taxonomy_mismatch_fixed_2023-08-11.csv")

#read in sampling month data
#use this to check for typos in sampling dates and also to assign survey months (samples aren't always taken with sampling month)
sample_month <- read_csv("phyto/data_input/other/survey_months_complete.csv")



#format the AWCA sample data set------------
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
         ,gald
         ,gald_1 # in last two files there are multiple gald values; combine gald and gald_1
         ,phyto_form = colony_filament_individual_group_code
         ,taxonomist #useful for look at potential ID biases
         ,comments #need to look at these before consider dropping this column
         ,shape
         ,biovolume_1:biovolume_10 #needed
         ) %>% 
  mutate(
    #combine gald and gald_1; they're the same things just with different names across files
    gald_new = case_when(!is.na(gald)~gald,!is.na(gald_1)~gald_1)
    #change some columns from text to numeric
    ,across(c(volume_received_m_l:bsa_tin,unit_abundance:gald,biovolume_1:biovolume_10), as.numeric)
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
    ,biovolume_cubic_micron_per_ml = round((total_cells* mean_cell_biovolume*slide_chamber_area_mm2)/(volume_analyzed_m_l*field_of_view_mm2*number_of_fields_counted),1)
    #,biovolume_per_ml_easy = factor * total_cells * mean_cell_biovolume
  )  %>% 
  select(file_name
         ,date1
         ,time1
         ,station 
         ,volume_analyzed_m_l #needed
         #,volume_analyzed_prop
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
         ,gald = gald_new
         ,phyto_form 
         #,taxonomist #useful for look at potential ID biases
         ,comments #need to look at these before consider dropping this column
         #,shape
         ,organisms_per_ml
         ,cells_per_ml
         ,biovolume_cubic_micron_per_ml
         #,mean_cell_biovolume #decided this is useful to users
  ) %>% 
  glimpse()
#I prefer to use the formulas based on the more raw version of the data 
#rather than the ones based on the factor column
#which is a derived column and therefore more prone to errors

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

#format the sample month df again
smonth2 <- smonth %>% 
  rename(date = date1) %>% 
  glimpse()

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
  #add date-time column; pretty sure we used PDT for all the biological data
  #,date_time_pst = ymd_hms(as.character(paste(date, time)),tz="Etc/GMT+8")
  ,date_time_pdt = ymd_hms(as.character(paste(date, time)),tz="America/Los_Angeles")
  )  %>% 
  #move up new time column
  relocate(c(date,time,date_time_pdt),.after = date1) %>% 
  #drop old time and date-time columns
  select(-c(time1,time2,date1)) %>% 
  #add columns from sample month dataframe
  left_join(smonth2) %>% 
  #move up the added columns
  relocate(island:s_month,.after = station) %>% 
  glimpse()

#check time zone
tz(phyto_cleanest$date_time_pdt)
#looks good - "America/Los_Angeles"

#look for date typos again
dtypo2 <- anti_join(phyto_cleanest,smonth2) %>% 
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
  filter(is.na(date_time_pdt)) %>% 
  distinct(date,station)
#no NAs for date-time

#look at number of samples per station
samp_count<-phyto_cleanest %>% 
  distinct(station, date) %>% 
  group_by(station) %>% 
  summarize(count = n())
#quick glance; looks pretty good

#look at ranges of some of the raw data columns to decide 
#whether to retain in dataset or just describe in metadata

range(phyto_cleanest$volume_analyzed_m_l,na.rm = T) #1-10; pretty wide range
range(phyto_cleanest$field_of_view_mm2,na.rm = T) #0.0683000 0.0697465; pretty narrow
range(phyto_cleanest$slide_chamber_area_mm2,na.rm = T) #314.159 314.159
#range(phyto_cleanest$area_counted,na.rm = T) #0.3415 1.5709, somewhat large range

hist(phyto_cleanest$volume_analyzed_m_l)

#look at taxonomist comments
phyto_cleanest_t <- phyto_cleanest %>% 
  distinct(comments) %>% 
  #distinct(station,date_time_pdt,comments) %>% 
  arrange(comments)
#a few types of comments
#most are about the amount of debris in samples
#the rest indicate that individual is broken or degraded
#some of the comments indicate there are broken diatoms. Does that mean they didn't try to ID any of them?

#write file with all comments
#write_csv(phyto_cleanest_t, "./phyto/data_output/phyto_comments.csv")

#are there cases where diatoms are IDed and there is a note about broken diatoms?
diatom_broken <- phyto_cleanest %>% 
  filter(diatom_soft_body=="Diatom" & grepl("broken|Broken",comments))
#yes, so the broken diatoms probably impacted ID but didn't completely prevent diatom ID in a sample

#look at synonyms
syn <- phyto_cleanest %>% 
  distinct(synonym) 
#no synonyms listed anywhere so drop this column


#continued data formatting
#replace comments column with quality check column
#drop some unneeded columns, rename some columns, reorder columns
phyto_format <- phyto_cleanest %>% 
  select(
    station
    ,site = island
    ,stratum
    ,date
    ,time
    ,date_time_pdt
    #,survey_year_month = month_survey
    ,survey_year = s_year
    ,survey_month = s_month
    #,bsa_tin 
    ,taxon
    #,diatom_soft_body
    ,genus
    ,species
    ,organisms_per_ml
    ,cells_per_ml
    #,mean_cell_biovolume
    ,biovolume_cubic_micron_per_ml
    ,gald
    ,phyto_form
    #,shape
    ,comments
  ) %>% 
  mutate(
    #change spp. to sp. in name and species columns
    name =str_replace_all(taxon, pattern = c(" spp."=" sp."))
    ,species2 = case_when(species=="spp."~"sp.",TRUE~species)
    #add column for quality based on comments
    ,quality_check = case_when(
    grepl("degraded", comments, ignore.case=T) ~ "degraded"
              ,grepl("fragment", comments,ignore.case=T) ~"fragmented"
              ,TRUE ~ "good"
  )
  #add column indicating amount of sediment and detritus
  #comments often note differing levels of sediment vs detritus
  #for simplicity combine them and use the highest level indicated
  #eg, low detritus and high sediment simply becomes high
  ,debris = case_when(
    grepl("high",comments, ignore.case=T)~"high"
    ,grepl("moderate",comments, ignore.case=T)~"moderate"
    ,grepl("low",comments, ignore.case=T)~"low"
    ,TRUE~NA
  )
  ) %>% 
  #drop two non-photosynthetic taxa
  filter(name!="cf. Amoeba sp." & name!="Strobilidium sp.") %>% 
  #drop comments column now
  select(-comments,-species,-taxon) %>% 
  select(station:survey_month
         ,name
         ,genus
         ,species = species2
         ,organisms_per_ml:phyto_form
         ,quality_check:debris) %>% 
  glimpse()

#make file with all taxa to run through AlgaeBase---------------
#NOTE: tried the search in a different script; ultimately didn't work that well
#just use the EMP taxonomy

#create df with unique taxa
tax_awca <- phyto_format %>% 
  distinct(name,genus,species) 
#148 taxa

#let's add a column to indicate whether genus or species level ID
taxa_awca_more <- tax_awca %>% 
  mutate(taxon_level = case_when(grepl("sp[.]", species,ignore.case=T) ~ "genus"
                                 ,grepl("spp[.]", species,ignore.case=T) ~ "genus"
                                 ,TRUE~"species")) %>% 
  arrange(taxon_level,genus,species)

#write the file with the taxa
#write_csv(taxa_awca_more,"./phyto/data_output/AWCA_taxon_list.csv")

#compare taxa between AWCA and EMP-----------------

#look at non-matches
tax_mism <- anti_join(tax_awca,taxonomy)

tax_mism_names <- tax_mism %>% 
  pull(name)

tax_mism_genera <- tax_mism %>% 
  pull(genus)
#20 mismatches
#over half are "cf." taxa; check to see if there are genus level matches at least
#some are taxa that should probably be dropped because they aren't phyto (cf. Amoeba sp.)
#some of these could be from name changes (eg, Chroococcus microscopicus should be Eucapsis microscopica)

#check algae base taxonomy for updates
#Chroococcus microscopicus = Eucapsis microscopica
#Plagioselmis prolonga (and cf. Plagioselmis prolonga) = Teleaulax amphioxeia
#Rhodomonas lacustris = Plagioselmis lacustris

#export mismatch taxa to discuss with Tiffany and Sarah P
#write_csv(tax_mism,"./phyto/data_input/other/phyto_taxonomy_mismatch_2023-08-11.csv")

#look for genus level matches in EMP data set for mismatching taxa
mismatch_gn <- tax_mism %>% 
  pull(genus) 
#18 genera

#which of the mismatched taxa are in EMP data set as genera at least?  
tax_emp_gn <- taxonomy %>% 
  distinct(kingdom, phylum, class, algal_group, genus) %>% 
  filter(genus %in% mismatch_gn) %>% 
  arrange(genus)

#just three genera didn't match with EMP: "Amoeba","Pedinomonas","Strobilidium"
#should probably drop "Amoeba", "Strobilidium"; I don' think they are photosynthetic
#Pedinomonas" is a green algae; maybe EMP just doesn't ever get it

#EMP has two different classes for Achnanthidium
#two different phyla for Gomphonema and Nitzschia
#these are just the redundancies in this subset of mismatches; could be more in full EMP data set 

#we got 21 matches, so even though there are some species level differences, perhaps there aren't genus level ones

#Add higher level taxonomic information and habitat information manually-------------

#start by creating two subsets of the data set
#one with matches to the EMP taxonomy and one without

#dataset with EMP taxon matches
phyto_emp_m <- phyto_format %>% 
  filter(!(name %in% tax_mism_names))

#dataset without EMP taxon matches
phyto_emp_nm <- phyto_format %>% 
  filter(name %in% tax_mism_names)

#prepare EMP taxonomy file
#will match genus name between the two data frames
taxon_high<-taxonomy %>% 
  select(kingdom
  ,phylum
  ,class
  ,algal_group
  ,genus
  ,name
  ,current_name
  ) %>%  
#this approach is very simple because it removes the need for exact matches in the taxon names
#part of the difficulty in matching taxon names is presence of "cf." for many taxa
#unfortunately by just matching by genus and not taxon, we lose the habitat type, and salinity range info
#also with species level data, there's a chance the taxonomy dataset doesn't include every species in the samples
  #remove some (likely incorrect) combinations that are creating duplicates for genus
  #check AlgaeBase to see which taxonomic info is correct
  filter(!(genus == "Achnanthidium" & class =="Fragilariophyceae") & 
           !(genus == "Elakatothrix" & class =="Chlorophyceae") &
           !(genus == "Leptocylindrus" & algal_group =="Centric diatom"))
  #initially some genera appeared more than once in this taxonomy data set
#but this has been corrected




#combine sample data and high level taxonomy----------

#match subset with EMP taxonomy
phyto_tax_emp<-left_join(phyto_emp_m,taxon_high) %>% 
  glimpse()
#looks good

#match subset that doesn't match EMP taxonomy
phyto_tax_gap<-left_join(phyto_emp_nm,taxonomy_fix) %>% 
  glimpse()

#bind two subsets back together
phyto_tax_comp <-bind_rows(phyto_tax_emp,phyto_tax_gap) %>% 
  arrange(date_time_pdt,station)

#final edits to complete final data version of data set
phyto_final<-phyto_tax_comp %>% 
  mutate(
    #fix one case of phyto_form from "f." to "f"
    phyto_form = case_when(phyto_form =="f." ~ "f",TRUE ~ phyto_form)
    #make time a character column for export; otherwise will automatically convert to UTC
    ,time = as.character(time)
    ,date_time_pdt = as.character(date_time_pdt)
                 ) %>% 
  #reorder columns data frame export
  select(station
         ,date_time_pdt
         ,survey_year
         ,survey_month
         ,taxon_original = name
         ,taxon_current = current_name
         ,genus
         ,species
         ,kingdom:algal_group
         ,organisms_per_ml:debris
  ) %>%
  glimpse()

#check for NAs
#check_na2 <- phyto_final[rowSums(is.na(phyto_final)) > 0,]

#write the formatted data as csv 
#write_csv(phyto_final,file = "./phyto/data_output/AWCA_phytoplankton_formatted_2023-08-11.csv")

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
         ,biovolume_cubic_micron_per_ml
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



            
  
  
  
  
  
  
  
  
