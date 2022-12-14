#Delta Smelt Resiliency Strategy 
#Aquatic Weed Control Action
#Phytoplankton Data 2017-2018
#Format raw data in preparation for visualization and analysis

#To do list-----------
#need to correct biovolumes

#required packages
library(tidyverse) #suite of data science tools
library(janitor) #functions for cleaning up data sets
library(hms) #working with date/time
library(lubridate) #working with dates
library(readxl) #importing data from excel files
#library(algaeClassify) #grab taxonomy info from AlgaeBase; doesn't work currently

#Notes
#For all BSA files from 2013 to 2021, the column "Number of cells per unit" really means "Total cells", 
#which is the total number of cells counted for that taxon in a particular sample
#calculations in this script were corrected accordingly on 2/10/2022

#create version of data set for phytoplankton synthesis effort


#to do list
#should check to see if organisms per ml and cells per ml are equal for cases where phyto_form = individual

# Read in and combine phyto sample data----------------------------------------------

#Create character vectors of all files (n = 5) 
#NOTE: the first three files have the same set of columns
#But the last two files do not; start with first three files
phyto_files <- dir(path = "./phyto/data_input/abundances",pattern = ".xlsx|.XLSX", full.names = T)

#Combine all of the data files into a single df

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
taxonomy <- read_csv("phyto/data_input/taxonomy/phyto_2018-12_taxonomy_complete.csv")

#format the sample data set------------
#NOTE: leave out derived columns (ie, calculated from other columns)
#then do calculations myself to minimize risk of errors

phyto_cleanest <- phytoplankton %>% 
  #rename the confusingly incorrectly name column
  rename(total_cells=number_of_cells_per_unit) %>% 
  #subset to just the needed columns
  select(file_name
         , sample_date
         , sample_time
         , station_code
         , depth_ft #lots of NAs; I think this should be 3 feet for all
         , volume_received_m_l
         , volume_analyzed_m_l
         , unit_abundance
         , slide_chamber_area_mm2
         , field_of_view_mm2
         , number_of_fields_counted
         , factor
         , total_cells
         , biovolume_1:biovolume_10 
         , bsa_tin
         , taxon
         , genus
         , species
         , synonym
         , colony_filament_individual_group_code
         ) %>% 
  mutate(
    #change some columns from text to numeric
    across(depth_ft:bsa_tin, as.numeric)
  ) %>% 
  glimpse()


  rowwise() %>% 
  mutate(  
    #use the date-time column with standardized time zone to extract time
    time = as_hms(date_time_PST)
    #create new column that calculates mean biovolume per cell
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
    ) %>% 
  #simplify column names
  rename(phyto_form =  colony_filament_individual_group_code) %>% 
  #subset and reorder columns again to just those needed
  select(collected_by
         ,region
         ,station
         ,station_comb
         ,date
         ,time
         ,genus
         ,taxon                              
         ,phyto_form           
         ,organisms_per_ml
         #,organisms_per_ml_easy
         ,cells_per_ml
         #,cells_per_ml_easy
         #,biovolume_per_ml_old
         ,biovolume_per_ml
         #,biovolume_per_ml_easy
           ) %>% 
  glimpse()
#I prefer to use the formulas based on the more raw version of the data 
#rather than the ones based on the factor column
#which is a derived column and therefore more prone to errors
#warnings indicate some missing times; I know some times weren't recorded

#look at station names
#unique(phyto_cleanest$station)

#look at number of samples per station
samp_count<-phyto_cleanest %>% 
  distinct(region,station, date) %>% 
  group_by(region,station) %>% 
  summarize(count = n())
#looks fine

#check for NAs
#check_na <- phyto_cleanest[rowSums(is.na(phyto_cleanest)) > 0,]
#most are just missing time, which is fine because time not always recorded
#could look up fish survey data to get times for some of these

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



            
  
  
  
  
  
  
  
  
