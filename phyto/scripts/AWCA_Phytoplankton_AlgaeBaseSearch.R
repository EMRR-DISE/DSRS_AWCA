#AWCA
#phytoplankton
#taxonomy
#algaeClassify package


#NOTE: this requires a key, which was provided for free for package review
#would need to buy one for future use

#load packages
library(algaeClassify)
library(viridis)
library(tidyverse)
library(janitor)

#added API key to environment variable
#file.edit("~/.Renviron")
#ALGAEBASE_APIKEY=yourKeyHere

#read in file containing all taxa---------
awca_taxa <- read_csv("./phyto/data_output/AWCA_taxon_list.csv")


#prepare dataset for searching-------------

#split taxa into list of genus level and species level taxa
taxa_genus <- awca_taxa %>% 
  filter(taxon_level=="genus")

taxa_species <- awca_taxa %>% 
  filter(taxon_level=="species")


#search AlgaeBase----------

#search for the species first (n = 69)
taxonomy_algaebase<-algaebase_search_df(taxa_species,higher=TRUE, long=T, exact.matches.only = F
                                         ,genus.name="genus",species.name="species")
#after browsing the results, I'm not sure I trust that this works very well

#search for the genera next (n = 87)
taxonomy_algaebase<-algaebase_search_df(taxa_species,higher=TRUE, long=T, exact.matches.only = F
                                        ,genus.name="genus",species.name="species")

