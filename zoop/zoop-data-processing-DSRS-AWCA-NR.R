# Aquatic Veg Study
# Zooplankton Data
# May 2017 - December 2018 (complete set of all samples)
# Updated Feb 2023 by TMF for preparing data for EDI

rm(list=ls()) #cleans workspace

#required packages
library(plyr)
library(readr) #needed to import and combine all csv files
library(purrr) #needed to import and combine all csv files
library(tidyr) #splitting one column into two
library(dplyr) #filter()
library(lubridate) #format dates
library(matrixStats) #colMins(),colMaxs()

#set working directory
setwd("C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Zoop/Data_CSV")

#import and combine all zoop count csv files
zooplankton <- dir(pattern = ".csv$", recursive = F, full.names = T)

#some dates were imported as dates while others as character
#so in code below specified what each column type was and made all dates as characters
zoop <- map_dfr(zooplankton, ~ read_csv(.x, col_types = "cctccdddddd"))%>% 
  # Parse date_time variable from character to date_time format
  #also had to deal with fact that dates were in two different formats
  mutate(date = parse_date_time(date, c("Ymd", "mdY")))
str(zoop)

#import data used to calculate water volume sampled in field with tow net
tow<-read.csv("C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Zoop/zoop_tow_data.csv") 

#import environmental data associated with zoop tows (e.g., weather, water quality)
env<-read.csv("C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Zoop/zoop_env.csv") 


#tow net data-----------
#data cleaning and calculating volume of water sampled in field

#format date
tow$date<-mdy(tow$date)
str(tow)

#look at sample names
#fix some naming inconsistencies and also make them match the names from the 'zoop' df
tow$sample<-gsub('_','-',tow$sample)
tow$sample<-gsub(' #','-',tow$sample)
unique(tow$sample) #18 sample names which is correct

#count number of samples by site
tow %>% 
  group_by(sample) %>%
  summarise(no_rows = length(sample))

#create list of all unique site x date combos
#should be same length as original tow df if no duplicate samples
towsd <- unique(tow[c("sample","date","sample_month")])
#there is only one sample for each site x date combo as there should be

#there were no paper data sheets for the samples collected from USGS-5 and USGS-8
#from 3/12/2018, so adding them by hand for now


#see if there are any NAs 
sapply(tow, function(x) sum(is.na(x))) 
#there are no NAs anywhere in this df which is good

#look at ranges of values for numeric columns
colMins(as.matrix(tow[c(5:12)]))
colMaxs(as.matrix(tow[c(5:12)]))
#tow lengths range 2.5-7.0 m
#zoop tow proportion submerged ranges 0.10-1.0

#calculate volumes for each tow
#net mouth area based on net radius of 0.15 m
narea<-0.15^2*pi
tow$tow1_vol_m3<-narea*tow$tow1_sub_area_prop*tow$tow1_length_m
tow$tow2_vol_m3<-narea*tow$tow2_sub_area_prop*tow$tow2_length_m
tow$tow3_vol_m3<-narea*tow$tow3_sub_area_prop*tow$tow3_length_m
tow$tow4_vol_m3<-narea*tow$tow4_sub_area_prop*tow$tow4_length_m
#sum the four volumes for a given sampling location
tow$tot_vol_m3<-tow$tow1_vol_m3+tow$tow2_vol_m3+tow$tow3_vol_m3+tow$tow4_vol_m3

#trim data frame to just the columns to combine with zoop counts data frame
#sample, date, total volume
names(tow)
tw<-tow[c("sample","date","time_pdt","sample_month","tot_vol_m3")]  
str(tw)


#zoop count data--------------
#data cleaning

#divide sample into island, stratum, and sample (1 vs 2 within island-stratum combo)
#separate site into island and stratum
zoop<-separate(zoop,sample, into = c("island", "stratum","site"),remove=F)
#gives a warning because USGS stations only have two pieces to separate instead of three
#but the function does still work

#combine date and time
zoop$date_time<-as.POSIXct(paste(zoop$date, zoop$time), format="%Y-%m-%d %H:%M:%S")

#look at df structure
str(zoop) #looks good

#look at unique sample names and fix some typos and inconsistencies
zoop$sample<-gsub('_','-',zoop$sample) 
zoop$sample<-gsub('LH-SAV-02','LH-SAV-2',zoop$sample) 
unique(zoop$sample) #should be 18 names total

#see if there are any NAs 
sapply(zoop, function(x) sum(is.na(x))) 
#site = 204 (just the USGS stations, see below)
#sub1_ml = 12 (all from one sample, see below)  
#subsamples = 2112 (not sure this matters, numbers not used in density calculations)
#subsample = 9067 (not sure this matters, numbers not used in density calculations)
#NOTE: for the most part, I think "subsample" and "subsamples" are redundant
#which is why there are so many NAs

#look at NAs for site (n = 204)
sitena<-zoop[is.na(zoop$site),]
unique(sitena$sample) #"USGS-5" "USGS-8"
#there are only two part rather than three for USGS station names

#look at unique dates to see what the range is for our current data set 
zoopsd <- unique(zoop[c("sample","date")])

#look at number of samples for each site
zoopsd %>% 
  group_by(sample) %>%
  summarise(no_rows = length(sample))
#sample numbers look roughly right but maybe not exactly right

#make sure there is only one sample for each site x month combo
smc <- unique(zoop[c("sample","date")])

#compare sample x date combos between tow and zoop counts
#if there isn't complete match between the two data frames, then something is missing
#first add unique column for zoop count df
smc$z<-"z"
sdcb<-join(smc,towsd,type="full") 

#look at subset that don't match (i.e., rows with NAs)
miss <- sdcb[rowSums(is.na(sdcb)) > 0,]
#FC-WAT-1, 2018-07-09: missing from zoop counts df because sample collected but jar broken during shipping to BSA

#find and replace MICROZOOPLANKTON & NAUPLII with MICROZOOPLANKTON
zoop$category <- gsub("MICROZOOPLANKTON & NAUPLII","MICROZOOPLANKTON", zoop$category) 
unique(zoop$category) #successfully removed redundant microzooplankton category

#replace the typo "Dunhevidia" with correct name "Dunhevedia"
zoop$taxon <- gsub("Dunhevidia","Dunhevedia", zoop$taxon) 

#replace " HARPACTICOIDS" with "Harpacticoids" for taxon
zoop$taxon <- gsub("HARPACTICOIDS","Harpacticoids", zoop$taxon)

#make sure that taxonomic category and taxon are properly paired
#ie, for a given taxon, there should be only one category that is ever applied to it
tu<-data.frame(table(zoop$category,zoop$taxon))
#shows all possible category and taxon combinations and their frequencies
#not quite what I wanted but can use it; I'll subset to exclude rows with frequency=0
tun<-subset(tu,Freq>0) #125 unique combinations of category and taxon

#need to make sure no taxa was ever paired with more than one category
#that means that in the "tun" df, each taxon should only appear once
tuni<-data.frame(table(tun$Var2)) 
#there are 2 more observation in "tun" than "tuni"

#look at taxa that appear more than once
tunis<-subset(tuni,Freq>1) #2 taxa each show up twice (both Monospilus)

mono<-tun[grep("Monospilus", tun$Var2), ]
#Monospilus appears under more than one category

#Monospilus is a cladoceran; change category for all Monospilus to CLADOCERA
monoz<-zoop[grep("Monospilus", zoop$taxon), ]
#change category
monoz$category<-"CLADOCERA"

#Replace all entries for Monospilus in main data frame
zoom <- filter(zoop, !grepl("Monospilus", taxon)) 
zoo<-rbind(zoom,monoz)

#look at taxa with life stage specified (adults vs copepodid vs nauplii)
#create data set containing all rows with one of these three life stages specified
stages<-c("adult","copepodid","nauplii")
stg <- filter(zoo, grepl(paste(stages, collapse="|"), taxon))
sttx<-unique(stg$taxon) #20 taxa

#separate life stage from taxon name
stsp<-transform(stg, taxon = sub("(.*) .*", "\\1", taxon), stage = sub(".* ", "", taxon))
#The first regular expression (.*)_ matches everything up to the last underscore followed by 
#everything remaining .* and the first sub replaces the entire match with the matched part within 
#parens. This works because such matches are greedy so the first .* will take everything it can 
#leaving the rest for the second .* . The second regular expression matches everything up to the 
#last underscore and the second sub replaces that with the empty string.
unique(stsp$taxon)
unique(stsp$stage)
#it worked correctly

#now remove taxa with life stages from main df
zoos <- filter(zoo, !grepl(paste(stages, collapse="|"), taxon)) 
#removes 2117 rows as expected (see "stg" df which is the reverse of this)

#add a stage column with indication that no life stage was specified for those rows remaining
zoos$stage<-"not_spec"

#then add back the rows with the life stage separated out
zom<-rbind(zoos,stsp)

#sp. and spp. are not used consistently; oftentimes a genus name is simply listed without either
#isn't clear to me that I can trust that sp. indicated that all organisms were of a single unIDed species
#while spp. contains multiple cogeners not IDed to species, even though that is what those words should mean
#therefore, just remove sp. and spp.
zom$taxon <- gsub(" sp.","" , zom$taxon,fixed=T) #need ",fixed=T" part to remove the periods
zom$taxon <- gsub(" spp.","" , zom$taxon,fixed=T)
zom$taxon <- gsub(" SPP.","" , zom$taxon,fixed=T)


#add new columns that split genus and species
#note that some taxon are higher level or common names (eg, "Rotifers", "Shrimp")
#point is that by spliting genus and species, we can later lump congeners together for some graphs/analyses
zom$taxon2<-zom$taxon
zom<-separate(zom,taxon2, into = c("genus", "sp"))
#throws an error but works

#remove all rows in which count = 0 (ie, no members of that taxon were found in sample)
nzd<-subset(zom,count>0)
#dropped number of rows from 11161 to 3615

#how many unique taxa now?
ztx<-unique(nzd$taxon) #n=62

#there is one category that refers to life stage that I missed ("Immature Cladoceran")
usp<-unique(nzd$sp) #n=23 (one of these is NA which is fine)
unique(nzd$category) #n=5, no macrozooplankton ever present

#how common is "Immature Cladoceran" in the data set?
icl<-subset(nzd,taxon=="Immature Cladoceran") 
#a single specimen in a single sample; just remove this from the data set
nzdc<-subset(nzd,taxon!="Immature Cladoceran") 


#reorder columns
names(nzdc)
nz<-nzdc[c("sample","island","stratum","site","date_time","date","time",
          "subsamples","v1_ml","sub1_ml","v2_ml","sub2_ml","category","taxon",
          "genus","sp","stage","count")]

#look at unique combos of category, genus, and taxon
ft<-data.frame(table(nz$category,nz$genus,nz$taxon,nz$stage))
fft<-subset(ft,Freq>0)
fft<-fft[order(fft$Var3),]
#looks like all taxa are unique

fftt<-fft[order(fft$Freq),]
#make quick histogram of frequency of occurrence of taxa
hist(fftt$Freq,breaks=12)

#combine zoop counts with tow volumes
final<-join(nz,tw)
str(final)


#calculate zoop densities -----------------

#see if there are any NAs 
sapply(final, function(x) sum(is.na(x)))
#there are some NAs for site, subsamples, and sp but these are fine 

#look at range of volumes
range(final$tot_vol_m3) #0.4594579 1.8378317 m^3
hist(final$tot_vol_m3)
#NOTE: these are in m^3 instead of mL

#look closer at small tow volumes (<0.9 m^3)
smv<-subset(final,tot_vol_m3<0.9)
unique(smv[c("sample","date","tot_vol_m3")])
#DI-SAV-1 2017-05-25  0.6361725
#DI-SAV-2 2017-06-07  0.4594579
#both were from very beginning of study
#both had very low proportion of net submerged for most tows

#quick look at preserved sample volumes
range(final$v1_ml) #14 565
range(final$v2_ml) #14 565

#quick look at subsamples
range(final$sub1_ml) #0.1 75.0
range(final$sub2_ml) #0.09 69.00

#calculate the number of organisms per unit volume

# (subsample count / (subsample volume/bottle volume))/tow volume
#e.g. (72 rotifers/(0.25 mL subsample/78 mL bottle volume))/1414000 ml tow volume = 0.01588685 rotifers/mL
#make sure to convert tow volume from m^3 to mL for calculations
#note that there is different subsampling for meso/macrozooplankton vs micro zooplankton

#mesozooplankton
#figure out how to apply this to specific categories (ie, all but "MICROZOOPLANKTON")
#this is a bit of a hack-y approach but will work in a pinch
#subset final df to exclude microzooplankton
fmes<-subset(final,category!="MICROZOOPLANKTON")
fmes$indiv_per_ml<-(fmes$count/(fmes$sub1_ml/fmes$v1_ml))/(fmes$tot_vol_m3*1000000)

#plot histogram of mesozooplankton densities
range(fmes$indiv_per_ml) #7.073553e-07 9.502715e-02
hist(fmes$indiv_per_ml)

#microzooplankton
#subset final df to include only microzooplankton
fmic<-subset(final,category=="MICROZOOPLANKTON")
fmic$indiv_per_ml<-(fmic$count/(fmic$sub2_ml/fmic$v2_ml))/(fmic$tot_vol_m3*1000000)

#plot histogram of mesozooplankton densities
range(fmic$indiv_per_ml) #7.073553e-07 1.490221e-01
hist(fmic$indiv_per_ml)

#combine the meso/macro and micro zooplankton dfs
fin<-rbind(fmes,fmic)

#remove columns that won't be needed in subsequent analysis (ie, stuff about various sample volumes)
names(fin)
end<-fin[c("sample","island","stratum","site","date_time","date","sample_month",
           "category","taxon","genus","sp","stage","tot_vol_m3","indiv_per_ml")]
names(end)
str(end)

#see if there are any NAs 
sapply(end, function(x) sum(is.na(x))) #no unexpected NAs

tax<-unique(end$taxon)

#remove extra sample: FI-SAV_2 on 6/29/17
endn<-end[!(end$date==as.Date("2017-06-29") & end$sample=="FI-SAV-2"),]

#look at total densities of samples
td<-aggregate(endn$tot_vol_m3,by=list(endn$sample,endn$date),FUN=sum, na.rm=T)
hist(td$x,breaks=25)

#look at high density samples
hds<-subset(td,x>30)
#LH-SAV-1 2018-06-06, nothing obviously unusual about the tows for this sample

#environmental data-----------
#clean data so it can be merged with main df

str(env)

#format date
env$date<-mdy(env$date)
str(env)

#look at sample names
#fix some naming inconsistencies and also make them match the names from the 'zoop' df
env$sample<-gsub('_','-',env$sample)
env$sample<-gsub(' #','-',env$sample)
#unique(env$sample) #18 sample names which is correct

#trim df to just the necessary columns

#join env data with main df
zoev<-join(endn,env,type="full") 

names(zoev)

zov<-subset(zoev, select=-c(time_pdt)) #exclude time column

#export prepared file
#write.csv(zov,"C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Zoop/zoop_densities.csv",row.names=F)




