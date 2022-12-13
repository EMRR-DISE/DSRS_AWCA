#Aquatic Vegetation Study
#Phytoplankton
#Dates: May 2017 - Dec. 2018 (all samples)

#good nMDS tutorial
#http://www.flutterbys.com.au/stats/tut/tut15.1.html
#discussion of multivariate homeogeneity of groups dispersions
#https://www.researchgate.net/post/How_should_I_correctly_manage_PERMANOVA_for_factors_with_interactions

#required packages
library(plyr) #combining data frames, summary stats
library(readr) #import all files at once and combining them
library(dplyr) #import all files at once and combining them; also count spp by factor level
library(ggplot2) #plotting data
library(tidyr) #splitting one column into two
library(purrr) #creating and exports series of plots
library(vegan) #nMDS, adonis2(),betadisper()
library(lubridate) #decimal_date() for shaded rectangles in plots
library(colorspace) #plot color palettes
library(Matrix) #nnzero function to count non-zero values in matrix
library(gridExtra) #grouping plots with grid.arrange()

#working directory
setwd("C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Phytoplankton/Phyto_Data")

#folder where sample data are located
data_folder <- "C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Phytoplankton/Phyto_Data"

#folder where graphs will be saved
plot_folder <- "C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Phytoplankton/Phyto_Graphs"

#import taxonomy df
taxonomy<-read.csv("C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Phytoplankton/Phyto_Taxonomy/phyto_2018-10_taxonomy_complete.csv")

#import all phyto data sets and combined them
#the formatting of the columns is the same across files
filenames <- list.files(path=data_folder,pattern=".csv")
phyto <- lapply(filenames, read_csv) %>% bind_rows()

#designate date as a date
phyto$date<-as.Date(phyto$date,"%m/%d/%Y")

#look at structure of data
#glimpse(phyto)

#look at prevalence of microcystis in samples
mc<-subset(phyto,genus=="Microcystis")

#look at unique dates included in data set
unique(sort(phyto$date)) 
#pre-treatment = June 2017 = "2017-05-23" "2017-05-25" "2017-06-05" "2017-06-07" 
#July 2017 = "2017-06-29" 
#August  2017 = "2017-07-31" "2017-08-02"
#September 2017 = "2017-08-28" "2017-08-30" 
#October 2017 = "2017-09-27" "2017-09-28" 
#November 2017 = "2017-10-30" "2017-11-01" 
#December 2017 = "2017-11-28" "2017-11-30" 
#January 2018 = "2018-01-10" "2018-01-11" 
#February 2018 = "2018-02-06" "2018-02-08"
#March 2018 = "2018-03-12"  "2018-03-14" ("2018-03-13" incorrect)
#April 2018 = "2018-04-10"  "2018-04-17" ("2018-04-16" incorrect)
#May 2018 = "2018-05-07" "2018-05-08"
#June 2018 = "2018-06-05" "2018-06-06"
#July 2018 = "2018-07-05" "2018-07-09" 
#August 2018 = "2018-08-07" "2018-08-08"
#September 2018 = "2018-09-17" "2018-09-18"
#October 2018 =  "2018-10-16" "2018-10-17" ("2018-10-10" incorrect)
#November 2018 = no sampling due to smoke
#December 2018 = "2018-12-17" "2018-12-18"


#look at date typos

#March 2018
#typo<-subset(phyto,date == "2018-03-13") #15 obs
#unique(typo$site) #"DI-SAV-1" "DI-WAT-1" "DI-SAV-2"
#should all be "2018-03-14"

#April 2018
#atypo<-subset(phyto,date == "2018-04-16") #9 obs
#unique(atypo$site) #"DI_SAV_1" "DI_WAT_1"
#should all be "2018-04-17"

#October 2018
#otypo<-subset(phyto,date == "2018-10-10") #7 obs
#unique(otypo$site) #"LH-SAV-1"
#should all be "2018-10-16"

#find and replace three incorrect sampling dates
phyto$date<-gsub('2018-03-13','2018-03-14',phyto$date) 
phyto$date<-gsub('2018-04-16','2018-04-17',phyto$date) 
phyto$date<-gsub('2018-10-10','2018-10-16',phyto$date)

#make sure site names are consistent

#inconsistent use of "-" and "_" in site names
#Replace all occurrences of "-" with "_" in site names
phyto$site<-gsub('-','_',phyto$site)

#also DI_SAV_1 was incorrected called DI_SAV_18 in Dec. 2018
phyto$site<-gsub('DI_SAV_18','DI_SAV_1',phyto$site)

unique(phyto$site) 
#there are now 16 names as there should be (4 locations x 2 habitats x 2 reps)

#add columns that separate island, stratum, and replicate
phyto<-separate(phyto, site, into = c("island", "stratum","rep"),remove=F) 

#create table that shows all unique island x date combos
#should show whether there are any dates associated with the wrong site
sd<-unique(phyto[c("date", "island")])
#reorder by date
sd<-sd[order(sd$date,sd$island),]
#the date x island combos look good
#I don't think there are any more typos

#create a new column with sample month in place of date
#this is because sampling dates are frequently not in the month the are meant to represent
#not very efficient approach but it works
#this version keeps the two pre-treatment surveys separate

#reduce df to just needed columns
names(phyto)
#date, site, island, stratum, month_survey
surv<-phyto[c("date","site","island","stratum","month_survey")]

#add separate columns for year and month
surv<-separate(surv, month_survey, into = c("s_year", "s_month"),remove=F) 

#create df with just unique combos of date and site
suv<-unique(surv[c("date","site","island","stratum","month_survey","s_year", "s_month")])
#reorder by date
suv<-suv[order(suv$date,suv$site),]#used for nMDS plots with each individual sample as a data point, instead of averaging within
#an island-stratum-date combo
phyto$month_survey<-phyto$date
phyto$month_survey<-gsub('2017-05-23','2017-05',phyto$month_survey ) 
phyto$month_survey<-gsub('2017-05-25','2017-05',phyto$month_survey ) 
phyto$month_survey<-gsub('2017-06-05','2017-06',phyto$month_survey ) 
phyto$month_survey<-gsub('2017-06-07','2017-06',phyto$month_survey ) 
phyto$month_survey<-gsub('2017-06-29','2017-07',phyto$month_survey ) 
phyto$month_survey<-gsub('2017-07-31' ,'2017-08',phyto$month_survey )
phyto$month_survey<-gsub('2017-08-02' ,'2017-08',phyto$month_survey )
phyto$month_survey<-gsub('2017-08-28' ,'2017-09',phyto$month_survey )
phyto$month_survey<-gsub('2017-08-30' ,'2017-09',phyto$month_survey )
phyto$month_survey<-gsub('2017-09-27' ,'2017-10',phyto$month_survey )
phyto$month_survey<-gsub('2017-09-28' ,'2017-10',phyto$month_survey )
phyto$month_survey<-gsub('2017-10-30' ,'2017-11',phyto$month_survey )
phyto$month_survey<-gsub('2017-11-01' ,'2017-11',phyto$month_survey )
phyto$month_survey<-gsub('2017-11-28' ,'2017-12',phyto$month_survey )
phyto$month_survey<-gsub('2017-11-30' ,'2017-12',phyto$month_survey )
phyto$month_survey<-gsub('2018-01-10' ,'2018-01',phyto$month_survey )
phyto$month_survey<-gsub('2018-01-11' ,'2018-01',phyto$month_survey )
phyto$month_survey<-gsub('2018-02-06' ,'2018-02',phyto$month_survey )
phyto$month_survey<-gsub('2018-02-08' ,'2018-02',phyto$month_survey )
phyto$month_survey<-gsub('2018-03-12' ,'2018-03', phyto$month_survey )
phyto$month_survey<-gsub('2018-03-14' ,'2018-03', phyto$month_survey )
phyto$month_survey<-gsub('2018-04-10' ,'2018-04', phyto$month_survey )
phyto$month_survey<-gsub('2018-04-17' ,'2018-04', phyto$month_survey )
phyto$month_survey<-gsub('2018-05-07' ,'2018-05', phyto$month_survey )
phyto$month_survey<-gsub('2018-05-08' ,'2018-05', phyto$month_survey )
phyto$month_survey<-gsub('2018-06-05' ,'2018-06', phyto$month_survey )
phyto$month_survey<-gsub('2018-06-06' ,'2018-06', phyto$month_survey )
phyto$month_survey<-gsub('2018-07-05' ,'2018-07', phyto$month_survey )
phyto$month_survey<-gsub('2018-07-09' ,'2018-07', phyto$month_survey )
phyto$month_survey<-gsub('2018-08-07' ,'2018-08', phyto$month_survey )
phyto$month_survey<-gsub('2018-08-08' ,'2018-08', phyto$month_survey )
phyto$month_survey<-gsub('2018-09-17' ,'2018-09', phyto$month_survey )
phyto$month_survey<-gsub('2018-09-18' ,'2018-09', phyto$month_survey )
phyto$month_survey<-gsub('2018-10-16' ,'2018-10', phyto$month_survey )
phyto$month_survey<-gsub('2018-10-17' ,'2018-10', phyto$month_survey )
phyto$month_survey<-gsub('2018-12-17' ,'2018-12', phyto$month_survey )
phyto$month_survey<-gsub('2018-12-18' ,'2018-12', phyto$month_survey )


#export df with info on how dates relate to year and month


#export df
#write.csv(suv,"C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Phytoplankton/Phyto_Metadata/survey_months_complete.csv",row.names = F)

#create a slight variation of the 'month_survey' column that combines the two pre-treatment surveys
#used for nMDS plots in which points represent means of island-stratum-date 
phyto$month_survey2<-phyto$month_survey
phyto$month_survey2<-gsub('2017-05','2017-06',phyto$month_survey2) 

#look closer at Strobilidium; must be very large in volume even though rare
#stobp<-subset(phyto,genus=="Strobilidium")
#disji<-subset(phyto,site=="DI_SAV_2" & date=="2017-06-07")
#Yeah they're very large

#similarly, look closer at members of the phylum Miozoa
#Gymnodinium, Peridinium
#gpphy<-subset(phyto,genus=="Gymnodinium" | genus=="Peridinium")
#lhtp<-subset(phyto,island=="LH" & stratum=="WAT" & date=="2018-12-17")
#they're very large too


#calculate estimated total volume per mL by multiplying density x average cell volume
phyto$volume<-phyto$density*phyto$volume_avg

#look at the unique taxa present across all samples
taxa<-unique(phyto[c("genus", "taxon")])
tax<-taxa[order(taxa$genus,taxa$taxon),] #reorder by genus, then taxon
str(tax)

#look at number of taxa within each genus
#use this to decide whether to use species or genus level for nMDS
txf<-tax %>% count(tax$genus) #I think I need to restructure this and make it a df
str(txf)

#now visually examine list to make sure there ar no "genera" due to typos
txf<-txf[order(txf$genus),] #reorder by genus
#didn't observe any names that look like slight derivations of one another

#look closer at number of taxa by genus
txfo<-txf[order(txf$n,txf$genus),] #reorder by n
hist(txfo$n) #look at distribution of number of taxa per genus
#vast majority have one taxon per genus

#combine number of taxa per genera with df w/ genus and taxon included
#then look closer at all genera with >1 taxon
#make sure no unique taxa due to typos
txfg<-join(tax,txf)
txfs<-subset(txfg,n>1) #only need to look at genera w/ >1 taxon
txfs<-txfs[order(txfs$genus,txfs$taxon),] #order by genus, then taxon 
#After visually reviewing the taxa within each genus, I think it makes
#sense to focus analyses on genus level and above
#many taxa within a genus are different forms of uncertain IDs rather than
#a large number of species per genus
#e.g., cf. [genus], [genus] sp., [genus] spp. 
#no good way to deal with these types of "taxa" in nMDS analyses

#aggregate density and volume by genus
gen <-aggregate(list(phyto$density,phyto$volume)
      , by=list(phyto$month_survey,phyto$month_survey2,phyto$date,phyto$site
          ,phyto$island,phyto$stratum,phyto$rep,phyto$genus),FUN=sum, na.rm=T)
names(gen)<-c("survey","survey2","date","site","island","stratum","rep","genus","density","volume")
taxa2<-sort(unique(gen$genus)) #76 levels

#look closer at Strobilidium. based on plots they are relative much larger cells 
strob<-subset(gen,genus=='Strobilidium')
#yeah they're very large

#generate taxonomic summary 

#remove the genus 'Amoeba' because it's a heterotroph (and also only one individual ever)
#it is also huge
genn<-subset(gen,genus!="Amoeba")

#add island-stratum column
genn$island_stratum<-paste(genn$island,genn$stratum,sep="_")
colnames(genn)
cols<-c("survey", "survey2", "site", "island","stratum","rep", "island_stratum")      
genn[cols] <- lapply(genn[cols], factor)
genn$date<-as.Date(genn$date)
str(genn)
#write.csv(genn,"C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Phytoplankton/Phyto_Density&Volume.csv",row.names = F)


#look at frequency of each genus across samples
fg<-genn %>% count(genus)
fg<-fg[order(fg$n),] #reorder by n
hist(fg$n,breaks=50)
#like most communities, there are a few taxa that are very common
#and many taxa that are very rare

#count the number of genera within each sample (i.e., each site x date combo)
ngen <- genn %>% 
  group_by(site,date) %>%
  summarise(no_rows = length(site))

#now get mean number of genera across all samples 
mean(ngen$no_rows) #6.119835
sd(ngen$no_rows) #2.024256

#how many genera appear in only one sample?
one<-subset(fg,n==1) #19 genera 
19/75 #0.2533333 or 25%

#how many genera appear in only two sample?
two<-subset(fg,n==2) #15 genera 
15/75 #0.2 or 20%

#what % of total genera are rare genera (i.e, occur in 1-2 samples)?
34/75 #0.4533333 or 45% of all genera 
#should make two data sets: w/ and w/out rare genera

#create a list of genera that occur in 3 or more samples
#will use this list to create subset of data w/o rare genera
com<-subset(fg,n>2)
coml<-com$genus 
rare<-subset(fg,n<3)
rarel<-rare$genus

#now subset main data frame to include only common genera (i.e., those that appear at least three samples)
common<-genn[genn$genus %in% coml,] #removed 49 rows from data set

#create rare only dataset just to see if things are working properly
rarer<-genn[genn$genus %in% rarel,] #49 rows which is how many were removed from 'common' df 


#Custom plot formatting function-----------------
theme_sav <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"
                                      ,margin=margin(t = 0, r = 10, b = 0, l = 0)
          ),
          strip.text = element_text(size = 11, face = "bold"),
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          #plot.margin = unit(c(0.1, 0.3, 0.1, 0.9), units = , "cm"), #top, right, bottom, left
          plot.margin = unit(c(0.2, 0.5, 0.1, 0.9), units = , "cm"), #adjusted the "top" and "right" values so nothing is cut off
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "plain"),
          legend.title=element_text(size=14))
}

#Density-based nMDS plots: genus level, site level, all genera, CONVERGED---------------------

#start with "genn" df which is aggregated by genus but still has each sample separate
#reduce to just essential columns
nmds<-genn[c(1,8,9,4:6)]

#convert from long to wide
genw <- spread(nmds, genus, density)

#replace NAs with zeros
genw[is.na(genw)] <- 0

#look at number of observations per station
table(genw$site)
#looks like about the right number of samples per site  

#paste abbreviated sample and date
genw$sd<-paste(genw$site,genw$survey,sep="_")

#final formatted matrix, just the species abundances columns
done<-genw[c(5:79)] #exclude the first four columns and very last column
row.names(done)<-genw$sd
str(done)

nnzero(done) #1481 of 18150 values are non-zero 
1481/18150 #0.0815978 or 8% aren't zeros

#run the nMDS
res<-metaMDS(done, k=3,trymax=100)
res #Stress:     0.194281 
#it didn't converge with k=2 but did with k=3
#A good rule of thumb: stress < 0.05 provides an excellent representation in reduced dimensions,
#< 0.1 is great, < 0.2 is good/ok, and stress < 0.3 provides a poor representation.

#look at Shepard plot which shows scatter around the regression between the interpoint distances 
#in the final configuration (i.e., the distances between each pair of communities) against their 
#original dissimilarities.
stressplot(res)

#Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores <- as.data.frame(scores(res))

# create a column of site names, from the rownames of data.scores
data.scores$site <- rownames(data.scores)

#break up island, stratum, and survey into separate columns
dscore<-separate(data.scores, site, into = c("island","stratum","site", "survey"),"_") 
#probably should keep an island_stratum column
dscore$island_stratum <- paste(dscore$island,"_",dscore$stratum)
#create separate month and year columns; use month column to create season column
dscore<-separate(dscore, survey, into = c("year","month"),"-") 

#use month column to create season column
dscore$season<-dscore$month
str(dscore)
dscore$season<-sub("12|01|02", "W", dscore$season)
dscore$season<-sub("03|04|05", "Sp", dscore$season)
dscore$season<-sub("06|07|08", "Su", dscore$season)
dscore$season<-sub("09|10|11", "F", dscore$season)

#create new column with full names of islands
dscore$island2<-dscore$island
dscore$island2<-gsub('DI',"Decker",dscore$island2)
dscore$island2<-gsub('FI',"French",dscore$island2)
dscore$island2<-gsub('LH',"Little Hastings",dscore$island2)
dscore$island2<-gsub('FC',"Fisherman's Cut",dscore$island2)

#Using the scores function from vegan to extract the species scores and convert to a data.frame
#species.scores <- as.data.frame(scores(res, "species"))  

# create a column of species, from the rownames of species.scores
#species.scores$species <- rownames(species.scores)  


#nMDS plot: each island-stratum as different color
ggplot(dscore,aes(x=NMDS1,y=NMDS2, col=island_stratum))+geom_point()+stat_ellipse()+theme_bw()
#not very interesting looking

#nMDS plot: stratum as different color (island ignored)
ggplot(dscore,aes(x=NMDS1,y=NMDS2, col=stratum))+geom_point()+stat_ellipse()+theme_bw()
ggplot(dscore,aes(x=NMDS1,y=NMDS3, col=stratum))+geom_point()+stat_ellipse()+theme_bw()
ggplot(dscore,aes(x=NMDS3,y=NMDS2, col=stratum))+geom_point()+stat_ellipse()+theme_bw()
#not very interesting looking

#pretty version of plot

# Mutate variables in dataframe to apply plot order
dscore <- dscore %>% 
  mutate(
    island = factor(
      island,
      levels = c(
        "LH",
        "FI",
        "DI",
        "FC")
    ),
    stratum = factor(
      stratum,
      levels = c(
        "WAT",
        "SAV")         
    ),
    season = factor(
      season,
      levels = c(
        "W",
        "Sp",
        "Su",
        "F")         
    )
  )

# Define custom plot formatting for each Stratum for the plots
# Colors
StratumColors <- c("WAT" = "dodgerblue3",
                   "SAV" = "green3")

# Point type
StratumPoint <- c("WAT" = 21,
                  "SAV" = 24)

(pstrat <- ggplot(dscore,aes(x=NMDS1,y=NMDS2, shape=stratum, color=stratum,fill = stratum))+
  geom_point()+
  stat_ellipse(geom = "polygon", alpha = 0.3, aes(fill = stratum))+
  scale_color_manual(values = StratumColors,name="Stratum") +  
  scale_fill_manual(values = StratumColors,name="Stratum")+  
  scale_shape_manual(values = StratumPoint,name="Stratum")+
    theme_sav()+theme(legend.position=c(0.15,0.9))
)


#anosim: stratum
pred <- dscore[4:11]
strat <- with(pred, anosim(done, grouping =stratum ,permutations=999, distance="bray"))
summary(strat)
plot(strat)
#ANOSIM statistic R: -0.005311; Significance: 0.932
#clearly not signficant

#adonis: stratum
adonis2(done ~ stratum, data = pred)
#adonis2(formula = done ~ stratum, data = pred)
#Df SumOfSqs      R2      F Pr(>F)
#stratum    1    0.029 0.00104 0.2504   0.84
#Residual 240   27.804 0.99896              
#Total    241   27.833 1.00000 
#still clearly not significant (no surprise)

#repeated measures adonis



#nMDS plot: islands as different color  (stratum ignored)
ggplot(dscore,aes(x=NMDS1,y=NMDS2, col=island))+geom_point()+stat_ellipse()+theme_bw()
ggplot(dscore,aes(x=NMDS1,y=NMDS3, col=island))+geom_point()+stat_ellipse()+theme_bw()
ggplot(dscore,aes(x=NMDS3,y=NMDS2, col=island))+geom_point()+stat_ellipse()+theme_bw()
#slightly more interesting but still not very interesting

#pretty version of plot
# Define custom plot formatting for each Island for the plots
# Colors
IslandColors <- c("DI" = "#FFB953",
                   "FC" = "#8F2662",
                  "FI" = "#47175B",
                  "LH" = "#EF7D54")

# Point type
IslandPoint <- c("DI" = 24,
                 "FC" = 22,
                 "FI" = 21,
                 "LH" = 25)

(pisl <- ggplot(dscore,aes(x=NMDS1,y=NMDS2, shape=island, color=island,fill = island))+
    geom_point()+
    stat_ellipse(geom = "polygon", alpha = 0.3, aes(fill = island))+
    scale_color_manual(values = IslandColors,name="Island") +  
    scale_fill_manual(values = IslandColors,name="Island")+  
    scale_shape_manual(values = IslandPoint,name="Island")+
    theme_sav()+theme(legend.position=c(0.14,0.87))
)


#anosim:island
isl <- with(pred, anosim(done, grouping =island ,permutations=999, distance="bray"))
summary(isl)
plot(isl)
#ANOSIM statistic R: 0.03028, Significance: 0.016 
#significant p-value but probably just because of different within-group varation (dispersion)

#ADONIS
adonis2(done ~ island, data = pred)
#adonis2(formula = done ~ island, data = pred)
#Df SumOfSqs      R2      F Pr(>F)  
#island     3   0.8076 0.02901 2.3706  0.016 *
#Residual 238  27.0258 0.97099                
#Total    241  27.8333 1.00000     
#results agree with those from ANOSIM but both are sensitive to dispersion though less so for ADONIS

#example
#data(dune)
#data(dune.env)
## default test by terms
#adonis2(dune ~ Management*A1, data = dune.env)


#nMDS plot: seasons as different color, (island and stratum ignored)
ggplot(dscore,aes(x=NMDS1,y=NMDS2, col=season))+geom_point()+stat_ellipse()+theme_bw()
ggplot(dscore,aes(x=NMDS1,y=NMDS3, col=season))+geom_point()+stat_ellipse()+theme_bw()
ggplot(dscore,aes(x=NMDS3,y=NMDS2, col=season))+geom_point()+stat_ellipse()+theme_bw()
#slightly more interesting but still not very interesting

#pretty plot: season

# Define custom plot formatting for each Island for the plots
# Colors
SeasonColors <- c("W" = "dodgerblue4",
                  "Sp" = "darkolivegreen4",
                  "Su" = "darkorange3",
                  "F" = "darkorange4")

# Point type
SeasonPoint <- c("W" = 24,
                 "Sp" = 22,
                 "Su" = 21,
                 "F" = 25)

(pseas <- ggplot(dscore,aes(x=NMDS1,y=NMDS2, shape=season, color=season,fill = season))+
    geom_point()+
    stat_ellipse(geom = "polygon", alpha = 0.3, aes(fill = season))+
    scale_color_manual(values = SeasonColors,name="Season") +  
    scale_fill_manual(values = SeasonColors,name="Season")+  
    scale_shape_manual(values = SeasonPoint,name="Season")+
    theme_sav()+theme(legend.position=c(0.14,0.87))
)

#combine stratum, island, and season plots
combp <- grid.arrange(pstrat,pisl,pseas,ncol=3)
ggsave(path=plot_folder,"Phyto_nMDS_SingleFactorPanel.png",combp,scale=1.6, height=4,width=8, units="in",dpi=300)

#write.csv(dscore, "C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Phytoplankton/phyto_nmds_density_sample.csv",row.names=F)


#anosim:season
seas <- with(pred, anosim(done, grouping =season ,permutations=999, distance="bray"))
summary(seas)
plot(seas)
#ANOSIM statistic R: 0.2303, Significance: 0.001 


#plot all seasons (across the two years) on the same plot but put each island into a different facet
ggplot(dscore,aes(x=NMDS1,y=NMDS2, group=season, color=season,fill = season))+
  #scale_color_manual(values=c('green3','dodgerblue3'))+
  #scale_fill_manual(values=c('green3','dodgerblue3'))+
  geom_point()+stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = season))+
  facet_grid(~island2)+theme_bw()+
  theme(strip.text.x = element_text(size =15),panel.grid.major = element_blank(),panel.grid.minor = element_blank())

#just look at seasons across the first year
y17<-dscore[dscore$year==2017,]
dscore$month2<-as.numeric(dscore$month) #good to keep as character up to this point but now use as number
y18<-dscore[dscore$year==2018 & dscore$month2<5,]
yone<-rbind(y17,y18)
str(yone)
ytwo<-dscore[dscore$year==2018 & dscore$month2>4,]

#plot all seasons (for year one) on the same plot but put each island into a different facet
ggplot(yone,aes(x=NMDS1,y=NMDS2, group=season, color=season,fill = season))+
  #scale_color_manual(values=c('green3','dodgerblue3'))+
  #scale_fill_manual(values=c('green3','dodgerblue3'))+
  geom_point()+stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = season))+
  facet_grid(~island2)+theme_bw()+
  theme(strip.text.x = element_text(size =15),panel.grid.major = element_blank(),panel.grid.minor = element_blank())

#plot all seasons (for year two) on the same plot but put each island into a different facet
ggplot(ytwo,aes(x=NMDS1,y=NMDS2, group=season, color=season,fill = season))+
  #scale_color_manual(values=c('green3','dodgerblue3'))+
  #scale_fill_manual(values=c('green3','dodgerblue3'))+
  geom_point()+stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = season))+
  facet_grid(~island2)+theme_bw()+
  theme(strip.text.x = element_text(size =15),panel.grid.major = element_blank(),panel.grid.minor = element_blank())


#plot the two strata for each island on the same plot (all survey dates as points) but put each island
#into a different facet

#custom facet labels
island_label <- c('DI'="Decker Island",
                  'FC'="Fisherman's Cut",
                  'FI'="French Island",
                  'LH'="Little Hastings"
)

#custom stratum names for legend
stratum_label<-c("Vegetation","Open Water")

ggplot(dscore,aes(x=NMDS1,y=NMDS2, shape=stratum,group=stratum, color=stratum,fill = stratum))+
  scale_color_manual(values=c('green3','dodgerblue3'),name="Stratum",labels=stratum_label)+
  scale_fill_manual(values=c('green3','dodgerblue3'),name="Stratum",labels=stratum_label)+
  scale_shape_manual(values = StratumPoint,name="Stratum",labels=stratum_label)+
      geom_point()+stat_ellipse(geom = "polygon", alpha = 0.3, aes(fill = stratum))+
  facet_grid(~island,labeller = labeller(island = island_label))+theme_sav()+
theme(legend.position=c(0.34,0.88))
#ggsave(path=plot_folder,"Phyto_nMDS_SiteByStratumPanel.png",scale=1.2, height=4,width=8, units="in",dpi=300)


  

#Biovolume-based nMDS plots: genus level, site level, all genera, NO CONVERGENCE---------------------

#start with "genn" df which is aggregated by genus but still has each sample separate
#reduce to just essential columns
nmds2<-genn[c(1,8,10,4:6)]

#convert from long to wide
genw2 <- spread(nmds2, genus, volume)

#replace NAs with zeros
genw2[is.na(genw2)] <- 0

#look at number of observations per station
table(genw2$site)
#looks like about the right number of samples per site  

#create abbreviated sample ID column for use as matrix row name labels
genw2$site<-gsub("_SAV_","S",genw2$site)
genw2$site<-gsub("_WAT_","W",genw2$site)

#paste abbreviated sample and date
genw2$sd<-paste(genw2$site,genw2$survey,sep="_")

#final formatted matrix, just the species abundances columns
done2<-genw2[c(5:79)] #exclude the first four columns and very last column
row.names(done2)<-genw2$sd
#str(done2)

nnzero(done2) #1481 of 18150 values are non-zero (same as density obviously)
1481/18150 #0.0815978 or 8% aren't zeros

#run the nMDS
#res2<-metaMDS(done2, k=2,trymax=1000)
#it didn't converge with k=2 or k=3


#Density-based nMDS plots: genus level, site level, only common genera, CONVERGED---------------------

#start with "common" df which is aggregated by genus but still has each sample separate
#reduce to just essential columns
cnmds<-common[c(1,8,9,4:6)]

#convert from long to wide
cgenw <- spread(cnmds, genus, density)

#replace NAs with zeros
cgenw[is.na(cgenw)] <- 0

#look at number of observations per station
table(cgenw$site)
#looks like about the right number of samples per site  

#create abbreviated sample ID column for use as matrix row name labels
cgenw$site<-gsub("_SAV_","S",cgenw$site)
cgenw$site<-gsub("_WAT_","W",cgenw$site)

#paste abbreviated sample and date
cgenw$sd<-paste(cgenw$site,cgenw$survey,sep="_")

#final formatted matrix, just the species abundances columns
cdone<-cgenw[c(5:45)] #exclude the first four columns and very last column
row.names(cdone)<-cgenw$sd
#str(cdone)

nnzero(cdone) #1432 of 9922 values are non-zero 
1432/9922 #0.1443257 or 15% aren't zeros

#run the nMDS
#cres<-metaMDS(cdone, k=3,trymax=100)
#it didn't converge with k=2 but did with k=3 (but not on the first attempt)
cres #Stress:     0.1965976

#Biovolume-based nMDS plots: genus level, site level, only common genera, CONVERGED---------------------

#start with "genn" df which is aggregated by genus but still has each sample separate
#reduce to just essential columns
cnmds2<-common[c(1,8,10,4:6)]

#convert from long to wide
cgenw2 <- spread(cnmds2, genus, volume)

#replace NAs with zeros
cgenw2[is.na(cgenw2)] <- 0

#look at number of observations per station
table(cgenw2$site)
#looks like about the right number of samples per site  

#create abbreviated sample ID column for use as matrix row name labels
cgenw2$site<-gsub("_SAV_","S",cgenw2$site)
cgenw2$site<-gsub("_WAT_","W",cgenw2$site)

#paste abbreviated sample and date
cgenw2$sd<-paste(cgenw2$site,cgenw2$survey,sep="_")

#final formatted matrix, just the species abundances columns
cdone2<-cgenw2[c(5:45)] #exclude the first four columns and very last column
row.names(cdone2)<-cgenw2$sd
#str(done2)

nnzero(cdone2) #1432 of 9922 values are non-zero (same as density)
1432/9922 #0.1443257 or 15% aren't zeros

#run the nMDS
#cres2<-metaMDS(cdone2, k=3,trymax=500)
#it didn't converge with k=2 but did converge with k=3 (but not on the first attempt)
cres2 #Stress:     0.2128444 

#Density-based nMDS plots: genus level, island-stratum level, all genera, NO CONVERGENCE---------------------

#calculate means for both density and volume by island-stratum (not site), date, and genus
scoll <- aggregate(list(genn$density,genn$volume),by=list(genn$survey,genn$date,genn$island_stratum,genn$island,genn$stratum,genn$genus),FUN="mean") 
names(scoll)<-c("survey","date","island_stratum","island","stratum","genus","density","volume")
#some of these columns were preserved just for making plots further down

#look closer at taxa in Oct 2017 when biovolume was high
#oct<-subset(scoll,date>"2017-09-20" & date<"2017-10-05" & volume > 1000000)

#start with "scoll" df which is aggregated by genus and has sites within island-stratum combined
#reduce to just essential columns
snmds<-scoll[c(1,3,6,7)]

#convert from long to wide
swide <- spread(snmds, genus, density)

#replace NAs with zeros
swide[is.na(swide)] <- 0

#look at number of observations per station
table(swide$island_stratum)
#looks about right 

#create abbreviated sample ID column for use as matrix row name labels
swide$island_stratum<-gsub("_SAV","S",swide$island_stratum)
swide$island_stratum<-gsub("_WAT","W",swide$island_stratum)

#paste abbreviated sample and date
swide$sd<-paste(swide$island_stratum,swide$survey,sep="_")
str(swide)
#final formatted matrix, just the species abundances columns
sdone<-swide[c(3:77)] #exclude the first two columns and very last column
row.names(sdone)<-swide$sd
nnzero(sdone) #1053 of 9450 values are non-zero 
1053/9450 #0.1114286 or 11% aren't zeros

#run the nMDS
#sres<-metaMDS(sdone, k=3,trymax=500)
#no convergence with k=2 or k=3
#A good rule of thumb: stress < 0.05 provides an excellent representation in reduced dimensions,
#< 0.1 is great, < 0.2 is good/ok, and stress < 0.3 provides a poor representation.


#Biovolume-based nMDS plots: genus level, island-stratum level, all genera, CONVERGED---------------------
#very similar to density

#start with "scoll" df which is aggregated by genus and has sites within island-stratum combined
#reduce to just essential columns
snmds2<-scoll[c(1,3,6,8)]

#convert from long to wide
swide2 <- spread(snmds2, genus, volume)

#replace NAs with zeros
swide2[is.na(swide2)] <- 0

#create abbreviated sample ID column for use as matrix row name labels
swide$island_stratum<-gsub("_SAV","S",swide$island_stratum)
swide$island_stratum<-gsub("_WAT","W",swide$island_stratum)

#paste abbreviated sample and date
swide2$sd<-paste(swide2$island_stratum,swide2$survey,sep="_")

#final formatted matrix, just the species abundances columns
sdone2<-swide2[3:77] #exclude first two columns and last column
row.names(sdone2)<-swide2$sd
nnzero(sdone2) #1053 of 9450 values are non-zero (same as density obviously)
1053/9450 #0.1114286 or 11% aren't zeros

#run the nMDS
#vres<-metaMDS(sdone2, k=3,trymax=100)
#didn't converge with k=2 but did converge with k=3
vres #Stress: 0.2189936 not great but probably sufficient

#A good rule of thumb: stress < 0.05 provides an excellent representation in reduced dimensions,
#< 0.1 is great, < 0.2 is good/ok, and stress < 0.3 provides a poor representation.


#Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores2 <- as.data.frame(scores(vres))

# create a column of site names, from the rownames of data.scores2
data.scores2$site <- rownames(data.scores2)

#break up island, stratum, and survey into separate columns
vscore<-separate(data.scores2, site, into = c("island", "stratum","survey"),"_") 
#probably should keep an island_stratum column
vscore$island_stratum <- paste(vscore$island,"_",vscore$stratum)

#Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores2 <- as.data.frame(scores(vres, "species"))  

# create a column of species, from the rownames of species.scores2
species.scores2$species <- rownames(species.scores2)  


#nMDS plot: each island-stratum as different color, all survey dates included as points
ggplot(vscore,aes(x=NMDS1,y=NMDS2, col=island_stratum))+geom_point()+stat_ellipse()+theme_bw()
#not very interesting looking

#nMDS plot: stratum as different color, all survey dates included as points (island ignored)
ggplot(vscore,aes(x=NMDS1,y=NMDS2, col=stratum))+geom_point()+stat_ellipse()+theme_bw()
#not very interesting looking

#nMDS plot: islands as different color, all survey dates included as points (stratum ignored)
ggplot(vscore,aes(x=NMDS1,y=NMDS2, col=island))+geom_point()+stat_ellipse()+theme_bw()
#slightly more interesting but still not very interesting

#plot the two strata for each island on the same plot (all survey dates as points) but put each island
#into a different facet
ggplot(vscore,aes(x=NMDS1,y=NMDS2, group=stratum, color=stratum,fill = stratum))+
  scale_color_manual(values=c('green3','dodgerblue3'))+
  scale_fill_manual(values=c('green3','dodgerblue3'))+
  geom_point()+stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = stratum))+facet_wrap(~island)+theme_bw()


#look at genus-level diversity through time by island_stratum------------

#get count of number of genera by island_stratum
diver<-scoll %>% count(survey,date,island,stratum,island_stratum) 
diver$n<-as.numeric(diver$n)
str(diver)

# Mutate variables in dataframe to apply plot order
diver <- diver %>% 
  mutate(
    island = factor(
      island,
      levels = c(
        "LH",
        "FI",
        "DI",
        "FC")
    ),
    stratum = factor(
      stratum,
      levels = c(
        "WAT",
        "SAV")         
    )
  )

#create df with coordinates showing date ranges when treatment took place at LH and DI
#there are three date ranges, one in 2017 and two in 2018
trects <- data.frame(island = c("DI", "FC", "FI", "LH"), #island name (note different order than in plot)
                     ymin = c(-Inf, NA,NA,-Inf), #min y value for all shaded boxes in each island panel
                     ymax = c(Inf, NA,NA,Inf), #max y value for all shaded boxes in each island panel
                     xmin = c('2017-06-06', NA,NA,'2017-06-07'), #min x value for first shaded box in each panel
                     xmax = c('2017-09-18',NA,NA,'2017-09-19'), #max x value for first shaded box in each panel
                     xmin2 = c('2018-03-29', NA,NA,'2018-03-19'), #min x value for 2nd shaded box in each panel
                     xmax2 = c('2018-06-21',NA,NA,'2018-07-02'), #max x value for 2nd shaded box in each panel
                     xmin3 = c(NA,NA,NA,'2018-09-17'), #min x value for 3rd shaded box in each panel
                     xmax3 = c(NA,NA,NA,'2018-11-26') #max x value for 3rd shaded box in each panel
)
#format date columns as dates in data frame for making shaded regions
ix<-4:9
trects[ix] <- lapply(trects[ix], as.Date, "%Y-%m-%d")
str(trects)

# Define custom plot formatting for each Stratum for the plots
# Colors
StratumColors <- c("WAT" = "dodgerblue3",
                   "SAV" = "green3")

# Point type
StratumPoint <- c("WAT" = 21,
                  "SAV" = 24)

# Line type
StratumLine <- c("WAT" = 1,
                 "SAV" = 2)

#custom facet labels
island_label <- c('DI'="Decker Island",
                  'FC'="Fisherman's Cut",
                  'FI'="French Island",
                  'LH'="Little Hastings"
)

#custom stratum names for legend
stratum_label<-c("Open Water","Vegetation")

#Number of genera through time by site
(divp1<-ggplot() + 
    geom_line(data=diver,
              aes(x=date, y=n, group=stratum,color=stratum,linetype=stratum))+
    geom_point(data=diver,aes(x=date, y=n, group=stratum
                             ,color=stratum, fill=stratum, shape=stratum))+
    scale_color_manual(values = StratumColors,name="Stratum",labels=stratum_label) +  
    scale_fill_manual(values = StratumColors,name="Stratum",labels=stratum_label)+  
    scale_shape_manual(values = StratumPoint,name="Stratum",labels=stratum_label) +  
    scale_linetype_manual(values = StratumLine,name="Stratum",labels=stratum_label)+  
    labs(x="Date", y="Phytoplankton Genus-level Richness")+
    facet_grid(island~.
               ,labeller = labeller(island = island_label))+
    theme_sav()+
    geom_rect(data = trects, aes(xmin = xmin,
                                 xmax = xmax,
                                 ymin = ymin, 
                                 ymax = ymax,
                                 fill=island),
              fill = 'grey', alpha=0.3)+
    geom_rect(data = trects, aes(xmin = xmin2,
                                 xmax = xmax2,
                                 ymin = ymin, 
                                 ymax = ymax,
                                 fill=island),
              fill = 'grey', alpha=0.3)+
    geom_rect(data = trects, aes(xmin = xmin3,
                                 xmax = xmax3,
                                 ymin = ymin, 
                                 ymax = ymax,
                                 fill=island),
              fill = 'grey', alpha=0.3)+
        theme(legend.position=c(0.2,0.16))
)
#ggsave(path=plot_folder,"Phyto_TimeSeries_Richness_Genus.png",scale=0.7, height=10,width=8, units="in",dpi=300)
#no clear patterns



#look at total density and biovolume through time by island_stratum------------

#get total density by island_stratum
#density is currently organisms/mL by genus
#so just need to sum organisms across entire sample for each date-island-stratum combo
dens <- aggregate(list(scoll$density,scoll$volume),by=list(scoll$date,scoll$island_stratum,scoll$island,scoll$stratum),FUN="sum") 
names(dens)<-c("date","island_stratum","island","stratum","density","volume")
dens$island<-factor(dens$island)
dens$stratum<-factor(dens$stratum)
dens$island_stratum<-factor(dens$island_stratum)
str(dens)

#summary stats
mean(dens$density) #110091.9
sd(dens$density) #54797.85
mean(dens$volume) #1248161
sd(dens$volume) #2522432

#plot: density



# Mutate variables in dataframe to apply plot order
dens <- dens %>% 
  mutate(
    island = factor(
      island,
      levels = c(
        "LH",
        "FI",
        "DI",
        "FC")
    ),
    stratum = factor(
      stratum,
      levels = c(
        "WAT",
        "SAV")         
    )
  )



#plot: log(density)
(denp1<-ggplot() + 
      geom_line(data=dens,aes(x=date, y=log(density), group=stratum
                ,color=stratum,linetype=stratum))+
      geom_point(data=dens,aes(x=date, y=log(density), group=stratum
                ,color=stratum, fill=stratum, shape=stratum))+
      scale_color_manual(values = StratumColors,name="Stratum",labels=stratum_label) +  
      scale_fill_manual(values = StratumColors,name="Stratum",labels=stratum_label)+  
      scale_shape_manual(values = StratumPoint,name="Stratum",labels=stratum_label) +  
      scale_linetype_manual(values = StratumLine,name="Stratum",labels=stratum_label)+  
      labs(x="Date", y="Phytoplankton LN(Density) (Organisms / mL)")+
      facet_grid(island~.
                 ,labeller = labeller(island = island_label))+
      theme_sav()+
      geom_rect(data = trects, aes(xmin = xmin,
                                   xmax = xmax,
                                   ymin = ymin, 
                                   ymax = ymax,
                                   fill=island),
                fill = 'grey', alpha=0.3)+
      geom_rect(data = trects, aes(xmin = xmin2,
                                   xmax = xmax2,
                                   ymin = ymin, 
                                   ymax = ymax,
                                   fill=island),
                fill = 'grey', alpha=0.3)+
    geom_rect(data = trects, aes(xmin = xmin3,
                                 xmax = xmax3,
                                 ymin = ymin, 
                                 ymax = ymax,
                                 fill=island),
              fill = 'grey', alpha=0.3)+
  theme(legend.position=c(0.2,0.16))
  
  )
#ggsave(path=plot_folder,"Phyto_TimeSeries_Density_LN.png",scale=0.7, height=10,width=8, units="in",dpi=300)
#phyto density decreases from summer to winter but no patterns within or among islands

#plot: volume
(volp1<-ggplot() + 
    geom_line(data=dens,aes(x=date, y=log(volume), group=dens$stratum
                            ,color=stratum,linetype=stratum))+
    geom_point(data=dens,aes(x=date, y=log(volume), group=dens$stratum
                             ,color=stratum, fill=stratum, shape=stratum))+
    scale_color_manual(values = StratumColors,name="Stratum",labels=stratum_label) +  
    scale_fill_manual(values = StratumColors,name="Stratum",labels=stratum_label)+  
    scale_shape_manual(values = StratumPoint,name="Stratum",labels=stratum_label) +  
    scale_linetype_manual(values = StratumLine,name="Stratum",labels=stratum_label)+  
    scale_y_continuous(expression(paste("Phytoplankton LN(Biovolume) (",mu,"m" ^ "3"," / mL)"))) +
    xlab("Date")+
    facet_grid(island~.
               ,labeller = labeller(island = island_label))+
    theme_sav()+
    geom_rect(data = trects, aes(xmin = xmin,
                                 xmax = xmax,
                                 ymin = ymin, 
                                 ymax = ymax,
                                 fill=island),
              fill = 'grey', alpha=0.3)+
    geom_rect(data = trects, aes(xmin = xmin2,
                                 xmax = xmax2,
                                 ymin = ymin, 
                                 ymax = ymax,
                                 fill=island),
              fill = 'grey', alpha=0.3)+
    geom_rect(data = trects, aes(xmin = xmin3,
                                 xmax = xmax3,
                                 ymin = ymin, 
                                 ymax = ymax,
                                 fill=island),
              fill = 'grey', alpha=0.3)+
    theme(legend.position=c(0.2,0.16))
  
)
#ggsave(path=plot_folder,"Phyto_TimeSeries_Biovolume_LN.png",scale=0.7, height=10,width=8, units="in",dpi=300)
#there are some spikes in biovolume in the SAV that don't appear in the WAT
#for LH and FI in Oct 2017, it's caused primarily by Oedogonium (class Chlorophyceae)


#combine the list of genera from all samples with the taxonomic categories data frame----------------
#i.e., dataframe called 'taxonomy'
genum<-unique(gen$genus) #76 genera including Amoeba

#first make a data frame with all taxa
id<-1:76
taxall<-data.frame(cbind(id,unique(as.character(gen$genus))))
names(taxall)[1:2]<-c("id","genus")
str(taxall)
str(taxonomy)

#combine fill list of genera with the 'tax' df with higher order taxonomic levels
tall<-join(taxall,taxonomy,type="left")

#now just the columns needed for export
tallr<-tall[2:9]

#now export this data frame to fill in the remaining taxa
#write.csv(tallr, "C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Phytoplankton/Phyto_Taxonomy/phyto_2018-12_taxonomy_incomplete.csv",row.names=F)

#read in the file with the filled out taxonomy
tall2<-read.csv("C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Phytoplankton/Phyto_Taxonomy/phyto_2018-12_taxonomy_complete.csv") 
#taxonomy for all samples 
#use this file to bin taxa into categories at varying higher order taxon levels

#look at high order taxonomy diversity
fam<-unique(tall2$family) #54 families
ord<-unique(tall2$order) #34 orders
cls<-unique(tall2$class) #15 classes
phy<-unique(tall2$phylum) #10 phyla

#look at class with high biovolume during some months
#chl<-subset(tall2,class=="Chlorophyceae")

#create table summarizing the proportion of samples containing each genus
#start with fg data frame (shows how many samples include each genus)
#divide all counts by number of samples by 242, which is the total number of samples
#round to the nearest thousandths place
fg$prop <- round(fg$n/242,3)
range(fg$prop) #0.004132231 0.971074380

#combine with tall2 data frame which contains all higher level taxonomic categories
tfg <- join(fg,tall2)

#sort most to least common
tfg<-tfg[order(-tfg$prop),]

#subset and reorder columns for export
etfg <- tfg[c(5:10,1:3)]

#now export this data frame to fill in the remaining taxa
#write.csv(etfg, "C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Phytoplankton/Phyto_Taxonomy/Phyto_freq_by_genus.csv",row.names=F)



#add higher level taxa to main data frame
tpall<-join(scoll,tall2)

#DI_SAV in June 2017 has high proportion of biovolume as phylum Ciliophora
#Ciliophora is low by density and represented only by genus Strobilidium
#are the few cells of this genus just very large in volume or is there an error?

#first look at subset for that site and date
disj<-subset(tpall,island_stratum=='DI_SAV' & survey=="2017-06")
#the biovolume for Strobilidium is an order of magnitude greater than that of any other group
#so the plot is accurate


#aggregate density and volume by phyla (10 unique phyla)
#for stacked bar plot 
names(tpall)
phylum <-aggregate(list(tpall$density,tpall$volume), by=list(tpall$survey,tpall$date,tpall$island_stratum,tpall$island,tpall$stratum,tpall$phylum),FUN=sum, na.rm=T)
names(phylum)<-c("survey","date","island_stratum","island","stratum","phylum","density","volume")
str(phylum)
phylum_names<-unique(phylum$phylum)

#Relative density: Stacked bar plots 
#one graph per survey (year_month)
#stacked bars show phyla
#x axis shows strata
#facets are islands

# Mutate variables in dataframe to apply plot order
phylum <- phylum %>% 
  mutate(
    island = factor(
      island,
      levels = c(
        "LH",
        "FI",
        "DI",
        "FC")
    ),
    stratum = factor(
      stratum,
      levels = c(
        "WAT",
        "SAV")         
    )
  )



#http://colorbrewer2.org/#type=diverging&scheme=BrBG&n=10
#10 colors: diverging color palette (colorblind safe)
phylum_colors<-c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')
names(phylum_colors) <- levels(phylum$phylum)

#Plots: Total Density
#phylum %>%   #turned 1st line to annotation to prevent running code which could write over existing plots
split(.$survey) %>% 
  map(~ggplot(.x, aes(x=stratum, y= density,  fill = phylum, width=0.95))+
 geom_bar(position = "stack", stat = "identity",colour="grey25") + 
  ylab("Phytoplankton Density (Organisms / mL)") + xlab("Stratum") + 
  theme_sav() + 
   scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
   scale_fill_manual(name = "Phylum",
                     values=phylum_colors,
                     breaks=phylum_names,
                     labels = phylum_names)+ 
  facet_grid(~island,labeller = labeller(island = island_label) 
))%>% 
  imap(~ ggsave(path=plot_folder,paste0("phyto_density_total_stacked_bar_",.y, ".png"),plot=.x,
                , scale=1.05, height=10,width=8, units="in",dpi=300))
#maybe assign some rare phyla to "Other" category (or make final bars really tall to show them)
#could also remove all taxa that occur in 2 or fewer samples, which would probably help
#consider breaking up Cyanobacteria which is most of the abundance

#Plots: Relative Density
#phylum %>%   #turned 1st line to annotation to prevent running code which could write over existing plots
split(.$survey) %>% 
  map(~ggplot(.x, aes(x=stratum, y= density,  fill = phylum, width=0.95))+
        geom_bar(position = "fill", stat = "identity",colour="grey25") + 
        ylab("Proportion Density") + xlab("Stratum") + 
        theme_sav() + 
        scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
        scale_fill_manual(name = "Phylum",
                          values=phylum_colors,
                          breaks=phylum_names,
                          labels = phylum_names)+ 
        facet_grid(~island,labeller = labeller(island = island_label) 
        ))%>% 
  imap(~ ggsave(path=plot_folder,paste0("phyto_density_relative_stacked_bar_",.y, ".png"),plot=.x,
                , scale=1.05, height=10,width=8, units="in",dpi=300))

#Plots: Total Biovolume

#look at total biovolume range (refer to more summarized data set 'dens')
range(dens$volume) #95049.69 18636353.36

#phylum %>%   #turned 1st line to annotation to prevent running code which could write over existing plots
split(.$survey) %>% 
  map(~ggplot(.x, aes(x=stratum, y= volume,  fill = phylum, width=0.95))+
        geom_bar(position = "stack", stat = "identity",colour="grey25") + 
        scale_y_continuous(limits=c(0,19000000),expression(paste("Biovolume (",mu,"m" ^ "3"," / mL)"))) +
        xlab("Stratum") + theme_sav() +
        scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
        scale_fill_manual(name = "Phylum",
                          values=phylum_colors,
                          breaks=phylum_names,
                          labels = phylum_names)+ 
        facet_grid(~island,labeller = labeller(island = island_label) 
        ))%>% 
  imap(~ ggsave(path=plot_folder,paste0("phyto_biovolume_total_stacked_bar_",.y, ".png"),plot=.x
                , scale=1.05, height=10,width=8, units="in",dpi=300))
#total density is highly variable across sites and dates; not sure fixed y axis is best option


#Plots: Relative Biovolume

#phylum %>%   #turned 1st line to annotation to prevent running code which could write over existing plots
  split(.$survey) %>% 
  map(~ggplot(.x, aes(x=stratum, y= volume,  fill = phylum, width=0.95))+
        geom_bar(position = "fill", stat = "identity",colour="grey25") + 
        ylab("Proportion Biovolume")+ xlab("Stratum") + 
        theme_sav() + 
        scale_colour_discrete(drop=T, limits = levels(phylum$phylum))+
        scale_fill_manual(name = "Phylum",
                          values=phylum_colors,
                          breaks=phylum_names,
                          labels = phylum_names)+ 
        facet_grid(~island,labeller = labeller(island = island_label) 
        ))%>% 
  imap(~ ggsave(path=plot_folder,paste0("phyto_biovolume_relative_stacked_bar_",.y, ".png"),plot=.x
                , scale=1.05, height=10,width=8, units="in",dpi=300))




#aggregate density and volume by phyla (10 unique phyla) across all samples to look at dominant phyla
aphylum <-aggregate(list(tpall$density,tpall$volume), by=list(tpall$phylum),FUN=sum, na.rm=T)
names(aphylum)<-c("phylum","density","volume")
str(aphylum)
aphylum<-aphylum[order(aphylum$volume),]

  
#Stacked line Plot: Biovolume by island and stratum at------------------ 

#create list of site names to replace abbreviations in plot headings
site_names <- list(
  'DI'="Decker",
  'FC'="Fisherman's Cut",
  'FI'="French",
  'LH'="Little Hastings"
)
site_labeller <- function(variable,value){
  return(site_names[value])
}

#create list of site names to replace abbreviations in plot headings
stratum_names <- list(
  'SAV'="Vegetation",
  'WAT'="Open Water"
)

stratum_labeller <- function(variable,value){
  return(stratum_names[value])
}


ggplot(phylum, aes(x = date, y = volume, fill = phylum)) + 
  geom_area(position = 'stack')+
  facet_grid(stratum~island,labeller = labeller(
    stratum = stratum_labeller,
    island = site_labeller))+
  ylab("Biovolume (um/mL)") + xlab("Sampling Month") + theme_bw() + #NOTE: biovolume units look wrong
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45,hjust=1),
        axis.title.x = element_text(margin = unit(c(6, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 6, 0, 0), "mm")))
#scale_x_continuous(labels=c("Jun-17","Jul-17","Aug-17","Sep-17"))
#ggsave("Phyto_Density_LN_StackedLine.png",type ="cairo-png",width=12, height=6,units="in",dpi=300)
