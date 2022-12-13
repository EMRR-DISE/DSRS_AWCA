#DSRS Aquatic Weed Control Action
#correlation betweeen phyto biovolume and chlorophyll-a and pheophytin

#packages
library(tidyverse)
library(lubridate)
library(readxl)
library(ggcorrplot)

#import the data
#Phyto
phyto<-read.csv("C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Phytoplankton/Phyto_Density&Volume.csv")
#WQ
wq<-read_excel("C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - WaterQuality_Discrete/AquaticVeg_DiscreteWQ_AllSites_FINAL.xlsx")


#reformatting phyto data frame------------

#format date in phyto df
phyto$date<-ymd(phyto$date)
#str(phyto) #looks good now

#add a station column
#phyto<-phyto %>% mutate(
#  station = paste(island, stratum, rep, sep = "-")
#)
#export this df
#write.csv(phyto,"C:/Users/nrasmuss/California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - Phytoplankton/phyto_densities.csv",row.names = F)


#create new data set that sums biovolume and density by site
phytosum<-phyto %>% 
  group_by(site, island, stratum, rep, date) %>% #columns of tibble to group by
  summarize(
    tden = sum(density, na.rm=T),
    tvol = sum(volume, na.rm=T)) %>% 
  filter(rep==1) #subset to remove the replicate 2 phyto stations which have no discrete wq data



#reformatting WQ data frame-----------------

#remove time from date column
wq$date<- as.Date(format(as.POSIXct(wq$CollectionDatePST,format='%Y-%m-%d %H:%M:%S')
                            ,format='%Y-%m-%d'))

#look at dataframe structure
#str(wq)

#look at list of analytes
unique(wq$Analyte) #n=28
#includes "Chlorophyll a", "Pheophytin a"

#look at subset of data that are non-detects (i.e., ND=1)
#wqnd<-filter(wq,ND==1) #n=111
#look at what is entered in the results column
#unique(wqnd$Result) #all say "< RL"

#removes non-detects
wqnew<-filter(wq,ND==0) 

#reduce to just needed columns
#names(wq)
wqsht<-wqnew[c("Site","Stratum", "date", "Analyte", "Result", "Units")]
wqsht$Result<-as.numeric(wqsht$Result)
#str(wqsht)  

#rename columns
names(phytosum)
names(wqsht)<-c("island","stratum","date","analyte","result","units")

#rename the islands to match the phyto df
#unique(wqsht$island) 
#"French Island"   "Little Hastings" "Decker Island"   "Fisherman's Cut"

wqsht$island<-ifelse(wqsht$island == "French Island", "FI", 
                     ifelse(wqsht$island == "Little Hastings", "LH",
                     ifelse(wqsht$island == "Decker Island", "DI","FC")))
#unique(wqsht$island) # "FI" "LH" "DI" "FC"

#rename the strata to match the phyto df
#unique(wqsht$stratum) #"Vegetation" "Open Water"
wqsht$stratum<-ifelse(wqsht$stratum == "Vegetation", "SAV","WAT")
#unique(wqsht$stratum) #"SAV" "WAT"

#unique(wqsht$analyte)

#remove data for LH WAT on 2017-05-23
#it looks like LH SAV data got mislabelled, so there appears to be duplication of this sample
wqst<-filter(wqsht, !(island=="LH" & stratum=="WAT" & date=="2017-05-23"))

#convert wq df to wide format
wide<- pivot_wider(data=wqst,
                   id_cols=c(island, stratum, date),
                   names_from=analyte,
                   values_from=result
                   )

#make Site and Stratum into factors
wide$island<-as.factor(wide$island)
wide$stratum<-as.factor(wide$stratum)

#export this data set for use in R exercises
#write.csv(wide, "chlora_conc.csv",row.names=F)

#now combine the phyto and wq data--------------

#str(phytosum)
#str(wide)
comb<-full_join(phytosum,wide)
#not sure this worked well for all analytes; lots of NAs that shouldn't be NAs
#but it looks OK for Chlorophyll a and Pheophytin a 

#look relationship between phytoplankton and discrete wq-------------

#first look at data distributions for each
hist(comb$tvol)
hist(comb$tden)
hist(comb$`Chlorophyll a`)
hist(comb$`Pheophytin a`)

#look at correlation before removing outliers
cut<-comb[c("tvol","tden","Chlorophyll a","Pheophytin a")]
corrdata<-cor(cut, method="pearson", use = "complete.obs")
p.mat <- cor_pmat(cut)
ggcorrplot(corrdata, lab=TRUE, p.mat = p.mat, sig.level = .05)

#remove outliers for chlorophyll a
cout <- boxplot(cut$`Chlorophyll a`, plot = FALSE)$out
cout #8.88  4.88 15.30 18.50  5.43  6.66  9.42 38.10  5.17
cut[cut$`Chlorophyll a` %in% cout, "Chlorophyll a"] = NA

#remove outliers for pheophytin a
pout <- boxplot(cut$`Pheophytin a`, plot = FALSE)$out
pout #4.81  2.37  2.40  5.79 12.30  2.20 12.00  2.28  2.80  4.80  8.30  3.32
cut[cut$`Pheophytin a` %in% pout, "Pheophytin a"] = NA

#remove outliers for phyto density
dout <- boxplot(cut$tden, plot = FALSE)$out
dout #274118.7
cut[cut$tden %in% dout, "tden"] = NA

#remove outliers for phyto volume
vout <- boxplot(cut$tvol, plot = FALSE)$out
vout #2515774  3159381  1618966  8856599  1687991 11258556  2851773
cut[cut$tvol %in% vout, "tvol"] = NA

plot(cut$tvol,cut$`Chlorophyll a`)
plot(cut$tvol,cut$`Pheophytin a`)
plot(cut$tden,cut$`Chlorophyll a`)
plot(cut$tden,cut$`Pheophytin a`)

#do correlation tests

#plot matrix of all correlations
corrdata2<-cor(cut, method="pearson", use = "complete.obs")
p.mat2 <- cor_pmat(cut)
ggcorrplot(corrdata2, lab=TRUE, p.mat = p.mat2, sig.level = .05)
#significant correlations:
#chlorophyll a and pheophytin a
#phyto volume vs chlorophyll a


    