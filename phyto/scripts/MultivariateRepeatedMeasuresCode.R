#Multivariate Repeated Measurements with adonis():
  
#https://thebiobucket.blogspot.com/2011/04/repeat-measure-adonis-lately-i-had-to.html#more

## Load packages
require(vegan)

### Data:
sp <- matrix(rnorm(3 * 6 * 20, 50, 10), nrow = 3 * 6, ncol = 20,
             dimnames = list(1:18, paste("Sp", 1:20, sep = "")))

time <- as.ordered(rep(1:3, 6))
site <- gl(6, 3)
df <- cbind(site, time, sp)

### add time effect at timepoint 3,
### this will effect will be tested by adonis():
sp_1 <- sp
sp_1[time==3,] <-  sp[time==3,] + rnorm(20, 10, 1)
df2 <- cbind(site, time, sp_1)

### choose which species set to test:
test_sp <- sp_1

### computing the true R2-value

### (btw, using dist() defaults to euclidean distance):
print(fit <- adonis(dist(test_sp) ~ time, permutations=1))

### number of perms
B <- 1999

### setting up frame which will be populated by
### random r2 values:
pop <- rep(NA, B + 1)

### the first entry will be the true r2:
pop[1] <- fit$aov.tab[1, 5]

### set up a "permControl" object:
### we turn off mirroring as time should only flow in one direction
#ctrl <- permControl(strata = site, within = Within(type = "series", mirror = FALSE)) #obsolete code
ctrl <- how(blocks = site, within = Within(type = "series", mirror = FALSE)) #"permControl" is not used at all


### Number of observations:
nobs <- nrow(test_sp)

### check permutation (...rows represent the sample id):
### ..they are ok!
### within in each repeated sample (= sites) timepoints are shuffled,
### with keeping the sequence intact (e.g., for  site 1: 1,2,3 - 2,3,1 - 3,2,1)
shuffle(nobs, control = ctrl)

### loop:
### in adonis(...) you need to put permutations = 1, otherwise 
### adonis will not run
set.seed(123)
for(i in 2:(B+1)){
  idx <- shuffle(nobs, control = ctrl)
  fit.rand <- adonis(dist(test_sp) ~ time[idx], permutations = 1)
  pop[i] <- fit.rand$aov.tab[1, 5]
}

### get the p-value:
print(pval <- sum(pop >= pop[1]) / (B + 1))
### [1] 0.0035

### the sign. p-value supports the H1 (->there is a time effect).
### ..and the fact that samples are not iid is allowed by
### the customized perms - so this p-value is trustworthy as opposed
### to tests not acknowledging dependency of data points..

### test sp set without an effect:
### replace test_sp with sp set without effect:
test_sp <- sp

### now re-run the script and see the result:
### it is insign. - as expected:

### setting up frame which will be populated by
### random r2 values:
pop <- rep(NA, B + 1)

### computing the true R2-value:
print(fit <- adonis(dist(test_sp) ~ time, permutations = 1))

### the first entry will be the true r2:
pop[1] <- fit$aov.tab[1, 5]

### run the loop:
set.seed(123)
for(i in 2:(B+1)){
  idx <- shuffle(nobs, control = ctrl)
  fit.rand <- adonis(dist(test_sp) ~ time[idx], permutations = 1)
  pop[i] <- fit.rand$aov.tab[1, 5]
}
print(pval <- sum(pop >= pop[1]) / (B + 1))
### [1] 0.701

## make a histogram to see random R2-values and the true one:
hist(pop, xlab = "Population R2")
abline(v = pop[1], col = 2, lty = 3)
text(0.08, 300, paste("true R2,\np = ", pval, sep = ""))



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
trects <- data.frame(island = c("DI", "FC", "FI", "LH"),
                     ymin = c(-Inf, NA,NA,-Inf),
                     ymax = c(Inf, NA,NA,Inf),
                     xmin = c('2017-06-06', NA,NA,'2017-06-07'),
                     xmax = c('2017-09-18',NA,NA,'2017-09-19'),
                     xmin2 = c('2018-03-29', NA,NA,'2018-03-19'),
                     xmax2 = c('2018-06-21',NA,NA,'2018-07-02'),
                     xmin3 = c(NA,NA,NA,'2018-09-17'),
                     xmax3 = c(NA,NA,NA,'2018-11-26')
)
#format date columns as dates
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


(pseas <- ggplot(dscore,aes(x=NMDS1,y=NMDS2, shape=season, color=season,fill = season))+
    geom_point()+
    stat_ellipse(geom = "polygon", alpha = 0.3, aes(fill = season))+
    scale_color_manual(values = SeasonColors,name="Season") +  
    scale_fill_manual(values = SeasonColors,name="Season")+  
    scale_shape_manual(values = SeasonPoint,name="Season")+
    theme_sav()+theme(legend.position=c(0.13,0.9))
)

