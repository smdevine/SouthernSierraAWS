library(raster)
mainDir <- 'C:/Users/smdevine/Desktop/SpatialData'
SSURGOdir <- file.path(mainDir, 'soils_data/SSURGOrevised')
OSDdir <- file.path(mainDir, 'soils_data/master_soils_watershed_study')
PastResults <- file.path(mainDir, 'soils_data/SSURGO_summary')
ShpDir <- file.path(mainDir, 'soils_data/SSURGOdata')
#ssurgo_dirs <- list.dirs(SSURGOdir)
ResultsDir <- 'C:/Users/smdevine/Desktop/GITprojects/SouthernSierraAWS/results'
RastersDir <- file.path(mainDir, 'watershed_characteristics')
paralithic_awc <- 0.05
zone1_maxthickness <- 200
zone2_maxthickness <- 500
zone3_maxthickness <- 150

# this is San Joaquin: ssurgo_dirs[20]
list.files(ssurgo_dirs[20])
ssurgo_tables <- list.files(ssurgo_dirs[20], pattern = glob2rx('*.csv'))
ssurgo_tables
ssurgo_horizon <-read.csv(file.path(ssurgo_dirs[20], ssurgo_tables[2]), stringsAsFactors = FALSE, na.strings=c(""," ", 'NA'))
dim(ssurgo_horizon)
ssurgo_horizon$hzthickness <- ssurgo_horizon$hzdepb_r - ssurgo_horizon$hzdept_r
ssurgo_horizon$hzawc_cm <- ssurgo_horizon$hzthickness*ssurgo_horizon$awc_r
ssurgo_horizon$fragvol_temp <- ssurgo_horizon$hzthickness*ssurgo_horizon$fragvol_r_sum
ssurgo_horizon$clay_content <- (ssurgo_horizon$claytotal_r/100)*ssurgo_horizon$dbthirdbar_r*((100-ssurgo_horizon$fragvol_r_sum)/100)*ssurgo_horizon$hzthickness*10
ssurgo_horizon$om_content <- (ssurgo_horizon$om_r/100)*ssurgo_horizon$dbthirdbar_r*((100-ssurgo_horizon$fragvol_r_sum)/100)*ssurgo_horizon$hzthickness*10
ssurgo_horizon$awc_qc <- ssurgo_horizon$hzawc_cm
ssurgo_horizon$awc_qc[ssurgo_horizon$awc_qc > 0] <- 1 #this means that horizons with a reported '0' are not included as part of the depth calc

#some exploration of awc stats in San Joaquin
hist(ssurgo_horizon$awc_r)
summary(ssurgo_horizon$awc_r)
mean(ssurgo_horizon$awc_r[ssurgo_horizon$awc_r > 0], na.rm=TRUE)
summary(as.factor(ssurgo_horizon$hzname[which(is.na(ssurgo_horizon$awc_r))]))
summary(as.factor(ssurgo_horizon$hzname[which(ssurgo_horizon$awc_r==0)])) #49 equal to 0 are Cr
summary(as.factor(ssurgo_horizon$hzname[which(ssurgo_horizon$awc_r > 0)]))
summary(as.factor(ssurgo_horizon$hzname[which(ssurgo_horizon$awc_r > 0.1)]))
summary(as.factor(ssurgo_horizon$hzname[which(ssurgo_horizon$awc_r >= 0.15)]))
sum(ssurgo_horizon$awc_r==0, na.rm = TRUE) #141 equal to 0
sum(ssurgo_horizon$hzname=='Cr')
summary(as.factor(ssurgo_horizon$awc_r[which(ssurgo_horizon$hzname=='Cr')]))
mean(ssurgo_horizon$awc_r[ssurgo_horizon$hzname=='Cr' & ssurgo_horizon$awc_r > 0], na.rm=TRUE)
max(ssurgo_horizon$awc_r[ssurgo_horizon$hzname=='Cr'], na.rm=TRUE)
hist(ssurgo_horizon$awc_r[which(ssurgo_horizon$hzname != 'Cr' & ssurgo_horizon$awc_r > 0)])
mean(ssurgo_horizon$awc_r[ssurgo_horizon$hzname != 'Cr' & ssurgo_horizon$awc_r > 0], na.rm=TRUE)

#some of exploration of soil comp stats in San Joaquin
list.files(list.dirs(PastResults)[20])
comp_results <- read.csv(file.path(list.dirs(PastResults)[20], "uf22_final_comp_aggregation.csv"), stringsAsFactors = FALSE)
majcomps_paralithic <- comp_results[which(comp_results$reskind=='Paralithic bedrock' & comp_results$majcompflag=='Yes'),]
comps_paralithic <- comp_results[which(comp_results$reskind=='Paralithic bedrock'), ]
sum(comps_paralithic$hectares)
sum(majcomps_paralithic$hectares)
unique(majcomps_paralithic$compname)
summary(as.factor(majcomps_paralithic$compname))
summary(as.factor(majcomps_paralithic$hzname_deepest))
hist(majcomps_paralithic$awc_H2Ocm)
hist(majcomps_paralithic$awc_soilthickness_cm)
plot(majcomps_paralithic$awc_soilthickness_cm, majcomps_paralithic$awc_H2Ocm)
summary(as.factor(majcomps_paralithic$awc_soilthickness_cm))

dim(comp_results)
colnames(comp_results)
sum(comp_results$hectares)
area_by_compname <- data.frame(hectares=round(tapply(comp_results$hectares, comp_results$compname, sum), 1))
area_by_compname$compname <- rownames(area_by_compname)
rownames(area_by_compname) <- NULL
area_by_compname$hectares <- as.numeric(area_by_compname$hectares)
area_by_compname <- area_by_compname[order(area_by_compname$hectares, decreasing = TRUE),]
head(area_by_compname, 20)
dim(area_by_compname) #152 unique components
area_by_compname[area_by_compname$hectares==max(area_by_compname$hectares),]
write.csv(area_by_compname, file.path(ResultsDir, 'area_by_compname_raw.csv'), row.names = FALSE)

#fix compname errors
area_by_compname$compname_simplified <- area_by_compname$compname
area_by_compname$compname_simplified <- gsub(' family', '', area_by_compname$compname_simplified)
area_by_compname$compname_simplified <- gsub(' variant', '', area_by_compname$compname_simplified)
area_by_compname$compname_simplified <- gsub(' volcanic', '', area_by_compname$compname_simplified)
area_by_compname$compname_simplified <- gsub(' tephritic', '', area_by_compname$compname_simplified)
area_by_compname$compname_simplified <- gsub(',', '', area_by_compname$compname_simplified)
area_by_compname$compname_simplified[area_by_compname$compname =='Rock outcrop and rubble land' | area_by_compname$compname =='Rock land' | area_by_compname$compname_simplified == 'Rubble land'] <- 'Rock outcrop' 
area_by_compname$compname[grepl('water', area_by_compname$compname)]
area_by_compname$compname[area_by_compname$compname == 'Dystric Cryochreots'] <- 'Dystric Cryochrepts' #misspelled

area_by_compname <- data.frame(hectares=round(tapply(area_by_compname$hectares, area_by_compname$compname_simplified, sum), 1))
area_by_compname$compname <- rownames(area_by_compname)
rownames(area_by_compname) <- NULL
area_by_compname$hectares <- as.numeric(area_by_compname$hectares)
area_by_compname <- area_by_compname[order(area_by_compname$hectares, decreasing = TRUE),]
write.csv(area_by_compname, file.path(ResultsDir, 'area_by_compname_edited.csv'), row.names = FALSE)


#map unit level SSURGO aggregatiion from SSURGO_analysis_v2.R
mu_results_SSURGO <- read.csv(file.path(list.dirs(results)[20], "uf22_mu_summary.csv"), stringsAsFactors = FALSE) #this is the component based summary from SSURGO only
mu_results_STATSGO <- read.csv(file.path(list.dirs(results)[20], "uf22_mu_summary_SSURGO_STATSGO.csv"), stringsAsFactors = FALSE) #this the component based summary after replacing 'NOTCOM' components with STATSGO2 data
colnames(mu_results_STATSGO)
sum(mu_results_SSURGO$hectares) #this matches raster estimated watershed acreage in RasterCalcs.R
sum(mu_results_STATSGO$hectares[mu_results_STATSGO$data_source=='SSURGO']) #417695.3 in SSURGO, only one STATSGO map unit
mu_results_STATSGO$hectares[mu_results_STATSGO$data_source=='STATSGO2'] <- mu_results_SSURGO$hectares[mu_results_SSURGO$compname_dom=='NOTCOM'] #fixes the STATSGO mapunit acreage to be specific to San Joaquin watershed
sum(mu_results_STATSGO$hectares) #now is correct
hist(mu_results_STATSGO$awc_H2Ocm_wtdavg)
sum(is.na(mu_results_STATSGO$awc_H2Ocm_wtdavg)) #7 components are compleltely NA for AWC but this is expected for these components
mu_results_STATSGO$component_summary[is.na(mu_results_STATSGO$awc_H2Ocm_wtdavg)]
sum(mu_results_STATSGO$hectares[is.na(mu_results_STATSGO$awc_H2Ocm_wtdavg)]) #14325.12 hectares
sum(mu_results_STATSGO$hectares*mu_results_STATSGO$awc_ppct_tot/100) #403,662 hectares have estimates for SSURGO AWC
sum(mu_results_STATSGO$hectares*mu_results_STATSGO$NAhorizon_pct/100) #94,086 hectares within watershed have an unnamed deep horizon
sum(mu_results_STATSGO$hectares*mu_results_STATSGO$lithic_pct/100) #165,821 ha have lithic termination
sum(mu_results_STATSGO$hectares*mu_results_STATSGO$paralithic_pct/100) # 170,455.7 ha have paralithic termination
sum(mu_results_STATSGO$hectares*mu_results_STATSGO$Ahorizon_pct/100) #0 ha have A horizon termination
sum(mu_results_STATSGO$hectares*mu_results_STATSGO$Bhorizon_pct/100) #188 ha have B horizon termination
sum(mu_results_STATSGO$hectares*mu_results_STATSGO$Chorizon_pct/100) #3654 ha C horizon termination


dim(comp_results)
summary(comp_results$NAhorizon_ppct_tot) #this is the percentage of a map unit that had a NA deepest horizon even after attempting to fix SSURGO (see 'SSURGO_analysis_v2.R' for methodological details)
summary(comp_results$paralithic_ppct_tot)

#create component level estimates of plant available water based on reported thickness of soil with AWC > 0 and add additional water to 2 m for zone 1, to 5 m for zone 2, and to 1.5 m for zone 3
#start with san joaquin mu aggregated data (STATSGO and SSURGO) to be able to pull in relevant STATSGO components into the SSURGO component data
mu_results_all <- read.csv(file.path(PastResults, 'uf22', "uf22_mu_summary_SSURGO_STATSGO.csv"), stringsAsFactors = FALSE) #this the component based summary after replacing 'NOTCOM' components with STATSGO2 data
length(unique(mu_results_all$mukey))
dim(mu_results_all)
mukeys_statsgo_san.joaquin <- mu_results_all$mukey[mu_results_all$data_source=='STATSGO2']

#san joaquin mapunits
uf22_shp <- shapefile(file.path(ShpDir, 'uf22', 'uf22_mapunits.shp'))
sum(uf22_shp$hectares) #correct hectares, but this shapefile does not actually have the STATSGO2 mapunits

#san joaquin mukey raster
mukey_raster_san.joaquin <- raster(file.path(RastersDir, 'san.joaquin', 'soil_mukey.tif'))
cellcount_mukey_presence <- cellStats(mukey_raster_san.joaquin==mukeys_statsgo_san.joaquin, stat = 'sum')
hectares_mu_san.joaquin <- cellcount_mukey_presence*900/10000
mukeys_raster <- unique(mukey_raster_san.joaquin)
length(mukeys_raster) #300 unique mukeys in raster
mukeys_raster

#STATSGO components
statsgo_comps <- read.csv(file.path(PastResults, 'statsgo2', 'statsgo2_final_comp_aggregation.csv'), stringsAsFactors = FALSE)
statsgo_comps_san.joaquin <- statsgo_comps[statsgo_comps$mukey==mukeys_statsgo_san.joaquin,] #only one STATSGO mapunit needed for San Joaquin
statsgo_comps_san.joaquin <- statsgo_comps_san.joaquin[ ,c('mukey', 'cokey', 'compname', 'majcompflag', 'hectares', 'comppct_r', 'awc_H2Ocm', 'awc_soilthickness_cm', 'hzname_deepest_simplified', 'reskind', 'resdept_r')]
statsgo_comps_san.joaquin$data.source <- 'STATSGO2'
statsgo_comps_san.joaquin$hectares <- statsgo_comps_san.joaquin$comppct_r/100 * hectares_mu_san.joaquin

#SSURGO components
ssurgo_comps_san.joaquin <- read.csv(file.path(PastResults, 'uf22', "uf22_final_comp_aggregation.csv"), stringsAsFactors = FALSE)
ssurgo_comps_san.joaquin <- ssurgo_comps_san.joaquin[ ,c('mukey', 'cokey', 'compname', 'majcompflag', 'hectares', 'comppct_r', 'awc_H2Ocm', 'awc_soilthickness_cm', 'hzname_deepest_simplified', 'reskind', 'resdept_r')]
ssurgo_comps_san.joaquin$data.source <- 'SSURGO'
ssurgo_comps_san.joaquin <- ssurgo_comps_san.joaquin[-which(ssurgo_comps_san.joaquin$compname=='NOTCOM'),]
comp_awc_revised <- rbind(ssurgo_comps_san.joaquin, statsgo_comps_san.joaquin)
comp_awc_revised$compname_lithic <- grepl('Lithic', comp_awc_revised$compname) #53 instances
length(unique(comp_awc_revised$mukey)) #303 mukeys
sum(uf22_shp$hectares) # 434589.9
sum(comp_awc_revised$hectares) #lost several hundred hectares from rasters 
sum(comp_awc_revised$hectares[is.na(comp_awc_revised$awc_H2Ocm)]) #47,437.75 hectares affected by NA for awc (303 components)
summary(as.factor(comp_awc_revised$majcompflag[is.na(comp_awc_revised$awc_H2Ocm)])) #295 are minor components, 8 major
unique(comp_awc_revised$reskind) #add "Undefined" which came from STATSGO2
summary(as.factor(comp_awc_revised$reskind[is.na(comp_awc_revised$hzname_deepest_simplified)]))
summary(as.factor(comp_awc_revised$hzname_deepest_simplified[is.na(comp_awc_revised$reskind)]))
summary(as.factor(comp_awc_revised$awc_H2Ocm[is.na(comp_awc_revised$reskind)])) #NA reskinds are all NA for AWC also
summary(as.factor(comp_awc_revised$awc_H2Ocm[is.na(comp_awc_revised$hzname_deepest_simplified)])) #whereas this is a mix
summary(as.factor(comp_awc_revised$hzname_deepest_simplified))
sum(comp_awc_revised$hectares[is.na(comp_awc_revised$hzname_deepest_simplified)])
sum(comp_awc_revised$compname_lithic==TRUE & comp_awc_revised$reskind != 'Lithic bedrock', na.rm=TRUE) #19 instances
comp_awc_revised$zone1_awc <- ifelse(is.na(comp_awc_revised$awc_H2Ocm), NA, ifelse(comp_awc_revised$reskind=='Lithic bedrock' | comp_awc_revised$reskind == 'Duripan' | comp_awc_revised$reskind == 'Densic material' | comp_awc_revised$compname_lithic==TRUE | comp_awc_revised$reskind=='Undefined', comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$reskind == 'Paralithic bedrock', ifelse(comp_awc_revised$awc_soilthickness_cm > zone1_maxthickness, comp_awc_revised$awc_H2Ocm, comp_awc_revised$awc_H2Ocm + paralithic_awc*(zone1_maxthickness - comp_awc_revised$awc_soilthickness_cm)), ifelse(comp_awc_revised$reskind == 'None' | comp_awc_revised$reskind == 'Abrupt textural change', ifelse(comp_awc_revised$awc_soilthickness_cm > zone1_maxthickness, comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$awc_H2Ocm==0, 0, comp_awc_revised$awc_H2Ocm + (comp_awc_revised$awc_H2Ocm/comp_awc_revised$awc_soilthickness_cm)*(zone1_maxthickness - comp_awc_revised$awc_soilthickness_cm))), ifelse(is.na(comp_awc_revised$reskind), comp_awc_revised$awc_H2Ocm, -9999)))))

#now do zone 2
comp_awc_revised$zone2_awc <- ifelse(is.na(comp_awc_revised$awc_H2Ocm), NA, ifelse(comp_awc_revised$reskind=='Lithic bedrock' | comp_awc_revised$reskind == 'Duripan' | comp_awc_revised$reskind == 'Densic material' | comp_awc_revised$compname_lithic==TRUE | comp_awc_revised$reskind=='Undefined', comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$reskind == 'Paralithic bedrock', ifelse(comp_awc_revised$awc_soilthickness_cm > zone2_maxthickness, comp_awc_revised$awc_H2Ocm, comp_awc_revised$awc_H2Ocm + paralithic_awc*(zone2_maxthickness - comp_awc_revised$awc_soilthickness_cm)), ifelse(comp_awc_revised$reskind == 'None' | comp_awc_revised$reskind == 'Abrupt textural change', ifelse(comp_awc_revised$awc_soilthickness_cm > zone2_maxthickness, comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$awc_H2Ocm==0, 0, comp_awc_revised$awc_H2Ocm + (comp_awc_revised$awc_H2Ocm/comp_awc_revised$awc_soilthickness_cm)*(zone2_maxthickness - comp_awc_revised$awc_soilthickness_cm))), ifelse(is.na(comp_awc_revised$reskind), comp_awc_revised$awc_H2Ocm, -9999)))))

#now do zone 3
comp_awc_revised$zone3_awc <- ifelse(is.na(comp_awc_revised$awc_H2Ocm), NA, ifelse(comp_awc_revised$reskind=='Lithic bedrock' | comp_awc_revised$reskind == 'Duripan' | comp_awc_revised$reskind == 'Densic material' | comp_awc_revised$compname_lithic==TRUE | comp_awc_revised$reskind=='Undefined', comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$reskind == 'Paralithic bedrock', ifelse(comp_awc_revised$awc_soilthickness_cm > zone3_maxthickness, comp_awc_revised$awc_H2Ocm, comp_awc_revised$awc_H2Ocm + paralithic_awc*(zone3_maxthickness - comp_awc_revised$awc_soilthickness_cm)), ifelse(comp_awc_revised$reskind == 'None' | comp_awc_revised$reskind == 'Abrupt textural change', ifelse(comp_awc_revised$awc_soilthickness_cm > zone3_maxthickness, comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$awc_H2Ocm==0, 0, comp_awc_revised$awc_H2Ocm + (comp_awc_revised$awc_H2Ocm/comp_awc_revised$awc_soilthickness_cm)*(zone3_maxthickness - comp_awc_revised$awc_soilthickness_cm))), ifelse(is.na(comp_awc_revised$reskind), comp_awc_revised$awc_H2Ocm, -9999)))))

comp_awc_revised[which(comp_awc_revised$zone1_awc=='NaN'),]
comp_awc_revised[which(comp_awc_revised$zone1_awc==-9999),]
comp_awc_revised[which(comp_awc_revised$zone2_awc=='NaN'),]
comp_awc_revised[which(comp_awc_revised$zone2_awc==-9999),]
comp_awc_revised$zone1_awc_diffs <- comp_awc_revised$zone1_awc - comp_awc_revised$awc_H2Ocm 
comp_awc_revised$zone1_awc_pct_gain <- ifelse(comp_awc_revised$awc_H2Ocm == 0 | is.na(comp_awc_revised$awc_H2Ocm), 0, 100 * (comp_awc_revised$zone1_awc - comp_awc_revised$awc_H2Ocm) / comp_awc_revised$awc_H2Ocm)
comp_awc_revised$zone2_awc_diffs <- comp_awc_revised$zone2_awc - comp_awc_revised$awc_H2Ocm 
comp_awc_revised$zone2_awc_pct_gain <- ifelse(comp_awc_revised$awc_H2Ocm == 0 | is.na(comp_awc_revised$awc_H2Ocm), 0, 100 * (comp_awc_revised$zone2_awc - comp_awc_revised$awc_H2Ocm) / comp_awc_revised$awc_H2Ocm)
comp_awc_revised$zone3_awc_diffs <- comp_awc_revised$zone3_awc - comp_awc_revised$awc_H2Ocm
comp_awc_revised$zone3_awc_pct_gain <- ifelse(comp_awc_revised$awc_H2Ocm == 0 | is.na(comp_awc_revised$awc_H2Ocm), 0, 100 * (comp_awc_revised$zone3_awc - comp_awc_revised$awc_H2Ocm) / comp_awc_revised$awc_H2Ocm)
hist(comp_awc_revised$zone1_awc)
hist(comp_awc_revised$zone1_awc_diffs)
hist(comp_awc_revised$zone1_awc_pct_gain)
hist(comp_awc_revised$zone2_awc)
hist(comp_awc_revised$zone2_awc_diffs)
hist(comp_awc_revised$zone2_awc_pct_gain)
hist(comp_awc_revised$zone3_awc_diffs)
hist(comp_awc_revised$zone3_awc_pct_gain)
comp_awc_revised[which.max(comp_awc_revised$zone1_awc_pct_gain),]
sum(comp_awc_revised$hectares[comp_awc_revised$majcompflag=='Yes'])/sum(comp_awc_revised$hectares) #86% of watershed
comp_awc_revised_majcomps <- comp_awc_revised[comp_awc_revised$majcompflag=='Yes',] #all STATSGO comps for San Joaquin are major
dim(comp_awc_revised_majcomps)
hist(comp_awc_revised_majcomps$zone1_awc_pct_gain)
hist(comp_awc_revised_majcomps$zone2_awc_pct_gain)
hist(comp_awc_revised_majcomps$zone3_awc_pct_gain)
comp_awc_revised_majcomps[which.max(comp_awc_revised_majcomps$zone1_awc_pct_gain),]
comp_awc_revised_majcomps[which(comp_awc_revised_majcomps$zone1_awc_pct_gain > 100),]
sum(comp_awc_revised_majcomps$compname=='Rock outcrop') #110 major component rock outcrop
comp_awc_revised_majcomps[which(comp_awc_revised_majcomps$compname=='Rock outcrop'),]
sum(comp_awc_revised_majcomps$hectares[which(comp_awc_revised_majcomps$compname=='Rock outcrop')]) #91,190 rock outcrop hectares as major components
sum(comp_awc_revised$hectares[which(comp_awc_revised$compname=='Rock outcrop')]) #97,107 total hectares as rock outcrop, so only 6,000 minor component hectares as 'rock outcrop'. conclusion is to reaggregate ssurgo based on major components only
length(unique(comp_awc_revised_majcomps$mukey)) #302 mukeys here

#get only the majcomps with AWC info
sum(is.na(comp_awc_revised_majcomps$awc_H2Ocm)) #7 are NA
comp_awc_revised_majcomps2 <- comp_awc_revised_majcomps[!is.na(comp_awc_revised_majcomps$awc_H2Ocm),]
dim(comp_awc_revised_majcomps2)
length(unique(comp_awc_revised_majcomps2$mukey)) #295 mukeys here

#now aggregate back to mapunit level using only majcomps for rasterization
compsums <- data.frame(compsums=tapply(comp_awc_revised_majcomps2$comppct_r, comp_awc_revised_majcomps2$mukey, sum))
compsums$compsums <- as.numeric(compsums$compsums)
compsums$mukey <- rownames(compsums)
comp_awc_revised_majcomps2$compsums <- compsums$compsums[match(comp_awc_revised_majcomps2$mukey, compsums$mukey)]
mu_agg_majcomps <- data.frame(zone1_awc=tapply(comp_awc_revised_majcomps2$zone1_awc * (comp_awc_revised_majcomps2$comppct_r / comp_awc_revised_majcomps2$compsums), comp_awc_revised_majcomps2$mukey, sum))
mu_agg_majcomps$zone1_awc <- as.numeric(mu_agg_majcomps$zone1_awc)
mu_agg_majcomps$mukey <- rownames(mu_agg_majcomps)
mu_agg_majcomps$awc_ssurgo <- as.numeric(tapply(comp_awc_revised_majcomps2$awc_H2Ocm * (comp_awc_revised_majcomps2$comppct_r / comp_awc_revised_majcomps2$compsums), comp_awc_revised_majcomps2$mukey, sum))
mu_agg_majcomps$zone2_awc <- as.numeric(tapply(comp_awc_revised_majcomps2$zone2_awc * (comp_awc_revised_majcomps2$comppct_r / comp_awc_revised_majcomps2$compsums), comp_awc_revised_majcomps2$mukey, sum))
mu_agg_majcomps$zone3_awc <- as.numeric(tapply(comp_awc_revised_majcomps2$zone3_awc * (comp_awc_revised_majcomps2$comppct_r / comp_awc_revised_majcomps2$compsums), comp_awc_revised_majcomps2$mukey, sum))
hist(mu_agg_majcomps$zone1_awc)
hist(mu_agg_majcomps$zone2_awc)
hist(mu_agg_majcomps$zone3_awc)
hist(mu_agg_majcomps$awc_ssurgo)
dim(mu_agg_majcomps)
sum(!is.na(mu_agg_majcomps$awc_ssurgo))

#now, merge back with mu_results_all
sum(!is.na(mu_results_all$awc_H2Ocm_wtdavg)) #for some reason, got 2 more with mu_agg_majcomps
mu_results_all[is.na(mu_results_all$awc_H2Ocm_wtdavg),]
mu_results_all$awc_ssurgo_majcomps <- as.numeric(NA)
mu_results_all$awc_ssurgo_majcomps <- mu_agg_majcomps$awc_ssurgo[match(mu_results_all$mukey, mu_agg_majcomps$mukey)] #returns 8 NAs, missing one more than before probably do to majcomps error [yes, confirmed that mukey 463521 is 100% Terrace Escarpments but is not listed as a major component] so will fix manually
mu_results_all$awc_zone1 <- mu_agg_majcomps$zone1_awc[match(mu_results_all$mukey, mu_agg_majcomps$mukey)]
mu_results_all$awc_zone2 <- mu_agg_majcomps$zone2_awc[match(mu_results_all$mukey, mu_agg_majcomps$mukey)]
mu_results_all$awc_zone3 <- mu_agg_majcomps$zone3_awc[match(mu_results_all$mukey, mu_agg_majcomps$mukey)]
mu_results_all$awc_zone1[mu_results_all$mukey==463521] <- 0
mu_results_all$awc_zone2[mu_results_all$mukey==463521] <- 0
mu_results_all$awc_zone3[mu_results_all$mukey==463521] <- 0
mu_results_all$awc_ssurgo_majcomps[mu_results_all$mukey==463521] <- 0
plot(mu_results_all$aws0150wta, mu_results_all$awc_ssurgo_majcomps)
plot(mu_results_all$aws0150wta, mu_results_all$awc_zone1)
plot(mu_results_all$aws0150wta, mu_results_all$awc_zone2)
plot(mu_results_all$aws0150wta, mu_results_all$awc_zone3)

#correct STATSGO mapunit acreage
mu_results_all$hectares[mu_results_all$data_source=='STATSGO2'] <- hectares_mu_san.joaquin
write.csv(mu_results_all, file.path(ResultsDir, 'mu_aggregated_san.joaquin.3.22.18.csv'), row.names = FALSE)

#repeat process with kings watershed
#start with kings mu aggregated data (STATSGO and SSURGO) to be able to pull in relevant STATSGO components into the SSURGO component data
mu_results_all <- read.csv(file.path(PastResults, 'uf_kings', "uf_kings_mu_summary_SSURGO_STATSGO.csv"), stringsAsFactors = FALSE) #this the component based summary after replacing 'NOTCOM' components with STATSGO2 data
length(unique(mu_results_all$mukey)) #277 mukeys
dim(mu_results_all)
mukeys_statsgo_kings <- mu_results_all$mukey[mu_results_all$data_source=='STATSGO2']

#kings mapunits
kings_shp <- shapefile(file.path(ShpDir, 'uf_kings', 'uf_kings_mapunits.shp'))
sum(kings_shp$hectares) #400014.4 hectares, but this shapefile does not actually have the STATSGO2 mapunits

#san joaquin mukey raster
mukey_raster_kings <- raster(file.path(RastersDir, 'kings', 'soil_mukey.tif'))
cellcount_mukey_presence <- lapply(mukeys_statsgo_kings, function(x) {cellStats(mukey_raster_kings==x, stat='sum')})
statsgo_mu_area <- data.frame(mukey=mukeys_statsgo_kings, cellcount=unlist(cellcount_mukey_presence))
statsgo_mu_area$hectares <- statsgo_mu_area$cellcount * 900 / 10000 #164,078.7 statsgo hectares
mukeys_raster <- unique(mukey_raster_kings)
length(mukeys_raster) #277 unique mukeys in raster
mukeys_raster

#STATSGO components
statsgo_comps <- read.csv(file.path(PastResults, 'statsgo2', 'statsgo2_final_comp_aggregation.csv'), stringsAsFactors = FALSE)

statsgo_comps_kings <- statsgo_comps[unlist(lapply(statsgo_mu_area$mukey, function(x) {which(statsgo_comps$mukey==x)})),] #only one STATSGO mapunit needed for San Joaquin, so different strategy for kings
statsgo_comps_kings <- statsgo_comps_kings[ ,c('mukey', 'cokey', 'compname', 'majcompflag', 'hectares', 'comppct_r', 'awc_H2Ocm', 'awc_soilthickness_cm', 'hzname_deepest_simplified', 'reskind', 'resdept_r')]
statsgo_comps_kings$data.source <- 'STATSGO2'
statsgo_comps_kings$hectares <- statsgo_comps_kings$comppct_r/100 * (statsgo_mu_area$hectares[match(statsgo_comps_kings$mukey, statsgo_mu_area$mukey)]) #164078.7 ha matches

#SSURGO components
ssurgo_comps_kings <- read.csv(file.path(PastResults, 'uf_kings', "uf_kings_final_comp_aggregation.csv"), stringsAsFactors = FALSE)
ssurgo_comps_kings <- ssurgo_comps_kings[ ,c('mukey', 'cokey', 'compname', 'majcompflag', 'hectares', 'comppct_r', 'awc_H2Ocm', 'awc_soilthickness_cm', 'hzname_deepest_simplified', 'reskind', 'resdept_r')]
ssurgo_comps_kings$data.source <- 'SSURGO'
ssurgo_comps_kings <- ssurgo_comps_kings[-which(ssurgo_comps_kings$compname=='NOTCOM'),] #only one instance in Kings, area is 164,075 ha
comp_awc_revised <- rbind(ssurgo_comps_kings, statsgo_comps_kings)
comp_awc_revised$compname_lithic <- grepl('Lithic', comp_awc_revised$compname) #53 instances
length(unique(comp_awc_revised$mukey)) #278 mukeys
sum(kings_shp$hectares) #400014.4 ha
sum(comp_awc_revised$hectares) #400008.1 ha
sum(comp_awc_revised$hectares[is.na(comp_awc_revised$awc_H2Ocm)]) #11,301 hectares affected by NA for awc (247 components)
summary(as.factor(comp_awc_revised$majcompflag[is.na(comp_awc_revised$awc_H2Ocm)])) #243 are minor components, 4 major
unique(comp_awc_revised$reskind) #add "Undefined" which came from STATSGO2
summary(as.factor(comp_awc_revised$reskind[is.na(comp_awc_revised$hzname_deepest_simplified)]))
summary(as.factor(comp_awc_revised$hzname_deepest_simplified[is.na(comp_awc_revised$reskind)]))
summary(as.factor(comp_awc_revised$awc_H2Ocm[is.na(comp_awc_revised$reskind)])) #NA reskinds are all NA for AWC also
summary(as.factor(comp_awc_revised$hzname_deepest_simplified))
sum(comp_awc_revised$hectares[is.na(comp_awc_revised$hzname_deepest_simplified)])
sum(comp_awc_revised$compname_lithic==TRUE & comp_awc_revised$reskind != 'Lithic bedrock', na.rm=TRUE) #14 instances
comp_awc_revised$zone1_awc <- ifelse(is.na(comp_awc_revised$awc_H2Ocm), NA, ifelse(comp_awc_revised$reskind=='Lithic bedrock' | comp_awc_revised$reskind == 'Duripan' | comp_awc_revised$reskind == 'Densic material' | comp_awc_revised$compname_lithic==TRUE | comp_awc_revised$reskind=='Undefined', comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$reskind == 'Paralithic bedrock', ifelse(comp_awc_revised$awc_soilthickness_cm > zone1_maxthickness, comp_awc_revised$awc_H2Ocm, comp_awc_revised$awc_H2Ocm + paralithic_awc*(zone1_maxthickness - comp_awc_revised$awc_soilthickness_cm)), ifelse(comp_awc_revised$reskind == 'None' | comp_awc_revised$reskind == 'Abrupt textural change', ifelse(comp_awc_revised$awc_soilthickness_cm > zone1_maxthickness, comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$awc_H2Ocm==0, 0, comp_awc_revised$awc_H2Ocm + (comp_awc_revised$awc_H2Ocm/comp_awc_revised$awc_soilthickness_cm)*(zone1_maxthickness - comp_awc_revised$awc_soilthickness_cm))), ifelse(is.na(comp_awc_revised$reskind), comp_awc_revised$awc_H2Ocm, -9999)))))

#now do zone 2
comp_awc_revised$zone2_awc <- ifelse(is.na(comp_awc_revised$awc_H2Ocm), NA, ifelse(comp_awc_revised$reskind=='Lithic bedrock' | comp_awc_revised$reskind == 'Duripan' | comp_awc_revised$reskind == 'Densic material' | comp_awc_revised$compname_lithic==TRUE | comp_awc_revised$reskind=='Undefined', comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$reskind == 'Paralithic bedrock', ifelse(comp_awc_revised$awc_soilthickness_cm > zone2_maxthickness, comp_awc_revised$awc_H2Ocm, comp_awc_revised$awc_H2Ocm + paralithic_awc*(zone2_maxthickness - comp_awc_revised$awc_soilthickness_cm)), ifelse(comp_awc_revised$reskind == 'None' | comp_awc_revised$reskind == 'Abrupt textural change', ifelse(comp_awc_revised$awc_soilthickness_cm > zone2_maxthickness, comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$awc_H2Ocm==0, 0, comp_awc_revised$awc_H2Ocm + (comp_awc_revised$awc_H2Ocm/comp_awc_revised$awc_soilthickness_cm)*(zone2_maxthickness - comp_awc_revised$awc_soilthickness_cm))), ifelse(is.na(comp_awc_revised$reskind), comp_awc_revised$awc_H2Ocm, -9999)))))

#now do zone 3
comp_awc_revised$zone3_awc <- ifelse(is.na(comp_awc_revised$awc_H2Ocm), NA, ifelse(comp_awc_revised$reskind=='Lithic bedrock' | comp_awc_revised$reskind == 'Duripan' | comp_awc_revised$reskind == 'Densic material' | comp_awc_revised$compname_lithic==TRUE | comp_awc_revised$reskind=='Undefined', comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$reskind == 'Paralithic bedrock', ifelse(comp_awc_revised$awc_soilthickness_cm > zone3_maxthickness, comp_awc_revised$awc_H2Ocm, comp_awc_revised$awc_H2Ocm + paralithic_awc*(zone3_maxthickness - comp_awc_revised$awc_soilthickness_cm)), ifelse(comp_awc_revised$reskind == 'None' | comp_awc_revised$reskind == 'Abrupt textural change', ifelse(comp_awc_revised$awc_soilthickness_cm > zone3_maxthickness, comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$awc_H2Ocm==0, 0, comp_awc_revised$awc_H2Ocm + (comp_awc_revised$awc_H2Ocm/comp_awc_revised$awc_soilthickness_cm)*(zone3_maxthickness - comp_awc_revised$awc_soilthickness_cm))), ifelse(is.na(comp_awc_revised$reskind), comp_awc_revised$awc_H2Ocm, -9999)))))

comp_awc_revised[which(comp_awc_revised$zone1_awc=='NaN'),]
comp_awc_revised[which(comp_awc_revised$zone1_awc==-9999),]
comp_awc_revised[which(comp_awc_revised$zone2_awc=='NaN'),]
comp_awc_revised[which(comp_awc_revised$zone2_awc==-9999),]
comp_awc_revised[which(comp_awc_revised$zone3_awc=='NaN'),]
comp_awc_revised[which(comp_awc_revised$zone3_awc==-9999),]
comp_awc_revised$zone1_awc_diffs <- comp_awc_revised$zone1_awc - comp_awc_revised$awc_H2Ocm 
comp_awc_revised$zone1_awc_pct_gain <- ifelse(comp_awc_revised$awc_H2Ocm == 0 | is.na(comp_awc_revised$awc_H2Ocm), 0, 100 * (comp_awc_revised$zone1_awc - comp_awc_revised$awc_H2Ocm) / comp_awc_revised$awc_H2Ocm)
comp_awc_revised$zone2_awc_diffs <- comp_awc_revised$zone2_awc - comp_awc_revised$awc_H2Ocm 
comp_awc_revised$zone2_awc_pct_gain <- ifelse(comp_awc_revised$awc_H2Ocm == 0 | is.na(comp_awc_revised$awc_H2Ocm), 0, 100 * (comp_awc_revised$zone2_awc - comp_awc_revised$awc_H2Ocm) / comp_awc_revised$awc_H2Ocm)
comp_awc_revised$zone3_awc_diffs <- comp_awc_revised$zone3_awc - comp_awc_revised$awc_H2Ocm
comp_awc_revised$zone3_awc_pct_gain <- ifelse(comp_awc_revised$awc_H2Ocm == 0 | is.na(comp_awc_revised$awc_H2Ocm), 0, 100 * (comp_awc_revised$zone3_awc - comp_awc_revised$awc_H2Ocm) / comp_awc_revised$awc_H2Ocm)
hist(comp_awc_revised$zone1_awc)
hist(comp_awc_revised$zone1_awc_diffs)
hist(comp_awc_revised$zone1_awc_pct_gain)
hist(comp_awc_revised$zone2_awc)
hist(comp_awc_revised$zone2_awc_diffs)
hist(comp_awc_revised$zone2_awc_pct_gain)
hist(comp_awc_revised$zone3_awc_diffs)
hist(comp_awc_revised$zone3_awc_pct_gain)
comp_awc_revised[which.max(comp_awc_revised$zone1_awc_pct_gain),]
sum(comp_awc_revised$hectares[comp_awc_revised$majcompflag=='Yes'])/sum(comp_awc_revised$hectares) #83.5% of watershed
comp_awc_revised_majcomps <- comp_awc_revised[comp_awc_revised$majcompflag=='Yes' | comp_awc_revised$data.source=='STATSGO2', ] #all STATSGO comps for San Joaquin are major; not for Kings; 130,095 ha out of 164,078.7 are major components
dim(comp_awc_revised_majcomps)
hist(comp_awc_revised_majcomps$zone1_awc_pct_gain)
hist(comp_awc_revised_majcomps$zone2_awc_pct_gain)
hist(comp_awc_revised_majcomps$zone3_awc_pct_gain)
comp_awc_revised_majcomps[which.max(comp_awc_revised_majcomps$zone1_awc_pct_gain),] #get rid of this one; data seems bad; there are ??? instances
comp_awc_revised_majcomps <- comp_awc_revised_majcomps[-which(comp_awc_revised_majcomps$compname=='Xerorthents'),]
comp_awc_revised_majcomps[which(comp_awc_revised_majcomps$zone1_awc_pct_gain > 100),]
sum(comp_awc_revised_majcomps$compname=='Rock outcrop') #122 major component or STATSGO2 rock outcrop
comp_awc_revised_majcomps[which(comp_awc_revised_majcomps$compname=='Rock outcrop'),]
sum(comp_awc_revised_majcomps$hectares[which(comp_awc_revised_majcomps$compname=='Rock outcrop')]) #157,070.6 rock outcrop hectares as major components or STATSGO2 components
sum(comp_awc_revised$hectares[which(comp_awc_revised$compname=='Rock outcrop')]) #161,135.6 total hectares as rock outcrop
sum(statsgo_comps_kings$hectares[statsgo_comps_kings$majcompflag=='No' & statsgo_comps_kings$compname=='Rock outcrop']) #5748.6 minor component STATSGO2 rock outcrop
length(unique(comp_awc_revised_majcomps$mukey)) #278 mukeys here

#get only the majcomps with AWC info
sum(is.na(comp_awc_revised_majcomps$awc_H2Ocm)) #4 are NA
comp_awc_revised_majcomps2 <- comp_awc_revised_majcomps[!is.na(comp_awc_revised_majcomps$awc_H2Ocm),]
dim(comp_awc_revised_majcomps2)
length(unique(comp_awc_revised_majcomps2$mukey)) #274 mukeys here

#now aggregate back to mapunit level using only majcomps for rasterization
compsums <- data.frame(compsums=tapply(comp_awc_revised_majcomps2$comppct_r, comp_awc_revised_majcomps2$mukey, sum))
compsums$compsums <- as.numeric(compsums$compsums)
compsums$mukey <- rownames(compsums)
comp_awc_revised_majcomps2$compsums <- compsums$compsums[match(comp_awc_revised_majcomps2$mukey, compsums$mukey)]
mu_agg_majcomps <- data.frame(zone1_awc=tapply(comp_awc_revised_majcomps2$zone1_awc * (comp_awc_revised_majcomps2$comppct_r / comp_awc_revised_majcomps2$compsums), comp_awc_revised_majcomps2$mukey, sum))
mu_agg_majcomps$zone1_awc <- as.numeric(mu_agg_majcomps$zone1_awc)
mu_agg_majcomps$mukey <- rownames(mu_agg_majcomps)
mu_agg_majcomps$awc_ssurgo <- as.numeric(tapply(comp_awc_revised_majcomps2$awc_H2Ocm * (comp_awc_revised_majcomps2$comppct_r / comp_awc_revised_majcomps2$compsums), comp_awc_revised_majcomps2$mukey, sum))
mu_agg_majcomps$zone2_awc <- as.numeric(tapply(comp_awc_revised_majcomps2$zone2_awc * (comp_awc_revised_majcomps2$comppct_r / comp_awc_revised_majcomps2$compsums), comp_awc_revised_majcomps2$mukey, sum))
mu_agg_majcomps$zone3_awc <- as.numeric(tapply(comp_awc_revised_majcomps2$zone3_awc * (comp_awc_revised_majcomps2$comppct_r / comp_awc_revised_majcomps2$compsums), comp_awc_revised_majcomps2$mukey, sum))
hist(mu_agg_majcomps$zone1_awc)
hist(mu_agg_majcomps$zone2_awc)
hist(mu_agg_majcomps$zone3_awc)
hist(mu_agg_majcomps$awc_ssurgo)
dim(mu_agg_majcomps)
sum(!is.na(mu_agg_majcomps$awc_ssurgo))

#now, merge back with mu_results_all
sum(!is.na(mu_results_all$awc_H2Ocm_wtdavg)) #for some reason, got 1 more with mu_agg_majcomps
sum(is.na(mu_results_all$awc_H2Ocm_wtdavg))
mu_results_all[is.na(mu_results_all$awc_H2Ocm_wtdavg),]
mu_results_all$awc_ssurgo_majcomps <- as.numeric(NA)
mu_results_all$awc_ssurgo_majcomps <- mu_agg_majcomps$awc_ssurgo[match(mu_results_all$mukey, mu_agg_majcomps$mukey)] #returns 8 NAs, missing one more than before probably do to majcomps error [yes, confirmed that mukey 463521 is 100% Terrace Escarpments but is not listed as a major component] so will fix manually
mu_results_all$awc_zone1 <- mu_agg_majcomps$zone1_awc[match(mu_results_all$mukey, mu_agg_majcomps$mukey)]
mu_results_all$awc_zone2 <- mu_agg_majcomps$zone2_awc[match(mu_results_all$mukey, mu_agg_majcomps$mukey)]
mu_results_all$awc_zone3 <- mu_agg_majcomps$zone3_awc[match(mu_results_all$mukey, mu_agg_majcomps$mukey)]
plot(mu_results_all$aws0150wta, mu_results_all$awc_ssurgo_majcomps)
plot(mu_results_all$aws0150wta, mu_results_all$awc_zone1)
plot(mu_results_all$aws0150wta, mu_results_all$awc_zone2)
plot(mu_results_all$aws0150wta, mu_results_all$awc_zone3)

#correct STATSGO mapunit acreage
mu_results_all <- mu_results_all[order(mu_results_all$data_source, mu_results_all$mukey), ]
statsgo_mu_area <- statsgo_mu_area[order(statsgo_mu_area$mukey),]
mu_results_all$hectares[mu_results_all$data_source=='STATSGO2'] <- statsgo_mu_area$hectares
sum(mu_results_all$hectares) #now, toal is correct: 400,017.3 ha
write.csv(mu_results_all, file.path(ResultsDir, 'mu_aggregated_kings.3.22.18.csv'), row.names = FALSE)
