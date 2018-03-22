mainDir <- 'C:/Users/smdevine/Desktop/SpatialData'
SSURGOdir <- file.path(mainDir, 'soils_data/SSURGOrevised')
OSDdir <- file.path(mainDir, 'soils_data/master_soils_watershed_study')
PastResults <- file.path(mainDir, 'soils_data/SSURGO_summary')
ssurgo_dirs <- list.dirs(SSURGOdir)
ResultsDir <- 'C:/Users/smdevine/Desktop/GITprojects/SouthernSierraAWS/results'
paralithic_awc <- 0.05

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
unique(comp_results$reskind)
summary(as.factor(comp_awc_revised$reskind[is.na(comp_awc_revised$hzname_deepest_simplified)]))
summary(as.factor(comp_awc_revised$hzname_deepest_simplified[is.na(comp_awc_revised$reskind)]))
summary(as.factor(comp_awc_revised$awc_H2Ocm[is.na(comp_awc_revised$reskind)])) #NA reskinds are all NA for AWC also
summary(as.factor(comp_awc_revised$awc_H2Ocm[is.na(comp_awc_revised$hzname_deepest_simplified)])) #whereas this is a mix
summary(as.factor(comp_awc_revised$hzname_deepest_simplified))
sum(comp_awc_revised$hectares[is.na(comp_awc_revised$hzname_deepest_simplified)])
comp_awc_revised <- comp_results[ ,c('mukey', 'cokey', 'compname', 'majcompflag', 'hectares', 'comppct_r', 'awc_H2Ocm', 'awc_soilthickness_cm', 'hzname_deepest_simplified', 'reskind', 'resdept_r')]
comp_awc_revised$zone1_awc <- as.numeric(ifelse(comp_awc_revised$reskind=='Lithic bedrock' | comp_awc_revised$reskind == 'Duripan' | comp_awc_revised$reskind == 'Densic material', comp_awc_revised$awc_H2Ocm, ifelse(comp_awc_revised$reskind == 'Paralithic bedrock', ifelse(comp_awc_revised$awc_soilthickness_cm > 150, comp_awc_revised$awc_H2Ocm, comp_awc_revised$awc_H2Ocm + paralithic_awc*(150 - comp_awc_revised$awc_soilthickness_cm)), ifelse(comp_awc_revised$reskind == 'None' | comp_awc_revised$reskind == 'Abrupt textural change', ifelse(comp_awc_revised$awc_soilthickness_cm > 150, comp_awc_revised$awc_H2Ocm, comp_awc_revised$awc_H2Ocm + (comp_awc_revised$awc_H2Ocm/comp_awc_revised$awc_soilthickness_cm)*(150 - comp_awc_revised$awc_soilthickness_cm)), ifelse(is.na(comp_awc_revised$reskind), comp_awc_revised$awc_H2Ocm, print('Fix the ifelses!'))))))
summary(as.factor(comp_awc_revised$zone1_awc))
