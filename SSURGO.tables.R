mainDir <- 'C:/Users/smdevine/Desktop/SpatialData'
SSURGOdir <- file.path(mainDir, 'soils_data/SSURGOrevised')
OSDdir <- file.path(mainDir, 'soils_data/master_soils_watershed_study')
results <- file.path(mainDir, 'soils_data/SSURGO_summary')
ssurgo_dirs <- list.dirs(SSURGOdir)

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
mean(ssurgo_horizon$awc_r[ssurgo_horizon$hzname !=  & ssurgo_horizon$awc_r > 0], na.rm=TRUE)

#some of exploration of soil comp stats in San Joaquin

list.files(list.dirs(results)[20])
comp_results <- read.csv(file.path(list.dirs(results)[20], "uf22_final_comp_aggregation.csv"), stringsAsFactors = FALSE)
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
