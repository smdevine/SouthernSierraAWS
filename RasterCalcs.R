library(raster)
#points from Toby
#point 1: 37.10807 lat, -119.7325 long
#point 2: 37.06813 lat, -119.1944 long

#convert from geographic to California Teale Albers
# coords <- SpatialPoints(cbind(-2045640, 1740060), proj4string = crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")) #this is Albers Equal Area coordinates
# spTransform(coords, "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
ResultsDir <- 'C:/Users/smdevine/Desktop/GITprojects/SouthernSierraAWS/results'
SanJoaquinDir <- 'C:/Users/smdevine/Desktop/SpatialData/watershed_characteristics/san.joaquin'
KingsDir <- 'C:/Users/smdevine/Desktop/SpatialData/watershed_characteristics/uf_kings_stats'
list.files(SanJoaquinDir)
DEM.san.joaquin <- raster(file.path(SanJoaquinDir, 'DEM.tif'))
elev.zones <- DEM.san.joaquin
elev.zones[elev.zones < 600] <- 1
elev.zones[elev.zones >= 600 & elev.zones <= 2000] <- 2
elev.zones[elev.zones > 2000] <- 3
names(elev.zones) <- 'elev.zones'
elev.zone.summary <- freq(elev.zones)
elev.zone.summary <- as.data.frame(elev.zone.summary)
elev.zone.summary <- elev.zone.summary[-4,]
sum(elev.zone.summary$count)*900/10000
elev.zone.summary$hectares <- elev.zone.summary$count*900/10000
elev.zone.summary$zone.pct <- 100*elev.zone.summary$hectares/sum(elev.zone.summary$hectares)
write.csv(elev.zone.summary, file.path(ResultsDir, 'elev.zone.summary.san.joaquin.csv'), row.names=FALSE)

aws.san.joaquin <- raster(file.path(SanJoaquinDir, 'aws_soils.tif'))
zonal.san.joaquin.aws <- as.data.frame(zonal(aws.san.joaquin, elev.zones, fun = 'mean'))
zonal.san.joaquin.aws$NA.count <- NA
colnames(zonal.san.joaquin.aws)[2] <- 'aws_mu_wtdavg_cmH2O'
san.joaquin.stack <- stack(elev.zones, aws.san.joaquin)
names(san.joaquin.stack) <- c('zones', 'aws_mu_wtdavg')
zonal.san.joaquin.aws$NA.count[1] <- cellStats(san.joaquin.stack$zones==1 & is.na(san.joaquin.stack$aws_mu_wtdavg), 'sum') #24482
zonal.san.joaquin.aws$NA.count[2] <- cellStats(san.joaquin.stack$zones==2 & is.na(san.joaquin.stack$aws_mu_wtdavg), 'sum') #19032
zonal.san.joaquin.aws$NA.count[3] <- cellStats(san.joaquin.stack$zones==3 & is.na(san.joaquin.stack$aws_mu_wtdavg), 'sum') #117816
#total NA count is 14,519 ha, close to 14,325 ha in uf22_mu_summary.csv.  consists of 34 ha in dams, 5526 ha water, and 8766 wilderness.  also a 16,894.1 NOTCOM mapunit that was replaced by STATSGO
zonal.san.joaquin.aws$hectares <- elev.zone.summary$hectares
#add addtional files to stack
san.joaquin.stack$aws0150.san.joaquin <- raster(file.path(SanJoaquinDir, 'aws0150wta.tif'))
san.joaquin.stack$lithic_soils_mu_pct <- raster(file.path(SanJoaquinDir, 'lithic_soils_pct.tif'))
san.joaquin.stack$paralithic_soils_mu_pct <- raster(file.path(SanJoaquinDir, 'paralithic_soils_pct.tif'))
san.joaquin.stack$ABC_soils_mu_pct <- raster(file.path(SanJoaquinDir, 'ABC_soils_pct.tif'))
san.joaquin.stack$NAdeephorizon_soils_mu_pct <- raster(file.path(SanJoaquinDir, 'NAhorizon_soils_pct.tif'))
san.joaquin.stack$DEM <- DEM.san.joaquin

#now add some stats to table
zonal.san.joaquin.aws$lithic.mean.pct <- zonal(san.joaquin.stack$lithic_soils_mu_pct, san.joaquin.stack$zones, fun = 'mean')[,2] #this gets column 2
zonal.san.joaquin.aws$lithic.hectares <- zonal.san.joaquin.aws$hectares*(zonal.san.joaquin.aws$lithic.mean.pct/100)
zonal.san.joaquin.aws$paralithic.mean.pct <- zonal(san.joaquin.stack$paralithic_soils_mu_pct, san.joaquin.stack$zones, fun = 'mean')[,2]
zonal.san.joaquin.aws$paralithic.hectares <- zonal.san.joaquin.aws$hectares*(zonal.san.joaquin.aws$paralithic.mean.pct/100)
zonal.san.joaquin.aws$no.id.deephorizon.mean.pct <- zonal(san.joaquin.stack$NAdeephorizon_soils_mu_pct, san.joaquin.stack$zones, fun = 'mean')[,2]
zonal.san.joaquin.aws$no.id.deephorizon.hectares <- zonal.san.joaquin.aws$hectares*(zonal.san.joaquin.aws$no.id.deephorizon.mean.pct/100)
zonal.san.joaquin.aws$sensible.NA.hectares <- zonal.san.joaquin.aws$NA.count*900/10000
zonal.san.joaquin.aws$aws_0150wta <- zonal(san.joaquin.stack$aws0150.san.joaquin, san.joaquin.stack$zones, fun = 'mean')[,2]
zonal.san.joaquin.aws$ABC.mean.pct <- zonal(san.joaquin.stack$ABC_soils_mu_pct, san.joaquin.stack$zones, fun = 'mean')[,2]
zonal.san.joaquin.aws$ABC.deephorizon.hectares <- zonal.san.joaquin.aws$hectares*(zonal.san.joaquin.aws$ABC.mean.pct/100)
zonal.san.joaquin.aws$elevation <- zonal(san.joaquin.stack$DEM, san.joaquin.stack$zones, fun='mean') 
zonal.san.joaquin.aws
write.csv(zonal.san.joaquin.aws, file.path(ResultsDir, 'zonal.summary.san.joaquin.csv'), row.names = FALSE)
sum(zonal.san.joaquin.aws$no.id.deephorizon.hectares)

#now do same for Kings watershed
DEM.kings <- raster(file.path(KingsDir, 'DEM.tif'))
elev.zones.kings <- DEM.kings
elev.zones.kings[elev.zones.kings < 600] <- 1
elev.zones.kings[elev.zones.kings >= 600 & elev.zones.kings <= 2000] <- 2
elev.zones.kings[elev.zones.kings > 2000] <- 3
elev.zone.summary.kings <- freq(elev.zones.kings)
elev.zone.summary.kings <- as.data.frame(elev.zone.summary.kings)
elev.zone.summary.kings <- elev.zone.summary.kings[-4,]
sum(elev.zone.summary.kings$count)*900/10000
elev.zone.summary.kings$hectares <- elev.zone.summary.kings$count*900/10000
elev.zone.summary.kings$zone.pct <- 100*elev.zone.summary.kings$hectares/sum(elev.zone.summary.kings$hectares)
elev.zone.summary.kings
write.csv(elev.zone.summary.kings, file.path(ResultsDir, 'elev.zone.summary.kings.csv'), row.names=FALSE)

aws.kings <- raster(file.path(KingsDir, 'aws_soils.tif'))
zonal.kings.aws <- as.data.frame(zonal(aws.kings, elev.zones.kings, fun = 'mean'))
zonal.kings.aws$NA.count <- NA
colnames(zonal.kings.aws)[2] <- 'aws_mu_wtdavg_cmH2O'
kings.stack <- stack(elev.zones.kings, aws.kings)
names(kings.stack) <- c('zones', 'aws_mu_wtdavg')
zonal.kings.aws$NA.count[1] <- cellStats(kings.stack$zones==1 & is.na(kings.stack$aws_mu_wtdavg), 'sum')
zonal.kings.aws$NA.count[2] <- cellStats(kings.stack$zones==2 & is.na(kings.stack$aws_mu_wtdavg), 'sum')
zonal.kings.aws$NA.count[3] <- cellStats(kings.stack$zones==3 & is.na(kings.stack$aws_mu_wtdavg), 'sum')
zonal.kings.aws$NA.AWS.hectares <- zonal.kings.aws$NA.count*900/10000
#total NA count needs to be confirmed
zonal.kings.aws$hectares <- elev.zone.summary.kings$hectares
#add addtional files to stack
kings.stack$aws0150.kings <- raster(file.path(KingsDir, 'aws0150wta.tif'))
kings.stack$lithic_soils_mu_pct <- raster(file.path(KingsDir, 'lithic_soils_pct.tif'))
kings.stack$paralithic_soils_mu_pct <- raster(file.path(KingsDir, 'paralithic_soils_pct.tif'))
kings.stack$ABC_soils_mu_pct <- raster(file.path(KingsDir, 'ABC_soils_pct.tif'))
kings.stack$NAdeephorizon_soils_mu_pct <- raster(file.path(KingsDir, 'NAhorizon_soils_pct.tif'))
kings.stack$DEM <- DEM.kings

#now add some stats to table
zonal.kings.aws$lithic.mean.pct <- zonal(kings.stack$lithic_soils_mu_pct, kings.stack$zones, fun = 'mean')[,2] #this gets column 2
zonal.kings.aws$lithic.hectares <- zonal.kings.aws$hectares*(zonal.kings.aws$lithic.mean.pct/100)
zonal.kings.aws$paralithic.mean.pct <- zonal(kings.stack$paralithic_soils_mu_pct, kings.stack$zones, fun = 'mean')[,2]
zonal.kings.aws$paralithic.hectares <- zonal.kings.aws$hectares*(zonal.kings.aws$paralithic.mean.pct/100)
zonal.kings.aws$no.id.deephorizon.mean.pct <- zonal(kings.stack$NAdeephorizon_soils_mu_pct, kings.stack$zones, fun = 'mean')[,2]
zonal.kings.aws$no.id.deephorizon.hectares <- zonal.kings.aws$hectares*(zonal.kings.aws$no.id.deephorizon.mean.pct/100)
zonal.kings.aws$sensible.NA.hectares <- zonal.kings.aws$NA.count*900/10000
zonal.kings.aws$aws_0150wta <- zonal(kings.stack$aws0150.kings, kings.stack$zones, fun = 'mean')[,2]
zonal.kings.aws$ABC.mean.pct <- zonal(kings.stack$ABC_soils_mu_pct, kings.stack$zones, fun = 'mean')[,2]
zonal.kings.aws$ABC.deephorizon.hectares <- zonal.kings.aws$hectares*(zonal.kings.aws$ABC.mean.pct/100)
zonal.kings.aws$elevation <- zonal(kings.stack$DEM, kings.stack$zones, fun='mean') 
zonal.kings.aws
write.csv(zonal.kings.aws, file.path(ResultsDir, 'zonal.summary.kings.csv'), row.names = FALSE)
sum(zonal.kings.aws$no.id.deephorizon.hectares)

#subsitute mukey raster with new major component only and zonal soils data
mu_results_all <- read.csv(file.path(ResultsDir, 'mu_aggregated_san.joaquin.3.22.18.csv'), stringsAsFactors = FALSE)
mukey_raster_san.joaquin <- raster(file.path(SanJoaquinDir, 'soil_mukey.tif'))
#names(mukey_raster_san.joaquin) <- 'mukey'
head(mu_results_all)
sum(mu_results_all$hectares)
aws_results_all <- mu_results_all[ ,c('mukey', "awc_ssurgo_majcomps", 'awc_zone1', 'awc_zone2', 'awc_zone3')] #by column needs to be number 1 for some reason for subs to work
subs(mukey_raster_san.joaquin, aws_results_all, by='mukey', which='awc_ssurgo_majcomps', filename=file.path(SanJoaquinDir, 'aws_majcomps_only.tif'), format='GTiff', overwrite=TRUE)

#now make a stack to create full regolith estimate
full_regolith_stack <- stack(mukey_raster_san.joaquin, elev.zones)
full_regolith_stack$aws_full_regolith <- full_regolith_stack$elev.zones #to be replaced below
full_regolith_stack$zone1_assumption <- subs(full_regolith_stack$soil_mukey, aws_results_all, by='mukey', which='awc_zone1', filename=file.path(SanJoaquinDir, 'zone1_assumption.tif'), format='GTiff', overwrite=TRUE)
full_regolith_stack$zone2_assumption <- subs(full_regolith_stack$soil_mukey, aws_results_all, by='mukey', which='awc_zone2', filename=file.path(SanJoaquinDir, 'zone2_assumption.tif'), format='GTiff', overwrite=TRUE)
full_regolith_stack$zone3_assumption <- subs(full_regolith_stack$soil_mukey, aws_results_all, by='mukey', which='awc_zone3', filename=file.path(SanJoaquinDir, 'zone3_assumption.tif'), format='GTiff', overwrite=TRUE)
full_regolith_stack$aws_full_regolith[full_regolith_stack$elev.zones==1] <- full_regolith_stack$zone1_assumption[full_regolith_stack$elev.zones==1]
full_regolith_stack$aws_full_regolith[full_regolith_stack$elev.zones==2] <- full_regolith_stack$zone2_assumption[full_regolith_stack$elev.zones==2]
full_regolith_stack$aws_full_regolith[full_regolith_stack$elev.zones==3] <- full_regolith_stack$zone3_assumption[full_regolith_stack$elev.zones==3]
writeRaster(full_regolith_stack$aws_full_regolith, filename = file.path(SanJoaquinDir, 'aws_full_regolith.tif'), format = 'GTiff', overwrite=TRUE)
full_regolith_stack$aws_majcomps_only <- raster(file.path(SanJoaquinDir, 'aws_majcomps_only.tif'))
full_regolith_stack$aws_0150 <- raster(file.path(SanJoaquinDir, 'aws0150wta.tif'))
full_regolith_stack$precip.annual <- raster(file.path(SanJoaquinDir, 'ppt1981_2010avg.tif'))
full_regolith_stack$aprsnpck.annual <- raster(file.path(SanJoaquinDir, 'aprpck1981_2010avg.tif'))
full_regolith_stack$slope <- raster(file.path(SanJoaquinDir, 'slope.tif'))
full_regolith_stack$pet.annual <- raster(file.path(SanJoaquinDir, 'pet1981_2010avg.tif'))

#1 km3 is 810,714 acre feet
cellStats(full_regolith_stack$aws_full_regolith, stat='mean') #13.14 cm
cellStats(full_regolith_stack$aws_full_regolith/100 * 900 / 1000^3, 'sum') #0.5518573 km3 or 447,398.7 acre feet
cellStats(!is.na(full_regolith_stack$aws_full_regolith), 'sum') #4667392 which is 420,065.3 ha
(13.14/100 * 900 * 4667392) / 1000^3 #check answer above
cellStats(full_regolith_stack$aws_majcomps_only, stat = 'mean') #7.36 cm
cellStats(full_regolith_stack$aws_majcomps_only/100 * 900 / 1000^3, 'sum') #0.3093358 or 250,782.9 acre-feet
cellStats(full_regolith_stack$aws_0150, stat = 'mean') #7.36 cm
cellStats(full_regolith_stack$aws_0150/100 * 900 / 1000^3, 'sum') #0.3592872 or 291,279.2 acre-feet

#add these back to the zonal summary table
#use this as template: zonal(kings.stack$aws0150.kings, kings.stack$zones, fun = 'mean')[,2]
elev.zone.summary.san.joaquin <- read.csv(file.path(ResultsDir, 'zonal.summary.san.joaquin.csv'), stringsAsFactors = FALSE)
elev.zone.summary.san.joaquin$aws_majcomps_SSURGOmodified <- zonal(full_regolith_stack$aws_majcomps_only, full_regolith_stack$elev.zones, fun='mean')[,2]
elev.zone.summary.san.joaquin$aws_full_regolith <- zonal(full_regolith_stack$aws_full_regolith, full_regolith_stack$elev.zones, fun='mean')[,2]
elev.zone.summary.san.joaquin$precip.annual <- zonal(full_regolith_stack$precip.annual, full_regolith_stack$elev.zones, fun='mean')[,2]
elev.zone.summary.san.joaquin$aprsnpck.annual <- zonal(full_regolith_stack$aprsnpck.annual, full_regolith_stack$elev.zones, fun='mean')[,2]
elev.zone.summary.san.joaquin$pet.annual <- zonal(full_regolith_stack$pet.annual, full_regolith_stack$elev.zones, fun='mean')[,2]
elev.zone.summary.san.joaquin$slope <- zonal(full_regolith_stack$slope, full_regolith_stack$elev.zones, fun='mean')[,2]
write.csv(elev.zone.summary.san.joaquin, file.path(ResultsDir, 'zonal.summary.san.joaquin.v2.csv'), row.names = FALSE)

