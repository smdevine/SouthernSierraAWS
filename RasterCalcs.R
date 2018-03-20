library(raster)
#points from Toby
#point 1: 37.10807 lat, -119.7325 long
#point 2: 37.06813 lat, -119.1944 long

#convert from geographic to California Teale Albers
# coords <- SpatialPoints(cbind(-2045640, 1740060), proj4string = crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")) #this is Albers Equal Area coordinates
# spTransform(coords, "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

SanJoaquinDir <- 'C:/Users/smdevine/Desktop/SpatialData/watershed_characteristics/san.joaquin'
list.files(SanJoaquinDir)
DEM.san.joaquin <- raster(file.path(SanJoaquinDir, 'DEM.tif'))
elev.zones <- DEM.san.joaquin
elev.zones[elev.zones < 600] <- 1
elev.zones[elev.zones >= 600 & elev.zones <= 2000] <- 2
elev.zones[elev.zones > 2000] <- 3
elev.zone.summary <- freq(elev.zones)
elev.zone.summary <- as.data.frame(elev.zone.summary)
elev.zone.summary <- elev.zone.summary[-4,]
sum(elev.zone.summary$count)*900/10000
elev.zone.summary$hectares <- elev.zone.summary$count*900/10000
elev.zone.summary$zone.pct <- 100*elev.zone.summary$hectares/sum(elev.zone.summary$hectares)
elev.zone.summary

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
zonal.san.joaquin.aws
sum(zonal.san.joaquin.aws$no.id.deephorizon.hectares)
