modis_url <- c("https://e4ftl01.cr.usgs.gov/MOLT/MOD17A3H.006/2003.01.01/MOD17A3H.A2003001.h11v10.006.2015166204055.hdf.xml")
exdir <- "data/modis/"
if(!dir.exists(exdir)){
  dir.create(exdir)}

exdir_fn <- paste0(exdir, "MOD17A3H.006")
utils::download.file(file.path(modis_url), destfile = exdir_fn)

#devtools:::install_github("gearslaboratory/gdalUtils")

h_load("rgdal")
library(gdalUtils)

gdalUtils::gdal_setInstallation(ignore.full_scan = F)
# Provides detailed data on hdf4 files but takes ages

gdalinfo("data/modis/MOD17A3H.006")

# Tells me what subdatasets are within my hdf4 MODIS files and makes them into a list

sds <- get_subdatasets("data/modis/MOD17A3H.006")
sds

#[1] "HDF4_EOS:EOS_GRID:MOD17A3H.A2000001.h21v09.006.2015141183401.hdf:MOD_Grid_MOD17A3H:Npp_500m"   
#[2] "HDF4_EOS:EOS_GRID:MOD17A3H.A2000001.h21v09.006.2015141183401.hdf:MOD_Grid_MOD17A3H:Npp_QC_500m"

# I'm only interested in the first subdataset and I can use gdal_translate to convert it to a .tif

gdal_translate(sds[1], dst_dataset = "NPP2000.tif")

# Load and plot the new .tif

rast <- raster("NPP2000.tif")
plot(rast)


v <-  xmlparsedata::xml_parse_data(x = "data/modis/MOD17A3H.A2003001.h13v02.006.2015166203838.hdf.xml")

