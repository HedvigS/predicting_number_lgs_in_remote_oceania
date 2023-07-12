source("01_requirements.R")

#script written by Russell Dinnage (ORCID= 0000-0003-0846-2819), with smaller tweaks by Hedvig Skirgård

#### extracting ecoClimate data for points ####

#### Load Points ####
pnts_all <- readr::read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = F)

pnts <- pnts_all %>%
  dplyr::filter(!is.na(Smallest_Island_group))

#### Read rasters into RasterStack ####
raster_files <- list.files("data/ecoClimate/bio # CCSM_piControl(1760)/", full.names = TRUE, pattern = "\\.bil$") ## list files

clim_stack <- raster::stack(raster_files, native = T) ## read rasters into single RasterStack
names(clim_stack) <- str_replace_all(names(clim_stack), "bio...", "bio_._") #the names of the layers are varying in terms of the characters after "bio". Resetting to one string if there is a difference. This is due to the way different computers, OSes etc deal with " # " in the filename that ecoClimate gives.

#plot(clim_stack$bio...CCSM_piControl.1760._bio11)

## rasters have no projection but looks like standard longlat WGS84
raster::crs(clim_stack) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#plot(clim_stack$bio...CCSM_piControl.1760._bio12)

## make sure there are no projection issue by making a SpatialPoints object with same projection
points_to_extract <- sp::SpatialPoints(pnts %>%
                                         dplyr::select(Longitude, Latitude) %>%
                                         as.data.frame(),
                                       proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

## Do these points look right? Based on my limited knowledge of these island groups, looks ok to me?
#plot(clim_stack$bio...CCSM_piControl.1760._bio11)
#plot(points_to_extract, add = TRUE)

## Extract climate data

clim_dat <- raster::extract(clim_stack, points_to_extract)

## Join it back to orginal data and save!!

all_dat <- pnts_all %>%
  dplyr::left_join(pnts %>%
                     dplyr::bind_cols(dplyr::as_tibble(clim_dat)),by = join_by(Unique_ID, `AREA (sq km)`, `COASTLINE (km) (perimeter)`, Longitude, Latitude, Region, `COUNTRY NAME`, Melanesia_or_not, `Island name`, Marck_group, Inhabited, glottocodes, Left_over, Smallest_Island_group, Medium_island_group, Medium_island_group_language_merged, Medium_only_merged_for_shared_language))

all_dat_grouped <-all_dat %>% 
  filter(!is.na(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>%
    summarise(mean_CCSM_piControl_1760_bio1 = mean(bio_._CCSM_piControl.1760._bio1),
              mean_CCSM_piControl_1760_bio2 = mean(bio_._CCSM_piControl.1760._bio2),
              mean_CCSM_piControl_1760_bio3 = mean(bio_._CCSM_piControl.1760._bio3),
              mean_CCSM_piControl_1760_bio4 = mean(bio_._CCSM_piControl.1760._bio4),
              mean_CCSM_piControl_1760_bio5 = mean(bio_._CCSM_piControl.1760._bio5),
              mean_CCSM_piControl_1760_bio6 = mean(bio_._CCSM_piControl.1760._bio6),
              mean_CCSM_piControl_1760_bio7 = mean(bio_._CCSM_piControl.1760._bio7),
              mean_CCSM_piControl_1760_bio8 = mean(bio_._CCSM_piControl.1760._bio8),
              mean_CCSM_piControl_1760_bio9 = mean(bio_._CCSM_piControl.1760._bio9),
              mean_CCSM_piControl_1760_bio10 = mean(bio_._CCSM_piControl.1760._bio10),
              mean_CCSM_piControl_1760_bio11 = mean(bio_._CCSM_piControl.1760._bio11),
              mean_CCSM_piControl_1760_bio12 = mean(bio_._CCSM_piControl.1760._bio12),
              mean_CCSM_piControl_1760_bio13 = mean(bio_._CCSM_piControl.1760._bio13),
              mean_CCSM_piControl_1760_bio14 = mean(bio_._CCSM_piControl.1760._bio14),
              mean_CCSM_piControl_1760_bio15 = mean(bio_._CCSM_piControl.1760._bio15),
              mean_CCSM_piControl_1760_bio16 = mean(bio_._CCSM_piControl.1760._bio16),
              mean_CCSM_piControl_1760_bio17 = mean(bio_._CCSM_piControl.1760._bio17),
              mean_CCSM_piControl_1760_bio18 = mean(bio_._CCSM_piControl.1760._bio18),
              mean_CCSM_piControl_1760_bio19 = mean(bio_._CCSM_piControl.1760._bio19)
              )
  
write_tsv(all_dat_grouped, "output/processed_data/RO_polygons_grouped_with_languages_with_climate_grouped.tsv")