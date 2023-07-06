library(sf)
library(tidyverse)
library(units)
library(googledrive)

#Download FACTS database version from Nov 2022 (the database is updated frequently; this is the version used for the manuscript)

options(  gargle_oauth_cache = ".secrets",  gargle_oauth_email = TRUE)
folder_url <- "https://drive.google.com/drive/folders/1V9-sottDdKSoLt6EgRK9Qzr246a2e5yd"
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
dir.create(here::here("Data/S_USA.Actv_CommonAttribute_PL.gdb"))
lapply(gdrive_files$id, function(x) drive_download(as_id(x),                                                    path = paste0(here::here("Data/S_USA.Actv_CommonAttribute_PL.gdb/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


  


# read in FACTS database --------------------------------------------------
facts <- st_read(here::here("Data/S_USA.Actv_CommonAttribute_PL.gdb/"), layer="Actv_CommonAttribute_PL")


# Geometries in FACTS are not all valid so need to clean up ---------------
library(gdalUtilities)
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}



facts.mltp <- ensure_multipolygons(facts)

empty <- facts.mltp[st_is_empty(facts.mltp),,drop=FALSE]
sum(empty$SHAPE_Area)
#all empty geoms originally included with 0 area

sf_use_s2(FALSE)
facts.valid <- st_make_valid(facts.mltp)


# filter by the appropriate start date ------------------------------------


fcts.fltr <- facts.valid %>% 
  filter(., NEPA_SIGNED_DATE >= as.Date("2009-01-01"))


# combine activity geometries into project geometries ---------------------

fcts.union <- 
  fcts.fltr %>% 
  group_by(., NEPA_DOC_NBR) %>% 
  summarise(., geometry = sf::st_union(geom)) %>%
  ungroup()

# estimate area for projects ----------------------------------------------


fcts.area <- fcts.union %>% 
  mutate(aream2 = st_area(.)) %>% 
  filter(., !st_is_empty(geometry)) %>% 
  group_by(., NEPA_DOC_NBR) %>% 
  summarise(., totalarea = sum(aream2))


fcts.fltr.area <- facts.valid %>% 
  filter(., NEPA_SIGNED_DATE >= as.Date("2009-01-01")) %>% 
  filter(., NEPA_DOC_NBR %in% fcts.area$NEPA_DOC_NBR) %>% 
  mutate(aream2 = st_area(.)) %>% 
  filter(., !st_is_empty(geom)) %>% 
  group_by(., NEPA_DOC_NBR) %>% 
  summarise(., impactarea = sum(aream2))


fcts.flt.act.area <- facts.valid %>% 
  filter(., NEPA_SIGNED_DATE >= as.Date("2009-01-01")) %>% 
  filter(., NEPA_DOC_NBR %in% fcts.area$NEPA_DOC_NBR) %>% 
  mutate(aream2 = st_area(.)) %>% 
  filter(., !st_is_empty(geom)) %>% 
  group_by(NEPA_DOC_NBR, ACTIVITY_CODE) %>% 
  summarize(., actarea = sum(aream2))

fcts.join <- fcts.flt.act.area %>%
  st_drop_geometry(.) %>% 
  left_join(., st_drop_geometry(fcts.fltr.area)) %>% 
  left_join(., st_drop_geometry(fcts.area))

fcts.prop <- fcts.join %>% 
  mutate(., prop = drop_units(actarea/impactarea),
         step1 = -prop * log(prop))


shannon <- fcts.prop %>% 
  group_by(., NEPA_DOC_NBR) %>% 
  summarise(., numact = n(), 
            H = sum(step1),
            projarea = unique(impactarea),
            footprint = unique(totalarea),
            E = H/log(drop_units(projarea)))
shannon$E <- ifelse(drop_units(shannon$projarea) == 0, 1, shannon$E)





write.csv(shannon, "/Users/mattwilliamson/Downloads/shannonindex_0120.csv")
write.csv(simpson, "/Users/mattwilliamson/Downloads/simpsonindex.csf")
