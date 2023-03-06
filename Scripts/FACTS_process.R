library(sf)
library(tidyverse)
library(units)



library(rgdal)  

ogrListLayers("/Users/mattwilliamson/Downloads/S_USA.Actv_CommonAttribute_PL.gdb/")

facts <- st_read("/Users/mattwilliamson/Downloads/S_USA.Actv_CommonAttribute_PL.gdb/", layer="Actv_CommonAttribute_PL")

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


fcts.fltr <- facts.valid %>% 
  filter(., NEPA_SIGNED_DATE >= as.Date("2009-01-01"))


fcts.union <- 
  fcts.fltr %>% 
  group_by(., NEPA_DOC_NBR) %>% 
  summarise(., geometry = sf::st_union(geom)) %>%
  ungroup()

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
