library(tidyverse)
library(sf)
library(tidygeocoder)

str<-read_csv("strs_athens.csv")
parcels<-st_read("C:/Users/jshannon/Dropbox/Jschool/GIS data/Georgia/Athens data/Parcelcentroid_alldata_sqft_bedbath_disttract.gpkg") %>%
  bind_cols(data.frame(st_coordinates(.)))

parcel_join<-parcels %>%
  mutate(parcel_num1=str_replace_all(PARCEL_NO, fixed(" "), "")) %>%
  inner_join(str %>% 
               mutate(parcel_num1=str_replace_all(parcel_num, fixed(" "), "")))

str_non<-str %>%
  anti_join(parcel_join) %>%
  mutate(parcel_add1=paste(parcel_add,", Athens, GA",sep="")) %>%
  geocode(address=parcel_add1,method='arcgis')

str_all<-parcel_join %>%
  anti_join(str_non) %>%
  bind_rows(str_non %>%
              rename(X=long,Y=lat) %>%
              select(parcel1,parcel_add,X,Y) %>%
              rename(parcel_num=parcel1)) %>%
  select(parcel_num,parcel_add,X,Y) %>%
  st_set_geometry(NULL)

write_csv(str_all,"athens_strparcels.csv")
str_sf<-str_all %>%
  st_as_sf(coords=c("X","Y"),crs=4326,remove=FALSE)
st_write(str_sf,"athens_strparcels.geojson")
