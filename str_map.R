library(tidyverse)
library(sf)
library(tidygeocoder)
sf_use_s2(FALSE)


str<-read_csv("strs_athens.csv")%>% 
  mutate(PAR_ADD=parcel_add,
         parcel_num=if_else(is.na(parcel_num),parcel1,parcel_num)) %>%
  select(parcel_num,PAR_ADD) %>%
  filter(is.na(PAR_ADD)==FALSE) %>%
  mutate(PAR_ADD=if_else(PAR_ADD="220 DR MARTIN LUTHER KING JR",
                         "220 DR MARTIN LUTHER KING JR PKWY",
                         PAR_ADD))


parcels<-st_read("ACC_Parcels.shp") %>% 
  st_transform(4326) %>%
  st_centroid() %>%
  bind_cols(data.frame(st_coordinates(.))) %>%
  st_set_geometry(NULL) %>%
  select(PARCEL_NO,PAR_ADD,X,Y) %>%
  distinct()

parcel_join<-parcels %>%
  mutate(parcel_num=PARCEL_NO) %>%
  inner_join(str) %>%
  distinct()

parcels_addonly<-parcels %>%
  group_by(PAR_ADD) %>%
  summarise(X=mean(X),
            Y=mean(Y))

str_non<-str %>%
  anti_join(parcel_join) %>%
  rename(parcel_num1=parcel_num) %>%
  left_join(parcels_addonly) %>%
  distinct()

str_all<-parcel_join %>%
  anti_join(str_non) %>%
  bind_rows(str_non) %>%
  select(-parcel_num1) 

write_csv(str_all,"athens_strparcels.csv")
str_sf<-str_all %>%
  st_as_sf(coords=c("X","Y"),crs=4326,remove=FALSE)
st_write(str_sf,"athens_strparcels.geojson")
