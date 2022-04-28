#hack-a-thon team 3
#KT notes 

library(tidyverse)
library(sf)

# Read in boundaries of 8 major river basins in SC
# https://sc-department-of-health-and-environmental-control-gis-sc-dhec.hub.arcgis.com/datasets/sc-major-river-basins/explore
master_data_basin=st_read("data/SC_Major_River_Basins", "SC_Major_River_Basins")
master_data_basin

# Read in shapefile of all sites assessed for 2018 303d list
# Provided by Wade Cantrell at SC DHEC on 2022-04-11
master_data_shp_2018=st_read("data/bw.SDE.DHEC_303D_18", "bw.SDE.DHEC_303D_18")
master_data_shp_2018

# Larger shapefile of ALL stations (including shellfish monitoring sites, macroinvertebrate sites, much more than the ambient water quality sites used for the 303d list)
# Provided by Wade Cantrell at SC DHEC on 2022-04-11
master_data_shp_all=st_read("data/bw.SDE.STATIONS", "bw.SDE.STATIONS")
master_data_shp_all





## A look at the imparied water list for 2018 SC


# SC 2018 303d list: https://scdhec.gov/bow/south-carolina-303d-list-impaired-waters-tmdls
# Converted 303d list from .xls to .csv before reading in
master_data_2018=read_csv("data/2018303d_final.csv")   #SC Impaired Waters List 303d for 2018

master_data_2018=master_data_2018%>%
  filter(PRIORITY.RANK!="")%>%
  mutate(PRIORITY.RANK=as.factor(PRIORITY.RANK))

glimpse(master_data_2018)


# determining classes
class(master_data_2018$PRIORITY.RANK)   #character
class(master_data_2018$NOTE)   #character
class(master_data_2018$BASIN)   #character
class(master_data_2018$HUC_12)   #numeric
class(master_data_2018$COUNTY)   #character
class(master_data_2018$DESCRIPTION)   #character
class(master_data_2018$STATION)   #character
class(master_data_2018$USE)   #character
class(master_data_2018$`CAUSE(S)`)   #character


tail(master_data_2018)

summary(master_data_2018)

# clean up data
master_data_2018_clean=master_data_2018%>%
  filter(PRIORITY.RANK%in%c(1,2,3))%>%
  mutate(PRIORITY.RANK=as.factor(PRIORITY.RANK))   # change class from character to categorical

summary(master_data_2018_clean)


# basic bar plot of priority ranks
ggplot(data=master_data_2018_clean)+
  geom_bar(aes(x=PRIORITY.RANK, fill=PRIORITY.RANK))+
  theme_bw()


# plot
ggplot(data=master_data_2018_clean)+
  geom_bar(aes(x=PRIORITY.RANK))+
  facet_wrap(~COUNTY)


# counties and ranks
imp_counties=master_data_2018%>%
  select(COUNTY,
         PRIORITY.RANK)%>%
  group_by(COUNTY)%>%
  summarise(count_ranks=n())   #count no. ranks and groups them by county
head(imp_counties)
dim(imp_counties)

imp_counties=rename(imp_counties, NAME=COUNTY)

# read in county shape file

county_shp = st_read(dsn='data/USA_county_boundaries_cb_2018_us_county_5m', layer='cb_2018_us_county_5m')   #Can specify "layer" parameter

shp = st_read('data/USA_county_boundaries_cb_2018_us_county_5m/cb_2018_us_county_5m.shp')   #Or just read in .shp file (reads in all layers)
head(county_shp)

sc = county_shp %>%
  filter(STATEFP==45) %>%
  mutate(NAME = tolower(NAME))
glimpse(sc)

head(imp_counties)

imp_counties = imp_counties %>%
  mutate(NAME = tolower(NAME))

imp_sc_join=sc%>%
  left_join(imp_counties, by="NAME")

glimpse(imp_sc_join)

# plot
ggplot() +
  geom_sf(data=imp_sc_join, aes(fill=count_ranks))+ # color counties by no. ranks
  scale_fill_gradientn(colors = c("green", "orange", "red"))

ggplot() +
  geom_sf(data=imp_sc_join, aes(fill=AWATER))+ # color counties by water area
  scale_fill_gradientn(colors = c("green", "orange", "red"))


# create a map with ratio of water to land per county
area_ratio=imp_sc_join%>%
  select(NAME,
         count_ranks,
         ALAND,
         AWATER)%>%
  mutate(ratio=AWATER/(ALAND+AWATER))   # ratio of water to land
glimpse(area_ratio)


ggplot() +
  geom_sf(data=area_ratio, aes(fill=ratio))+ # color counties by water area
  scale_fill_gradientn(colors = c("green", "orange", "red"))








