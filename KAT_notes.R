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





### A look at the imparied water list for 2018 SC


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
imp_counties_2018=master_data_2018%>%
  select(COUNTY,
         PRIORITY.RANK)%>%
  group_by(COUNTY)%>%
  summarise(count_ranks=n())   # count no. ranks and groups them by county

head(imp_counties_2018)
dim(imp_counties_2018)


# read in county shape file
county_shp = st_read(dsn='data/USA_county_boundaries_cb_2018_us_county_5m', layer='cb_2018_us_county_5m')   #Can specify "layer" parameter

shp = st_read('data/USA_county_boundaries_cb_2018_us_county_5m/cb_2018_us_county_5m.shp')   #Or just read in .shp file (reads in all layers)
head(county_shp)

sc = county_shp %>%
  filter(STATEFP==45) %>%
  mutate(NAME = tolower(NAME))
glimpse(sc)


# join files
imp_counties_2018=rename(imp_counties_2018, NAME=COUNTY)   # rename to match shp file when joining

imp_counties_2018=imp_counties_2018%>%
  mutate(NAME=tolower(NAME))

imp_sc_join_2018=sc%>%
  left_join(imp_counties_2018, by="NAME")

glimpse(imp_sc_join_2018)

# plot
ggplot() +
  geom_sf(data=imp_sc_join_2018, aes(fill=count_ranks))+ # color counties by no. ranks
  scale_fill_gradientn(colors = c("green", "orange", "red"))+
  ggtitle("2018 Ranks by County")

# ggplot() +
#  geom_sf(data=imp_sc_join_2018, aes(fill=AWATER))+ # color counties by water area
#  scale_fill_gradientn(colors = c("green", "orange", "red"))


# create a map with ratio of water to land per county
  ### ONLY USED ONCE FOR ALL 3 YEAR COMPARISONS
area_ratio=imp_sc_join_2018%>%
  select(NAME,
         count_ranks,
         ALAND,
         AWATER)%>%
  mutate(ratio=AWATER/(ALAND+AWATER))   # ratio of water to land
glimpse(area_ratio_2018)


ggplot() +
  geom_sf(data=area_ratio_2018, aes(fill=ratio))+ # color counties by water area
  scale_fill_gradientn(colors = c("green", "orange", "red"))+
  ggtitle("Ratio of Water to Land (m^2)")



### 2008 data
# Converted 303d list from .xls to .csv before reading in
master_data_2008=read_csv("data/SC_303d_lists_2006to2016/2008303dfinal020608prtyrk.csv")   # SC Impaired Waters List 303d for 2008

glimpse(master_data_2008)

master_data_2008=master_data_2008%>%   # doesn't have priority ranks
  filter(COUNTY!="")

glimpse(master_data_2008)


# counties and ranks
imp_counties_2008=master_data_2008%>%
  select(COUNTY)%>%
  group_by(COUNTY)%>%
  summarise(count_ranks=n())   # count no. ranks and groups them by county

head(imp_counties_2008)
dim(imp_counties_2008)


#join shp and ranks
imp_counties_2008=rename(imp_counties_2008, NAME=COUNTY)   # rename to match shp file when joining

imp_counties_2008=imp_counties_2008%>%
  mutate(NAME=tolower(NAME))

imp_sc_join_2008=sc%>%
  left_join(imp_counties_2008, by="NAME")

glimpse(imp_sc_join_2008)


# plot counties by no. ranks
ggplot() +
  geom_sf(data=imp_sc_join_2008, aes(fill=count_ranks))+
  scale_fill_gradientn(colors = c("green", "orange", "red"))+
  ggtitle("2008 Ranks by County")



### MAP BY BASINS
# 2018
imp_basins_2018=master_data_2018%>%
  select(BASIN)%>%
  group_by(BASIN)%>%
  summarise(count_ranks=n())

imp_basins_2018=rename(imp_basins_2018, Basin=BASIN)

imp_basins_2018=imp_basins_2018%>%
  mutate(Basin=tolower(Basin))

master_data_basin=master_data_basin%>%
  mutate(Basin=tolower(Basin))

imp_basins_join_2018=master_data_basin%>%
  left_join(imp_basins_2018, by="Basin")


# plot
ggplot() +
  geom_sf(data=imp_basins_join_2018, aes(fill=count_ranks))+ # color counties by no. ranks
  scale_fill_gradientn(colors = c("green", "orange", "red"))+
  ggtitle("2018 Impairments by Basin")


# 2008
imp_basins_2008=master_data_2008%>%
  select(BASIN)%>%
  group_by(BASIN)%>%
  summarise(count_ranks=n())

imp_basins_2008=rename(imp_basins_2008, Basin=BASIN)

imp_basins_2008=imp_basins_2008%>%
  mutate(Basin=tolower(Basin))

master_data_basin=master_data_basin%>%
  mutate(Basin=tolower(Basin))

imp_basins_join_2008=master_data_basin%>%
  left_join(imp_basins_2008, by="Basin")


# plot
ggplot() +
  geom_sf(data=imp_basins_join_2008, aes(fill=count_ranks))+ # color basins by no. ranks
  scale_fill_gradientn(colors = c("green", "orange", "red"))+
  ggtitle("2008 Impairments by Basin")







