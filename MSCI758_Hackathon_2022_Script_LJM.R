# Hack-a-thon
# MSCI 758
# Levi McKercher
# 2022-04-12

library(ggplot2)
library(tidyverse)
library(sf)

# Reading in 1998 Impaired Waters List

imp_1998 = read.csv("data/SC_303d_lists_1998to2004/1998303dfin_al_rec_only.csv")

imp_1998 = imp_1998 %>% 
  filter(ï..SITE != "")
dim(imp_1998)


# Reading in 2018 Impaired Waters List

imp = read.csv("data/2018303d_final.csv") 
summary(imp) # There are 861 empty rows
dim(imp) #1979 rows
imp = imp %>%
  filter(ï..PRIORITY.RANK != "") 
dim(imp)

# Determining the class of each column in 2018 Data Frame

typeof(imp$NOTE) #Character
typeof(imp$BASIN) #Character
typeof(imp$HUC_12) #Double
typeof(imp$COUNTY) #Character
typeof(imp$DESCRIPTION) #Character
typeof(imp$STATION) #Character
typeof(imp$USE) #Character
typeof(imp$CAUSE.S.) #Character
typeof(imp$ï..PRIORITY.RANK) #Character

summary(imp)

# Determining the Number of Impairments per Basin in 2018

# Basin Names
# 1. BROAD
# 2. CATAWBA
# 3. EDISTO
# 4. PEEDEE
# 5. SALKEHATCHIE
# 6. SALUDA
# 7. SANTEE
# 8. SAVANNAH

basin_impairments = imp %>%
  group_by(BASIN) %>%
  summarize(n = n())

basin_impairments_1998 = imp_1998 %>%
  group_by(BASIN) %>%
  summarize(n = n())

basin_impairments_1998

basin_impairments_1998_2018 = data.frame(Basins = c("Broad", "Catawba", "Edisto", "Peedee", "Salkehatchie", "Saluda", "Santee", "Savannah"), 
                                         Year_1998 = c(82, 53, 18, 55, 36, 54, 41, 56),
                                         Year_2018 = c(79, 113, 100, 251, 144, 133, 181, 117))

basin_impairments_1998_2018_longer = basin_impairments_1998_2018 %>%
  pivot_longer(cols = c("Year_1998", "Year_2018"), names_to = "Years",
               values_to = "Impairments")

ggplot(data = basin_impairments_1998_2018_longer) + 
  geom_col(aes(y = Impairments, x = Basins, fill = Basins)) +
  facet_wrap(~Years) +
  xlab("Basin") + 
  ylab("# of Waterbody Impairments") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle("Waterbody Impairments Within Each Basin")


