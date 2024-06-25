
library(dplyr)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(maps)
library(mapdata)
library(viridis)


rm(list=ls()) # Caution: this clears the Environment

#windowsFonts(A = windowsFont("Times New Roman"))

## upload files

fsa2023 <- read.csv("FSAcounty2023.csv")

fsa2023$cropusename <- tolower(fsa2023$cropusename)

croplist <- fsa2023[, c("cropname", "cropnametype","cropusename")]

croplist <- croplist[!duplicated(croplist), ]
specialtycroplist <- croplist[!croplist$cropusename %in% c("forage","grain","grazing","silage"),]
specialtycroplist <- specialtycroplist[!specialtycroplist$cropname %in% c("CRP","WHEAT", "COVER CROP", "COTTON  UPLAND", "ALFALFA","BARLEY","BUCKWHEAT",
                                                                          "CANOLA","CLOVER","CONSERVATION STEWARDSHIP PROGRAM", "CRUSTACEAN", "COTTON  ELS",
                                                                          "EMERGENCY WATERSHED/FLOODPLAIN", "EQIP", "FALLOW", "FINFISH", "FLAX", "GRASSLAND RESERVE PROGRAM",
                                                                          "HEMP", "IDLE", "MILLET", "MIXED FORAGE", "MOLLUSK", "OATS", "RAPESEED", "RICE", "RICE  WILD", 
                                                                          "RYE", "SORGHUM", "SORGHUM  DUAL PURPOSE", "SORGHUM FORAGE", "SOYBEANS", "SUGARCANE", "SUNN HEMP",
                                                                          "TREES  TIMBER", "TRITICALE", "VETCH", "WETLAND BANK RESERVE", "WETLAND RESERVE PROGRAM",
                                                                          "WILDLIFE FOOD PLOT", "WILDLIFE HABITAT INCENTIVE PROGRAM"),]

specialtycroplist$type <- "S"
fsa2023_spec <- left_join(fsa2023,specialtycroplist)
fsa2023_spec <- fsa2023_spec %>% replace_na(list(type = "N"))
county_cropratio <- fsa2023_spec %>% group_by(state, county, type) %>% summarise(sum = sum(planted_failedacres)) 

county_cropratio2 <- county_cropratio %>% group_by(state, county) %>% summarise(total = sum(sum))
county_cropratio <- left_join(county_cropratio,county_cropratio2)
county_cropratio <- county_cropratio[county_cropratio$type %in% c("S"),]
county_cropratio$percent <- (county_cropratio$sum/county_cropratio$total)*100


#fips_data <- read.csv("mapdata/fips.csv")
#fips_data$statelong <- tolower(fips_data$statelong)
#fips_data$county <- tolower(fips_data$county)


countydat <- map_data("county")
statedat <- map_data("state")

county_cropratio$state <- tolower(county_cropratio$state)
county_cropratio$county <- tolower(county_cropratio$county)

county_cropratio$code <- paste(county_cropratio$state,county_cropratio$county,sep="_")
countydat$code <- paste(countydat$region,countydat$subregion,sep="_")

lookupcode <- split(county_cropratio$percent, county_cropratio$code)
countydat$percent <- ifelse(countydat$code %in% names(lookupcode), unlist(lookupcode[countydat$code]), "")
countydat$percent <- as.numeric(countydat$percent)


s <- ggplot() +
  geom_polygon(data = statedat, aes(x = long, y = lat, group = group), fill = "light grey", color = "white")
s

c <- ggplot() + geom_polygon(data = countydat, aes(x = long, y = lat, group = group,fill = percent), color = "white", size = .08) +
  coord_map()+
  theme_void() +
  labs(
    title = "Specialty Crop Production",
    subtitle = "Percentage of County Ag Land in Specialty Crop Production"
  )
c


