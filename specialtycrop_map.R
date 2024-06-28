
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
specialtycroplist <- specialtycroplist[!specialtycroplist$cropname %in% c("CRP","WHEAT", "COVER CROP", "COTTON  UPLAND", "ALFALFA","BARLEY","","BIRDSFOOT/TREFOIL","BUCKWHEAT",
                                                                          "CANOLA","CAMELINA","CLOVER","CONSERVATION STEWARDSHIP PROGRAM", "CRUSTACEAN", "COTTON  ELS",
                                                                          "EMERGENCY WATERSHED/FLOODPLAIN", "EQIP", "FALLOW", "FINFISH", "FLAX", "GRASSLAND RESERVE PROGRAM",
                                                                          "HEMP", "IDLE", "MILLET", "MIXED FORAGE", "MOLLUSK", "OATS", "RAPESEED", "RICE", "RICE  WILD", 
                                                                          "RYE", "SORGHUM", "SORGHUM  DUAL PURPOSE", "SORGHUM FORAGE", "SOYBEANS", "SUGARCANE", "SUNN HEMP",
                                                                          "TREES  TIMBER", "TRITICALE", "VETCH", "WETLAND BANK RESERVE", "WETLAND RESERVE PROGRAM",
                                                                          "WILDLIFE FOOD PLOT", "WILDLIFE HABITAT INCENTIVE PROGRAM","PEANUTS","PENNYCRESS","TOBACCO  CIGAR WRAPPER","PSYLLIUM",
                                                                          "SKIP ROWS","WATER IMPOUNDMENT STRUCTURE","WATERBANK"),]

specialtycroplist$code <- paste(specialtycroplist$cropname,specialtycroplist$cropnametype,sep="_")
specialtycroplist <- specialtycroplist[!specialtycroplist$code %in% c("CORN_YELLOW","CORN_ORNAMENTAL","CORN_POPCORN","CORN_CORN NUTS",
                                                                      "CORN_TROPICAL","CORN_WHITE","CORN_BLUE","CORN_STRAWBERRY POPCORN","BEANS_BUTTER",
                                                                      "BEANS_PINTO", "BEANS_FLAT SMALL WHITE","BEANS_GARBANZO  LG KABULI (CHICKPEAS)",
                                                                      "BEANS_CRANBERRY","BEANS_CANARIO - YELLOW","BEANS_GARBANZO  SM DESI (CHICKPEAS)","BEANS_FAVA/FABA",
                                                                      "BEANS_LUPINE","BEANS_YELLOW EYE","BEANS_SMALL RED","BEANS_ANASAZI","BEANS_GREAT NORTHERN",
                                                                      "BEANS_POLE","BEANS_BLACK TURTLE","BEANS_DARK RED KIDNEY","BEANS_MAYOCOBA","BEANS_LIGHT RED KIDNEY",
                                                                      "BEANS_GARBANZO  SM KABULI (CHICKPEAS)","BEANS_KENTUCKY BLUE","BEANS_SHELLI",
                                                                      "BEANS_WING","BEANS_YARDLONG","BEANS_ADZUKI","BEANS_SMALL WHITE/NAVY",
                                                                      "BEANS_PINK","BEANS_ROMA","BEANS_WHITE KIDNEY","BEANS_MUNG","BEANS_LONG",
                                                                      "BEANS_WHITE HALF RUNNER","BEANS_JACOBS CATTLE","BEANS_SOLDIER","BEANS_CHINESE STRING",
                                                                      "BEANS_OCTOBER","BEANS_PAPDAI VALOR","BEANS_KINTOKI","BEANS_TEBO","SUNFLOWERS_SUNFLOWER OIL",""),]
specialtycroplist$code <- paste(specialtycroplist$cropname,specialtycroplist$cropusename,sep="_")
specialtycroplist <- specialtycroplist[!specialtycroplist$code %in% c("GRASS_left standing","GRASS_seed","GRASS_green manure","GRASS_processed"),]

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
countydat$factor <- ifelse(isna(countydat$percent), "0","0")
countydat$factor <- ifelse(countydat$percent < 5, "1","0")
countydat$factor <- ifelse(countydat$percent > 5 & countydat$percent < 10, "2",countydat$factor)
countydat$factor <- ifelse(countydat$percent > 10 & countydat$percent < 20, "3",countydat$factor)
countydat$factor <- ifelse(countydat$percent > 20 & countydat$percent < 50, "4",countydat$factor)
countydat$factor <- ifelse(countydat$percent > 50, "5",countydat$factor)

countydat$factor = factor(countydat$factor, levels = c("1","2", "3", "4", "5"))
vcolors <- c("#ffc9bb","#ff9090","#ff5757","#D32431","#910000")

s <- ggplot() +
  geom_polygon(data = statedat, aes(x = long, y = lat, group = group), fill = "light grey", color = "white")
s

c <- ggplot() + geom_polygon(data = countydat, aes(x = long, y = lat, group = group,fill = factor), color = "white", size = .08) +
  coord_map()+
  theme_void() + scale_fill_manual(values=vcolors, name="Specialty Acres Percentage") +
  labs(
    title = "Specialty Crop Production",
    subtitle = "Percentage of County Ag Land in Specialty Crop Production"
  )
c

countyonly <- select(fsa2023, c("code","fips","soypercentbin", "soybeanpercent"))

