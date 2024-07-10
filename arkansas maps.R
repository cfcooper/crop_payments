# arkansas maps


library(dplyr)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)


rm(list=ls()) # Caution: this clears the Environment

#windowsFonts(A = windowsFont("Times New Roman"))

## import area data

agland <- read.csv("mapdata/arkansasacres.csv")
agland$state <- tolower(agland$state)
agland$county <- tolower(agland$county)

areadat <- read.csv("mapdata/ararea.csv")
areadat$area <- areadat$area*640
areadat$county <- tolower(areadat$county)
areadat <- left_join(areadat, agland)

areadat$percent <- (areadat$value/areadat$area)*100

countydat <- map_data("county")
countydat <- countydat[countydat$region %in% c("arkansas"),]

areadat$code <- paste(areadat$state,areadat$county,sep="_")
countydat$code <- paste(countydat$region,countydat$subregion,sep="_")

lookupcode <- split(areadat$percent, areadat$code)
countydat$value <- ifelse(countydat$code %in% names(lookupcode), unlist(lookupcode[countydat$code]), "")
countydat$value <- as.numeric(countydat$value)



c <- ggplot() + geom_polygon(data = countydat, aes(x = long, y = lat, group = group,fill = value), color = "#d8d8d8", size = .02) +
  coord_map()+
  theme_void() + scale_fill_continuous(
    low = "#dde5b6",
    high = "#718355",
    guide = "colorbar",
    na.value = "#d8d8d8") +
  labs(
    title = "Arkansas Ag Production",
    subtitle = "Percentage of County Area in Ag Land"
  )
c


