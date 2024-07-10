
library(dplyr)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)


rm(list=ls()) # Caution: this clears the Environment

#windowsFonts(A = windowsFont("Times New Roman"))

## import tree data

xmas <- read.csv("xmastrees.csv")



countydat <- map_data("county")
statedat <- map_data("state")

xmas$state <- tolower(xmas$state)
xmas$county <- tolower(xmas$county)
xmas <- xmas[!xmas$value %in% c("X"),]

xmas$code <- paste(xmas$state,xmas$county,sep="_")
countydat$code <- paste(countydat$region,countydat$subregion,sep="_")

lookupcode <- split(xmas$value, xmas$code)
countydat$value <- ifelse(countydat$code %in% names(lookupcode), unlist(lookupcode[countydat$code]), "")
countydat$value <- as.numeric(countydat$value)


s <- ggplot() +
  geom_polygon(data = statedat, aes(x = long, y = lat, group = group), fill = "light grey", color = "white")
s

c <- ggplot() + geom_polygon(data = countydat, aes(x = long, y = lat, group = group,fill = value), color = "#d8d8d8", size = .02) +
  coord_map()+
  theme_void() + scale_fill_continuous(
    low = "#dde5b6",
    high = "#718355",
    guide = "colorbar",
    na.value = "#d8d8d8") +
  labs(
    title = "Christmas Tree Production",
    subtitle = "Percentage of County Ag Land in Specialty Crop Production"
  )
c


## import veg total data ------------------------------------------------------------------------------------------

veg <- read.csv("vegtotals.csv")

veg$state <- tolower(veg$state)
sum(veg$value)
veg$percent <- (veg$value/4305685)*100

statedat <- left_join(statedat,veg, by = join_by(region == state), keep = FALSE, relationship = "many-to-many")
statedat$value <- as.numeric(statedat$value)


s <- ggplot() +
  geom_polygon(data = statedat, aes(x = long, y = lat, group = group), fill = "light grey", color = "white")
s

c <- ggplot() + geom_polygon(data = statedat, aes(x = long, y = lat, group = group,fill = percent), color = "#d8d8d8", size = .02) +
  coord_map()+
  theme_void() + scale_fill_continuous(
    low = "#dce6e1",
    high = "#3f7c63",
    guide = "colorbar",
    na.value = "#d8d8d8") +
  labs(
    title = "Figure 1",
    subtitle = "State share of total US harvested vegetable acreage, 2022"
  )
c




