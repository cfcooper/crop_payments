
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

RMASOB <- read.csv("RMA_SOB.csv")
croplist <- read.csv("croptypelist.csv")


RMASOB23 <- read.delim("sobcov_2023.txt", 
                      sep ="|", header = FALSE, dec =".")
RMASOB22 <- read.delim("sobcov_2022.txt", 
                       sep ="|", header = FALSE, dec =".")

RMASOB_comb <- rbind(RMASOB22,RMASOB23)

colnames <- read.csv("colnames.csv")

match(colnames[,"old"], names(RMASOB_comb))

names(RMASOB_comb)[match(colnames[,"old"], names(RMASOB_comb))] = colnames[,"new"]


RMASOB <- merge(RMASOB, croplist)

RMASOB$statename <- tolower(RMASOB$statename)
RMASOB$county <- tolower(RMASOB$county)


WFRP <- RMASOB[RMASOB$insuranceplanabrv %in% c("WFRP"),]

RMASOB <- RMASOB[!RMASOB$croptype %in% c("non"),]
RMASOB <- RMASOB[!RMASOB$commodity %in% c("All Other Commodities"),]


# general insurance






# WFRP 

WFRP_state <- WFRP %>% group_by(statename) %>% summarise(sum = sum(policiessold))
WFRP_county <- WFRP %>% group_by(statename, county) %>% summarise(sum = sum(policiessold))

sum(WFRP_state$sum)

# WFRP map


countydat <- map_data("county")
statedat <- map_data("state")

WFRP_state$percent <- (WFRP_state$sum/2128)*100

statedat <- left_join(statedat,WFRP_state, by = join_by(region == statename), keep = FALSE, relationship = "many-to-many")
statedat$sum <- as.numeric(statedat$sum)



statedat$brks <- cut(statedat$percent, 
                   breaks=c(0, 5, 10, 20, 35), 
                   labels=c("0 - 5%", "6 - 10%", "10 - 20%", 
                            "20 - 35%"))


c <- ggplot() + geom_polygon(data = statedat, aes(x = long, y = lat, group = group,fill = brks), color = "#e9f5db", size = .02) +
  coord_map()+
  theme_void() + scale_fill_manual(values = c("#cfe1b9", "#b5c99a", "#97a97c","#718355"), na.value = "#cfe1b9") +
  labs(
    title = "WFRP Participation",
    subtitle = "x x x"
  )
c


# state map

WFRP_county$code <- paste(WFRP_county$statename,WFRP_county$county,sep="_")
countydat$code <- paste(countydat$region,countydat$subregion,sep="_")

countydat <- left_join(countydat,WFRP_county, by = join_by(code == code), keep = FALSE, relationship = "many-to-many")
countydat$sum <- as.numeric(countydat$sum)

c <- ggplot() + geom_polygon(data = countydat, aes(x = long, y = lat, group = group,fill = sum), color = "#d8d8d8", size = .02) +
  coord_map()+
  theme_void() + scale_fill_continuous(
    low = "#cfe1b9",
    high = "#718355",
    guide = "colorbar",
    na.value = "#d8d8d8") +
  labs(
    title = "WFRP Participation",
    subtitle = "x x x"
  )
c
