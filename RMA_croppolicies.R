
library(dplyr)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(maps)
library(mapdata)
library(viridis)


rm(list=ls()) # Caution: this clears the Environment


## upload files


croplist <- read.csv("croptypelist.csv")

RMASOB <- list.files(path="RMAdata", pattern="*txt", full.names = T)
RMASOB <- lapply(RMASOB, FUN=read.delim, sep ="|", header=FALSE, dec =".")
RMASOB <- do.call("rbind", RMASOB)



colnames <- read.csv("colnames.csv")

match(colnames[,"old"], names(RMASOB))

names(RMASOB)[match(colnames[,"old"], names(RMASOB))] = colnames[,"new"]

RMASOB$commodity <- trimws(RMASOB$commodity)
RMASOB <- left_join(RMASOB, croplist)


RMASOB$county <- tolower(RMASOB$county)

RMASOB$insuranceplanabrv <- trimws(RMASOB$insuranceplanabrv)

WFRP <- RMASOB[RMASOB$insuranceplanabrv %in% c("WFRP"),]

RMA_sum <- RMASOB %>% group_by(insuranceplanabrv) %>% summarise(sum = sum(policiessold))


#RMASOB <- RMASOB[!RMASOB$croptype %in% c("non"),]
#RMASOB <- RMASOB[!RMASOB$commodity %in% c("All Other Commodities"),]


# general insurance






# WFRP 

WFRP_state <- WFRP %>% group_by(year, stateabrv) %>% summarise(sum = sum(policiessold))
WFRP_year <- WFRP %>% group_by(year) %>% summarise(sum = sum(policiessold))
sum(WFRP_state$sum)
mean(WFRP_year$sum)
WFRP_state <- WFRP_state %>% 
  pivot_wider(names_from = year, values_from = sum)
WFRP_state[is.na(WFRP_state)] <- 0
WFRP_county <- WFRP %>% group_by(year, stateabrv, county) %>% summarise(sum = sum(policiessold))



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

# yearly participation




year <- ggplot() + geom_line(data = WFRP_year, aes(x = year, y = sum), color = "#718355") +
  theme_classic() +
  labs(
    title = "WFRP Annual Participation",
    subtitle = "All WFRP policies by year",
    y = "policies"
  )
year







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
