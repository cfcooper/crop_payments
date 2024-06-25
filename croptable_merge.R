
library(dplyr)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)


rm(list=ls()) # Caution: this clears the Environment

#windowsFonts(A = windowsFont("Times New Roman"))

## upload files

fsa2023 <- read.csv("FSAcounty2023.csv")
nct2023 <- read.csv("NCT2023.csv")
payment2023 <- readRDS("payment2023.rds")
payment2023 <- payment2023 %>% rename("state" = "State.FSA.Name", "county" = "County.FSA.Name")

nct2023$cropusename <- tolower(nct2023$cropusename)
fsa2023$cropusename <- tolower(fsa2023$cropusename)
nct2023 <- nct2023[, !names(nct2023) %in% "plantingperiod"]
nct2023 <- nct2023[!duplicated(nct2023), ]


napacres2023 <- inner_join(nct2023, fsa2023, unmatched = "drop")
#napacres2023$planted_failedacres <- as.numeric(napacres2023$planted_failedacres)

croplist <- nct2023[, c("cropname", "cropnametype","cropusename","irrigation")]

croplist <- croplist[!duplicated(croplist), ]
specialtycroplist <- croplist[!croplist$cropusename %in% c("forage","grain","grazing","silage","seed"),]

napacres2023_spec <- right_join(napacres2023,specialtycroplist)
napacres2023$type <- if_else(napacres2023$cropusename %in% c("forage","grain","grazing","silage","seed"),"N","S")

county_cropratio <- napacres2023 %>% group_by(state, county, type) %>% summarise(sum = sum(planted_failedacres)) 
specialty_countylist <- county_cropratio[county_cropratio$type %in% c("S"),]


full_countylist <- fsa2023[, c("state", "county")]
full_countylist <- full_countylist[!duplicated(full_countylist), ]

non_specialty <- full_countylist %>%
  anti_join(specialty_countylist, by = c("state", "county"))

non_specialty <- inner_join(payment2023, non_specialty, by = c("state", "county"))

single_obs <- county_cropratio %>%
  group_by(state, county) %>%
  filter(n() == 1)

duplicated_obs <- single_obs %>%
  mutate(source = "fake")



duplicated_obs <- single_obs %>%
  mutate(duplicate = TRUE)

AR_napacres2023 <- napacres2023[napacres2023$state %in% c("Arkansas"),]
AR_fsaacres2023 <- fsa2023[fsa2023$state %in% c("Arkansas"),]




