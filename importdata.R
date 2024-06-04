

library(dplyr)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(readxl)

rm(list=ls()) # Caution: this clears the Environment

#windowsFonts(A = windowsFont("Times New Roman"))

## create NAP files

rawdat <- list.files(path="NAPdata", pattern="*csv", full.names = T)
rawdat <- lapply(rawdat, read.csv)
rawdat <- do.call("rbind", rawdat)

rawdat <- rawdat[!rawdat$Formatted.Payee.Name %in% c("CCC","Farm Service Agency","FARM SERVICE AGENCY/COMMODITY CRE",
                                                     "USDA-Farm Service Agency","FARM CREDIT OF WESTERN OKLAHOMA P",
                                                     "FARM SERVICE AGENCY", "FARM SERVICE AGENCY/COMMODITY CREDIT CORPORATION",
                                                     "USDA-FARM SERVICE AGENCY","USDA/Farm Service Agency",
                                                     "Farm Service Agency/Commodity Cre","AG PREFERENCE CREDIT ASSN PCA"),]


year_sum <- rawdat %>% group_by(programyear, State.Abbreviation) %>% summarise(count = n()) 
year_sum <- year_sum[!year_sum$State.Abbreviation %in% c("AP", "AS","PR","GU","L5","NA","MP"),]
year_sum_top5 <- year_sum[year_sum$State.Abbreviation %in% c("TX","OK","NM","SD","CA"),]


data_wide <- spread(year_sum, key = programyear, value = count) %>%
  mutate_all(~ replace(., is.na(.), 0))

data_long <- spread(year_sum, key = State.Abbreviation, value = count) %>%
  mutate_all(~ replace(., is.na(.), 0))

write.csv(data_wide, "NAPbyyear.csv")


ggplot(year_sum_top5, aes(x = programyear, y = count, color = State.Abbreviation)) +
  geom_line() +
  labs(title = "Payments Over Time by State",
       subtitle = "Top 5 States",
       x = "Year",
       y = "Payments") +
  theme_minimal()



year_total <- rawdat %>% group_by(programyear, State.Abbreviation) %>% summarise(sum = sum(Disbursement.Amount)) 
state_total <- rawdat %>% group_by(State.Abbreviation) %>% summarise(sum = sum(Disbursement.Amount)) 

state_year_avg <- rawdat %>% group_by(programyear, State.Abbreviation) %>% summarise(avg = mean(Disbursement.Amount)) 


write.csv(year_total, "NAPtotal_byyear.csv")

state_year_top5 <- state_year_avg[state_year_avg$State.Abbreviation %in% c("TX","OK","NM","SD","CA"),]

ggplot(state_year_top5, aes(x = programyear, y = avg, color = State.Abbreviation)) +
  geom_line() +
  labs(title = "Avg Payments Over Time by State",
       subtitle = "xxx",
       x = "Year",
       y = "Payments") +
  theme_minimal()


## individual payee

payee_sum <- rawdat %>% group_by(State.Abbreviation, Formatted.Payee.Name) %>% summarise(count = n())
payee_total <- rawdat %>% group_by(programyear, State.Abbreviation, Formatted.Payee.Name) %>% summarise(sum = sum(Disbursement.Amount))

carpenter_produce <- rawdat[rawdat$Formatted.Payee.Name %in% c("CARPENTER PRODUCE"),]
sum(carpenter_produce$Disbursement.Amount)



payee_sum <- rawdat %>% group_by(State.Abbreviation, Formatted.Payee.Name) %>% summarise(count = n())


state_year_avg <- rawdat %>% group_by(programyear, State.Abbreviation) %>% summarise(avg = mean(Disbursement.Amount)) 




state_year_top5 <- state_year_avg[state_year_avg$State.Abbreviation %in% c("TX","OK","NM","SD","CA"),]

ggplot(year_sum_top5, aes(x = programyear, y = count, color = State.Abbreviation)) +
  geom_line() +
  labs(title = "Payments Over Time by State",
       subtitle = "Top 5 States",
       x = "Year",
       y = "Payments") +
  theme_minimal()


NAPdata$paymentdate <- as.Date(NAPdata$paymentdate, format="%m/%d/%Y")
NAPdata$paymentdate <- format(NAPdata$paymentdate, "%B-%Y")










