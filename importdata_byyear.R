

library(dplyr)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(readxl)

rm(list=ls()) # Caution: this clears the Environment

#windowsFonts(A = windowsFont("Times New Roman"))

## create NAP files

#2023
rawdat <- list.files(path="byyear/2023", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_23 <- rawdat[rawdat$`Accounting Program Code` %in% c("2920", "2775","2695"),]
NAPdata_23 <- rename(NAPdata_23, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                  programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_23, "NAPdata/NAPdata_23.csv")

#2022
rawdat <- list.files(path="byyear/2022", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_22 <- rawdat[rawdat$`Accounting Program Code` %in% c("2920", "2775","2695"),]
NAPdata_22 <- rename(NAPdata_22, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_22, "NAPdata/NAPdata_22.csv")

#2021
rawdat <- list.files(path="byyear/2021", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_21 <- rawdat[rawdat$`Accounting Program Code` %in% c("2920", "2775","2695"),]
NAPdata_21 <- rename(NAPdata_21, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_21, "NAPdata/NAPdata_21.csv")


#2020
rawdat <- list.files(path="byyear/2020", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_20 <- rawdat[rawdat$`Accounting Program Code` %in% c("2920", "2775","2695"),]
NAPdata_20 <- rename(NAPdata_20, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_20, "NAPdata/NAPdata_20.csv")


#2019
rawdat <- list.files(path="byyear/2019", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_19 <- rawdat[rawdat$`Accounting Program Code` %in% c("2920", "2775","2695"),]
NAPdata_19 <- rename(NAPdata_19, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_19, "NAPdata/NAPdata_19.csv")


#2018
rawdat <- list.files(path="byyear/2018", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_18 <- rawdat[rawdat$`Accounting Program Code` %in% c("2920", "2775","2695"),]
NAPdata_18 <- rename(NAPdata_18, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_18, "NAPdata/NAPdata_18.csv")


#2017
rawdat <- list.files(path="byyear/2017", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_17 <- rawdat[rawdat$`Accounting Program Code` %in% c("2920", "2775","2695"),]
NAPdata_17 <- rename(NAPdata_17, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_17, "NAPdata/NAPdata_17.csv")


#2016
rawdat <- list.files(path="byyear/2016", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_16 <- rawdat[rawdat$`Accounting Program Code` %in% c("2920", "2775","2695"),]
NAPdata_16 <- rename(NAPdata_16, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_16, "NAPdata/NAPdata_16.csv")

#2015
rawdat <- list.files(path="byyear/2015", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_15 <- rawdat[rawdat$`Accounting Program Code` %in% c("2920", "2775","2695"),]
NAPdata_15 <- rename(NAPdata_15, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_15, "NAPdata/NAPdata_15.csv")


#2014
rawdat <- list.files(path="byyear/2014", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_14 <- rawdat[rawdat$`Accounting Program Code` %in% c("2920", "2775","2695"),]
NAPdata_14 <- rename(NAPdata_14, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_14, "NAPdata/NAPdata_14.csv")


#2013
rawdat <- list.files(path="byyear/2013", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_13 <- rawdat[rawdat$`Accounting Program Code` %in% c("2920", "2775","2695"),]
NAPdata_13 <- rename(NAPdata_13, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_13, "NAPdata/NAPdata_13.csv")

#2012
rawdat <- list.files(path="byyear/2012", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_12 <- rawdat[rawdat$`Accounting Program Code` %in% c("2920", "2775","2695"),]
NAPdata_12 <- rename(NAPdata_12, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_12, "NAPdata/NAPdata_12.csv")

#2011
rawdat <- list.files(path="byyear/2011", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_11 <- rawdat[rawdat$`Accounting Program Code` %in% c("2775","2695","2622"),]
NAPdata_11 <- rename(NAPdata_11, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_11, "NAPdata/NAPdata_11.csv")

#2010
rawdat <- list.files(path="byyear/2010", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_10 <- rawdat[rawdat$`Accounting Program Code` %in% c("2775","2695","2622"),]
NAPdata_10 <- rename(NAPdata_10, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_10, "NAPdata/NAPdata_10.csv")

#2009
rawdat <- list.files(path="byyear/2009", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_09 <- rawdat[rawdat$`Accounting Program Code` %in% c("2775","2695","2622"),]
NAPdata_09 <- rename(NAPdata_09, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_09, "NAPdata/NAPdata_09.csv")


#2008
rawdat <- list.files(path="byyear/2008", pattern="*xlsx", full.names = T)
rawdat <- lapply(rawdat, read_excel)
rawdat <- do.call("rbind", rawdat)
NAPdata_08 <- rawdat[rawdat$`Accounting Program Code` %in% c("2775","2695","2622"),]
NAPdata_08 <- rename(NAPdata_08, programyear = 'Accounting Program Year', program = 'Accounting Program Description', 
                     programcode = 'Accounting Program Code', paymentdate = 'Payment Date')
write.csv(NAPdata_08, "NAPdata/NAPdata_08.csv")
















