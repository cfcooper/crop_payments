

# Creating a dot density map of the var - ALL INTEREST EXP
rm(list = ls(all = TRUE))

# `httr` provides parse_url(), build_url(), GET(), and content() functions for
# requesting data from API servers via the HTTP protocol, and handling the
# responses that are sent back
library(httr)

# `jsonlite` provides fromJSON() for converting the response from the JSON-format
# data NASS sends us into an R-style dataframe.
library(jsonlite)

# `magrittr` provides pipe operators like %>%, %<>%, and %T>%
library(magrittr)

# `tidyverse` and `data.table` provide all the data reshaping and filtering stuff
library(tidyverse)
library(data.table)
library(snakecase)

# other packages you need to load
library(ggplot2)
library(geofacet)
library(devEMF)
library(systemfonts)
library(svglite)
library(statebins)
library(gridExtra)
library(grid)
library(readxl)
library(purrr)
library(Census2016)
library(officer)
library(utils)
library(sf)

library(maps)
library(ggthemes)
library(patchwork)
library(viridis)
library(scales)
library(usdata)
library(proxy)
library(tigris)
options(tigris_use_cache = TRUE)


library(tidycensus)
library(socviz)
library(tmap)

# Step 1. Query NASS QuickStats via API

# With low-security APIs like this one, I like to just set my API token right
# here at the top.
my_api_token <- '44DE50DD-ECE5-338E-A0FB-96A1CE0E9319'

# Convert the address of the NASS API into a format that allows the `httr`
# package to differentiate between whom we're asking for data, which data we're
# requesting from them, and our API token/password, etc. Start by using the
# parse_url() function on the URL of the NASS API server:
nass_api_url_total <- parse_url('https://quickstats.nass.usda.gov/api/api_GET')
nass_api_url_dtc <- parse_url('https://quickstats.nass.usda.gov/api/api_GET')


# Note how the URL object has a bunch of different elements we can change:
#print(nass_api_url)

### insert names of variables you want to pull from the Census of Ag in the short_desc area. 
### If you want the share of sales or acres in a county from specialty crops, you'll need one variable to be the sales or acres 
### of specialty crops and one value to be total sales or acres 

nass_api_url_total$query <- list(source_desc                 = 'CENSUS',
                                 freq_desc                   = 'ANNUAL' ,
                                 reference_period_desc       = 'YEAR' ,
                                 year                        = '2022',
                                 short_desc                  = 'AG LAND, CROPLAND - ACRES' ,
                                 domain_desc                 = 'TOTAL' ,
                                 agg_level_desc              = 'COUNTY',
                                 key                         = my_api_token)

nass_api_url_dtc$query <- list(source_desc                 = 'CENSUS',
                               freq_desc                   = 'ANNUAL' ,
                               reference_period_desc       = 'YEAR' ,
                               year                        = '2022',
                               short_desc                  = 'VEGETABLE TOTALS, IN THE OPEN - ACRES HARVESTED' ,
                               domain_desc                 = 'TOTAL' ,
                               agg_level_desc              = 'COUNTY',
                               key                         = my_api_token)



# Use build_url() to squish the URL object into a web address that you could,
# if you wanted to, copy and paste into your web browser and see what the API
# server returns right there. But instead we'll submit a `GET()` request from R,
# separate the actual `content()` of the response from the headers and other
# transmission-related data, convert the JSON format of the content into an R
# dataframe using `fromJSON()`, pick the actual $data element out of the content
# and convert it into a `tidyverse`-style `tibble` with `as_tibble()`, and then
# finally convert each column of the tibble into the appropriate data class:

build_url(nass_api_url_total) %T>% print() 
build_url(nass_api_url_dtc) %T>% print()



data_tbl_total <-
  build_url(nass_api_url_total) %>%
  GET() %>%
  content(type = 'text') %>%
  fromJSON() %>%
  .$data %>%
  as_tibble() %>%
  type_convert()

data_tbl_dtc <-
  build_url(nass_api_url_dtc) %>%
  GET() %>%
  content(type = 'text') %>%
  fromJSON() %>%
  .$data %>%
  as_tibble() %>%
  type_convert()

# create GEOID variable that is the state and county fips codes together
data_tbl_total$GEOID <- with(data_tbl_total, paste0(state_fips_code, county_code))
data_tbl_dtc$GEOID <- with(data_tbl_dtc, paste0(state_fips_code, county_code))


# Note this next step is in order to clean the Census data
# NASS has written withhelds as character strings. Replace with NAs
data_red_total <- data_tbl_total %>%
  #filter(state_fips_code != c("02")) %>%
  #filter(state_fips_code != c("15")) %>%
  select(c("GEOID","Value","short_desc")) %>%
  dplyr::rename(total = Value) %>%
  mutate(total = replace(total, total == "(D)", NA)) %>%
  mutate(value = as.numeric(gsub(",", "", total))) %>%
  select(-c(total)) %>%
  group_by(GEOID) %>% 
  dplyr::summarise(total = sum(value))

data_red_dtc <- data_tbl_dtc %>%
  #filter(state_fips_code != c("02")) %>%
  #filter(state_fips_code != c("15")) %>%
  select(c("GEOID","Value","short_desc")) %>%
  dplyr::rename(dtc = Value) %>%
  mutate(dtc = replace(dtc, dtc == "(D)", NA)) %>%
  mutate(value = as.numeric(gsub(",", "", dtc))) %>%
  select(-c(dtc)) %>%
  group_by(GEOID) %>% 
  dplyr::summarise(dtc = sum(value))


# merge datasets based on geoid
# Put all data frames into a list
df_list <- list(data_red_total, data_red_dtc)

# Merge all data frames together
data_red<-df_list %>% reduce(full_join, by='GEOID')

# compute dtc as share of total
data_red <- data_red %>%
  mutate(share = ifelse(is.na(total) | is.na(dtc), NA_real_,
                        (dtc / 4305685)*100))



# drop everything but change and geoid
data_red <- data_red[, !(names(data_red) %in% c("dtc", "total"))]

# Get data from tidycensus - you need it because it's already in proper spatial polygon format
# Using this as a base will allow you to properly plot the dot density map

#census_api_key("ccfc48f3f868f52f269fef53920f92881cbd95f5", install = TRUE)

race_vars <- c(
  White = "P2_005N"
)

county_race <- get_decennial(
  geography = "county",
  variables = race_vars,
  geometry = TRUE,
  year = 2020
) %>%
  filter(GEOID <= 72000)

alaska_codes <- read_csv("./alaska_dist.csv") 

# Now join it with your data
re_sf <- county_race %>% left_join(., data_red, by = "GEOID" ) %>%
  select(-c("variable","value")) %>%
  mutate(value = share) %>%
  select(-c("share")) %>%
  mutate(variable = "DTC sales as a share of total saless") %>%
  left_join(., alaska_codes, by = "GEOID") %>%
  mutate(GEOID_NEW = ifelse(is.na(NASS_GEOID), GEOID, NASS_GEOID)) %>%
  mutate(across(c(value), ~case_when(NASS_DIST >= 10 & is.na(value) ~ 0, TRUE ~ .))) %>%
  select(-c("NASS_GEOID","NASS_DIST","DIST_NAME","GEOID")) %>%
  rename(GEOID = GEOID_NEW)

re_sf1 <- re_sf %>%
  group_by(GEOID) %>%
  summarize(Sum_value = sum(value)) %>%
  mutate(variable = "DTC sales as a share of total sales") %>%
  rename(value = Sum_value) %>%
  mutate(across(c("value"), ~ na_if(., 0)))

#Now transform it into the correct Albers projection, w/ Alaska, Hawaii, and PR rescaled
re_sf_alt <- st_transform(re_sf1, crs =5070) %>%
  shift_geometry()


#To make sure you only are keeping distinct topologies
md_base <- re_sf_alt %>% 
  distinct(GEOID, .keep_all =TRUE)

# drop PR geoid - 72
md_base<-md_base %>%
  filter(!grepl("^72", GEOID))

# FINAL MAP 

bbox_new <- st_bbox(md_base) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
# bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] - (0.05 * yrange) # ymin - bottom
# bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

breaks <- c(-Inf, 5, 10, 20, Inf)

# Create labels for the legend
labels <- c("0 - 5%", "6 - 10%", "10 - 20%", 
            "20 - 35%")

# Create a color palette for the breaks
colors <- c("#dce6e1", "#b8cdc4","#92b3a4","#6a9884", "#3f7c63")

# Create the map
intexpmap<- tm_shape(md_base) +
  tm_borders(lwd = 0.4, col = "grey60") +  # Add county borders
  tm_fill(col = "value", style = "fixed", breaks = breaks, palette = colors,
          title = "Percent of Ag Land in Vegetables", labels = labels,
          textNA = "Missing data", colorNA = "gray90") +
  tm_layout(legend.outside = TRUE, legend.position = c("right", "top"),
            legend.title.size = 0.8, legend.text.size = 0.7,
            legend.format = list(fun = function(x) formatC(x, format = "d"))) +
  tm_basemap(server = "OpenStreetMap")  # Choose a basemap (you can customize this)


intexpmap

# Now let us output it as an Adobe file

tmap_save(intexpmap, "AW_FarmBill.png")
tmap_save(intexpmap, "AW_FarmBill.eps")


# drop data to export
md_base <- st_drop_geometry(md_base)
new_df <- md_base[, c("GEOID", "value")]
write.csv(new_df, "AW_Spalding.csv", row.names=FALSE)





############## map of total sales ##########################

md_dots = as_dot_density(
  md_base,
  value = "value",
  values_per_dot = 2500000,
  group = "variable"
)