# RGAI Rating: 4 to 5
# https://chatgpt.com/share/69962c59-3838-8003-84f4-340e126a124b
# https://chatgpt.com/share/6996104a-dbec-8003-8d0c-0655d888c942

library(data.table)
library(readr)
library(dplyr)
library(tidyverse)
library(fastDummies)

#################################################################################################
# Aggregate nonprofit data by county and year
#################################################################################################

df <- readRDS("mega.rds") # 1773 orgs have "00000" as geoid; 86,752 are missing NTEE code (~6.74%) -- 1,285,846 total orgs

# Split up NTEEV2 into 14 dummy variables (12 categories, 1 unknown category, 1 missing category)
ntee <- df |> 
      select(EIN2, NTEEV2, TAX_YEAR, county.census.geoid) |>
      filter(county.census.geoid != "00000") |>
      dummy_cols(select_columns = c("NTEEV2"), ignore_na = FALSE, remove_selected_columns = TRUE)

# Add aggregate the totals of each NTEEV2 category by county and year
ntee <- ntee |> select(-EIN2) |>
      group_by(county.census.geoid, TAX_YEAR) |>
      summarise(across(everything(), ~sum(.x, na.rm = TRUE))) |>
      ungroup()

# Add in total number of organizations and proportion missing column
ntee <- ntee |> mutate(NUM_ORGS = rowSums(ntee |> select(-county.census.geoid, -TAX_YEAR)),
                       NTEE_NA_PROP = NTEEV2_NA/NUM_ORGS)

# Within each county,year pair, I want to keep track of the proportion of records with missing financial data
# I also want to compute the total revenue, expenses, and assets across that county in that year
# For now, we are dropping records that listed the invalid county code "00000" (1773 orgs)
df <- df |> select(TAX_YEAR, TOT_REV, TOT_EXP, TOT_ASSET, county.census.geoid) |>
      rename(REV = TOT_REV, EXP = TOT_EXP, ASSET = TOT_ASSET) |>
      group_by(county.census.geoid, TAX_YEAR) |> 
      summarize(across(where(is.numeric), ~sum(is.na(.x))/n(), .names = "{.col}_prop_miss"),
                across(any_of(c("REV", "EXP", "ASSET")), ~sum(.x, na.rm = TRUE), .names = "TOT_{.col}"),
                .groups = "drop") |>
      ungroup() |>
      filter(county.census.geoid != "00000")

df <- merge(df, ntee, by = c("county.census.geoid", "TAX_YEAR")) 

# IDs for two counties were changed after 2010. Some datasets use latest ID, others use 2010 ID
df <- df |> mutate(geoid_2010 = county.census.geoid)
df$geoid_2010[df$county.census.geoid == "02158"] <- "02270"
df$geoid_2010[df$county.census.geoid == "46102"] <- "46113"

rm(ntee)
#################################################################################################
# Add location data: county name, latitude & longitude of centroid, state, region, and division
#################################################################################################
counties <- as.data.table(read_csv("counties_data/uscounties.csv", show_col_types = FALSE))
counties <- counties |> select(-county_ascii, - county, - population) |> rename(CENSUS_STATE_ABBR = state_id)

source("../SCRIPTS/clean_helper.R")
counties <- add_regions_and_divisions(counties)

# Some counties must be filled in manually
cnames <- c("Valdez-Cordova Census Area", 
            "Fairfield County", "Hartford County", "Litchfield County", "Middlesex County",
            "New Haven County", "New London County", "Tolland County", "Windham County")
fips <- c("02261", "09001", "09003", "09005", "09007", "09009", "09011", "09013", "09015")
states <- c("AK", "CT", "CT", "CT", "CT", "CT", "CT", "CT", "CT")
state_names <- c("Alaska", 
                 "Connecticut", "Connecticut", "Connecticut", "Connecticut", 
                 "Connecticut", "Connecticut", "Connecticut", "Connecticut")
lats <- c(61.34984, 41.227413, 41.806053, 41.79188, 41.433003, 41.349717, 41.472652, 41.858081, 41.824999)
longs <- c(-145.023141, -73.367061, -72.732916, -73.235404, -72.52278, -72.900204, -72.108634, -72.340978, -71.990702)
regs <- c("WEST",
          "NORTHEAST", "NORTHEAST", "NORTHEAST", "NORTHEAST", 
          "NORTHEAST", "NORTHEAST", "NORTHEAST", "NORTHEAST")
divs <- c("PACIFIC", 
          "NEW-ENGLAND", "NEW-ENGLAND", "NEW-ENGLAND", "NEW-ENGLAND", 
          "NEW-ENGLAND", "NEW-ENGLAND", "NEW-ENGLAND", "NEW-ENGLAND")

additions <- data.frame(cnames, fips, states, state_names, lats, longs, regs, divs)
names(additions) <- names(counties)

counties <- rbind(counties, additions)

df <- merge(df, counties, by.x = "county.census.geoid", by.y = "county_fips", all.x = TRUE) # df and counties use the newer FIPS code not 2010

rm(counties, additions, cnames, divs, fips, lats, longs, regs, state_names, states, compare_pair_dt)
#################################################################################################
# add census crosswalk data (socioeconomic factors)
# Since some of these may be used as pre-treatment variables to adjust for, make sure they never happen AFTER treatment
# So, for the year 2000 my variables need to come from before 2000. For 2010, before 2010.
# The following creates a LAG_YEAR which I will join with TAX_YEAR and make sure the data is always from BEFORE the TAX_YEAR to be safe
#################################################################################################
source("https://raw.githubusercontent.com/UI-Research/nccs-geo/main/get_census_data.R")
census_df <- get_census_data(geo="county", 
                             years=c(1990, 2000, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

dt <- as.data.table(census_df)
dt[, CENSUS_YEAR := as.integer(substr(year, nchar(year)-3, nchar(year)))] # since year column is char with ranges, get last year

# Create all county × LAG_YEAR combinations
lag_years <- CJ(geoid_2010 = unique(dt$geoid_2010),
                LAG_YEAR = 1991:2021)

# Create helper column for rolling join
lag_years[, JOIN_YEAR := LAG_YEAR - 1]

# Set keys for rolling join
setkey(dt, geoid_2010, CENSUS_YEAR)
setkey(lag_years, geoid_2010, JOIN_YEAR)

# Rolling join: get most recent census year < LAG_YEAR
result <- dt[lag_years, on = .(geoid_2010, CENSUS_YEAR = JOIN_YEAR), roll = Inf]

result[, .(year, CENSUS_YEAR, LAG_YEAR)]

# Now, I can merge with df!
df <- merge(df, result, 
            by.x = c("geoid_2010", "TAX_YEAR"), 
            by.y = c("geoid_2010", "LAG_YEAR"), all.x = TRUE)

rm(census_df, dt, lag_years, result)
#################################################################################################
# Now, add in the climate data - UPDATE March 2026: since don't need weather data for causal identification, leaving out for now
# Since I have monthly precipitation and average temperature data, for each county,year pair compute the median and average of the values 
# Note: climate data for Hawaii starts in 1991 so 4 counties have missing climate data for TAX_YEAR 1991 since we don't have 1990 data
#################################################################################################
# precipitation <- readRDS("climate_data/precipitation.rds") |> 
#       group_by(geoid, year) |> 
#       summarize(med_precip = median(val, na.rm = TRUE), 
#                 mean_precip = mean(val, na.rm = TRUE),
#                 var_precip = var(val, na.rm = TRUE)) |>
#       mutate(JOIN_YEAR = year + 1) |>
#       rename(PRECIP_YEAR = year)
# 
# df <- merge(df, precipitation, by.x = c("geoid_2010", "TAX_YEAR"), by.y = c("geoid", "JOIN_YEAR"), all.x = TRUE)
# 
# avg_temp <- readRDS("climate_data/average_temperature.rds") |> 
#       group_by(geoid, year) |> 
#       summarize(med_avg_temp = median(val, na.rm = TRUE), 
#                 mean_avg_temp = mean(val, na.rm = TRUE),
#                 var_avg_temp = var(val, na.rm = TRUE)) |>
#       mutate(JOIN_YEAR = year + 1) |>
#       rename(TEMP_YEAR = year)
# 
# df <- merge(df, avg_temp, by.x = c("geoid_2010", "TAX_YEAR"), by.y = c("geoid", "JOIN_YEAR"), all.x = TRUE)
# 
# rm(precipitation, avg_temp)
#################################################################################################
# Population estimates for each year (for computing per capita damage)
#################################################################################################
# col_positions <- fwf_widths(widths = c(4, 2, 5, 2, 1, 1, 1, 2, 8), 
#                             col_names = c("year", "state", "county.geoid", "blank", "race", "origin", "sex", "age", "population"))
# 
# pop_df <- read_fwf("counties_data/us.1990_2023.20ages.adjusted.txt", col_positions)
# 
# # Drop blank column
# pop_df$blank <- NULL
# 
# # Convert numeric columns
# pop_df <- pop_df |>
#       mutate(year = as.integer(year),
#              race = as.integer(race),
#              origin = as.integer(origin),
#              sex = as.integer(sex),
#              population = as.integer(population))
# 
# # get county population estimates per year
# pop_by_year <- pop_df |>
#       group_by(county.geoid, year) |>
#       summarise(pop_estimate = sum(population))
# 
# # Merge and if pop_by_year estimate missing, replace with total_population from census crosswalk (258 records total)
# df <- merge(df, pop_by_year, by.x = c("geoid_2010", "TAX_YEAR"), by.y = c("county.geoid", "year"), all.x = TRUE)
# df <- df |> mutate(pop_estimate = ifelse(is.na(pop_estimate), as.numeric(total_population), pop_estimate))
# 
# rm(col_positions, pop_by_year, pop_df)
#################################################################################################
# Treatment Variable for each year
#################################################################################################
# disaster data
disasters <- readRDS("disasters.rds")
disasters$county.geoid[disasters$county.geoid == "02158"] <- "02270"
disasters$county.geoid[disasters$county.geoid == "46102"] <- "46113"

disasters <- disasters |>  
      mutate(LOG_TotPropDmgADJ = log(`PropertyDmg(ADJ 2021)_TOT` + 1),
             LOG_TotCropDmgADJ = log(`CropDmg(ADJ 2021)_TOT` + 1),
             LOG_TotDmgADJ = log(`PropertyDmg(ADJ 2021)_TOT` + `CropDmg(ADJ 2021)_TOT` + 1)) |>
      group_by(county.geoid, TAX_YEAR) |>
      slice_head(n = 1) |>
      ungroup()

df <- merge(df, 
            disasters |> select(county.geoid, TAX_YEAR, LOG_TotPropDmgADJ, LOG_TotCropDmgADJ, LOG_TotDmgADJ, n_disasters), 
            by.x = c('geoid_2010', 'TAX_YEAR'), 
            by.y = c('county.geoid', 'TAX_YEAR'), all.x = TRUE)

df <- df |> mutate(LOG_TotPropDmgADJ = replace_na(LOG_TotPropDmgADJ, 0),
                   LOG_TotCropDmgADJ = replace_na(LOG_TotCropDmgADJ, 0),
                   LOG_TotDmgADJ = replace_na(LOG_TotDmgADJ, 0),
                   n_disasters = replace_na(n_disasters, 0))

df <- df |> mutate(treat.atleast1 = as.numeric(LOG_TotPropDmgADJ > 0),
                   treat.small_log_damage = as.numeric(LOG_TotPropDmgADJ > 11), # about 60K of property damage
                   treat.large_log_damage = as.numeric(LOG_TotPropDmgADJ > 14), # about 1.2M of property damage
                   treat.cat = case_when(
                         LOG_TotPropDmgADJ <= 0 ~ "none",
                         LOG_TotPropDmgADJ <= 11 ~ "small", # up to ~60K
                         LOG_TotPropDmgADJ <= 13 ~ "med", # up to ~442K
                         LOG_TotPropDmgADJ <= 22 ~ "large", # up to 3.5 trillion
                   ))

# Based on FEMA county per capita impact indicator
county_FEMA_indicator <- 3.89 # 2019: 3.78, 2021: 3.89

n.counties <- length(unique(df$county.census.geoid))
avg.n.dis.per.year <- disasters |> 
      group_by(TAX_YEAR) |> 
      summarise(avg.num.dis = sum(n_disasters)/n.counties) |>
      mutate(FEMA_threshADJ = county_FEMA_indicator * avg.num.dis)

df <- merge(df, avg.n.dis.per.year, by = "TAX_YEAR")

df <- df |> mutate(TotPerCapADJ = (exp(LOG_TotDmgADJ)-1)/as.numeric(total_population),
                   treat.FEMA = as.numeric(TotPerCapADJ >= county_FEMA_indicator),
                   treat.FEMA_nDis = as.numeric(TotPerCapADJ >= FEMA_threshADJ)
                   )

saveRDS(df, "county_long.rds")







