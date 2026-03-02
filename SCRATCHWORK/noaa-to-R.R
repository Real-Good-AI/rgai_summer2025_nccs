# Scraping county-level climate data from NOAA "Climate at a Glance" online tool
# RGAI rating: 4
# ChatGPT log: https://chatgpt.com/share/6973a2be-ed00-8003-9639-9dd3fa4566d1
library(data.table)
library(readr)
library(dplyr)
library(tidyverse)

# Every record in df corresponds to a unique county. We will loop through each record to get the NOAA climate data.
df <- readRDS("county_id_name.rds")
df$url_location <- paste0(df$CENSUS_STATE_ABBR, "-", 
                          substr(df$geoid_2010, nchar(df$geoid_2010) - 2, nchar(df$geoid_2010)))
state_groups <- split(seq_len(nrow(df)), df$CENSUS_STATE_ABBR)

# These are the parameters that remain the same for each API call
url_pars <- list(scope = "county",
                 parameter = "pcp", # tavg = average temperature, pcp = precipitation
                 timescale = 1,
                 month = 0,
                 begYear = 1990,
                 endYear = 2020,
                 format = "csv")

all_counties <- data.frame(state = character(), geoid = character(), year = integer(), month = integer(), value = numeric())

start.Time <- Sys.time()
for (curr_state in names(state_groups)){
      message("Processing state: ", curr_state)
      
      curr_rows <- state_groups[[curr_state]]
      for (i in curr_rows){
            curr_geoid <- df$geoid_2010[i]
            curr_location <- df$url_location[i]
            
            url <- sprintf("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/%s/time-series/%s/%s/%d/%d/%d-%d/data.%s", 
                           url_pars$scope, curr_location, url_pars$parameter, url_pars$timescale, 
                           url_pars$month, url_pars$begYear, url_pars$endYear, url_pars$format)
            
            # try to download data
            county_data <- tryCatch(read.csv(url), 
                                    error = function(msg){
                                          message(paste("Error downloading data for county:", curr_geoid))
                                          return(NULL)}
            )
            # if the download succeeded, proceed. else, skip to next iteration
            if (!is.null(county_data)) {
                  county_data <- county_data |> slice(3:n()) # first two rows are unecessary
                  dates <- county_data[[1]]
                  dat.year <- as.integer(substr(dates, 1, 4))
                  dat.month <- as.integer(substr(dates, 5, 6))
                  dat.values <- as.double(county_data[[2]])
                  
                  all_counties <- rbind(all_counties, 
                                        data.frame(state = df$state_name[i], geoid = curr_geoid, year = dat.year, month = dat.month, val = dat.values))
            } else {
                  write(curr_geoid, file = "noaa_fail_log.txt", append = TRUE) # keep track of counties data download failed at
                  next
            }
            Sys.sleep(0.2)
      }
      message("Finished processing state: ", curr_state)
}
end.Time <- Sys.time()
print(end.Time - start.Time)

saveRDS(all_counties, "all_county_precip_data.rds") # "all_county_avg_temp_data.rds" if parameter = "tavg"

# The following counties had a data download failure: 02261, 02270, 11001, 46113, 51678