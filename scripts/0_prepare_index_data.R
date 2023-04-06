####################################################################################################
#
#  Prepare Index Data for Analysis
#
####################################################################################################

 ## Load Libraries

 library(tidyverse)
 library(tidycensus)
 library(sf)

 ## Set Parameters

  # Start date
  start_date <- as.Date('2013-01-01')
  
  # Census Bureau API (Note, you may need to get one of these)
  census_api_key = Sys.getenv('census_api_key')
  
 ## Load Functions
  
  source(file.path(getwd(), 'functions', 'index_functions.R'))
  
 ## Load data
 
  # Set Path to Zillow Data
  metro_zhvi_path <- paste0('https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi',
                            '_uc_sfr_tier_0.33_0.67_sm_sa_month.csv?t=1678492501')
  
  zip_zhvi_path <- paste0('https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi',
                          '_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1678492501')
  
  # Read Metro Data  
  metro_df <- readr::read_csv(metro_zhvi_path) %>%
    dplyr::mutate(StateName = ifelse(is.na(StateName), 'US', StateName))
  
  # Read ZIP Data
  zip_df <- readr::read_csv(zip_zhvi_path)
  
### Convert to indexes and MoM ---------------------------------------------------------------------
  
  # Create indexes
  metro_idf <- metro_df %>% 
    zhviToIndex(base_period = start_date)
  zip_idf <- zip_df %>% 
    zhviToIndex(base_period = start_date)
  
  ## Get Zip Code Boundaries from Census and shape to necessary size
  tidycensus::census_api_key(census_api_key)
  zipcodes_sf <- tidycensus::get_acs(geography = "zip code tabulation area", 
                                     variables = "B10001_001",
                                     geometry = TRUE) %>%
    dplyr::select(ZIP5 = GEOID, 
                  ZIPCode = NAME, 
                  Population = estimate)

### Write out data ---------------------------------------------------------------------------------
  
  saveRDS(metro_idf, file.path(getwd(), 'data', 'metro_index.RDS'))
  saveRDS(zip_idf, file.path(getwd(), 'data', 'zip_index.RDS'))
  saveRDS(zipcodes_sf, file.path(getwd(), 'data', 'zip_boundaries.RDS'))
  
####################################################################################################
####################################################################################################  
  
  
  
