#***************************************************************************************************
#
#  Spatial Examples
#
#***************************************************************************************************

  library(sf)
  library(tidyverse)
  library(kingCoData)
  library(sp)
  
 ## Load Functions
  source(file.path(getwd(), 'functions', 'index_functions.R'))
  source(file.path(getwd(), 'functions', 'plotting_functions.R'))
  source(file.path(getwd(), 'functions', 'spatial_functions.R'))
  source(file.path(getwd(), 'functions', 'model_functions.R'))
  
 ## Load Data
  
  data("kingco_sales")
  data('kingco_submarkets')
  zips_sf <- readRDS(file.path(getwd(), 'data', 'king_zips.RDS'))
  study_area_sf <- sf::read_sf(file.path(getwd(), 'data', 'study_area.shp')) %>%
    sf::st_transform(., crs = 4269)
  
  # Subj
  subj_id <- '2022..35485'
  
  # Create Local Data
  ziplist <- c(98177, 98133, 98117, 98103, 98107, 98155, 98125, 98115, 98105)

### Feature Engineering ----------------------------------------------------------------------------

  ## Add ZIP, filter to ZIPS and dates
  train_df <- kingco_sales %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), 
                 crs = 4269,
                 remove = FALSE) %>%
    sf::st_join(., zips_sf) %>%
    dplyr::filter(ZIP5 %in% ziplist & 
                    sale_date >= '2018-01-01') %>%
    as.data.frame() %>%
    dplyr::select(-geometry)
  
  ## Create baths, view score and combined tax values
  train_df <- train_df %>%
    dplyr::mutate(baths = bath_full + (bath_3qtr * .75) + (bath_half * .5),
                  garage_sqft = garb_sqft + gara_sqft,
                  view_score = view_rainier + view_olympics + view_cascades + view_sound +
                    view_lakewash,
                  tax_value = land_val + imp_val,
                  waterfront_type = dplyr::case_when(
                    wfnt %in% c(4,5,8) ~ 'lake',
                    wfnt %in% c(1,9) ~ 'river',
                    wfnt %in% c(2,3) ~ 'puget_sound',
                    wfnt == 6 ~ 'lake_wash',
                    wfnt == 7 ~ 'lake_samm',
                    TRUE ~'none'),
                  age = as.numeric(substr(sale_date, 1, 4)) - year_built,
                  eff_age = ifelse(year_reno == 0, age,
                                   as.numeric(substr(sale_date, 1, 4)) - year_reno),
                  green_adjacent = ifelse(golf == 1 | greenbelt == 1, 1, 0),
                  townhome = ifelse(present_use == 29, 1, 0),
                  compmarket = paste0(ifelse(waterfront_type == 'none',
                                             submarket, waterfront_type))) %>%
    rotateCoordinates(rotations = c(15, 30, 45, 60, 75))
  
  # Create subidivision count
  subd_cdf <- train_df %>%
    dplyr::mutate(subdivision = ifelse(subdivision == '', 'NONE', subdivision)) %>%
    dplyr::mutate(subdivision = ifelse(is.na(subdivision), 'NONE', subdivision)) %>%
    dplyr::group_by(subdivision) %>%
    dplyr::summarize(subd_count = dplyr::n()) %>%
    dplyr::arrange(desc(subd_count))
  
  train_df <- train_df %>%
    dplyr::left_join(., subd_cdf,
                     by = 'subdivision')
  
  ## Remove any renovated after sale
  train_df <- train_df %>%
    dplyr::filter(eff_age >= 0)
  
  ## Create adjusted sale price values
  
  # Create sales data for model
  train_hhdf <- hpiR::hedCreateTrans(trans_df = train_df,
                                     prop_id = 'pinx',
                                     trans_id = 'sale_id',
                                     price = 'sale_price',
                                     date= 'sale_date',
                                     periodicity = 'weekly')
  
  # Specific hedonic model for index
  hed_spec <- stats::as.formula(log(price) ~ sqft + beds + baths + as.factor(submarket) +
                                  view_score + year_built + as.factor(grade) +
                                  as.factor(waterfront_type) +
                                  as.factor(present_use) + as.factor(join_status))
  
  # Estimate index model
  hed_index <- hpiR::hedIndex(trans_df = train_df,
                        periodicity = 'weekly',
                        date = 'sale_date',
                        price = 'sale_price',
                        trans_id = 'sale_id',
                        prop_id = 'pinx',
                        estimator = 'base',
                        hed_spec = hed_spec,
                        smooth = TRUE)
  
  # Apply index adjustment to training data
  train_df <- train_hhdf %>%
    dplyr::left_join(., data.frame(trans_period = hed_index$index$period,
                                   index_value = hed_index$index$value) %>%
                       dplyr::mutate(index_adj = index_value / index_value[1]),
                     by = 'trans_period') %>%
    dplyr::mutate(adjusted_price = price * index_adj)
  
  subj_df <- train_df %>%
    dplyr::filter(trans_id %in% subj_id) %>%
    dplyr::mutate(pred_id = 0)
  
### Write out data --------------------------------------------------

  saveRDS(train_df, file.path(getwd(), 'data', 'training_data.RDS'))
  saveRDS(subj_df, file.path(getwd(), 'data', 'subject_data.RDS'))

#***************************************************************************************************
#***************************************************************************************************  
 