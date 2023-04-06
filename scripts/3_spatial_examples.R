#***************************************************************************************************
#
#  Spatial Examples
#
#***************************************************************************************************

  library(tmap)
  library(sf)
  library(tidyverse)
  library(mapview)
  library(kingCoData)
  library(Rborist)
  library(sp)
  library(spdep)
  library(keras)
  
 ## Load Functions
  source(file.path(getwd(), 'functions', 'index_functions.R'))
  source(file.path(getwd(), 'functions', 'plotting_functions.R'))
  source(file.path(getwd(), 'functions', 'spatial_functions.R'))
  source(file.path(getwd(), 'functions', 'model_functions.R'))
  
 ## Load Data
  
  data("kingco_sales")
  data('kingco_submarkets')
  train_df <- readRDS(file.path(getwd(), 'data', 'training_data.RDS'))
  subj_df <- readRDS(file.path(getwd(), 'data', 'subject_data.RDS'))
  zips_sf <- readRDS(file.path(getwd(), 'data', 'king_zips.RDS'))
  study_area_sf <- sf::read_sf(file.path(getwd(), 'data', 'study_area.shp')) %>%
    sf::st_transform(., crs = 4269)
  
  ## Set Basemaps
  basemaps::reset_defaults()
  basemap <- leaflet::providers$CartoDB.DarkMatter
  tmap_mode('view')
  
  # Subj
  subj_id <- '2022..35485'
  
  # Create Local Data
  ziplist <- c(98177, 98133, 98117, 98103, 98107, 98155, 98125, 98115, 98105)

  ## Plot Parameters
  ramp_breaks <- c(-1, seq(-.2, -.05, by = .05), -.015, .015, seq(.05, .3, by = .05),
                   .4, .5, 1, 10)
  ramp_colors <- c(rev(RColorBrewer::brewer.pal(6, 'Blues')), 'white', 
                   RColorBrewer::brewer.pal(9, 'Oranges'))
  ramp_labels <- c('Less 20%', '-20% to -15%', '-15% to -10%', '-10% to -5%',
                   '-5% to -1.5%', '-1.5% to 1.5%', '1.5% to 5%', '5% to 10%',
                   '10% to 15%', '15% to 20%', '20% to 25%', '25% to 30%',
                   '30% to 40%', '40% to 50%', '50% to 100%', 'More 100%')
  
  plot_scale <- 1
  plot_width <- 1200
  plot_height <- 960
  plot_zoom <- 1
  plot_title = 'Location Adj'
  griddot_size <- 0.06
  griddot_alpha <- 0.85
  zipline_col <- '#262626'
  zipline_size <- 4
  subjdot_size <- 2
  subjdot_col <- 'red'
  canddot_col <- 'white'
  compdot_col <- 'blue'
  graywater <- '#262626'

### Data Prep -----------------------------------------------------------------------------    
#### Spatial Data Prep ---------------------------------------------------------------------  
  
  train_sf <- 
    train_df %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), 
                 crs = 4269,
                 remove = FALSE)
  
  subj_sf <- train_sf %>%
    dplyr::filter(trans_id %in% subj_id) %>%
    dplyr::mutate(pred_id = 0)

##### Create Grid ---------------------------------------------------------------------------
  
  ## Set up square grid
  x_range = seq(-122.3245, -122.405, length.out = 65)
  y_range = seq(47.685, 47.724, length.out = 50)
  grid_df <- expand.grid(x = x_range, y = y_range) %>%
    dplyr::rename(longitude = x, latitude = y)
  
  ## Create ID
  gridpred_df <- subj_df[rep(1, nrow(grid_df)), ] %>%
    dplyr::mutate(pred_id = 1:nrow(.))
  
  ## Add lat/longs to pred-grid
  gridpred_df$latitude = grid_df$latitude
  gridpred_df$longitude = grid_df$longitude
  
  ## Create rotations
  gridpred_sf <- gridpred_df %>% 
    sf::st_as_sf(coords = c("longitude", "latitude"), 
                 crs = 4269,
                 remove = FALSE) %>%
    rotateCoordinates(rotations = c(15, 30, 45, 60, 75))
  
  ## Clip to study area 
  gridpred_sf <- sf::st_intersection(gridpred_sf, study_area_sf) %>%
    dplyr::select(-id)
  
  ## Clip non-SF data to study area
  gridpred_df <- gridpred_df %>% 
    dplyr::inner_join(gridpred_sf %>%
                        as.data.frame() %>%
                        dplyr::select(pred_id),
                      by = c('pred_id'))   
  
  ## Set up plotting BBOX
  plot_bbox <- sf::st_bbox(gridpred_sf)
  plot_bbox[3] <- plot_bbox[3] + .012
  plot_bbox
    
  ## Clip ZIPs to study area
  zipssa_sf <- sf::st_intersection(zips_sf, study_area_sf)
  
### 1st Gen Models ---------------------------------------------------------------------------------
#### 1G - Subs -----------------------------------------------------------------------------  
  
  # Find Comps
  compids_df <- compFinder(train_df = train_df %>%
                              dplyr::filter(sale_date > as.Date('2021-12-31')),
                            test_df = subj_df,
                            features = c('latitude', 'longitude', 'present_use'),
                            k = 25,
                            algorithm = 'kd_tree')
  
  comps_sf <- train_sf %>%
    dplyr::filter(trans_id %in% compids_df$comp_id)
  
##### Plot: Comps (1S) ----------------------------------------------------------------------
  
  tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
    tm_shape(train_sf %>%
               dplyr::filter(trans_period > 224), 
             bbox = gridpred_sf) + 
       tm_dots(scale = plot_scale,
               size = griddot_size / 3,
               col = 'gray10',
               legend.show = FALSE) + 
    tm_shape(comps_sf, bbox = gridpred_sf) + 
      tm_dots(scale = plot_scale,
              size = griddot_size/1.5,
              col = canddot_col,
              legend.show = FALSE) + 
    tm_shape(comps_sf[1:6, ]) + 
      tm_dots(col = compdot_col,
              scale = plot_scale,
              size = griddot_size/1.5,
              legend.show = FALSE) +
    tm_shape(subj_sf) + 
      tm_dots(col = subjdot_col,
              scale = plot_scale,
              size = subjdot_size/8,
              legend.show = FALSE) +
    tm_add_legend('fill', 
                  col = c('gray50', canddot_col, compdot_col, subjdot_col),
                  border.col = "grey40",
                  size = .5,
                  labels = c('Sales', 'Candidates', 'Comps', 'Subject'),
                  title="")->
    s1_map
  s1_map
  
  ## Plot Map
  s1_map %>%
    tmap::tmap_leaflet(.) %>%
    mapview::mapshot(., file = file.path(getwd(), 'plots', "s1_map.png"),
                     vwidth = plot_width, vheight = plot_height, zoom = plot_zoom)
  
#### 1G Attr -----------------------------------------------------------------------------
  
  ### Fit Model
  lm_obj <- lm(log(price) ~ sqft + grade + condition + age + eff_age + waterfront_type + 
                 as.factor(ZIP5) + townhome + green_adjacent + view_score,
               data = train_df)
  
  ### Extract Spatial Coefficients
  coef_df <- summary(lm_obj)[[4]]
  coef_df <- data.frame(variable = rownames(coef_df),
                        coef = coef_df[,1])
  spcoef_df <- coef_df[grepl('ZIP', coef_df$variable), ]
  spcoef_df$ZIP5 <- as.numeric(substr(spcoef_df$variable, 16, 21))
  spcoef_df <- spcoef_df %>%
    dplyr::bind_rows(., data.frame(ZIP5 = 98103, variable='98103', 
                                   coef = 0))
  
  ### Scale
  spcoef_baseline <- spcoef_df$coef[spcoef_df$ZIP5 == subj_df$ZIP5]
  spcoef_df <- spcoef_df %>%
    dplyr::mutate(sc_coef = exp(coef - spcoef_baseline) - 1)
  
  zipssa1_sf <- zipssa_sf %>%
    dplyr::mutate(ZIP5 = as.numeric(ZIP5)) %>%
    dplyr::inner_join(., spcoef_df, by = 'ZIP5')

##### Plot: OLS (1A) ----------------------------------------------------
  
  tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
    tm_shape(zipssa1_sf, bbox = plot_bbox) + 
    tm_polygons(scale = 1, 
                col = 'sc_coef',
                alpha = .5,
                breaks = ramp_breaks,
                palette = ramp_colors,
                labels = ramp_labels,
                style = 'fixed',
                title = plot_title,
                legend.show = TRUE)  +
    tm_shape(subj_sf) + 
    tm_dots(col = subjdot_col,
            scale = subjdot_size, 
            legend.show = FALSE) ->
    a1_map
  a1_map
  
  a1_map %>%
    tmap::tmap_leaflet(.) %>%
    mapview::mapshot(., file = file.path(getwd(), 'plots', "a1_map.png"),
                     vwidth = plot_width, vheight = plot_height, zoom = plot_zoom)
  
### 2nd Gen Models -----------------------------------------------------------------
#### 2A GWR --------------------------------------------------------
  
  ## Prep SP Objects
  train_sp <- sf::as_Spatial(train_sf, cast = TRUE)
  subj_sp <- subj_sf %>%
    sf::as_Spatial(., cast = TRUE)
  grid_sp = gridpred_sf %>%
    sf::as_Spatial(.,  cast = TRUE)
  pred_sp <- rbind(subj_sp, grid_sp) 
  
  # Fit GWRs    
  mod2a_obj <- 
    spgwr::gwr(formula = log(adjusted_price) ~ sqft + condition + 
                 grade + beds + view_score + age + eff_age + baths, 
               adapt = .05, 
               data = train_sp,
               fit.points = pred_sp,
               prediction = TRUE)
  
  # Extract Predictions and add to grid object
  pred_sp$pred2A <- mod2a_obj$SDF@data$pred %>% exp()
  subj_pred2A <- pred_sp$pred2A[pred_sp$pred_id == 0]
  gridpred_sf <-
    gridpred_sf %>% dplyr::left_join(., pred_sp %>%
                                       as.data.frame() %>%
                                       dplyr::select('pred_id', 'pred2A'),
                                     by = 'pred_id') %>%
      dplyr::mutate(loc_adj2A = (pred2A / subj_pred2A) - 1)
    
    gridpred_df <-
      gridpred_df %>% dplyr::left_join(., pred_sp %>%
                                         as.data.frame() %>%
                                         dplyr::select('pred_id', 'pred2A'),
                                       by = 'pred_id') %>%
      dplyr::mutate(loc_adj2A = (pred2A / subj_pred2A) - 1)
    
##### Plot: GWR (2A) -------------------------------------------

  tm_basemap(basemap) +
    tm_shape(gridpred_sf, bbox = plot_bbox) + 
    tm_dots(scale = plot_scale, 
            size = griddot_size,
            border.lwd = NA,
            col = 'loc_adj2A',
            breaks = ramp_breaks,
            palette = ramp_colors,
            labels = ramp_labels,
            style = 'fixed',
            title = plot_title,
            legend.show = TRUE,
            alpha = griddot_alpha) + 
      tm_shape(subj_sf) + 
      tm_dots(col = subjdot_col,
              scale = subjdot_size, 
              legend.show = FALSE) +
      tm_shape(zips_sf) + 
      tm_borders(col = zipline_col,
                 lwd = zipline_size) ->
      a2_map
    a2_map
    
    a2_map %>%
      tmap::tmap_leaflet(.) %>%
      mapview::mapshot(., file = file.path(getwd(), 'plots', "a2_map.png"),
                       vwidth = plot_width, vheight = plot_height, zoom = plot_zoom)
    

#### Automated Comp (2S) -------------------------------------------
  
  # Create comps data object
  compsdata_ <- list(train = train_df,
             test = gridpred_df %>%
               dplyr::mutate(trans_id = pred_id))
  
  # Set parameters  
  mce_par <- list(
    comp_feat = c('latitude', 'longitude', 'sqft'),
    loc_feat = c('latitude', 'longitude', 'lat45', 'lon45'),
    other_feat = c('sqft', 'age'),
    k = 7,
    algorithm = 'kd_tree',
    valuefunction = stats::median)
  
  # Collect the comps
  mce_pred_ <- medCompEstimator(x_ = compsdata_,
                                model_par = mce_par,
                                test_data = 'test')
  
  ## Specify the adjustment model
  adj_spec = as.formula(log(adjusted_price) ~ age + sqft + beds + baths + grade)
  
  ## Adjust comps to get CMA values
  cma_pred <- cmaModel(x_ = compsdata_,
                       comps = mce_pred_$comps,
                       mod_spec = adj_spec)
  
  ## Repeat the above for the subject
  x_ <- list(train = train_df,
             test = subj_df)
  subj_mce <- medCompEstimator(x_ = x_,
                               model_par = mce_par,
                               test_data = 'test')
  subj_cma <-  cmaModel(x_ = x_,
                        comps = subj_mce$comps,
                        mod_spec = adj_spec)
  
  ## Extract predictions
  preds_df <- cma_pred$pred %>%
    dplyr::select(pred_id = trans_id,
                  pred2S = pred)
  
  ## Add all to grid
  gridpred_df <- gridpred_df %>%
    dplyr::left_join(., preds_df, by = 'pred_id')
  gridpred_sf <- gridpred_sf %>%
    dplyr::left_join(., preds_df, by = 'pred_id')
  
  # Calculate the location adjustment
  loc_adj2S <- (cma_pred$pred$pred / subj_cma$pred$pred) - 1
  gridpred_df$loc_adj2S <- loc_adj2S
  gridpred_sf$loc_adj2S <- loc_adj2S
  
  ## Plot
  tm_basemap(basemap) +
    tm_shape(gridpred_sf, bbox = plot_bbox) + 
    tm_dots(scale = plot_scale, 
            size = griddot_size,
            border.lwd = NA,
            col = 'loc_adj2S',
            breaks = ramp_breaks,
            palette = ramp_colors,
            labels = ramp_labels,
            style = 'fixed',
            title = plot_title,
            legend.show = TRUE,
            alpha = griddot_alpha) + 
    tm_shape(subj_sf) + 
    tm_dots(col = subjdot_col,
            scale = subjdot_size, 
            legend.show = FALSE) +
    tm_shape(zips_sf) + 
    tm_borders(col = zipline_col,
               lwd = zipline_size) +
    tm_layout(legend.bg.color = 'red') ->
    s2_map
  s2_map
  
  s2_map %>%
    tmap::tmap_leaflet(.) %>%
    mapview::mapshot(., file = file.path(getwd(), 'plots', "s2_map.png"),
                     vwidth = plot_width, vheight = plot_height, zoom = plot_zoom)
  
### 3rd Gen Models ---------------------------------------------------
  
#### 3S Modeling ------------
  
  # Fit RF model
  rf_obj <- randomForest::randomForest(adjusted_price ~ sqft + condition + baths + 
                                         view_score + latitude + longitude + lat45 + 
                                         lon45,
                                       data = train_df)
  
  ## Extract predictions
  gridpred_df <- gridpred_df %>% dplyr::arrange(pred_id)
  gridpred_sf <- gridpred_sf %>% dplyr::arrange(pred_id)
  pred3S_ll <- predict(rf_obj, 
                       newdata = gridpred_sf, 
                       type = 'response')
  subj_pred_ll <- predict(rf_obj,
                          newdata = subj_df, 
                          type = 'response')
  
  # Add to pred data  
  gridpred_df$loc_adj3S <- (pred3S_ll / subj_pred_ll)-1
  gridpred_sf$loc_adj3S <- (pred3S_ll / subj_pred_ll)-1

##### Plot: RF (3S) -----------------------  
  
  tm_basemap(basemap) +
    tm_shape(gridpred_sf, bbox = plot_bbox) + 
    tm_dots(scale = plot_scale, 
            size = griddot_size,
            border.lwd = NA,
            col = 'loc_adj3S',
            breaks = ramp_breaks,
            palette = ramp_colors,
            labels = ramp_labels,
            style = 'fixed',
            title = plot_title,
            legend.show = TRUE,
            alpha = griddot_alpha) + 
    tm_shape(subj_sf) + 
    tm_dots(col = subjdot_col,
            scale = subjdot_size, 
            legend.show = FALSE) +
    tm_shape(zips_sf) + 
    tm_borders(col = zipline_col,
               lwd = zipline_size) ->
    s3_map
  s3_map
  
  s3_map %>%
    tmap::tmap_leaflet(.) %>%
    mapview::mapshot(., file = file.path(getwd(), 'plots', "s3_map.png"),
                     vwidth = plot_width, vheight = plot_height, zoom = plot_zoom)
  
#### 3A NN ---------------------------------------------------------------------
  
  ## Set features
  resp_feature <- 'adjusted_price'
  feat <- c('sqft', 'age', 'grade', 'condition', 'sqft_lot', 'baths', 'view_score')
  
  ## Build regional data categories
  train_df <- train_df %>%
    dplyr::mutate(latlong1 = paste0(round(longitude, 1), '_', round(latitude, 1)),
                  latlong2 = paste0(round(longitude, 2), '_', round(latitude, 2)),
                  latlong3 = paste0(round(longitude, 3), '_', round(latitude, 3)),
                  longitudeOW = longitude - .005,
                  latitudeOS = latitude - .005,
                  latlongO2 = paste0(round(longitudeOW, 2), '_', round(latitudeOS, 2)),
                  latlongO3 = paste0(round(longitudeOW, 3), '_', round(latitudeOS, 3)),
                  latlong452 = paste0(round(lon45, 2), '_', round(lat45, 2)),
                  latlong453 = paste0(round(lon45, 3), '_', round(lat45, 3)))
  
  gridpred_df <- gridpred_df %>%
    dplyr::mutate(latlong1 = paste0(round(longitude, 1), '_', round(latitude, 1)),
                  latlong2 = paste0(round(longitude, 2), '_', round(latitude, 2)),
                  latlong3 = paste0(round(longitude, 3), '_', round(latitude, 3)),
                  longitudeOW = longitude - .005,
                  latitudeOS = latitude - .005,
                  latlongO2 = paste0(round(longitudeOW, 2), '_', round(latitudeOS, 2)),
                  latlongO3 = paste0(round(longitudeOW, 3), '_', round(latitudeOS, 3)),
                  latlong452 = paste0(round(lon45, 2), '_', round(lat45, 2)),
                  latlong453 = paste0(round(lon45, 3), '_', round(lat45, 3)))
  subj_df <- subj_df %>%
    dplyr::mutate(latlong1 = paste0(round(longitude, 1), '_', round(latitude, 1)),
                  latlong2 = paste0(round(longitude, 2), '_', round(latitude, 2)),
                  latlong3 = paste0(round(longitude, 3), '_', round(latitude, 3)),
                  longitudeOW = longitude - .005,
                  latitudeOS = latitude - .005,
                  latlongO2 = paste0(round(longitudeOW, 2), '_', round(latitudeOS, 2)),
                  latlongO3 = paste0(round(longitudeOW, 3), '_', round(latitudeOS, 3)),
                  latlong452 = paste0(round(lon45, 2), '_', round(lat45, 2)),
                  latlong453 = paste0(round(lon45, 3), '_', round(lat45, 3)))
  
##### Create Embeddings --------------------------------------
  
  emb_ll1 <- nnEmbedder(x_ <- NULL,
                        full_df = as.data.frame(train_df),
                        emb_feature = 'latlong1',
                        emb_name = 'll_1',
                        resp_feature = 'adjusted_price',
                        emb_size = 5,
                        epochs = 40,
                        batch_size = 250)
  
  emb_ll2 <- nnEmbedder(x_ <- NULL,
                        full_df = as.data.frame(train_df),
                        emb_feature = 'latlong2',
                        emb_name = 'll_2',
                        resp_feature = 'adjusted_price',
                        emb_size = 5,
                        epochs = 40,
                        batch_size = 250)
  
  emb_ll3 <- nnEmbedder(x_ <- NULL,
                        full_df = as.data.frame(train_df),
                        emb_feature = 'latlong3',
                        emb_name = 'll_3',
                        resp_feature = 'adjusted_price',
                        emb_size = 5,
                        epochs = 40,
                        batch_size = 250)
  
  emb_ll452 <- nnEmbedder(x_ <- NULL,
                          full_df = as.data.frame(train_df),
                          emb_feature = 'latlong452',
                          emb_name = 'll_452',
                          resp_feature = 'adjusted_price',
                          emb_size = 5,
                          epochs = 40,
                          batch_size = 250)
  
  emb_ll453 <- nnEmbedder(x_ <- NULL,
                          full_df = as.data.frame(train_df),
                          emb_feature = 'latlong453',
                          emb_name = 'll_453',
                          resp_feature = 'adjusted_price',
                          emb_size = 5,
                          epochs = 40,
                          batch_size = 250)
  
  emb_llO2 <- nnEmbedder(x_ <- NULL,
                         full_df = as.data.frame(train_df),
                         emb_feature = 'latlongO2',
                         emb_name = 'll_O2',
                         resp_feature = 'adjusted_price',
                         emb_size = 5,
                         epochs = 40,
                         batch_size = 250)
  
  emb_llO3 <- nnEmbedder(x_ <- NULL,
                         full_df = as.data.frame(train_df),
                         emb_feature = 'latlongO3',
                         emb_name = 'll_O3',
                         resp_feature = 'adjusted_price',
                         emb_size = 5,
                         epochs = 40,
                         batch_size = 250)
  
  ## Join Embeddings
  
  train_df <- train_df %>%
    dplyr::left_join(., emb_ll1,
                     by = 'latlong1') %>%
    dplyr::left_join(., emb_ll2,
                     by = 'latlong2') %>%
    dplyr::left_join(., emb_ll3,
                     by = 'latlong3') %>%
    dplyr::left_join(., emb_llO2,
                     by = 'latlongO2') %>%
    dplyr::left_join(., emb_llO3,
                     by = 'latlongO3') %>%
    dplyr::left_join(., emb_ll452,
                     by = 'latlong452') %>%
    dplyr::left_join(., emb_ll453,
                     by = 'latlong453')

  gridpred_df <- gridpred_df %>%
    dplyr::left_join(., emb_ll1,
                     by = 'latlong1') %>%
    dplyr::left_join(., emb_ll2,
                     by = 'latlong2') %>%
    dplyr::left_join(., emb_ll3,
                     by = 'latlong3') %>%
    dplyr::left_join(., emb_llO2,
                     by = 'latlongO2') %>%
    dplyr::left_join(., emb_llO3,
                     by = 'latlongO3') %>%
    dplyr::left_join(., emb_ll452,
                     by = 'latlong452') %>%
    dplyr::left_join(., emb_ll453,
                     by = 'latlong453')
  
  subj_df <- subj_df %>%
    dplyr::left_join(., emb_ll1,
                     by = 'latlong1') %>%
    dplyr::left_join(., emb_ll2,
                     by = 'latlong2') %>%
    dplyr::left_join(., emb_ll3,
                     by = 'latlong3') %>%
    dplyr::left_join(., emb_llO2,
                     by = 'latlongO2') %>%
    dplyr::left_join(., emb_llO3,
                     by = 'latlongO3') %>%
    dplyr::left_join(., emb_ll452,
                     by = 'latlong452') %>%
    dplyr::left_join(., emb_ll453,
                     by = 'latlong453')
  
  ## Fix NA embeddings
  
  gridpred_df <- gridpred_df %>%
    dplyr::mutate(ll_2_1 = ifelse(is.na(ll_2_1), 0, ll_2_1),
                  ll_2_2 = ifelse(is.na(ll_2_2), 0, ll_2_2),
                  ll_2_3 = ifelse(is.na(ll_2_3), 0, ll_2_3),
                  ll_2_4 = ifelse(is.na(ll_2_4), 0, ll_2_4),
                  ll_2_5 = ifelse(is.na(ll_2_5), 0, ll_2_5))%>%
    dplyr::mutate(ll_3_1 = ifelse(is.na(ll_3_1), 0, ll_3_1),
                  ll_3_2 = ifelse(is.na(ll_3_2), 0, ll_3_2),
                  ll_3_3 = ifelse(is.na(ll_3_3), 0, ll_3_3),
                  ll_3_4 = ifelse(is.na(ll_3_4), 0, ll_3_4),
                  ll_3_5 = ifelse(is.na(ll_3_5), 0, ll_3_5))%>%
    dplyr::mutate(ll_O3_1 = ifelse(is.na(ll_O3_1), 0, ll_O3_1),
                  ll_O3_2 = ifelse(is.na(ll_O3_2), 0, ll_O3_2),
                  ll_O3_3 = ifelse(is.na(ll_O3_3), 0, ll_O3_3),
                  ll_O3_4 = ifelse(is.na(ll_O3_4), 0, ll_O3_4),
                  ll_O3_5 = ifelse(is.na(ll_O3_5), 0, ll_O3_5))%>%
    dplyr::mutate(ll_O2_1 = ifelse(is.na(ll_O2_1), 0, ll_O2_1),
                  ll_O2_2 = ifelse(is.na(ll_O2_2), 0, ll_O2_2),
                  ll_O2_3 = ifelse(is.na(ll_O2_3), 0, ll_O2_3),
                  ll_O2_4 = ifelse(is.na(ll_O2_4), 0, ll_O2_4),
                  ll_O2_5 = ifelse(is.na(ll_O2_5), 0, ll_O2_5))%>%
    dplyr::mutate(ll_453_1 = ifelse(is.na(ll_453_1), 0, ll_453_1),
                  ll_453_2 = ifelse(is.na(ll_453_2), 0, ll_453_2),
                  ll_453_3 = ifelse(is.na(ll_453_3), 0, ll_453_3),
                  ll_453_4 = ifelse(is.na(ll_453_4), 0, ll_453_4),
                  ll_453_5 = ifelse(is.na(ll_453_5), 0, ll_453_5))
  
  subj_df <- subj_df %>%
    dplyr::mutate(ll_2_1 = ifelse(is.na(ll_2_1), 0, ll_2_1),
                  ll_2_2 = ifelse(is.na(ll_2_2), 0, ll_2_2),
                  ll_2_3 = ifelse(is.na(ll_2_3), 0, ll_2_3),
                  ll_2_4 = ifelse(is.na(ll_2_4), 0, ll_2_4),
                  ll_2_5 = ifelse(is.na(ll_2_5), 0, ll_2_5))%>%
    dplyr::mutate(ll_3_1 = ifelse(is.na(ll_3_1), 0, ll_3_1),
                  ll_3_2 = ifelse(is.na(ll_3_2), 0, ll_3_2),
                  ll_3_3 = ifelse(is.na(ll_3_3), 0, ll_3_3),
                  ll_3_4 = ifelse(is.na(ll_3_4), 0, ll_3_4),
                  ll_3_5 = ifelse(is.na(ll_3_5), 0, ll_3_5))%>%
    dplyr::mutate(ll_O3_1 = ifelse(is.na(ll_O3_1), 0, ll_O3_1),
                  ll_O3_2 = ifelse(is.na(ll_O3_2), 0, ll_O3_2),
                  ll_O3_3 = ifelse(is.na(ll_O3_3), 0, ll_O3_3),
                  ll_O3_4 = ifelse(is.na(ll_O3_4), 0, ll_O3_4),
                  ll_O3_5 = ifelse(is.na(ll_O3_5), 0, ll_O3_5))%>%
    dplyr::mutate(ll_O2_1 = ifelse(is.na(ll_O2_1), 0, ll_O2_1),
                  ll_O2_2 = ifelse(is.na(ll_O2_2), 0, ll_O2_2),
                  ll_O2_3 = ifelse(is.na(ll_O2_3), 0, ll_O2_3),
                  ll_O2_4 = ifelse(is.na(ll_O2_4), 0, ll_O2_4),
                  ll_O2_5 = ifelse(is.na(ll_O2_5), 0, ll_O2_5))%>%
    dplyr::mutate(ll_453_1 = ifelse(is.na(ll_453_1), 0, ll_453_1),
                  ll_453_2 = ifelse(is.na(ll_453_2), 0, ll_453_2),
                  ll_453_3 = ifelse(is.na(ll_453_3), 0, ll_453_3),
                  ll_453_4 = ifelse(is.na(ll_453_4), 0, ll_453_4),
                  ll_453_5 = ifelse(is.na(ll_453_5), 0, ll_453_5))

##### Valuation Model -------------------------------------------------------------------------------  
  
  ## Prep Data
  
  # Get none spatial features
  nn_train <- train_df[, feat] 
  nn_test <- gridpred_df[, feat]  
  nn_subj <- subj_df[, feat]
  
  # Standardize
  nn_means <- colMeans(nn_train)
  nn_sds <- apply(nn_train, 2, sd)
  
  # Combine with spatial features
  nn_train <- cbind(scale(nn_train, center = nn_means, scale = nn_sds),
                    train_df %>% dplyr::select(starts_with('ll')))
  nn_test <- cbind(scale(nn_test, center = nn_means, scale = nn_sds),
                   gridpred_df %>% dplyr::select(starts_with('ll')))
  nn_subj <- cbind(scale(nn_subj, center = nn_means, scale = nn_sds),
                   subj_df %>% dplyr::select(starts_with('ll')))
  
  # Scale dependent variable
  nn_resp <- scale(log(train_df$adjusted_price),
                   mean(log(train_df$adjusted_price)),
                   sd(log(train_df$adjusted_price)))
  
  ## Prep Model
  
  val_model <- keras_model_sequential()
  val_model %>%
    layer_dense(units = 400,
                activation = 'relu',
                input_shape = ncol(nn_train)) %>%
    layer_dense(units = 1)
  
  val_model %>%
    compile(loss = 'mse',
            optimizer = 'rmsprop',
            metrics = 'mae')
  
  ## Fit Model
  val_model %>%
    fit(as.matrix(nn_train),
        as.vector(nn_resp),
        epochs = 25,
        batch_size = 450,
        validation_split = 0.2)
  
  ## Score Grid Points
  grid_pred <- val_model %>% predict(as.matrix(nn_test))
  subj_pred <- val_model %>% predict(as.matrix(nn_subj))

  ## Convert scored values into dollar space
  grid_pred <- exp((grid_pred *sd(log(train_df$adjusted_price))) +
                    mean(log(train_df$adjusted_price)))
  subj_pred <- exp((subj_pred *sd(log(train_df$adjusted_price))) +
                     mean(log(train_df$adjusted_price)))
  
  ## Make Loc Adjs
  loc_adj3A <- (as.vector(grid_pred) / as.vector(subj_pred)) - 1
  gridpred_df$loc_adj3A <- loc_adj3A
  
  ## Create new grid just for NN
  
  nngp_sf <- gridpred_df %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), 
                 crs = 4269,
                 remove = FALSE)

##### Plot: NN (3A) -----------------------   
  
  tm_basemap(basemap) +
    tm_shape(nngp_sf, bbox = plot_bbox) + 
    tm_dots(scale = plot_scale, 
            size = griddot_size,
            border.lwd = NA,
            col = 'loc_adj3A',
            breaks = ramp_breaks,
            palette = ramp_colors,
            labels = ramp_labels,
            style = 'fixed',
            title = plot_title,
            legend.show = TRUE,
            alpha = griddot_alpha) + 
    tm_shape(subj_sf) + 
    tm_dots(col = subjdot_col,
            scale = subjdot_size, 
            legend.show = FALSE) +
    tm_shape(zips_sf) + 
    tm_borders(col = zipline_col,
               lwd = zipline_size) ->
    a3_map
  a3_map
  
  a3_map %>%
    tmap::tmap_leaflet(.) %>%
    mapview::mapshot(., file = file.path(getwd(), 'plots', "a3_map.png"),
                     vwidth = plot_width, vheight = plot_height, zoom = plot_zoom)
  
#***************************************************************************************************
#***************************************************************************************************  
  