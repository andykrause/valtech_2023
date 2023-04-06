#***************************************************************************************************
#
#  Index Data Analysis
#
#***************************************************************************************************

### Setup ------------------------------------------------------------------------------------------

  library(tmap)
  library(sf)
  library(tidyverse)
  library(mapview)
  library(basemaps)

 ## Set Parameters

  base_period <- as.Date('2012-12-31')
  top_metros <- 150
  zip_limit <- 24
  covid_start <- as.Date('2020-07-01')
  peak_period <- as.Date('2022-08-31')
  current_period <- as.Date('2023-02-28')
  zgyellow_col <- '#ffab40'
  runup_col <- '#a6e5ff'
  cooloff_col <- 'blue'
  period_alpha <- 0.2
  plot_width <- 15.1
  plot_height <- 7.3
 
 ## Load Functions
  source(file.path(getwd(), 'functions', 'index_functions.R'))
  source(file.path(getwd(), 'functions', 'plotting_functions.R'))
   
 ## Load Data
  metro_idf <- readRDS(file.path(getwd(), 'data', 'metro_index.RDS')) %>%
    dplyr::mutate(year_month = format(index_date, "%Y-%m"))
  zip_idf <- readRDS(file.path(getwd(), 'data', 'zip_index.RDS'))
  zip_sf <- readRDS(file.path(getwd(), 'data', 'zip_boundaries.RDS'))
  
### Analysis ---------------------------------------------------------------------------------------

 ## Calculate Between Region Variance

  brv_df <- metro_idf %>%
    dplyr::filter(SizeRank <= top_metros) %>%
    dplyr::group_by(index_date) %>%
    dplyr::summarize(mean_mom = mean(mom_value, na.rm=TRUE),
                     sd_mom = sd(mom_value, na.rm = TRUE))

 ## Calculate Within Region Variance
  wrv_df <- zip_idf %>%
    dplyr::group_by(Metro, index_date) %>%
    dplyr::summarize(count = dplyr::n(),
                     mean = mean(mom_value, na.rm = TRUE),
                     sd = sd(mom_value, na.rm = TRUE)) %>%
    dplyr::filter(count >= zip_limit) %>%
    as.data.frame() %>%
    dplyr::group_by(index_date) %>%
    dplyr::summarize(mean_mom = mean(mean, na.rm = TRUE),
                     sd_mom = mean(sd, na.rm = TRUE))
  
### Index Plots ----------------------------------------------------------------------------------

  ## Create Covid Time Overlay  
  covind_rdf <- data.frame(x1 = covid_start,
                           x2 = peak_period,
                           y1 = 50,
                           y2 = 500)
  covmom_rdf <- data.frame(x1 = covid_start,
                           x2 = peak_period,
                           y1 = -.02,
                           y2 = .04)
  coolind_rdf <- data.frame(x1 = peak_period,
                           x2 = current_period,
                           y1 = 50,
                           y2 = 500)
  coolmom_rdf <- data.frame(x1 = peak_period,
                           x2 = current_period,
                           y1 = -.02,
                           y2 = .04)
  
#### National Index --------------------------------------------------------------------------------
  
  us_idf <- metro_idf %>%
    dplyr::filter(RegionID == 102001)
  lv_idf <- metro_idf %>%
    dplyr::filter(RegionID == 394775)
  
  ## Plot
  ggplot() + 
    geom_line(data = us_idf,
              aes(x = index_date, y = index_value),
                  color = zgyellow_col, size = 1.5) +  
    geom_rect(data = covind_rdf,
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill=runup_col, 
              alpha=period_alpha) +
    geom_rect(data = coolind_rdf,
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill=cooloff_col, 
              alpha=period_alpha) +
    xlab('') + 
    ylab("Zillow Home Value Index (ZHVI) \n(Jan '13 = 100)\n") +
    coord_cartesian(ylim = c(90, max(us_idf$index_value)*1.05),
                    xlim = lubridate::as_date(c('2013-01-01', '2023-04-06'))) + 
    scale_y_continuous(breaks = seq(100, 240, by = 20),
                       labels = seq(100, 240, by = 20)) + 
    theme_vtc(base_size =  20) ->
    base_index
  base_index
    
  ggsave(file.path(getwd(), 'plots', 'baseindex.png'), 
         base_index,
         width = plot_width,
         height = plot_height,
         bg='transparent')
  
#### With Top Metros -------------------------------------------------------------------------------
  
  # Filter to top Metros
  topmetros_idf <- metro_idf %>%
    dplyr::filter(SizeRank > 0 & SizeRank < top_metros)
  
  ## Plot
  ggplot() + 
    geom_line(data = topmetros_idf,
              aes(x = index_date, y = index_value, group=RegionID),
              color = 'white', alpha=.3, size = .7) +  
    geom_line(data = us_idf,
              aes(x = index_date, y = index_value),
              color = zgyellow_col, size = 1.8) +  
    geom_line(data = lv_idf,
              aes(x = index_date, y = index_value),
              color = 'royalblue', size = 1.2) +  
    geom_rect(data = covind_rdf,
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill=runup_col, alpha=0.2) +
    geom_rect(data = coolind_rdf,
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill='blue', alpha=0.2)+
    xlab('') + 
    ylab("Zillow Home Value Index (ZHVI) \n(Jan '13 = 100)\n") +
    coord_cartesian(ylim = c(90, max(topmetros_idf$index_value)*1.05))  +
    theme_vtc(base_size =  20) -> 
    msa_index
    msa_index
  
  ggsave(file.path(getwd(), 'plots', 'msaindex.png'), 
         msa_index,
         width = plot_width,
         height = plot_height,
         bg='transparent')
  
#### National MoM ----------------------------------------------------------------------------------  
  
  ## Plot MoM
  ggplot() +
    geom_line(data = topmetros_idf,
              aes(x = index_date, y = mom_value, group=RegionID),
              color = 'white', alpha=.1, size = .5) + 
    geom_line(data = lv_idf,
              aes(x = index_date, y = mom_value, group=RegionID),
              color = 'blue', alpha=.5, size = 1.2) + 
    geom_line(data = brv_df %>% 
                dplyr::filter(index_date > base_period), 
              aes(x = index_date, y = mean_mom),
              color = zgyellow_col, size = 1.8) +  
    geom_rect(data = covmom_rdf,
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill=runup_col, alpha=0.2) +
    geom_rect(data = coolmom_rdf,
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill='blue', alpha=0.2)+
    xlab('') + ylab("Month-over-Month\nChange in Index") +
    scale_y_continuous(breaks = c(-.01, 0, .01, .02, .03),
                       labels = c('-1%', '0%', '1%', '2%', '3%')) + 
    coord_cartesian(ylim = c(-.011, 0.03)) + 
    theme_vtc(base_size =  20) -> 
    seasonal_index
    seasonal_index

    ggsave(file.path(getwd(), 'plots', 'seasonalindex.png'), 
           seasonal_index,
           width = plot_width,
           height = plot_height,
           bg='transparent')

#### Interregional Differences ---------------------------------------------    

  ggplot() +
    geom_point(data = brv_df %>%
                dplyr::filter(index_date > base_period), 
              aes(x = index_date, y = sd_mom), color = zgyellow_col, alpha = .5) + 
    tidyquant::geom_ma(data = brv_df %>% 
                         dplyr::filter(index_date > base_period) %>%
                         dplyr::mutate(index_date = index_date - 60), 
                       aes(x = index_date, y = sd_mom), color = zgyellow_col,
                       n = 6, ma_fun = SMA, linetype = 1, size = 1.5)+
    geom_rect(data = covmom_rdf,
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill=runup_col, alpha=0.2) +
    geom_rect(data = coolmom_rdf,
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill='blue', alpha=0.2)+
    xlab('') + ylab("StDev of Between Region\nMoM Differences") +
    coord_cartesian(ylim = c(0, 0.0075))+ 
    theme_vtc(base_size =  20)  ->
    interregion_index
  interregion_index
  
  ggsave(file.path(getwd(), 'plots', 'interregionindex.png'), 
         interregion_index,
         width = plot_width,
         height = plot_height,
         bg='transparent')
  
#### Intraregional differences ---------------------------------------------------------------------
  
  ggplot() +
    geom_point(data = wrv_df %>% 
                dplyr::filter(index_date > base_period), 
              aes(x = index_date, y = sd_mom), color = zgyellow_col, alpha = .3) +  
    tidyquant::geom_ma(data = wrv_df %>% 
                         dplyr::mutate(index_date = index_date - 60) %>%
                         dplyr::filter(index_date > base_period), 
                       aes(x = index_date, y = sd_mom), color = zgyellow_col,
                       n = 6, ma_fun = SMA, linetype = 1, size = 1.5) +
    geom_rect(data = covmom_rdf,
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill=runup_col, alpha=0.2) +
    geom_rect(data = coolmom_rdf,
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill='blue', alpha=0.2)+
    coord_cartesian(ylim = c(0.0028, 0.006)) + 
    xlab('') + ylab("Mean of StDev of Within Region\nMoM Differences") +
    theme_vtc(base_size =  20)  ->
    intraregion_index
    intraregion_index
  
    ggsave(file.path(getwd(), 'plots', 'intraregionindex.png'), 
           intraregion_index,
           width = plot_width,
           height = plot_height,
           bg='transparent') 
 
### Mapping ----------------------------------------------------------------------    

 ## Set Basemaps
  basemaps::reset_defaults()
  basemaps::set_defaults(map_service = "mapbox", 
                         map_type = "dark", 
                         map_token = Sys.getenv('mapbox_token'),
                         map_res = 1)

 ## Set mapping type
  tmap::tmap_mode('view')
  
 ## Create Data
    
  # Run up Indexes
  ziprunup_df <- zip_idf %>%
    dplyr::filter(index_date %in% c(covid_start - 1, peak_period)) %>%
    dplyr::group_by(RegionID, RegionName, State, CountyName) %>%
    summarize(index_value_r = index_value[2] / index_value[1] * 100) %>%
    dplyr::ungroup()
   
  # Cool Down Indexes 
  zipcool_df <- zip_idf %>%
    dplyr::filter(index_date %in% c(peak_period, current_period)) %>%
    dplyr::group_by(RegionID, RegionName, State, CountyName) %>%
    summarize(index_value_c = index_value[2] / index_value[1] * 100) %>%
    dplyr::ungroup()
  
  # Combine and Compare
  zipcompare_sf <- zip_sf %>%
    sf::st_transform(., 3857) %>%
    dplyr::left_join(., ziprunup_df %>%
      dplyr::left_join(., zipcool_df %>%
                         dplyr::select(RegionID, index_value_c),
                       by = 'RegionID'),
       by = c('ZIP5' = 'RegionName'))
  
  ## Set colors
  appr_colors <- c(rev(RColorBrewer::brewer.pal(5, 'Blues')), 'white', 
                   RColorBrewer::brewer.pal(5, 'Oranges'))
  map_height = 1200
  map_width = 1200
  
#### Seattle Example --------------------------------------------------------------
  
  ## Prepare Data
  sea_sf <- zipcompare_sf %>%
    dplyr::filter(State == 'WA' &
                    CountyName %in% c("King County", 'Pierce County',
                                      'Snohomish County', 'Kitsap County')) %>%
    dplyr::filter(!ZIP5 %in% c(98068, 98223, 98292, 98328, 98256, 98022, 98251, 98252)) %>%
    dplyr::mutate(covid_appr = makeDeciles(index_value_r),
                  cool_appr = makeDeciles(index_value_c))
  
  # Scale BBOX
  sv <- 20000
  scaled_bbox <- c(sf::st_bbox(sea_sf)[1:2] + sv,
                   sf::st_bbox(sea_sf)[3:4] - sv)
  
  ## Runup Map
  tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
    tm_shape(sea_sf, bbox = scaled_bbox) + 
    tm_polygons(col = 'covid_appr', 
                fill = 'covid_appr', 
                alpha = .6, 
                breaks = 0:10,
                n=11,
                palette = appr_colors,
                scale = 1, legend.show = FALSE)  + 
    tm_add_legend('fill', 
                  alpha = .6,
                  col = appr_colors,
                  border.col = "grey40",
                  labels = c('','Lowest','','Low','','Mid',
                             '', 'High','', 'Highest',''),
                  title="Appr. Rates") ->
    seaR_map
  seaR_map
  
  seaR_map %>%
    tmap::tmap_leaflet(.) %>%
    mapview::mapshot(., file = file.path(getwd(), 'plots', "sea_runup.png"),
                     vwidth = map_width, vheight=map_height, zoom = 1)

  ## Cool Off Map
  tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
  tm_shape(sea_sf, bbox = scaled_bbox) + 
    tm_polygons(col = 'cool_appr', 
                fill = 'cool_appr', 
                alpha = .6, 
                breaks = 0:10,
                n=11,
                palette = appr_colors,
                scale = 1, legend.show = FALSE)  + 
    tm_add_legend('fill', 
                  alpha = .6,
                  col = appr_colors,
                  border.col = "grey40",
                  labels = c('','Lowest','','Low','','Mid',
                             '', 'High','', 'Highest',''),
                  title="Appr. Rates") ->
    seaC_map
  seaC_map
  
  seaC_map %>%
    tmap::tmap_leaflet(.) %>%
    mapview::mapshot(., file = file.path(getwd(), 'plots', "sea_cooloff.png"),
                     vwidth = map_width, vheight=map_height, zoom = 1)
  
#### Las Vegas Example --------------------------------------------------------------

  ## Prepare Data
  lv_sf <- zipcompare_sf %>%
    dplyr::filter(State == 'NV' &
                    CountyName %in% c('Clark County')) %>%
    dplyr::filter(!ZIP5 %in% c(89029, 89027, 89007, 89021, 89040)) %>%
    dplyr::mutate(covid_appr = makeDeciles(index_value_r),
                  cool_appr = makeDeciles(index_value_c))
  
  # Scale BBOX
  sv <- 20000
  scaled_bbox <- c(sf::st_bbox(lv_sf)[1:2] + sv,
                   sf::st_bbox(lv_sf)[3:4] - sv)
  
  ## Runup Map
  tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
    tm_shape(lv_sf, bbox = scaled_bbox) + 
    tm_polygons(col = 'covid_appr', 
                fill = 'covid_appr', 
                alpha = .6, 
                breaks = 0:10,
                n=11,
                palette = appr_colors,
                scale = 1, legend.show = FALSE)  + 
    tm_add_legend('fill', 
                  alpha = .6,
                  col = appr_colors,
                  border.col = "grey40",
                  labels = c('','Lowest','','Low','','Mid',
                             '', 'High','', 'Highest',''),
                  title="Appr. Rates") ->
    lvR_map
  lvR_map
  
  lvR_map %>%
    tmap::tmap_leaflet(.) %>%
    mapview::mapshot(., file = file.path(getwd(), 'plots', "lv_runup.png"),
                     vwidth = map_width, vheight=map_height, zoom = 1)
  
  ## Cool Off Map
  tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
    tm_shape(lv_sf, bbox = scaled_bbox) + 
    tm_polygons(col = 'cool_appr', 
                fill = 'cool_appr', 
                alpha = .6, 
                breaks = 0:10,
                n=11,
                palette = appr_colors,
                scale = 1, legend.show = FALSE)  + 
    tm_add_legend('fill', 
                  alpha = .6,
                  col = appr_colors,
                  border.col = "grey40",
                  labels = c('','Lowest','','Low','','Mid',
                             '', 'High','', 'Highest',''),
                  title="Appr. Rates") ->
    lvC_map
  lvC_map
  
  lvC_map %>%
    tmap::tmap_leaflet(.) %>%
    mapview::mapshot(., file = file.path(getwd(), 'plots', "lv_cooloff.png"),
                     vwidth = map_width, vheight=map_height, zoom = 1)

### Save Data Objects --------------------------------------------------------------------  
  
  saveRDS(sea_sf %>%
            dplyr::filter(CountyName == 'King County') %>%
            dplyr::select(ZIP5, RegionID, geometry, index_value_r, index_value_c) %>%
            sf::st_transform(., crs = 4269), 
          file.path(getwd(), 'data', 'king_zips.RDS'))
  
#***************************************************************************************************
#***************************************************************************************************