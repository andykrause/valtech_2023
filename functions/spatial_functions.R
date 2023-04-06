

### Feature Engineering ----------------------------------------------------------------------------


#'
#' Rotate coordinates
#'
#' @param data_df Raw data
#' @param lon_field [longitude] Field containing the longitude value
#' @param lat_field [latitude] Field containing the latitude value
#' @param rotations [c(10, 15, 30, 45)] Degrees of rotation
#' @param ... Additional arguments
#' @return Original data with rotated coordinates appended
#' @export

rotateCoordinates <- function(data_df,
                              lon_field = 'longitude',
                              lat_field = 'latitude',
                              rotations = c(10, 15, 30, 45),
                              ...){
  
  for (rt in 1:length(rotations)){
    
    rot_coords <- rotateCoords(x = data_df[[lon_field]],
                               y = data_df[[lat_field]],
                               angle = rotations[rt],
                               ...)
    data_df[[paste0('lon', rotations[rt])]] <- rot_coords$V1
    data_df[[paste0('lat', rotations[rt])]] <- rot_coords$V2
  }
  data_df
}

#'
#' Convert to polar coordinates
#'
#' @param x Longitude values
#' @param y Latitude values
#' @param angle Degress to rotate
#' @importFrom spdep Rotation
#' @return Rotated coordinates
#' @export

rotateCoords <- function(x, y, angle){
  
  coords <- cbind(x, y)
  spdep::Rotation(coords, angle * pi / 180) %>% as.data.frame()
}



