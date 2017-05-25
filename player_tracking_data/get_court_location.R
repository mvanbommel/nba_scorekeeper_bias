get_court_location <- function(x,y,offense_basket){
  # Only works because of symmetry 
  
  location_id <- rep(NA, length(x))
  
  basket_right = which(offense_basket=="R")
  
  x[basket_right] = 94-x[basket_right]
  
  x_new = y-25
  y_new = x-5.25
  
  x = x_new
  y = y_new
  
  location_id[sqrt(x^2 + y^2) < 3] <- 'dunk'
  location_id[x < 6 &  x > -6 & y < 13.75 & is.na(location_id)] <- 'paint'
  location_id[sqrt(x^2 + y^2) < 23.75 & x<=22 & x>=-22 & is.na(location_id)] <- 'long2'
  location_id[(x>22 | x < -22) & y < 8.75] <- 'corner3'
  location_id[is.na(location_id)] <- 'arc3'
  location_id[(sqrt(x^2 + y^2) >= 33.75)] <- 'heave'
  
  factor(location_id)
}