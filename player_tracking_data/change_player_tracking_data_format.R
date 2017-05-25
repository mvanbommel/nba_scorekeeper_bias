###
# This script changes the format of the publicly available player tracking data
# to match the format of the player tracking data I used in my project.
###

# Load the player tracking data (obtained from https://github.com/dcervone/EPVDemo/blob/master/data/2013_11_01_MIA_BKN.csv)
data = read.csv('2013_11_01_MIA_BKN.csv')

convert_row = function(row) {
  game = rep(row['game'], 11)
  quarter = rep(row['quarter'], 11)
  time = rep(row['time'], 11)
  shot_clock = rep(row['shot_clock'], 11)
  game_clock = rep(row['game_clock'], 11)
  
  team = c(-1, rep(3, 5), rep(16, 5)) # -1 = Ball, 3 = Brooklyn (home), 16 = Miami (away)
  entity = c(-1, row['h1_ent'], row['h2_ent'], row['h3_ent'], row['h4_ent'], row['h5_ent'], row['a1_ent'], row['a2_ent'], row['a3_ent'], row['a4_ent'], row['a5_ent'])
  x = c(row['x'], row['h1_x'], row['h2_x'], row['h3_x'], row['h4_x'], row['h5_x'], row['a1_x'], row['a2_x'], row['a3_x'], row['a4_x'], row['a5_x'])
  y = c(row['y'], row['h1_y'], row['h2_y'], row['h3_y'], row['h4_y'], row['h5_y'], row['a1_y'], row['a2_y'], row['a3_y'], row['a4_y'], row['a5_y'])
  z = c(row['z'], rep(0, 10)) # Only the ball has a z-coordinate
  event_id = c(NA, row['h1_event'], row['h2_event'], row['h3_event'], row['h4_event'], row['h5_event'], row['a1_event'], row['a2_event'], row['a3_event'], row['a4_event'], row['a5_event'])
  
  new_data = cbind(game, quarter, time, shot_clock, game_clock, team, entity, x, y, z, event_id)

  return(new_data)
}


transformed_tracking_data_list = list(nrow(data))
for (r in 1:nrow(data)) {
  # Print progress of for loop
  if (r %% 1000 == 0) {
    print(r / nrow(data))
  }
  transformed_tracking_data_list[[r]] = as.matrix(convert_row(data[r,]))
}

moment_data = do.call(rbind, transformed_tracking_data_list)

# Unlist the columns of moment_data and create a data frame
game = unlist(moment_data[,1])
quarter = unlist(moment_data[,2])
time = unlist(moment_data[,3])
shot_clock = unlist(moment_data[,4])
game_clock = unlist(moment_data[,5])
team = unlist(moment_data[,6])
entity = unlist(moment_data[,7])
x = unlist(moment_data[,8])
y = unlist(moment_data[,9])
z = unlist(moment_data[,10])
event_id = unlist(moment_data[,11])

moment_data = data.frame(game, quarter, time, shot_clock, game_clock, team, entity, x, y, z, event_id)
save(moment_data, file='moment_data.RData')
