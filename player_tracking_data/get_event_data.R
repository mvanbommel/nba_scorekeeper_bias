#### 
# Script Requirements:
# moment_data.RData (produced by change_player_tracking_data_format.R)
###

library(dplyr)

load('moment_data.RData')

event_data = filter(moment_data, !is.na(event_id))

save(event_data, file='event_data.RData')
