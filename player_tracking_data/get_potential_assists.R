#### 
# Script Requirements:
# moment_data.RData (produced by change_player_tracking_data_format.R)
# event_data.RData (produced by get_event_data.R)
###

library(fields)
library(dplyr)

source('get_court_location.R')

load('moment_data.RData')
load('event_data.RData')

fgm = which(event_data$event_id == 3)

if(fgm[1] == 2 | fgm[1] == 1){
  fgm_poss = fgm_poss[-1]
  fgm = fgm[-1]
}

# Make a list of events which imply possession
poss = which(event_data$event_id == 23 | event_data$event_id == 5 | event_data$event_id == 6 | event_data$event_id == 3 | event_data$event_id == 4)

# Determine the index of the posession event for each fgm
fgm_poss = rep(NA, length(fgm))
for (i in 1:length(fgm)){
  fgm_poss[i] = max(poss[which(poss<fgm[i])])
}
# If someone scores right off the tip off, remove since there is no associated pass
if(fgm_poss[1] == 1){
  fgm_poss = fgm_poss[-1]
  fgm = fgm[-1]
}

# The method of possession is the event before the possession event
get_poss   = fgm_poss - 1

# Determine which fgm had a pass or assist as the method of possession
pass_poss  = which(event_data$event_id[get_poss] == 25 | event_data$event_id[get_poss] == 22)

# Get the index of the pass, possession, and shot event for each fgm
pass_index = get_poss[pass_poss]
poss_index = pass_index+1
shot_index = fgm[pass_poss]

# Get the row corresponding to the pass, possession, and shot for each fgm
passes = event_data[pass_index,]
possessions = event_data[poss_index,]
shots = event_data[shot_index, ]

# Determine the home and away teams 
home_team = 3
away_team = 16

# Determine the number of fgm in the game
N = length(shot_index)

# Determine the number of dribbles and the distance travelled (assumed to be 0 if no dribbles)
dribbles = rep(0, N)
distance_travelled = rep(0,N)
for(i in 1:N){
  dribbles[i] = length(which(event_data$event_id[poss_index[i]:shot_index[i]]==21))
  if(dribbles[i] != 0){
    # Total distance includes possession to first dribble, each dribble to next, and last dribble to shot
    total_distance = 0
    for (j in 0:(dribbles[i])){
      dribble_distance = dist(rbind(cbind(event_data$x[poss_index[i]+j],event_data$y[poss_index[i]+j]),cbind(event_data$x[poss_index[i]+j+1],event_data$y[poss_index[i]+j+1])))
      total_distance   = total_distance + dribble_distance    
    }
    distance_travelled[i] = total_distance
  }
}

# Determine the basket directions of each team
left_basket  = cbind(5.25,25)
right_basket = cbind(88.75,25)
baskets      = rbind(left_basket,right_basket)

home_fg1_xy = cbind(mean(shots$x[which(shots$team==home_team & shots$quarter<3)]), mean(shots$y[which(shots$team==home_team & shots$quarter<3)]))
away_fg1_xy = cbind(mean(shots$x[which(shots$team==away_team & shots$quarter<3)]), mean(shots$y[which(shots$team==away_team & shots$quarter<3)]))

home_fg2_xy = cbind(mean(shots$x[which(shots$team==home_team & shots$quarter>2)]), mean(shots$y[which(shots$team==home_team & shots$quarter>2)]))
away_fg2_xy = cbind(mean(shots$x[which(shots$team==away_team & shots$quarter>2)]), mean(shots$y[which(shots$team==away_team & shots$quarter>2)]))

home_basket_1 = which.min(rdist(rbind(home_fg1_xy,baskets))[2:3,1])
away_basket_1 = which.min(rdist(rbind(away_fg1_xy,baskets))[2:3,1])

home_basket_2 = which.min(rdist(rbind(home_fg2_xy,baskets))[2:3,1])
away_basket_2 = which.min(rdist(rbind(away_fg2_xy,baskets))[2:3,1])

if (home_basket_1==away_basket_1 | home_basket_2==away_basket_2 | home_basket_1==home_basket_2 | away_basket_1==away_basket_2){
  print(g)
  stop("Baskets are mixed up.")
}

basket = matrix(rep(cbind(0,0),each=N),nrow=N)
for (b in 1:N){
  if (b %in% which((event_data$team[poss_index]==home_team & event_data$quarter[poss_index]<3) | (event_data$team[poss_index]==away_team & event_data$quarter[poss_index]>2))){
    basket[b,] = baskets[home_basket_1,]
  } 
  if (b %in% which((event_data$team[poss_index]==home_team & event_data$quarter[poss_index]>2) | (event_data$team[poss_index]==away_team & event_data$quarter[poss_index]<3))){
    basket[b,] = baskets[home_basket_2,]
  }
  if(b %in% which((event_data$team[poss_index]==home_team & event_data$quarter[poss_index]<3) | (event_data$team[poss_index]==away_team & event_data$quarter[poss_index]>2)) & b %in% which((event_data$team[poss_index]==home_team & event_data$quarter[poss_index]>2) | (event_data$team[poss_index]==away_team & event_data$quarter[poss_index]<3))){
    print(b)
    stop("Both baskets")
  }
}

offense_basket = rep("L",N)
offense_basket[which(basket[,1]==88.75)]="R"

# Determine the distance to the basket when the shooter catches the ball
catch_position        = cbind(event_data$x[poss_index], event_data$y[poss_index])
basket_distance_catch = diag(rdist(catch_position, basket))

# Determine the indicies corresponding to the home and away teams
ht_index = which(event_data$team[poss_index] == home_team)
at_index = which(event_data$team[poss_index] == away_team)

# Determine whether the pass was a recorded assist
recorded_assist = rep(-1,N)
recorded_assist[which(passes$event_id==25)]=1
recorded_assist[which(passes$event_id==22)]=0

# Determine the entities of the passer and shooter
passer = passes$entity
shooter = shots$entity

# Determine contextual information from the data
game = passes$game
quarter = passes$quarter
pass_time = passes$time
poss_time = possessions$time
shot_time = shots$time

pass_shot_clock = passes$shot_clock
pass_game_clock = passes$game_clock
poss_shot_clock = possessions$shot_clock
poss_game_clock = possessions$game_clock
shot_shot_clock = shots$shot_clock
shot_game_clock = shots$game_clock

pass_x = passes$x
pass_y = passes$y
poss_x = possessions$x
poss_y = possessions$y
shot_x = shots$x
shot_y = shots$y

team = rep(home_team, N)
team[at_index] = away_team
opponent = rep(home_team, N)
opponent[ht_index] = away_team

home = rep(0,N)
home[ht_index] = 1

# Possession length (catch to shot)
poss_length = poss_game_clock - shot_game_clock
if(any(poss_length<0)){
  negative_index = which(poss_length<0)
  poss_length[negative_index] = poss_shot_clock[negative_index] - shot_shot_clock[negative_index]
}

pass_distance = rep(NA, N)
for(i in 1:N){
  pass_distance[i] = dist(rbind(cbind(pass_x[i], pass_y[i]), cbind(poss_x[i], poss_y[i])))
}

poss_court_location = as.character(get_court_location(poss_x, poss_y, offense_basket))
pass_court_location = as.character(get_court_location(pass_x, pass_y, offense_basket))
shot_court_location = as.character(get_court_location(shot_x, shot_y, offense_basket))

potential_assists = data.frame(cbind(game, quarter, team, opponent, home, offense_basket, recorded_assist, passer, pass_time, pass_shot_clock, pass_game_clock, pass_x, pass_y, shooter, poss_time, poss_shot_clock, poss_game_clock, poss_x, poss_y, shot_time, shot_shot_clock, shot_game_clock, shot_x, shot_y, dribbles, distance_travelled, poss_length, pass_distance, poss_court_location, pass_court_location, shot_court_location))


# Add defender distances
pass_defender_distance = rep(NA,nrow(potential_assists))
poss_defender_distance = rep(NA,nrow(potential_assists))
for (i in 1:nrow(potential_assists)) {
  pass_time = as.numeric(as.character(potential_assists[,'pass_time'][i]))
  poss_time = as.numeric(as.character(potential_assists[,'poss_time'][i]))
  
  pass_position = as.numeric(as.character(cbind(potential_assists[,'pass_x'][i], potential_assists[,'pass_y'][i])))
  poss_position = as.numeric(as.character(cbind(potential_assists[,'poss_x'][i], potential_assists[,'poss_y'][i])))
  
  game = as.numeric(as.character(potential_assists[,'game'][i]))
  home = as.numeric(as.character(potential_assists[,'home'][i]))
  
  if (home == 1){
    pass_defenders = filter(moment_data, game==game, time==pass_time, team==away_team)[c('x', 'y')]
    poss_defenders = filter(moment_data, game==game, time==poss_time, team==away_team)[c('x', 'y')]
  } else {
    pass_defenders = filter(moment_data, game==game, time==pass_time, team==home_team)[c('x', 'y')]
    poss_defenders = filter(moment_data, game==game, time==poss_time, team==home_team)[c('x', 'y')]
  }
  
  if(nrow(pass_defenders)!=5 | nrow(poss_defenders)!=5){
    next
  }
  
  pass_defender_distance_matrix = as.matrix(dist(rbind(pass_position, cbind(pass_defenders[,1],pass_defenders[,2]))))
  poss_defender_distance_matrix = as.matrix(dist(rbind(poss_position, cbind(poss_defenders[,1],poss_defenders[,2]))))
  
  pass_defender_distance[i] = min(pass_defender_distance_matrix[2:6,1])
  poss_defender_distance[i] = min(poss_defender_distance_matrix[2:6,1])
}

potential_assists = cbind(potential_assists, poss_defender_distance, pass_defender_distance)
save(potential_assists, file='potential_assists.RData')
