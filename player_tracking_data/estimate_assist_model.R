#### 
# Script Requirements:
# potential_assists.RData (produced by get_potential_assists.R)
###

library(glmnet)
library(xgboost)

source('team_codes.R')

load('potential_assists.RData')
load('player_id_list.RData')

set.seed(1)

home = as.integer(as.character(potential_assists$home))

pa_team = as.numeric(as.character(potential_assists$team))
pa_opponent = as.numeric(as.character(potential_assists$opponent))

team = team_codes[pa_team]
opponent = team_codes[pa_opponent]

pa_sk = rep(NA,length(home))
home_index = which(home==1)
away_index = which(home==0)
pa_sk[home_index] = as.character(team)[home_index]
pa_sk[away_index] = as.character(opponent)[away_index]
scorekeeper = pa_sk

passer = as.factor(potential_assists$passer)
shooter = as.factor(potential_assists$shooter)

dribbles = as.numeric(as.character(potential_assists$dribbles))
distance_travelled = as.numeric(as.character(potential_assists$distance_travelled))

poss_length = as.numeric(as.character(potential_assists$poss_length))
pass_distance = as.numeric(as.character(potential_assists$pass_distance))

pass_court_location = potential_assists$pass_court_location
poss_court_location = potential_assists$poss_court_location
shot_court_location = potential_assists$shot_court_location

pass_defender_distance = as.numeric(as.character(potential_assists$pass_defender_distance))
poss_defender_distance = as.numeric(as.character(potential_assists$poss_defender_distance))
shot_defender_distance = as.numeric(as.character(potential_assists$shot_defender_distance))

assist = potential_assists$recorded_assist

team = as.factor(team)
opponent = as.factor(opponent)
scorekeeper = as.factor(scorekeeper)

passer_position = rep(NA,length(passer))
for (i in 1:length(passer)){
  passer_position[i] = as.character(player_id_list$positions[which(player_id_list$player_id == as.numeric(as.character(passer[i])))])
}
passer_position = as.factor(passer_position)

contrast_list = list(team = contrasts(team, contrasts=F), opponent = contrasts(opponent, contrasts=F), scorekeeper = contrasts(scorekeeper, contrasts=F), passer = contrasts(passer, contrasts=F), passer_position = contrasts(passer_position, contrasts=F), pass_court_location = contrasts(pass_court_location, contrasts=F), poss_court_location = contrasts(poss_court_location, contrasts=F))

# Scale and center 
dribbles = (dribbles - mean(dribbles))/sd(dribbles)
distance_travelled = (distance_travelled - mean(distance_travelled))/sd(distance_travelled)
poss_length = (poss_length - mean(poss_length))/sd(poss_length)
pass_distance = (pass_distance - mean(pass_distance))/sd(pass_distance)
pass_defender_distance = (pass_defender_distance - mean(pass_defender_distance))/sd(pass_defender_distance)
poss_defender_distance = (poss_defender_distance - mean(poss_defender_distance))/sd(poss_defender_distance)

data_frame = data.frame(assist, home, team, opponent, scorekeeper, passer, passer_position, dribbles, distance_travelled, poss_length, pass_distance, pass_court_location, poss_court_location, pass_defender_distance, poss_defender_distance)
formula = as.formula(assist~home+team+opponent+scorekeeper+home:scorekeeper+passer+passer_position+dribbles+distance_travelled+poss_length+pass_distance+pass_court_location+poss_court_location+pass_court_location:poss_court_location+pass_defender_distance+poss_defender_distance)

###
# At this step the code will stop working because there is only one game of data (so there is no identifiability)
###
data = sparse.model.matrix(formula,data_frame,contrasts.arg=contrast_list)

full_model = cv.glmnet(data, assist, nfolds=100, type.measure = "class", alpha=0, family="binomial", standardize=FALSE)

