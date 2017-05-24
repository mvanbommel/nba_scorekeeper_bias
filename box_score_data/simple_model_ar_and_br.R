library(png)
library(raster)

source('get_team_id.R')
source('team_codes.R')

# Load the box score data and remove the id column.
# The box score data was scraped from ESPN using the scrape_nba_box_scores
# code available at https://github.com/mvanbommel/scrape_nba_box_scores
full_box_score = read.csv('2015_2016_NBA_box_score_team_data.csv')[, -1]

# Extract the necessary information from full_box_score
team_ids = sapply(full_box_score$Team, get_team_id)
box_score = cbind(full_box_score$Home, team_ids, full_box_score$FGM, full_box_score$AST, full_box_score$BLK)
colnames(box_score) = c('home', 'team', 'fgm', 'ast', 'blk')
box_score = as.data.frame(box_score)

home_box_score = box_score[which(box_score$home==1),]
away_box_score = box_score[which(box_score$home==0),]

model_data = matrix(NA, nrow=nrow(box_score), ncol=6)
colnames(model_data) = c('ar', 'br', 'home', 'team', 'opponent', 'scorekeeper')

for(i in 1:nrow(home_box_score)){
  # Compute model data for home team
  ar = home_box_score$ast[i] / home_box_score$fgm[i]
  br = home_box_score$blk[i] / away_box_score$fgm[i]
  home = 1
  team = team_codes[home_box_score$team[i]]
  opponent = team_codes[away_box_score$team[i]]
  scorekeeper = team
  model_data[i,] = c(ar, br, home, team, opponent, scorekeeper)
  
  # Compute model data for away team
  ar = away_box_score$ast[i] / away_box_score$fgm[i]
  br = away_box_score$blk[i] / home_box_score$fgm[i]
  home = 0
  team = team_codes[away_box_score$team[i]]
  opponent = team_codes[home_box_score$team[i]]
  scorekeeper = opponent
  model_data[i+1230,] = c(ar, br, home, team, opponent, scorekeeper)
}

# Compute the season average AR and BR
avg_ar_2016 = sum(box_score$ast) / sum(box_score$fgm)
avg_br_2016 = sum(box_score$blk) / sum(box_score$fgm)

model_data = as.data.frame(model_data)

ar = as.numeric(as.character(model_data$ar))
br = as.numeric(as.character(model_data$br))
home = as.integer(as.character(model_data$home))
team = model_data$team
opponent = model_data$opponent
scorekeeper = model_data$scorekeeper

# Estimate linear models for AR and BR
ar_model = lm(ar ~  + team + opponent + scorekeeper + scorekeeper:home)
br_model = lm(br ~  + team + opponent + scorekeeper + scorekeeper:home)

# Compute scorekeeper bias and scorekeeper generosity effects for each scorekeeper for both AR and BR
ar_scorekeeper_bias = avg_ar_2016 + scale(append(0,coefficients(ar_model)[90:118]), center=TRUE, scale=FALSE) + scale(append(0,coefficients(ar_model)[60:88]), center=TRUE, scale=FALSE)
ar_scorekeeper_generosity = avg_ar_2016 + scale(append(0,coefficients(ar_model)[60:88]), center=TRUE, scale=FALSE)
br_scorekeeper_bias = avg_br_2016 + scale(append(0,coefficients(br_model)[90:118]), center=TRUE, scale=FALSE) + scale(append(0,coefficients(br_model)[60:88]), center=TRUE, scale=FALSE)
br_scorekeeper_generosity = avg_br_2016 + scale(append(0,coefficients(br_model)[60:88]), center=TRUE, scale=FALSE)



###
# AR logo plot
###
# Determine the plot limits if the logos had no size
size = 0
bounds = cbind(ar_scorekeeper_bias-size,ar_scorekeeper_generosity-size,ar_scorekeeper_bias+size,ar_scorekeeper_generosity+size)
minx = min(bounds[,1],bounds[,3])
maxx = max(bounds[,3],bounds[,1])
miny = min(bounds[,2],bounds[,4])
maxy = max(bounds[,4],bounds[,2])
minxy = min(minx,miny)
maxxy = max(maxx,maxy)

# Update the plot limits based on a new relative logo size
size = (maxxy - minxy)/15
bounds = cbind(ar_scorekeeper_bias-size,ar_scorekeeper_generosity-size,ar_scorekeeper_bias+size,ar_scorekeeper_generosity+size)
minx = min(bounds[,1],bounds[,3])
maxx = max(bounds[,3],bounds[,1])
miny = min(bounds[,2],bounds[,4])
maxy = max(bounds[,4],bounds[,2])
minxy = min(minx,miny)
maxxy = max(maxx,maxy)

str_count = 25

png('2016_model_ar_logo.png', width=1500, height=1500, res = 150)
setwd("./team_logos")
par(mar=c(4,4.5,0,0.1))
plot(NA,xlab="Predicted Home Team Assist Ratio",ylab="Predicted Away Team Assist Ratio",xlim=c(minxy,maxxy),ylim=c(minxy,maxxy),xaxp  = c(0.45, 0.65, 5),yaxp  = c(0.45, 0.65, 5),cex.lab=2, cex.main=3, cex.axis=2)
text(avg_ar_2016, miny, paste(rep("League Average - ", str_count),collapse = ""), cex=1.5, srt=90, col="gray30")
text(minx, avg_ar_2016, paste(rep("League Average - ", str_count),collapse = ""), cex=1.5, col="gray30")
for (i in 1:30){
  img <- readPNG(paste(team_codes[i],".png",sep=""))
  rasterImage(img,bounds[i,1],bounds[i,2],bounds[i,3],bounds[i,4], interpolate = FALSE)
}
setwd('..')
dev.off()



###
# BR logo plot
###
# Determine the plot limits if the logos had no size
size = 0
bounds = cbind(br_scorekeeper_bias-size,br_scorekeeper_generosity-size,br_scorekeeper_bias+size,br_scorekeeper_generosity+size)
minx = min(bounds[,1],bounds[,3])
maxx = max(bounds[,3],bounds[,1])
miny = min(bounds[,2],bounds[,4])
maxy = max(bounds[,4],bounds[,2])
minxy = min(minx,miny)
maxxy = max(maxx,maxy)

# Update the plot limits based on a new relative logo size
size = (maxxy - minxy)/15
bounds = cbind(br_scorekeeper_bias-size,br_scorekeeper_generosity-size,br_scorekeeper_bias+size,br_scorekeeper_generosity+size)
minx = min(bounds[,1],bounds[,3])
maxx = max(bounds[,3],bounds[,1])
miny = min(bounds[,2],bounds[,4])
maxy = max(bounds[,4],bounds[,2])
minxy = min(minx,miny)
maxxy = max(maxx,maxy)

str_count = 25

png('2016_model_br_logo.png', width=1500, height=1500, res = 150)
setwd("./team_logos")
par(mar=c(4,4.5,0,0.1))
plot(NA,xlab="Predicted Home Team Block Ratio",ylab="Predicted Away Team Block Ratio",xlim=c(minxy,maxxy),ylim=c(minxy,maxxy),xaxp  = c(0.05, 0.2, 6),yaxp  = c(0.05, 0.2, 6),cex.lab=2, cex.main=3, cex.axis=2)
text(avg_br_2016, miny, paste(rep("League Average - ", str_count),collapse = ""), cex=1.5, srt=90, col="gray30")
text(minx, avg_br_2016, paste(rep("League Average - ", str_count),collapse = ""), cex=1.5, col="gray30")
for (i in 1:30){
  img <- readPNG(paste(team_codes[i],".png",sep=""))
  rasterImage(img,bounds[i,1],bounds[i,2],bounds[i,3],bounds[i,4], interpolate = FALSE)
}
setwd('..')
dev.off()
