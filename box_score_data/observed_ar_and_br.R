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

# Compute home and away assist ratio (AR) and block ratio (BR) for all 30 scorekeepers
sk = 1:30
sk_home_ar = rep(NA, 30)
sk_away_ar = rep(NA, 30)
sk_home_br = rep(NA, 30)
sk_away_br = rep(NA, 30)
for(i in 1:30){
  sk_index = which(home_box_score$team==sk[i])
  
  sk_home_ar[i] = sum(home_box_score$ast[sk_index]) / sum(home_box_score$fgm[sk_index])
  sk_away_ar[i] = sum(away_box_score$ast[sk_index]) / sum(away_box_score$fgm[sk_index])
  
  sk_home_br[i] = sum(home_box_score$blk[sk_index]) / sum(away_box_score$fgm[sk_index])
  sk_away_br[i] = sum(away_box_score$blk[sk_index]) / sum(home_box_score$fgm[sk_index])
}

sk_ar_results = cbind(sk,sk_home_ar,sk_away_ar)

# Compute the season average AR and BR
avg_ar_2016 = sum(box_score$ast) / sum(box_score$fgm)
avg_br_2016 = sum(box_score$blk) / sum(box_score$fgm)


###
# AR logo plot
###
# Determine the plot limits if the logos had no size
size = 0
bounds = cbind(sk_home_ar-size,sk_away_ar-size,sk_home_ar+size,sk_away_ar+size)
minx = min(bounds[,1],bounds[,3])
maxx = max(bounds[,3],bounds[,1])
miny = min(bounds[,2],bounds[,4])
maxy = max(bounds[,4],bounds[,2])
minxy = min(minx,miny)
maxxy = max(maxx,maxy)

# Update the plot limits based on a new relative logo size
size = (maxxy-minxy)/15
bounds = cbind(sk_home_ar-size,sk_away_ar-size,sk_home_ar+size,sk_away_ar+size)
minx = min(bounds[,1],bounds[,3])
maxx = max(bounds[,3],bounds[,1])
miny = min(bounds[,2],bounds[,4])
maxy = max(bounds[,4],bounds[,2])
minxy = min(minx,miny)
maxxy = max(maxx,maxy)

str_count = 25

png('2016_no_model_ar_logo.png', width=1500, height=1500, res = 150)
setwd("./team_logos")
par(mar=c(4,4.5,0,0.1))
plot(NA,xlab="Home Team Assist Ratio",ylab="Away Team Assist Ratio",xlim=c(minxy,maxxy),ylim=c(minxy,maxxy),cex.lab=2, cex.main=3, cex.axis=2, xaxp  = c(0.45,0.7,5),yaxp  = c(0.45,0.7,5))
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
bounds = cbind(sk_home_br-size,sk_away_br-size,sk_home_br+size,sk_away_br+size)
minx = min(bounds[,1],bounds[,3])
maxx = max(bounds[,3],bounds[,1])
miny = min(bounds[,2],bounds[,4])
maxy = max(bounds[,4],bounds[,2])
minxy = min(minx,miny)
maxxy = max(maxx,maxy)

# Update the plot limits based on a new relative logo size
size = (maxxy-minxy)/15
bounds = cbind(sk_home_br-size,sk_away_br-size,sk_home_br+size,sk_away_br+size)
minx = min(bounds[,1],bounds[,3])
maxx = max(bounds[,3],bounds[,1])
miny = min(bounds[,2],bounds[,4])
maxy = max(bounds[,4],bounds[,2])
minxy = min(minx,miny)
maxxy = max(maxx,maxy)

str_count = 25

png('2016_no_model_br_logo.png', width=1500, height=1500, res = 150)
setwd("./team_logos")
par(mar=c(4,4.5,0,0.1))
plot(NA,xlab="Home Team Block Ratio",ylab="Away Team Block Ratio",xlim=c(minxy,maxxy),ylim=c(minxy,maxxy),cex.lab=2, cex.main=3, cex.axis=2, xaxp  = c(0.05,0.2,6),yaxp  = c(0.05,0.2,6))
text(avg_br_2016, miny, paste(rep("League Average - ", str_count),collapse = ""), cex=1.5, srt=90, col="gray30")
text(minx, avg_br_2016, paste(rep("League Average - ", str_count),collapse = ""), cex=1.5, col="gray30")

for (i in 1:30){
  img <- readPNG(paste(team_codes[i],".png",sep=""))
  rasterImage(img,bounds[i,1],bounds[i,2],bounds[i,3],bounds[i,4], interpolate = FALSE)
}
setwd('..')
dev.off()
