get_team_id = function(team_name){
  teams =  rbind(
   "Hawks",          "Celtics",   "Nets",         
   "Hornets",        "Bulls",     "Cavaliers",   
   "Mavericks",      "Nuggets",   "Pistons",       
   "Warriors",       "Rockets",   "Pacers",        
   "Clippers",       "Lakers",    "Grizzlies",     
   "Heat",           "Bucks",     "Timberwolves",
   "Pelicans",       "Knicks",    "Thunder", 
   "Magic",          "76ers",     "Suns",          
   "Trail Blazers",  "Kings",     "Spurs",     
   "Raptors",        "Jazz",      "Wizards"
  )
  
  return(which(teams == team_name))
}