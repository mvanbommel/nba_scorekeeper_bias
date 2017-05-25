# nba_scorekeeper_bias
Code for the article "Adjusting for Scorekeeper Bias in NBA Box Scores" published in [Data Mining and Knowledge Discovery](https://link.springer.com/article/10.1007%2Fs10618-017-0497-y) ([PDF](http://www.readcube.com/articles/10.1007/s10618-017-0497-y?author_access_token=2d1Qnw6gJ2FSK8dQelHhePe4RwlQNchNByi7wbcMAY5wPAnuWMtbk1vJZa6JuvaOJZD6DGkcRLPph38FNvDQKme8m5GpOCTegQ2Lm8iZZmpCjTBA_8Vmho9PicjyUHYG9j5asAyYRLmxqCYJ9b_F3g%3D%3D)) and presented at the [MIT Sloan Sports Analytics Conference](http://www.sloansportsconference.com/wp-content/uploads/2016/02/1502-van-exel-effect.pdf).

## box_score_data
Includes code for the initial examination and simple models (and makes use of ESPN box score data scraped using the [scrape_nba_box_scores](https://github.com/mvanbommel/scrape_nba_box_scores) repository).
- **observed_ar_and_br.R** examines the observed assist ratio and block ratio for the home and away teams for each scorekeeper (and plots them)
- **simple_model_ar_and_br.R** uses a simple linear model to estimate the assist ratio and block ratio for the home and away teams for each scorekeeper (and plots them)

## player_tracking_data
Will include code for the more advanced contextual model (and makes use of the publicly available player tracking data [here](https://github.com/dcervone/EPVDemo/blob/master/data/2013_11_01_MIA_BKN.csv)).
- **change_player_tracking_data_format.R** changes the format of the player tracking (moment) data (producing *moment_data.RData*)
- **get_event_data.R** extracts the event data from the moment data (producing *event_data.RData*)
- **get_potential_assists.R** obtains the contextual and spatio-temporal information for each potential assist (producing *potential_assists.RData*)

## To Do:  
Finish adding player tracking data code
