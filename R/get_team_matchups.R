#'@title Gets games between two teams
#'
#'@description Get all of the teams that meet the provided criteria
#'
#'@param team1 (required) Name for team. "Michigan" and 'Oregon State' are valid examples.
#'
#'@param team2 (required) Name for team. "Michigan" and 'Oregon State' are valid examples.
#'
#'@param minYear (optional) Starting year for which matchups should be returned.
#'
#'@param maxYear (optional) Final year for which matchups should be returned
#'
#'@examples
#'
#'@export

get_team_matchups <- function(team1, team2, minYear = NULL, maxYear = NULL){
	source('R/get_json_from_url.R')
	queryParams <- get_matchup_query_parameters(team1, team2, minYear, maxYear)
	rawMatchups <- get_json_from_url('teams/matchup', queryParams = queryParams)
	matchups <- data.frame(Season = rawMatchups$games$season,
						   SeasonType = rawMatchups$games$seasonType,
						   Week = rawMatchups$games$week,
						   GameDateUtc = lubridate::ymd_hms(rawMatchups$games$date),
						   HomeTeam = rawMatchups$games$homeTeam,
						   AwayTeam = rawMatchups$games$awayTeam,
						   HomeScore = rawMatchups$games$homeScore,
						   AwayScore = rawMatchups$games$awayScore,
						   IsNeutralSite = rawMatchups$games$neutralSite,
						   Venue = rawMatchups$games$venue,
						   Dummy = rep(TRUE, times = nrow(rawMatchups$games)),
						   stringsAsFactors = FALSE)
	matchups <- matchups %>%
		inner_join(matchups, by = 'Dummy', suffix = c('', '.other')) %>%
		filter(GameDateUtc.other <= GameDateUtc) %>%
		group_by(Season, SeasonType, Week, GameDateUtc) %>%
		summarise(HomeTeam = first(HomeTeam),
				  AwayTeam = first(AwayTeam),
				  HomeScore = first(HomeScore),
				  AwayScore = first(AwayScore),
				  IsNeutralSite = first(IsNeutralSite),
				  Venue = first(Venue),
				  WinsHome = sum(
				  	ifelse(HomeTeam == HomeTeam.other & HomeScore.other > AwayScore.other,
				  		   1,
				  	ifelse(HomeTeam == AwayTeam.other & HomeScore.other < AwayScore.other,
				  	   	   1,
				  	   	   0))),
				  WinsAway = sum(
				  	ifelse(AwayTeam == HomeTeam.other & HomeScore.other > AwayScore.other,
				  		   1,
				  	ifelse(AwayTeam == AwayTeam.other & HomeScore.other < AwayScore.other,
				  	   	   1,
				  	   	   0))),
				  Ties = sum(ifelse(GameDateUtc < lubridate::now('UTC') & HomeScore.other == AwayScore.other, 1, 0)))
	matchups <- as.data.frame(matchups)
	return(matchups)
}

get_team_matchup_query_parameters <- function(team1, team2, minYear, maxYear){
	if(is.null(team1) || is.null(team2)){
		stop('Both team1 and team2 must be provided.')
	}
	queryParams <- list(team1 = team1, team2 = team2)

	if(!is.null(minYear)){
		if(minYear%%1!=0){
			stop('MinYear must be an integer.')
		}
		queryParams <- c(queryParams, minYear = minYear)
	}

	if(!is.null(maxYear)){
		if(maxYear%%1!=0){
			stop('MaxYear must be an integer.')
		}
		queryParams <- c(queryParams, maxYear = maxYear)
	}

	return(queryParams)
}
