#'@title Gets a set of college football games
#'
#'@description Get all of the games that meet the provided criteria
#'
#'@param year (required) Year for desired games
#'
#'@param week (optional) Week number
#'
#'@param seasonType (optional) [regular/postseason] Season Type for desired games
#'
#'@param home (optional) Name for home team. "Michigan" and 'Oregon State' are valid examples.
#'
#'@param away (optional) Name for away team. "Michigan" and 'Oregon State' are valid examples.
#'
#'@param conference (optional) Abbreviation for conference. "B1G", "PAC", and "CUSA" are valid examples.
#'
#'@examples
#'
#'@export

get_games <- function(year,
					  week = NULL,
					  seasonType = NULL,
					  team = NULL,
					  home = NULL,
					  away = NULL,
					  conference = NULL)
{
	source('R/get_json_from_url.R')
	queryParams <- get_query_params(year, week, seasonType, team, home, away, conference)
	rawGames <- get_json_from_url(urlPath = 'games', queryParams = queryParams)
	games <- data.frame(
		GameId = rawGames$id,
		Season = rawGames$season,
		SeasonType = rawGames$season_type,
		Week = rawGames$week,
		GameTimeUtc = lubridate::ymd_hms(rawGames$start_date),
		HomeTeamName = rawGames$home_team,
		HomeScore = rawGames$home_points,
		AwayScore = rawGames$away_points,
		AwayTeamName = rawGames$away_team,
		HomeConference = rawGames$home_conference,
		AwayConference = rawGames$away_conference,
		IsNeutralSite = rawGames$neutral_site,
		IsConferenceGame = rawGames$conference_game,
		Attendance = rawGames$attendance,
		VenueId = rawGames$venue_id,
		VenueName = rawGames$venue,
		HomeScoreQ1 = as.integer(lapply(rawGames$home_line_scores, function(x){x[1]})),
		HomeScoreQ2 = as.integer(lapply(rawGames$home_line_scores, function(x){x[2]})),
		HomeScoreQ3 = as.integer(lapply(rawGames$home_line_scores, function(x){x[3]})),
		HomeScoreQ4 = as.integer(lapply(rawGames$home_line_scores, function(x){x[4]})),
		HomeScoreQ5 = as.integer(lapply(rawGames$home_line_scores, function(x){x[5]})),
		AwayScoreQ1 = as.integer(lapply(rawGames$away_line_scores, function(x){x[1]})),
		AwayScoreQ2 = as.integer(lapply(rawGames$away_line_scores, function(x){x[2]})),
		AwayScoreQ3 = as.integer(lapply(rawGames$away_line_scores, function(x){x[3]})),
		AwayScoreQ4 = as.integer(lapply(rawGames$away_line_scores, function(x){x[4]})),
		AwayScoreQ5 = as.integer(lapply(rawGames$away_line_scores, function(x){x[5]})),
		stringsAsFactors = FALSE
	)

	return(games)
}

get_query_params <- function(year,
							 week = NULL,
							 seasonType = NULL,
							 team = NULL,
							 home = NULL,
							 away = NULL,
							 conference = NULL)
{
	#MANY CHANGING OPTIONS FOR TEAMS AND CONFERENCES. DON'T DO CHECK
	if(year %% 1 != 0){
		stop('Year value must be an integer')
	}
	else{
		queryParams <- list(year = year)
	}

	if(!is.null(week)){
		if(week %% 1 != 0){
			stop('If provided, week value must be an integer')
		}
		queryParams <- c(queryParams, week = week)
	}

	if(!is.null(seasonType)){
		seasonType <- tolower(seasonType)
		if(!(seasonType %in% c('regular', 'postseason'))){
			stop('If provided, seasonType must be either "regular" or "postseason"')
		}
		queryParams <- c(queryParams, seasonType = seasonType)
	}

	if(!is.null(team)){
		queryParams <- c(queryParams, team = team)
	}

	if(!is.null(home)){
		queryParams <- c(queryParams, home = home)
	}

	if(!is.null(away)){
		queryParams <- c(queryParams, away = away)
	}

	if(!is.null(conference)){
		queryParams <- c(queryParams, conference = conference)
	}
	return(queryParams)
}
