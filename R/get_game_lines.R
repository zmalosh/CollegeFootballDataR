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
#'@param team (optional) Name for home team. "Michigan" and 'Oregon State' are valid examples.
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

get_game_lines <- function(year,
						   week = NULL,
						   seasonType = NULL,
						   team = NULL,
						   home = NULL,
						   away = NULL,
						   conference = NULL){
	source('R/get_json_from_url.R')
	queryParams <- get_game_lines_query_parameters(year, week, seasonType, team, home, away, conference)
	rawLines <- get_json_from_url(urlPath = 'lines', queryParams = queryParams)
	df <- data.frame(GameId = c(), HomeTeam = c(), AwayTeam = c(), HomeScore = c(), AwayScore = c(),
						 Bookmaker = c(), HomeSpread = c(), OverUnder = c(), SpreadDisplay = c())
	for(i in seq_len(nrow(rawLines))){
		rawLine <- rawLines[i,]
		lines <- as.data.frame(rawLine$lines, stringsAsFactors = FALSE)
		if(!is.null(lines) && nrow(lines) > 0){
			df <- rbind(df, data.frame(GameId = rawLine$id,
									   HomeTeam = rawLine$homeTeam,
									   AwayTeam = rawLine$awayTeam,
									   HomeScore = ifelse(lapply(rawLine$homeScore, is.null), NA, rawLine$homeScore),
									   AwayScore = ifelse(lapply(rawLine$awayScore, is.null), NA, rawLine$awayScore),
									   Bookmaker = lines$provider,
									   HomeSpread = lines$spread,
									   OverUnder = lines$overUnder,
									   SpreadDisplay = lines$formattedSpread))
		}
	}
	if(nrow(df) == 0){
		return(NULL)
	}
	return(df)
}

get_game_lines_query_parameters <- function(year, week, seasonType, team,
											home, away, conference){
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
