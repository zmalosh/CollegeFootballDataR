#'@title Gets a set of college football team stats by game
#'
#'@description Get all of the team stats from games that meet the provided criteria
#'
#'@param gameId (optional) Single desired game ID. Required if year is not provided.
#'
#'@param year (optional) Year for desired games Required if gameId is not provided.
#'
#'@param week (optional) Week number. Required if year is provided but team and conference are not.
#'
#'@param seasonType (optional) [regular/postseason] Defaults to regular if not provided but week is provided.
#'
#'@param team (optional) Name for desired team. Results will include game stats for both the provided team and their opponents. Required if year is provided but week and conference are not.
#'
#'@param conference (optional) Name for desired team. Results will include game stats for both conference teams and their opponents. Required if year is provided but week and team are not.
#'
#'@examples
#'
#'@export

get_team_games <- function(gameId = NULL,
						   year = NULL,
						   seasonType = NULL,
						   week = NULL,
						   team = NULL,
						   conference = NULL){
	source('R/get_json_from_url.R')
	queryParams <- get_team_games_query_params(gameId, year, seasonType, week, team, conference)
	rawTeamGames <- get_json_from_url(urlPath = 'games/teams', queryParams = queryParams)
	allRawStats <- data.frame(GameId = c(), SchoolName = c(), OppName = c(),
							  StatName = c(),StatValue = c(),
							  stringsAsFactors = FALSE)
	for(rtgIdx in seq_len(nrow(rawTeamGames))){
		rawTeamGame <- rawTeamGames[rtgIdx,]
		rawGameTeams <- as.data.frame(rawTeamGame$teams)
		teamName1 <- rawGameTeams[1,'school']
		teamName2 <- rawGameTeams[2,'school']
		for(tIdx in 1:2){
			teamName = ifelse(tIdx == 1, teamName1, teamName2)
			oppName = ifelse(tIdx == 1, teamName2, teamName1)
			rawGameTeam <- rawGameTeams[tIdx,]
			rawGameTeamStats <- as.data.frame(rawGameTeam$stats)
			gameTeamStats <- data.frame(GameId = rawTeamGame$id,
										SchoolName = teamName,
										OppName = oppName,
										StatName = as.character(lapply(rawGameTeamStats$category, get_team_stat_name)),
										StatValue = rawGameTeamStats$stat)
			allRawStats <- rbind(allRawStats, gameTeamStats)
		}
	}

	result <- allRawStats %>%
		spread(StatName, StatValue) %>%
		separate(TimeOfPoss, c('TimeOfPossMin', 'TimeOfPossSec'), sep = ':') %>%
		separate(FourthDownEff, c('FourthDownConversions', 'FourthDownAtts'), sep = '-') %>%
		separate(PassAttemptsAndResults, c('PassCompletions', 'PassAtts'), sep = '-') %>%
		separate(PenaltyBreakdown, c('PenaltiesCommited', 'PenaltyYdsLost'), sep = '-') %>%
		separate(ThirdDownEff, c('ThirdDownConversions', 'ThirdDownAtts'), sep = '-') %>%
		mutate(FirstDowns = as.integer(FirstDowns),
			   FourthDownAtts = as.integer(FourthDownAtts),
			   FourthDownConversions = as.integer(FourthDownConversions),
			   FumblesLost = as.integer(FumblesLost),
			   IntsLost = as.integer(IntsLost),
			   NetPassYds = as.integer(NetPassYds),
			   PassCompletions = as.integer(PassCompletions),
			   PassAtts = as.integer(PassAtts),
			   PassYdsPerAtt = as.numeric(PassYdsPerAtt),
			   PenaltiesCommited = as.integer(PenaltiesCommited),
			   PenaltyYdsLost = as.integer(PenaltyYdsLost),
			   RushAtts = as.integer(RushAtts),
			   RushYds = as.integer(RushYds),
			   RushYdsPerAtt = as.numeric(RushYdsPerAtt),
			   ThirdDownConversions = as.integer(ThirdDownConversions),
			   ThirdDownAtts = as.integer(ThirdDownAtts),
			   TimeOfPossSec = (60 * as.integer(TimeOfPossMin)) + as.integer(TimeOfPossSec),
			   TotalYds = as.integer(TotalYds),
			   TurnoversLost = as.integer(TurnoversLost),
			   FourthDownEff = NULL,
			   ThirdDownEff = NULL,
			   PenaltyBreakdown = NULL,
			   TimeOfPossMin = NULL)

	return(result)
}

get_team_games_query_params <- function(gameId, year, seasonType, week, team, conference){
	# QUERY PARAMS MUST BE EITHER gameId OR year + (week OR team OR conference)
	if(!is.null(gameId)){
		if(gameId %% 1 != 0){
			stop('GameId must be an integer.')
		}
		return (list(gameId = gameId))
	}

	if(!is.null(year)){
		if(year %% 1 != 0){
			stop('Year must be an integer.')
		}
		queryParams <- list(year = year)

		hasRequiredParam <- FALSE
		if(!is.null(week)){
			if(week %% 1 != 0){
				stop('Week must be an integer.')
			}
			hasRequiredParam <- TRUE
			queryParams <- c(queryParams, week = week)
		}

		if(!is.null(seasonType)){
			seasonType <- tolower(seasonType)
			if(!(seasonType %in% c('regular', 'postseason')))
				queryParams <- c(queryParams, seasonType = seasonType)
		}

		if(!is.null(team)){
			hasRequiredParam <- TRUE
			queryParams <- c(queryParams, team = team)
		}

		if(!is.null(conference)){
			hasRequiredParam <- TRUE
			queryParams <- c(queryParams, conference = conference)
		}
		return(queryParams)
	}

	stop('Either gameId or year must be provided.')
}

get_team_stat_name <- function(statName){
	result <- switch(tolower(statName),
					 'possessiontime' = 'TimeOfPoss',
					 'interceptions' = 'IntsLost',
					 'fumbleslost' = 'FumblesLost',
					 'turnovers' = 'TurnoversLost',
					 'totalpenaltiesyards' = 'PenaltyBreakdown',
					 'yardsperrushattempt' = 'RushYdsPerAtt',
					 'rushingattempts' = 'RushAtts',
					 'rushingyards' = 'RushYds',
					 'yardsperpass' = 'PassYdsPerAtt',
					 'completionattempts' = 'PassAttemptsAndResults',
					 'netpassingyards' = 'NetPassYds',
					 'totalyards' = 'TotalYds',
					 'fourthdowneff' = 'FourthDownEff',
					 'thirddowneff' = 'ThirdDownEff',
					 'firstdowns' = 'FirstDowns')
	return(result)
}
