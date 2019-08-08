#'@title Gets a set of college football games
#'
#'@description Get all of the games that meet the provided criteria
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

get_player_games <- function(gameId = NULL,
							 year = NULL,
							 seasonType = NULL,
							 week = NULL,
							 team = NULL,
							 conference = NULL){
	queryParams <- get_player_games_query_params(gameId, year, seasonType, week, team, conference)
	rawPlayerGames <- get_CFB_json_from_url(urlPath = 'games/players', queryParams = queryParams)
	allRawStats <- data.frame(GameId = c(), PlayerId = c(), PlayerName = c(), SchoolName = c(),
							  SchoolConference = c(), StatName = c(), StatCategory = c(),
							  StatAbbr = c(), StatValue = c(), IsHomeTeam = c(),
							  stringsAsFactors = FALSE)
	for(rpgIdx in seq_len(nrow(rawPlayerGames))){
		rawPlayerGame <- rawPlayerGames[rpgIdx,]
		rawGameTeams <- as.data.frame(rawPlayerGame$teams)
		for(teamIdx in seq_len(nrow(rawGameTeams))){
			rawGameTeam <- rawGameTeams[teamIdx,]
			rawTeamCategories <- as.data.frame(rawGameTeam$categories)
			for(catIdx in seq_len(nrow(rawTeamCategories))){
				rawTeamCategory <- rawTeamCategories[catIdx,]
				rawPlayerStats <- as.data.frame(rawTeamCategory$types)
				for(rpsIdx in seq_len(nrow(rawPlayerStats))){
					rawPlayerStat <- rawPlayerStats[rpsIdx,]
					rawPlayers <- as.data.frame(rawPlayerStat$athletes)
					statName <- get_player_stat_name(rawTeamCategory$name, rawPlayerStat$name)
					df <- data.frame(GameId = rawPlayerGame$id,
									 PlayerId = rawPlayers$id,
									 PlayerName = rawPlayers$name,
									 SchoolName = rawGameTeam$school,
									 StatName = statName,
									 StatValue = rawPlayers$stat,
									 stringsAsFactors = FALSE)
					allRawStats <- rbind(allRawStats, df)
				}
			}
		}
	}

	playerStats <- allRawStats %>%
		unique() %>%
		tidyr::spread(StatName, StatValue) %>%
		tidyr::separate(PassCompPct, c('PassComps', 'PassAtts'), sep = '/') %>%
		tidyr::separate(FGMade, c('FGAtts', 'FGMade'), sep = '/')

	notIdVars <- names(playerStats)[!(names(playerStats) %in% c('GameId','PlayerId','PlayerName','SchoolName'))]

	playerStats <- playerStats %>% dplyr::mutate_at(vars(notIdVars), as.numeric)

	return(playerStats)
}

get_player_games_query_params <- function(gameId, year, seasonType, week, team, conference){
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
			if(!(seasonType %in% c('regular', 'postseason'))){
				stop('SeasonType must be either "regular" or "postseason".')
			}
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

get_player_stat_name <- function(statCategory, statAbbr){
	statToken <- tolower(paste0(statCategory, '-', statAbbr))
	result <- switch(statToken,
					 "defensive-pd" = 'DefPassDef',
					 "defensive-qb hur" = 'DefQbHurries',
					 "defensive-sacks" = 'DefSacks',
					 "defensive-solo" = 'DefSoloTkl',
					 "defensive-td" = 'DefTds',
					 "defensive-tfl" = 'DefTklForLoss',
					 "defensive-tot" = 'DefTotalTkl',
					 "fumbles-fum" = 'Fumbles',
					 "fumbles-lost" = 'FumblesLost',
					 "fumbles-rec" = 'FumblesRecovered',
					 "interceptions-int" = 'DefInts',
					 "interceptions-td" = 'DefIntTds',
					 "interceptions-yds" = 'DefIntYds',
					 "kicking-fg" = 'FGMade',
					 "kicking-long" = 'FGLongYds',
					 "kicking-pct" = 'FGPct',
					 "kicking-pts" = 'FGPts',
					 "kicking-xp" = 'XPMade',
					 "kickreturns-avg" = 'KickRetAvg',
					 "kickreturns-long" = 'KickRetLongYds',
					 "kickreturns-no" = 'KickRetAtts',
					 "kickreturns-td" = 'KickRetTds',
					 "kickreturns-yds" = 'KickRetYds',
					 "passing-avg" = 'PassAvgYds',
					 "passing-c/att" = 'PassCompPct',
					 "passing-int" = 'PassInts',
					 "passing-qbr" = 'PassQbRating',
					 "passing-td" = 'PassTds',
					 "passing-yds" = 'PassYds',
					 "punting-avg" = 'PuntAvg',
					 "punting-in 20" = 'PuntIn20',
					 "punting-long" = 'PuntLongYds',
					 "punting-no" = 'PuntAtts',
					 "punting-tb" = 'PuntTouchbacks',
					 "punting-yds" = 'PuntYds',
					 "puntreturns-avg" = 'PuntRetAvg',
					 "puntreturns-long" = 'PuntRetLongYds',
					 "puntreturns-no" = 'PuntRetAtts',
					 "puntreturns-td" = 'PuntRetTds',
					 "puntreturns-yds" = 'PuntRetYds',
					 "receiving-avg" = 'RecAvgYds',
					 "receiving-long" = 'RecLongYds',
					 "receiving-rec" = 'RecCatches',
					 "receiving-td" = 'RecTds',
					 "receiving-yds" = 'RecYds',
					 "rushing-avg" = 'RushAvgYds',
					 "rushing-car" = 'RushAtts',
					 "rushing-long" = 'RushLongYds',
					 "rushing-td" = 'RushTds',
					 "rushing-yds" = 'RushYds'
					 )
	return(result)
}
