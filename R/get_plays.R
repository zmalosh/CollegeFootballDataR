#'@title Gets a set of college football plays
#'
#'@description Get all of the plays from games that meet the provided criteria
#'
#'@param year (required) Year for desired games
#'
#'@param week (required) Week number
#'
#'@param seasonType (optional) [regular/postseason] Season Type for desired games
#'
#'@param team (optional) Name for team. "Michigan" and 'Oregon State' are valid examples.
#'
#'@param offense (optional) Name for team on offense. "Michigan" and 'Oregon State' are valid examples.
#'
#'@param defense (optional) Name for team on defense. "Michigan" and 'Oregon State' are valid examples.
#'
#'@param offenseConference (optional) Abbreviation for conference of team on defense. "B1G", "PAC", and "CUSA" are valid examples.
#'
#'@param defenseConference (optional) Abbreviation for conference of team on defense. "B1G", "PAC", and "CUSA" are valid examples.
#'
#'@param playTypeId (optional) playTypeId for desired type of play to return
#'
#'@examples
#'
#'@export

get_plays <- function(year,
					  week,
					  seasonType = 'regular',
					  team = NULL,
					  offense = NULL,
					  defense = NULL,
					  conference = NULL,
					  offenseConference = NULL,
					  defenseConference = NULL,
					  playTypeId = NULL){
	queryParams <- get_plays_query_parameters(year, week, seasonType, team, offense, defense,
									conference, offenseConference, defenseConference,
									playTypeId)
	rawPlays <- get_CFB_json_from_url(urlPath = 'plays', queryParams = queryParams)
	plays <- data.frame(
		PlayId = rawPlays$id,
		DriveId = rawPlays$drive_id,
		OffenseTeam = rawPlays$offense,
		DefenseTeam = rawPlays$defense,
		OffenseScore = rawPlays$offense_score,
		DefenseScore = rawPlays$defense_score,
		Period = rawPlays$period,
		PeriodTimeRemSec = (60 * ifelse(is.na(rawPlays$clock$minutes), 0, rawPlays$clock$minutes)) + ifelse(is.na(rawPlays$clock$seconds), 0, rawPlays$clock$seconds),
		YardsToGoal = rawPlays$yard_line,
		Down = rawPlays$down,
		Distance = rawPlays$distance,
		PlayType = rawPlays$play_type,
		YardsGained = rawPlays$yards_gained,
		PlayText = rawPlays$play_text,
		OffenseConference = rawPlays$offense_conference,
		DefenseConference = rawPlays$defense_conference,
		stringsAsFactors = FALSE
	)
}

get_plays_query_parameters <- function(year, week, seasonType, team, offense, defense,
									   conference, offenseConference, defenseConference,
									   playTypeId){
	if(is.null(year)){
		stop('Year must be provided.')
	}
	if(year%%1!=0){
		stop('Year must be an integer.')
	}
	if(is.null(week)){
		stop('Week must be provided.')
	}
	if(week%%1!=0){
		stop('Week must be an integer.')
	}
	if(!is.null(seasonType)){
		seasonType <- tolower(seasonType)
		if(!(seasonType %in% c('regular','postseason'))){
			stop('SeasonType must be either "regular" or "postseason"')
		}
	} else {
		seasonType = 'regular'
	}
	queryParams <- c(year = year, seasonType = seasonType, week = week)

	if(!is.null(team)){
		queryParams <- c(queryParams, team = team)
	}

	if(!is.null(offense)){
		queryParams <- c(queryParams, offense = offense)
	}

	if(!is.null(defense)){
		queryParams <- c(queryParams, defense = defense)
	}

	if(!is.null(conference)){
		queryParams <- c(queryParams, conference = conference)
	}

	if(!is.null(offenseConference)){
		queryParams <- c(queryParams, offenseConference = offenseConference)
	}

	if(!is.null(defenseConference)){
		queryParams <- c(queryParams, defenseConference = defenseConference)
	}

	if(!is.null(playTypeId)){
		if(playTypeId%%1!=0){
			stop('PlayTypeId must be an integer.')
		}
		source('R/get_play_types.R')
		playTypes <- get_play_types()
		if(!(playTypeId %in% playTypes$PlayTypeId)){
			stop('PlayTypeId not valid. Use get_play_types() to find valid PlayTypeId values.')
		}
		queryParams <- c(queryParams, playType = playTypeId)
	}

	return(queryParams)
}
