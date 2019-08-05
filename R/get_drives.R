#'@title Gets a set of college football drives
#'
#'@description Get all of the drives that meet the provided criteria
#'
#'@param year (required) Year for desired games
#'
#'@param week (optional) Week number
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
#'@examples
#'
#'@export

get_drives <- function(year,
					   week = NULL,
					   seasonType = 'regular',
					   team = NULL,
					   offense = NULL,
					   defense = NULL,
					   conference = NULL,
					   offenseConference = NULL,
					   defenseConference = NULL){
	source('R/get_json_from_url.R')
	queryParams <- get_drives_query_parameters(year, week, seasonType, team, offense, defense,
											  conference, offenseConference, defenseConference)
	rawDrives <- get_json_from_url(urlPath = 'drives', queryParams = queryParams)
	drives <- data.frame(DriveId = rawDrives$id,
						 GameId = rawDrives$game_id,
						 Offense = rawDrives$offense,
						 Defense = rawDrives$defense,
						 DriveResult = rawDrives$drive_result,
						 DriveTimeSec = (ifelse(is.na(rawDrives$elapsed$minutes), 0, rawDrives$elapsed$minutes) * 60) + ifelse(is.na(rawDrives$elapsed$seconds), 0, rawDrives$elapsed$seconds),
						 DriveLengthYds = rawDrives$yards,
						 PlayCount = rawDrives$plays,
						 StartPeriod = rawDrives$start_period,
						 StartYardsToGoal = rawDrives$start_yardline,
						 StartTimeRemSec = (ifelse(is.na(rawDrives$start_time$minutes), 0, rawDrives$start_time$minutes) * 60) + ifelse(is.na(rawDrives$start_time$seconds), 0, rawDrives$start_time$seconds),
						 EndPeriod = rawDrives$end_period,
						 EndYardsToGoal = rawDrives$end_yardline,
						 EndTimeRemSec = (ifelse(is.na(rawDrives$end_time$minutes), 0, rawDrives$end_time$minutes) * 60) + ifelse(is.na(rawDrives$end_time$seconds), 0, rawDrives$end_time$seconds),
						 IsScoringDrive = rawDrives$scoring,
						 OffenseConference = rawDrives$offense_conference,
						 DefenseConference = rawDrives$defense_conference,
						 stringsAsFactors = FALSE)
}

get_drives_query_parameters <- function(year, week, seasonType, team, offense, defense,
									   conference, offenseConference, defenseConference){
	if(is.null(year)){
		stop('Year must be provided.')
	}
	if(year%%1!=0){
		stop('Year must be an integer.')
	}
	queryParams <- list(year = year)
	if(!is.null(week)){
		if(week%%1!=0){
			stop('Week must be an integer.')
		}
		queryParams <- c(queryParams, week = week)
	}
	if(!is.null(seasonType)){
		seasonType <- tolower(seasonType)
		if(!(seasonType %in% c('regular', 'postseason'))){
			stop('SeasonType must be either "regular" or "postseason"')
		}
		queryParams <- c(queryParams, seasonType = seasonType)
	}

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

	return(queryParams)
}
