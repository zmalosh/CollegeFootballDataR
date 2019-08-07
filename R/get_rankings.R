#'@title Gets all available rankings for the requested year
#'
#'@description Gets all available rankings for the requested year
#'
#'@param year (required) Year for desired rankings
#'
#'@examples
#'
#'@export

get_rankings <- function(year){
	queryParams <- get_rankings_query_parameters(year, 'regular')
	rawRankings <- get_CFB_json_from_url('rankings', queryParams = queryParams)
	df <- rankings_raw_to_df(rawRankings)
	queryParams <- get_rankings_query_parameters(year, 'postseason')
	rawRankings <- get_CFB_json_from_url('rankings', queryParams = queryParams)
	df <- rbind(df, rankings_raw_to_df(rawRankings))
	return(df)
}

get_rankings_query_parameters <- function(year, seasonType){
	if(is.null(year)){
		stop('Year must be provided.')
	}
	if(year%%1!=0){
		stop('Year must be an integer.')
	}
	queryParams <- list(year = year)
	if(!is.na(seasonType)){
		seasonType <- tolower(seasonType)
		if(!(seasonType %in% c('regular', 'postseason'))){
			stop('SeasonType value must be "regular" or "postseason"')
		}
		queryParams <- c(queryParams, seasonType = seasonType)
	}
	return(queryParams)
}

rankings_raw_to_df <- function(rawRankings){
	df <- data.frame(Season = c(), SeasonType = c(), Week = c(),
					 PollName = c(), TeamRank = c(), TeamName = c(),
					 Conference = c(), PollPoints = c(), FirstPlaceVotes = c())
	for(weekIdx in seq_len(nrow(rawRankings))){
		rawRanking <- rawRankings[weekIdx,]
		rawPolls <- as.data.frame(rawRanking$polls)
		season <- rawRanking$season
		seasonType <- rawRanking$seasonType
		week <- rawRanking$week
		for(i in seq_len(nrow(rawPolls))){
			rawPoll <- as.data.frame(rawPolls[i,])
			pollCount <- length(rawPoll$poll)
			for(pollIdx in seq_len(pollCount)){
				pollName <- rawPoll[pollIdx, 'poll']
				pollRanks <- as.data.frame(rawPoll[pollIdx, 'ranks'])
				df <- rbind(df, data.frame(Season = season,
										   SeasonType = seasonType,
										   Week = week,
										   PollName = pollName,
										   TeamRank = pollRanks$rank,
										   TeamName = pollRanks$school,
										   Conference = pollRanks$conference,
										   PollPoints = pollRanks$points,
										   FirstPlaceVotes = pollRanks$firstPlaceVotes,
										   stringsAsFactors = FALSE))
			}
		}
	}
	return(df)
}
