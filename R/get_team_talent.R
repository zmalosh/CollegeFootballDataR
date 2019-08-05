#'@title Gets the calculated team talent for all teams in a desired year.
#'
#'@description Gets the calculated team talent for all teams in a desired year.
#'
#'@param year (required)
#'
#'@examples
#'
#'@export

get_team_talent <- function(year){
	source('R/get_json_from_url.R')
	queryParams <- get_team_talent_query_parameters(year)
	rawTalent <- get_json_from_url(urlPath = 'talent', queryParams = queryParams)
	talent <- data.frame(Year = rawTalent$year,
						 SchoolName = rawTalent$school,
						 Rating = rawTalent$talent,
						 stringsAsFactors = FALSE)
	return(talent)
}

get_team_talent_query_parameters <- function(year){
	if(is.null(year)){
		stop('Year must be provided.')
	}
	if(year%%1!=0){
		stop('Year must be an integer.')
	}
	queryParams <- list(year = year)
	return(queryParams)
}
