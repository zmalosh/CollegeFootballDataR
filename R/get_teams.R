#'@title Gets a set of college football teams
#'
#'@description Get all of the teams that meet the provided criteria
#'
#'@param conference (optional) Abbreviation for conference. "B1G", "PAC", and "CUSA" are valid examples.
#'
#'@examples
#'
#'@export

get_teams <- function(conference = NULL){
	source('R/get_json_from_url.R')
	queryParams <- get_teams_query_parameters(conference)
	rawTeams <- get_json_from_url(urlPath = 'teams', queryParams = queryParams)
	teams <- data.frame(SchoolName = rawTeams$school,
						Mascot = rawTeams$mascot,
						Conference = rawTeams$conference,
						ConferenceDivision = rawTeams$division,
						PrimaryColor = rawTeams$color,
						AltColor = rawTeams$alt_color,
						SchoolAbbr = rawTeams$abbreviation,
						Logo = as.character(lapply(rawTeams$logos, function(x){x[1]})),
						LogoDark = as.character(lapply(rawTeams$logos, function(x){x[2]})),
						stringsAsFactors = FALSE)
	return(teams)
}

get_teams_query_parameters <- function(conference){
	if(is.null(conference)){
		return(NULL)
	}
	return(list(conference = conference))
}
