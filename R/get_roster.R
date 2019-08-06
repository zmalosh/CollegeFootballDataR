#'@title Gets the current roster for a requested team
#'
#'@description Gets the current roster for a requested team
#'
#'@param team (required) Name for desired team. "Michigan" and 'Oregon State' are valid examples.
#'
#'@examples
#'
#'@export

get_roster <- function(team){
	queryParams <- get_roster_query_parameters(team)
	rawRoster <- get_CFB_json_from_url('roster', queryParams = queryParams)
	roster <- data.frame(PlayerId = rawRoster$id,
						 FirstName = rawRoster$first_name,
						 LastName = rawRoster$last_name,
						 Weight = rawRoster$weight,
						 Height = rawRoster$height,
						 JerseyNumber = rawRoster$jersey,
						 SchoolYear = rawRoster$year,
						 Position = rawRoster$position,
						 HomeCity = rawRoster$home_city,
						 HomeState = rawRoster$home_state,
						 HomeCountry = rawRoster$home_country,
						 stringsAsFactors = FALSE)
	return(roster)
}

get_roster_query_parameters <- function(team){
	if(is.null(team)){
		stop('Team must be provided.')
	}
	queryParams <- list(team = team)
	return(queryParams)
}
