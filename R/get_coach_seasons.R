#'@title Gets all available coach season data
#'
#'@description Gets all available coach season data
#'
#'@param firstName (optional) First Name of coach
#'
#'@param lastName (optional) Last Name of coach
#'
#'@param team (optional) Name for team. "Michigan" and 'Oregon State' are valid examples.
#'
#'@param year (optional) Individual year value
#'
#'@param minYear (optional) Starting year for which coaches should be returned. Will be ignored if year is provided.
#'
#'@param maxYear (optional) Final year for which coaches should be returned. Will be ignored if year is provided.
#'
#'@examples
#'
#'@export

get_coach_seasons <- function(firstName = NULL,
							  lastName = NULL,
							  team = NULL,
							  year = NULL,
							  minYear = NULL,
							  maxYear = NULL){
	queryParams <- get_coach_season_query_parameters(firstName, lastName, team, year, minYear, maxYear)
	rawCoaches <- get_CFB_json_from_url('coaches', queryParams = queryParams)
	coachSeasons <- data.frame(FirstName = c(), LastName = c(), SchoolName = c(), Year = c(),
							   Year = c(), Wins = c(), Losses = c(), Ties = c(),
							   PreseasonRank = c(), PostseasonRank = c())
	for(coachIdx in seq_len(nrow(rawCoaches))){
		rawCoach <- rawCoaches[coachIdx,]
		rawCoachSeasons <- as.data.frame(rawCoach$seasons)
		coachSeasons <- rbind(coachSeasons, data.frame(FirstName = rawCoach$first_name,
													   LastName = rawCoach$last_name,
													   SchoolName = rawCoachSeasons$school,
													   Year = rawCoachSeasons$year,
													   Games = rawCoachSeasons$games,
													   Wins = rawCoachSeasons$wins,
													   Losses = rawCoachSeasons$losses,
													   Ties = rawCoachSeasons$ties,
													   PreseasonRank = rawCoachSeasons$preseason_rank,
													   PostseasonRank = rawCoachSeasons$postseason_rank))
	}
	coachSeasons <- unique(coachSeasons)
	return(coachSeasons)
}

get_coach_season_query_parameters <- function(firstName, lastName, team, year, minYear, maxYear){
	queryParams <- list()
	if(!is.null(firstName)){
		queryParams <- c(queryParams, firstName = firstName)
	}

	if(!is.null(lastName)){
		queryParams <- c(queryParams, lastName = lastName)
	}

	if(!is.null(team)){
		queryParams <- c(queryParams, team = team)
	}

	if(!is.null(year)){
		if(year%%1!=0){
			stop('Year must be an integer.')
		}
		queryParams <- c(queryParams, year = year)
	} else {
		if(!is.null(minYear)){
			if(minYear%%1!=0){
				stop('MinYear must be an integer.')
			}
			queryParams <- c(queryParams, minYear = minYear)
		}
		if(!is.null(maxYear)){
			if(maxYear%%1!=0){
				stop('MaxYear must be an integer.')
			}
			queryParams <- c(queryParams, maxYear = maxYear)
		}
	}
	return(queryParams)
}
