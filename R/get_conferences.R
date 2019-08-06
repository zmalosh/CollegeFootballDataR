#'@title Gets all supported conferences (current and historical)
#'
#'@description Gets all supported conferences (current and historical)
#'
#'@examples
#'
#'@export

get_conferences <- function(){
	rawConferences <- get_CFB_json_from_url('conferences')
	conferences <- data.frame(ConferenceId = rawConferences$id,
							  ConferenceName = rawConferences$name,
							  ConferenceFullName = rawConferences$short_name,
							  ConferenceAbbr = rawConferences$abbreviation,
							  stringsAsFactors = FALSE)
	return(conferences)
}
