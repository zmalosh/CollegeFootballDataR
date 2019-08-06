#'@title Gets all supported conferences (current and historical)
#'
#'@description Gets all supported conferences (current and historical)
#'
#'@examples
#'
#'@export

get_conferences <- function(){
	safOpt <- getOption('stringsAsFactors')
	options(stringsAsFactors = FALSE)
	conferences <- get_CFB_json_from_url('conferences') %>%
		transform(ConferenceId = id,
				  ConferenceName = name,
				  ConferenceFullName = short_name,
				  ConferenceAbbr = abbreviation) %>%
		select(ConferenceId, ConferenceName, ConferenceAbbr, ConferenceFullName)
	options(stringsAsFactors = FALSE)
	return(conferences)
}
