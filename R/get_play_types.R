#'@title Get the available play type values
#'
#'@description Get the available play type values
#'
#'@examples
#'
#'@export

get_play_types <- function(){
	source('R/get_json_from_url.R')
	rawPlayTypes <- get_json_from_url('play/types')
	playTypes <- data.frame(PlayTypeId = rawPlayTypes$id,
							PlayTypeName = rawPlayTypes$text,
							PlayTypeAbbr = rawPlayTypes$abbreviation,
							stringsAsFactors = FALSE)
	return(playTypes)
}
