#'@title Makes call to CollegeFootballData.com API and returns the JSON from the response
#'
#'@description Gets desired data as JSON
#'
#'@param urlPath The path (no root) for the desired resource. Venues would pass in 'venues'.
#'
#'@param queryParams A named list with the desired query parameters. list(year=2016, week=1) will result in ?year=2016&week=1 being used as the query string.
#'
#'@param cacheLocation A check for a cached JSON response. This method does not currently cache on its own.
#'
#'@param cacheTimeSec Time in seconds that will result in the cache being ignored. A value of 30 will result in cached values being ignored if created over 30 seconds ago. A value of 0 mean no expiration.
#'
#'@examples
#'
#'@export

get_CFB_json_from_url <- function(urlPath,
							  queryParams = NULL,
							  cacheLocation = NULL,
							  cacheTimeSec = 0){
	if(!is.null(cacheLocation)){
		if(!dir.exists(dirname(cacheLocation))){
			dir.create(dirname(cacheLocation))
		}
		if(file.exists(cacheLocation) && (cacheTimeSec == 0 || (file.info(cacheLocation)$ctime + (cacheTimeSec)) > Sys.time())){
			json <- jsonlite::fromJSON(cacheLocation)
			return(json)
		}
	}

	urlPath <- str_replace(paste0('/', urlPath), '//', '/')
	url <- paste0('https://api.collegefootballdata.com', urlPath)

	if(!is.null(queryParams)){
		qp <- paste0(names(queryParams), '=', queryParams, collapse = '&')
		url <- paste0(url, '?', qp)
	}

	# URL ENCODE AS NEEDED
	url <- str_replace(url, ' ', '%20')

	response <- httr::GET(url)
	rawJson <- httr::content(response, as = 'text')
	json <- jsonlite::fromJSON(rawJson)
	if(!is.null(json) && !is.null(cacheLocation)){
		write(jsonlite::toJSON(json), cacheLocation)
	}
	return(json)
}
