get_json_from_url <- function(urlPath,
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

	url <- paste0('https://api.collegefootballdata.com/', urlPath)
	if(!is.null(queryParams)){
		qp <- paste0(names(queryParams), '=', queryParams, collapse = '&')
		url <- paste0(url, '?', qp)
	}

	response <- httr::GET(url)
	rawJson <- httr::content(response, as = 'text')
	json <- jsonlite::fromJSON(rawJson)
	if(!is.null(json) && !is.null(cacheLocation)){
		write(jsonlite::toJSON(json), cacheLocation)
	}
	return(json)
}
