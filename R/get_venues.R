#'@title Gets all available venue data
#'
#'@description Gets all available venue data
#'
#'@examples
#'
#'@export

get_venues <- function(){
	rawVenues <- get_CFB_json_from_url('venues')
	venues <- data.frame(VenueId = rawVenues$id,
						 VenueName = rawVenues$name,
						 Capacity = rawVenues$capacity,
						 City = rawVenues$city,
						 State = rawVenues$state,
						 Country = rawVenues$country_code,
						 ZipCode = rawVenues$zip,
						 Longitude = rawVenues$location$x,
						 Latitude = rawVenues$location$y,
						 Elevation = rawVenues$elevation,
						 YearConstructed = rawVenues$year_constructed,
						 IsDome = rawVenues$dome,
						 IsGrass = rawVenues$grass)
	return(venues)
}
