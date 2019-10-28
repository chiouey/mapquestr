distance_mapquest <- function(from, to,
                              route_type = "fastest",
                              key) {
   url_anchor <- "http://www.mapquestapi.com/directions/v2/route/"

   query <- paste0(url_anchor, "key=", key,
                   "&from=", stringr::str_sub(from, " ", "%20"),
                   "&to=", stringr::str_sub(from, " ", "%20"),
                   "&routeType=", route_type,
                   "&outFormat=json&ambiguities=ignore")

   response <- httr::GET(query)

   response_data <- httr:content(response)

   return(return_data)
}
