#' Interface with MapQuest API to geocode addresses
#'
#' \code{distances_mapquest} creates and executes calls to MapQuest
#' directions API via HTTP GET and returns the distance between two points.
#'
#' @param from A string address
#' @param to A string address
#' @param route_type A string specifying the type of route. Acceptable options
#' include {"fastest", "shortest", "pedestrian", "bicycle"}. More information
#' can be found here: https://developer.mapquest.com/documentation/directions-api/route/get/
#' @param key An individual API authentication key. MapQuest provides
#' 15,000 free monthly transactions with a non-gated account. An
#' account can be created here:
#' https://developer.mapquest.com/plan_purchase/steps/business_edition/business_edition_free/register
#' @return A dataframe will be returned with the given addresses and their
#' corresponding latitudes and longitudes. These are attributed to the
#' closest street location.
#'
#' @examples
#' distances_mapquest(from = "1555 Blake St Denver, CO 80202",
#' to = "2 Cedar St Newark, NJ 07102", key = Sys.getenv("MAPQUEST_API_KEY"))
#'
#'
#' @export
distances_mapquest <- function(from, to,
                               route_type = "shortest",
                               key) {

   url_anchor <- "http://www.mapquestapi.com/directions/v2/route?"

   query <- paste0(url_anchor, "key=", key,
                   "&from=", stringr::str_replace_all(from, " ", "%20"),
                   "&to=", stringr::str_replace_all(to, " ", "%20"),
                   "&outFormat=json&ambiguities=ignore",
                   "&routeType=", route_type)

   response <- httr::GET(query)

   response_data <- httr::content(response)

   return(response_data$route$distance)
}
