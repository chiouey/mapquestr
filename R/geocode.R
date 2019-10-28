#' Interface with MapQuest API to geocode addresses
#'
#' \code{geocode_mapquest_batch} creates and executes calls to MapQuest
#' batch geocoding API via HTTP GET. This batch version allows calls
#' of 100 locations at a time.
#'
#' @param addresses A string vector containing addresses, locations, or
#' points of interest to geocode.
#' @param key An individual API authentication key. MapQuest provides
#' 15,000 free monthly transactions with a non-gated account. An
#' account can be created here:
#' https://developer.mapquest.com/plan_purchase/steps/business_edition/business_edition_free/register
#' @return A dataframe will be returned with the given addresses and their
#' corresponding latitudes and longitudes. These are attributed to the
#' closest street location.
#'
#' @examples
#' geocode_mapquest_batch(schools, key = Sys.getenv("MAPQUEST_API_KEY"))
#'
#' @importFrom magrittr %>%
#'
#' @export
geocode_mapquest_batch <- function(addresses,
                                   key) {

   addresses %>%
      dplyr::filter(!is.na(addresses))

   n <- length(addresses)

   # calculate number of calls (batches) needed
   if (n %% 100 == 0) {
      batches <- n / 100
   } else {
      batches <- floor(n / 100) + 1
   }

   # initialize empty df that houses output
   output <- tibble::tibble(address = character(),
                            lat = double(),
                            lon = double())

   # GET anchor with certain changes that are immutable across calls
   url_anchor <- "https://www.mapquestapi.com/geocoding/v1/batch?&inFormat=kvp&outFormat=json&thumbMaps=false&maxResults=1"


   for (b in 1:batches) {

      # how many rows in the output data frame
      output_counter <- 0

      # query builder object container
      location_query <- paste0("")

      # for each address in a given batch
      for (a in (b * 100 - 99):(b * 100)) {

         output_counter <- output_counter + 1

         # concatenate location field; convert spaces to js encoding
         location_query <- paste0(location_query, "&location=",
                                  stringr::str_replace_all(addresses[a], " ", "%20"))
      }

      query <- paste0(url_anchor, location_query, "&key=", key)

      response <- httr::GET(query)

      if (response$status_code >= 400)
         stop(paste0("Error ", response$status_code,
                     ". See Mapquest API status code documentation for more information."))

      # parse json object
      response_data <- httr::content(response)


      for (i in 1:output_counter) {
         output <- output %>%
            dplyr::bind_rows(tibble::tibble(address = addresses[nrow(output) + 1],
                                            lat = response_data$results[[i]]$locations[[1]]$latLng$lat,
                                            lon = response_data$results[[i]]$locations[[1]]$latLng$lng))
         #latLng is the position of the nearest street
         #displayLatLng is the actual physical location
      }

   }

   return(output)
}


#' Interface with MapQuest API to geocode addresses
#'
#' \code{geocode_mapquest} creates and executes calls to MapQuest
#' batch geocoding API via HTTP GET.
#'
#' @param addresses A string vector containing addresses, locations, or
#' points of interest to geocode.
#' @param key An individual API authentication key. MapQuest provides
#' 15,000 free monthly transactions with a non-gated account. An
#' account can be created here:
#' https://developer.mapquest.com/plan_purchase/steps/business_edition/business_edition_free/register
#' @return A dataframe will be returned with the given addresses and their
#' corresponding latitudes and longitudes. These are attributed to the
#' closest street location.
#'
#' @examples
#' geocode_mapquest(schools, key = Sys.getenv("MAPQUEST_API_KEY"))
#'
#'
#' @importFrom magrittr %>%
#'
#' @export
geocode_mapquest <- function(addresses,
                             key) {


   output <- tibble::tibble(address = character(),
                            lat = double(),
                            lon = double())

   url_anchor <- "http://www.mapquestapi.com/geocoding/v1/address?"

   for (i in 1:length(addresses)) {

      query <- paste0(url_anchor, "key=", key,
                      "&inFormat=kvp&outFormat=json&thumbMaps=FALSE&location=",
                      stringr::str_replace_all(addresses[i], " ", "%20"))


      response <- httr::GET(query)

      if (response$status_code >= 400)
         stop(paste0("Error ", response$status_code,
                     ". See Mapquest API status code documentation for more information."))

      response_data <- httr::content(response)

      output <- output %>%
         dplyr::bind_rows(tibble::tibble(address = addresses[i],
                                         lat = response_data$results[[1]]$locations[[1]]$displayLatLng$lat,
                                         lon = response_data$results[[1]]$locations[[1]]$displayLatLng$lng))
   }

   return(output)

}
