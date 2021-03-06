% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_distance.R
\name{distances_mapquest}
\alias{distances_mapquest}
\title{Interface with MapQuest API to geocode addresses}
\usage{
distances_mapquest(from, to, route_type = "shortest", key)
}
\arguments{
\item{from}{A string address}

\item{to}{A string address}

\item{route_type}{A string specifying the type of route. Acceptable options
include {"fastest", "shortest", "pedestrian", "bicycle"}. More information
can be found here: https://developer.mapquest.com/documentation/directions-api/route/get/}

\item{key}{An individual API authentication key. MapQuest provides
15,000 free monthly transactions with a non-gated account. An
account can be created here:
https://developer.mapquest.com/plan_purchase/steps/business_edition/business_edition_free/register}
}
\value{
A dataframe will be returned with the given addresses and their
corresponding latitudes and longitudes. These are attributed to the
closest street location.
}
\description{
\code{distances_mapquest} creates and executes calls to MapQuest
directions API via HTTP GET and returns the distance between two points.
}
\examples{
distances_mapquest(from = "1555 Blake St Denver, CO 80202",
to = "2 Cedar St Newark, NJ 07102", key = Sys.getenv("MAPQUEST_API_KEY"))


}
