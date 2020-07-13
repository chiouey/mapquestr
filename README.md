# mapquestr

`mapquestr` provides an R interface to interact with (parts of) the MapQuest APIs.

## Installation

You can install the development version from GitHub by running:
```r
# install.packages("devtools")
devtools::install_github("chiouey/mapquestr")
```

You can obtain a key [here](https://developer.mapquest.com/plan_purchase/steps/business_edition/business_edition_free/register).

## Usage

Documentation can be interrogated, but as a teaser:

```r
library(mapquestr)

geocode_mapquest(c("765 Broad St Newark, NJ 07102", "219 Avon Ave Newark, NJ 07108"),
                 key = Sys.getenv("MAPQUEST_KEY")) %>%
  as.data.frame()

#>                        address       lat        lon
#>1 765 Broad St Newark, NJ 07102 40.736341 -74.171953
#>2 219 Avon Ave Newark, NJ 07108 40.726005 -74.200559
```
