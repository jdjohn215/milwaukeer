# This file builds functions which retrieve elections and campaign data
# It requires ckanr and tidyverse

# Get polling places

#' Retrieve the call center data dataset
#'
#' \code{get_PollingPlace} returns a data.frame containing information about polling places
#'   in the city of Milwaukee for the selected geography (if specified).
#'
#'  Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/votingward}
#'
#' @param spatial Logical. If TRUE the output is class sf. Defaults to FALSE.
#' @param shape An object of class sf. If included, the output will be filtered using
#' st_intersection
#' @param include_missing Logical. If TRUE values not geocoded will be added to the output. Defaults to FALSE.
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import sf
#' @importFrom ckanr resource_show
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_PollingPlace()
#' get_PollingPlace(spatial = TRUE)

get_PollingPlace <- function(shape, spatial) {
  # set defaults
  if(missing(spatial)){
    spatial = FALSE
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "b50aed9e-2893-483c-a56a-3c51530c2cc9", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
  d.output <- raw

  if(spatial == TRUE | !missing(shape)){
    mai <- mai %>%
      mutate(address = paste(HSE_NBR, DIR, STREET, STTYPE),
             address = stringr::str_to_upper(address))

    d.spatial <- list()
    spatial1 <- raw %>%
      mutate(address = stringr::str_to_upper(Polling.Place.Address),
             address = stringr::str_replace(address, " KK ", " KINNICKINNIC ")) %>%
      inner_join(mai[,c("address", "x", "y")]) %>%
      group_by(Ward, Aldermanic.District) %>%
      filter(row_number() == 1) %>%
      ungroup()

    d.spatial[[1]] <- spatial1

    # find missing
    if(nrow(spatial1) < nrow(raw)){
      spatial2 <- anti_join(raw, spatial1) %>%
        mutate(Polling.Place.Address = stringr::str_replace(Polling.Place.Address, " KK ", " KINNICKINNIC ")) %>%
        geocode_address(fields = "Polling.Place.Address") %>%
        mutate(x = as.numeric(x),
               y = as.numeric(y))

      d.spatial[[2]] <- spatial2
    }

    d.spatial <- bind_rows(d.spatial) %>%
      sf::st_as_sf(coords = c("x", "y"),
                   crs = 32054)
    missing.cases <- nrow(raw) - nrow(d.spatial)

    print(paste(missing.cases, "cases failed to be geocoded."))

    if(!missing(shape)){
      d.spatial <- d.spatial %>%
      sf::st_transform(crs = sf::st_crs(shape)) %>%
      sf::st_intersection(shape)
    }

    if(spatial == FALSE){
      d.spatial <- sf::st_set_geometry(d.spatial, NULL)
    }

    d.output <- d.spatial
  }
  d.output
}
