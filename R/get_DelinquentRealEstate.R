#' Retrieve a list of delinquent real estate
#' \code{get_DelinquentRealEstate} returns a data.frame containing all delinquent real estate
#' accounts geocoded (if specified) and filtered for the selected geography
#'   (if specified).
#'
#'  Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/delinquent-real-estate-tax-accounts}
#'
#' @param spatial Logical. If TRUE the output is class sf. Defaults to FALSE.
#' @param shape An object of class sf. If included, the output will be filtered using
#' st_intersection
#' @param include_missing Logical. If TRUE values not geocoded will be added to the output.
#' Defaults to FALSE.
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import sf
#' @importFrom ckanr resource_show
#' @importFrom ckanr ckan_fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_DelinquentRealEstate()
#' get_DelinquentRealEstate(spatial = TRUE)

get_DelinquentRealEstate <- function(shape, spatial = FALSE, include_missing = FALSE) {

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "83deeba1-0db5-4e2a-a39a-68e745ca1c20", as = "table")
  start <- Sys.time()
  raw <- ckan_fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw <- raw %>%
    mutate(`Tax.Key..` = stringr::str_pad(`Tax.Key..`, pad = "0", width = 10, side = "left"))
  d.output <- raw

  if(spatial == TRUE | !missing(shape)){
    d.spatial <- raw %>%
      mutate(uniqueID = 1:nrow(raw)) %>%
      inner_join(milwaukeer::mai[,c("TAXKEY", "x", "y")],
                 by = c("Tax.Key.." = "TAXKEY")) %>%
      group_by(uniqueID) %>%
      filter(row_number() == 1) %>%
      select(-uniqueID) %>%
      ungroup() %>%
      sf::st_as_sf(coords = c("x", "y"),
                   crs = 32054)

    missing.cases <- nrow(raw) - nrow(d.spatial)
    print(paste(missing.cases, "cases failed to be geocoded. Set include_missing = TRUE to include them in the output."))

    if(!missing(shape)){
      d.spatial <- d.spatial %>%
        st_transform(crs = st_crs(shape)) %>%
        st_intersection(shape)
    }

    if(include_missing == TRUE){
      d.missing <- anti_join(raw, d.spatial)
      d.spatial <- bind_rows(d.spatial, d.missing)
    }

    if(spatial == FALSE){
      d.spatial <- st_set_geometry(d.spatial, NULL)
    }

    d.output <- d.spatial
  }
  d.output
}
