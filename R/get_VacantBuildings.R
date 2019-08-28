#' Retrieve a list of vacant buildings due for inspection
#'
#' \code{get_VacantBuildings} returns a data.frame containing all registered vacant buildings
#' due for inspection geocoded (if specified) and filtered for the selected geography
#'   (if specified).
#'
#'  Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/accelavacantbuilding}
#'
#' @param start_date must be coercible to date format
#' @param end_date must be coercible to date format
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
#' get_VacantBuildings()
#' get_VacantBuildings(spatial = TRUE)
#'
# Get vacant buildings data
get_VacantBuildings <- function(start_date = NULL, end_date = NULL,
                                spatial = FALSE, shape, include_missing = FALSE) {

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "46dca88b-fec0-48f1-bda6-7296249ea61f", as = "table")
  start <- Sys.time()
  raw <- ckan_fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw <- raw %>%
    mutate(DATEOPENED = stringr::word(DATEOPENED, 1, 1),
           PARCELNBR = stringr::str_pad(PARCELNBR, width = 10, side = "left", pad = "0"),
           RECORDID = as.character(RECORDID),
           RECORDTYPE = as.character(RECORDTYPE),
           STATUS = as.character(STATUS),
           ADDRFULLLINE = as.character(ADDRFULLLINE),
           BOOK = as.character(BOOK),
           PARCELNBR = as.character(PARCELNBR),
           VALUEIMPROVED = as.numeric(as.character(VALUEIMPROVED)))

  date.filtered <- raw %>%
    filter(as.Date(DATEOPENED) >= as.Date(start_date),
           as.Date(DATEOPENED) <= as.Date(end_date))

  d.final <- date.filtered

  # spatial analysis
  if(!missing(shape) | spatial == TRUE){
    list.spatial <- list()
    date.filtered$uniqueID <- 1:nrow(date.filtered)
    d.mai <- milwaukeer::mai[,c("TAXKEY", "x", "y")] %>%
      mutate(TAXKEY = as.character(TAXKEY))

    spatial1 <- inner_join(date.filtered, d.mai,
                           by = c("PARCELNBR" = "TAXKEY")) %>%
      group_by(uniqueID) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      select(-uniqueID) %>%
      mutate(x = as.numeric(x),
             y = as.numeric(y))
    list.spatial[[1]] <- spatial1

    # try geocoder
    if(nrow(spatial1) < nrow(date.filtered)){
      spatial2 <- anti_join(date.filtered, spatial1) %>%
        tidyr::separate(col = "ADDRFULLLINE", sep = ",", into = c("address","drop"),
                        remove = FALSE) %>%
        geocode_address(fields = "address") %>%
        select(-address, -drop) %>%
        mutate(x = as.numeric(x),
               y = as.numeric(y))

      d.missing <- spatial2[is.na(spatial2$x),]

      list.spatial[[2]] <- spatial2[!is.na(spatial2$x),]
    }

    d.spatial <- bind_rows(list.spatial) %>%
      select(-uniqueID) %>%
      sf::st_as_sf(coords = c("x", "y"),
                   crs = 32054)

    print(paste(nrow(date.filtered) - nrow(d.spatial), "cases unable to be geocoded.",
                "Use include_missing = TRUE to include them in output."))

    if(!missing(shape)){
      d.spatial <- d.spatial %>%
        st_transform(crs = st_crs(shape)) %>%
        st_intersection(shape)
    }

    if(spatial == FALSE){
      d.spatial <- sf::st_set_geometry(d.spatial, NULL)
    }

    if(include_missing == TRUE & nrow(d.spatial) < nrow(date.filtered)){
      d.spatial <- bind_rows(d.spatial, d.missing)
    }

    d.final <- d.spatial
  }
  # output
  d.final
}
