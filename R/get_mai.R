#' Retrieve the Master Address Index (MAI)
#'
#' \code{get_mai} returns a data.frame containing the complete Master Address index
#' geocoded (if specified) and filtered for the selected geography
#'   (if specified).
#'
#'  Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/mai}
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
#' get_mai()
#' get_mai(spatial = TRUE)

get_mai <- function(spatial = FALSE, shape, include_missing = FALSE) {

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "9f905487-720e-4f30-ae70-b5ec8a2a65a1", as = "table")
  start <- Sys.time()
  raw <- ckan_fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw$TAXKEY <- stringr::str_pad(raw$TAXKEY, width = 10, side = "left", pad = "0")
  raw

  d.final <- raw

  if(spatial == TRUE | !missing(shape)){
    list.spatial <- list()
    raw$uniqueID <- 1:length(raw$TAXKEY)

    # match by taxkey
    spatial1 <- inner_join(raw, milwaukeer::mai[,c("TAXKEY", "x", "y")]) %>%
      group_by(uniqueID) %>%
      filter(row_number() == 1) %>%
      ungroup()
    list.spatial[[1]] <- spatial1

    # try geocoding missing cases
    spatial2 <- anti_join(raw, spatial1) %>%
      mutate(address = paste(HSE_NBR, DIR, STREET, STTYPE)) %>%
      geocode_address(fields = "address") %>%
      mutate(x = as.numeric(x),
             y = as.numeric(y))

    list.spatial[[2]] <- spatial2
    d.spatial <- bind_rows(list.spatial) %>%
      select(-uniqueID) %>%
      sf::st_as_sf(coords = c("x", "y"),
                   crs = 32054)

    # still missing
    missing <- anti_join(raw, d.spatial)
    print(paste(nrow(missing), "cases missing coordinates. Use include_missing = TRUE to include them."))

    # filter is shape is present
    if(!missing(shape)){
      d.spatial %>%
        st_transform(crs = st_crs(shape)) %>%
        st_intersection(shape)
    }

    # append missing if include_missing == TRUE
    if(include_missing == TRUE){
      d.spatial <- bind_rows(d.spatial, missing)
    }

    # Remove spatial attributes if spatial = FALSE
    if(spatial == FALSE){
      d.spatial <- st_set_geometry(d.spatial, NULL)
    }

    d.final <- d.spatial
  }

  d.final
}
