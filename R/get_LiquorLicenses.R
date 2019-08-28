#' Retrieve a list of current liquor licenses
#'
#' \code{get_LiquorLicenses} returns a data.frame containing all current liquor licenses
#' geocoded (if specified) and filtered for the selected geography
#'   (if specified).
#'
#'  Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/liquorlicenses}
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
#' get_LiquorLicenses()
#' get_LiquorLicenses(spatial = TRUE)
get_LiquorLicenses <- function(spatial = FALSE, shape, include_missing = FALSE) {

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "45c027b5-fa66-4de2-aa7e-d9314292093d", as = "table")
  start <- Sys.time()
  raw <- ckan_fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw <- raw %>%
    mutate(TAXKEY = stringr::str_pad(TAXKEY, width = 10, side = "left", pad = "0"))
  d.final <- raw

  # filter by geography if provided, make spatial if requested
  if(!missing(shape) | spatial == TRUE){
    list.spatial <- list()
    raw$uniqueID <- 1:nrow(raw)
    spatial1 <- inner_join(raw, milwaukeer::mai[,c("TAXKEY", "x", "y")]) %>%
      group_by(uniqueID) %>%
      filter(row_number() == 1) %>%
      ungroup()
    list.spatial[[1]] <- spatial1

    # try addresses if any cases are missing
    if(nrow(spatial1) < nrow(raw)){
      spatial2 <- anti_join(raw, spatial1) %>%
        mutate(address = paste(HOUSE_NR, SDIR, STREET, STTYPE)) %>%
        geocode_address(fields = "address") %>%
        mutate(x = as.numeric(x),
               y = as.numeric(y)) %>%
        select(-address)

      list.spatial[[2]] <- spatial2
    }

    d.spatial <- bind_rows(list.spatial) %>%
      sf::st_as_sf(coords = c("x", "y"),
                   crs = 32054)

    print(paste(nrow(raw) - nrow(d.spatial), "cases unable to be geocoded.",
                "Use include_missing = TRUE to include them."))

    # filter by shape
    if(!missing(shape)){
      d.spatial <- d.spatial %>%
        st_transform(crs = st_crs(shape)) %>%
        st_intersection(shape)
    }

    # append missing values
    if(include_missing == TRUE){
      missing <- anti_join(raw, d.spatial)
      d.spatial <- bind_rows(d.spatial, missing)
    }

    # remove spatial attributes if spatial == FALSE
    if(spatial == FALSE){
      d.spatial <- st_set_geometry(d.spatial, NULL)
    }

    d.final <- d.spatial %>%
      select(-uniqueID)

  }

  # output
  d.final
}
