#' Retrieve a list of current residential and commercial work permits
#'
#' \code{get_WorkPermits} returns a data.frame containing all residential and commercial
#' work permits geocoded (if specified) and filtered for the selected geography
#'   (if specified).
#'
#'  Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/liquorlicenses}
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
#' get_WorkPermits()
#' get_WorkPermits(spatial = TRUE)

get_WorkPermits <- function(start_date = NULL, end_date = NULL,
                            spatial = FALSE, shape, include_missing = FALSE) {

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "828e9630-d7cb-42e4-960e-964eae916397", as = "table")
  start <- Sys.time()
  raw <- ckan_fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw <- raw %>%
    mutate(date = stringr::word(`Date.Opened`, 1))
  date.filtered <- raw %>%
    filter(as.Date(date) >= start_date,
           as.Date(date) <= end_date)

  d.final <- date.filtered

  # filter and/or geocode
  if(!missing(shape) | spatial == TRUE){
    to.join <- date.filtered %>%
      mutate(Address = stringr::str_to_upper(Address),
             uniqueID = 1:nrow(date.filtered)) %>%
      tidyr::separate(col = "Address", into = c("address_match","drop"),
                      sep = ",", remove = FALSE) %>%
      select(-drop)

    mai <- milwaukeer::mai %>%
      mutate(STTYPE = replace(STTYPE, is.na(STTYPE), ""),
             address_match = paste(HSE_NBR, DIR, STREET, STTYPE),
             address_match = stringr::str_to_upper(address_match),
             address_match = stringr::str_squish(address_match))

    list.spatial <- list()

    spatial1 <- inner_join(to.join, mai[,c("address_match", "x", "y")]) %>%
      group_by(uniqueID) %>%
      filter(row_number() == 1) %>%
      ungroup()

    list.spatial[[1]] <- spatial1

    # try joining missing cases without STTYPE
    if(nrow(spatial1) < nrow(to.join)){
      to.join2 <- anti_join(to.join, spatial1)
      to.join2$address_nosttype <- remove_STTYPE(to.join2)
      mai$address_nosttype <- stringr::str_to_upper(paste(mai$HSE_NBR, mai$DIR, mai$STREET))

      spatial2 <- inner_join(to.join2, mai[,c("address_nosttype", "x", "y")]) %>%
        group_by(uniqueID) %>%
        filter(row_number() == 1) %>%
        ungroup()

      list.spatial[[2]] <- spatial2

      # Now try geocoder
      if(nrow(spatial2) < nrow(to.join2)){
        to.join3 <- anti_join(to.join2, spatial2)
        spatial3 <- geocode_address(to.join3, "address_match") %>%
          mutate(x = as.numeric(x),
                 y = as.numeric(y))

        d.missing <- spatial3[is.na(spatial3$x),]
        list.spatial[[3]] <- spatial3[!is.na(spatial3$x),]
      }
    }
    d.spatial <- bind_rows(list.spatial) %>%
      sf::st_as_sf(coords = c("x", "y"),
                   crs = 32054)

    print(paste(nrow(to.join) - nrow(d.spatial), "cases unable to be geocoded.",
                "Use include_missing = TRUE to include them."))

    # filter by geography
    if(!missing(shape)){
      d.spatial <- d.spatial %>%
        st_transform(crs = st_crs(shape)) %>%
        st_intersection(shape)
    }

    # remove spatial attributes
    if(spatial == FALSE){
      d.spatial <- st_set_geometry(d.spatial, NULL)
    }

    # add missing
    if(include_missing == TRUE & nrow(to.join) > nrow(d.spatial)){
      d.spatial <- bind_rows(d.spatial, d.missing)
    }

    d.final <- d.spatial
  }
  # output
  d.final
}
