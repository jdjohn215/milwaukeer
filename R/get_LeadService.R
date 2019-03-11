#' Get the locations of lead service lines
#' \code{get_LeadService} returns a data.frame containing addresses in which the city-owned
#' section of the water service line is made of lead. The data.frame can be geocoded
#'  (if specified) and filtered for the selected geography (if specified).
#'
#'  Refer to the data dictionary for further information:
#'   \url{https://data.milwaukee.gov/dataset/lead-service-line-data}
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
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_LeadService()
#' get_LeadService(spatial = TRUE)

get_LeadService <- function(shape, spatial = FALSE, include_missing = FALSE){

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "c8c72ec0-8331-4ccb-949b-bd284d0054db", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw <- raw[raw$City == "MILWAUKEE",]
  d.output <- raw

  if(!missing(shape) | spatial == TRUE){
    d.spatial <- list()

    mai <- milwaukeer::mai %>%
      mutate(address = paste(HSE_NBR, DIR, STREET, STTYPE),
             address = stringr::str_to_upper(address))

    spatial1 <- raw %>%
      mutate(address = paste(House.Number.Range, Street.Name),
             address = stringr::str_to_upper(address),
             uniqueID = 1:nrow(raw)) %>%
      inner_join(mai[,c("address", "x", "y")]) %>%
      group_by(uniqueID) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      select(-uniqueID)

    d.spatial[[1]] <- spatial1

    if(nrow(spatial1) < nrow(raw)){
      spatial2 <- anti_join(raw, spatial1) %>%
        mutate(address = paste(House.Number.Range, Street.Name),
               uniqueID = 1:length(address)) %>%
        inner_join(mai[,c("other_address", "x", "y")],
                   by = c("address" = "other_address")) %>%
        group_by(uniqueID) %>%
        filter(row_number() == 1) %>%
        select(-uniqueID)

      d.spatial[[2]] <- spatial2

      # Now try some custom changes
      if(nrow(raw) > (nrow(spatial1) + nrow(spatial2))){
        spatial3 <- anti_join(raw, spatial1) %>%
          anti_join(spatial2) %>%
          mutate(Street.Name = as.character(Street.Name),
                 Street.Name = replace(Street.Name, Street.Name == "N 35TH ST", "N MOTHER DANIELS WA"),
                 Street.Name = replace(Street.Name, Street.Name == "S 16TH ST", "S CESAR E CHAVEZ DR"),
                 Street.Name = replace(Street.Name, Street.Name == "W MITCHELL ST", "W HISTORIC MITCHELL ST"),
                 address = paste(House.Number.Range, Street.Name),
                 address = stringr::str_to_upper(address),
                 uniqueID = 1:length(address)) %>%
          inner_join(mai[,c("address", "x", "y")]) %>%
          group_by(uniqueID) %>%
          filter(row_number() == 1) %>%
          ungroup() %>%
          select(-uniqueID)

        d.spatial[[3]] <- spatial3
      }
    }

    d.spatial <- bind_rows(d.spatial) %>%
      sf::st_as_sf(coords = c("x", "y"),
                   crs = 32054)

    missing.cases <- nrow(raw) - nrow(d.spatial)
    print(paste(missing.cases, "cases unable to be geocoded. Set include_missing = TRUE to include them in output."))

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
