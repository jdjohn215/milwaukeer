#' Retrieve a list of current food establishment grades from the health department
#'
#' \code{get_FoodGrades} returns a data.frame containing all current restaurant grades
#' geocoded (if specified) and filtered for the selected geography
#'   (if specified).
#'
#'  Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/foodgrading}
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
#' get_FoodGrades()
#' get_FoodGrades(spatial = TRUE)
get_FoodGrades <- function(spatial = FALSE, shape, include_missing = FALSE) {

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "e2fecdb9-3de9-4d34-955e-17d2e815bad6", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  d.final <- raw

  # filter by geography if provided, make spatial if requested
  if(!missing(shape) | spatial == TRUE){
    list.spatial <- list()
    mai <- milwaukeer::mai %>%
      mutate(ADDRESS = paste(HSE_NBR, DIR, STREET, STTYPE),
             ADDRESS = replace(ADDRESS, is.na(STTYPE),
                               stringr::str_sub(ADDRESS[is.na(STTYPE)], 1, -3)),
             ADDRESS = stringr::str_squish(stringr::str_to_upper(ADDRESS))) %>%
      add_row(x = 2541641, y = 382047.4, ADDRESS = "1 S MILLER PARK WA")
    spatial1 <- raw %>%
      mutate(ADDRESS = stringr::str_to_upper(ADDRESS),
             uniqueID = 1:nrow(raw)) %>%
      inner_join(mai[,c("ADDRESS", "x", "y")]) %>%
      group_by(uniqueID) %>%
      filter(row_number() == 1) %>%
      ungroup()
    list.spatial[[1]] <- spatial1

    # try addresses if any cases are missing
    if(nrow(spatial1) < nrow(raw)){
      spatial2 <- anti_join(raw, spatial1) %>%
        # remove address outside milwaukee
        filter(ADDRESS != "6821 W LINCOLN AV") %>%
        mutate(ADDRESS = stringr::str_to_upper(ADDRESS)) %>%
        geocode_address(fields = "ADDRESS") %>%
        mutate(x = as.numeric(x),
               y = as.numeric(y))

      list.spatial[[2]] <- spatial2
    }

    d.spatial <- bind_rows(list.spatial) %>%
      sf::st_as_sf(coords = c("x", "y"),
                   crs = 32054)

    missing.cases <- nrow(raw[raw$ADDRESS != "6821 W LINCOLN AV",]) - nrow(d.spatial)

    print(paste(missing.cases, "cases unable to be geocoded.",
                "Use include_missing = TRUE to include them."))

    # filter by shape
    if(!missing(shape)){
      d.spatial <- d.spatial %>%
        st_transform(crs = st_crs(shape)) %>%
        st_intersection(shape)
    }

    # append missing values
    if(include_missing == TRUE & missing.cases > 0){
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
