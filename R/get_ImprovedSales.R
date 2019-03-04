#' Return a list of city-owned properties sold
#' \code{get_ImprovedSales} returns a data.frame containing addresses of every foreclosed
#' property sold by the city. The data.frame can be geocoded
#'  (if specified) and filtered for the selected geography (if specified).
#'
#'  Refer to the data dictionary for further information:
#'   \url{https://data.milwaukee.gov/dataset/total-number-of-city-owned-improved-properties-sold-by-year}
#'
#' @param start_year a numeric value. Defaults to earliest available
#' @param end_year a numeric value. Defaults to last available.
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
#' get_ImprovedSales()
#' get_ImprovedSales(spatial = TRUE)

get_ImprovedSales <- function(start_year = NULL, end_year = NULL,
                              shape, spatial = FALSE, include_missing = FALSE) {

  ckanr_setup(url = "https://data.milwaukee.gov")
  start <- Sys.time()
  years <- start_year:end_year
  df.list <- list()
  if(is.element(2014, years)){
    res14 <- resource_show(id = "147ce69a-6fe4-41e2-b8c5-fd7e234068c1", as = "table")
    raw14 <- fetch(res14$url) %>%
      mutate_all(as.character) %>%
      rename(`Key.No.` = `Key.No..`, `Property.Type` = `Property.Type.`)
    df.list[[length(df.list)+1]] <- raw14
  }
  if(is.element(2015, years)){
    res15 <- resource_show(id = "f533f6ad-1328-4d3f-bc50-f8fc0e2bc14a", as = "table")
    raw15 <- fetch(res15$url) %>%
      mutate_all(as.character)
    df.list[[length(df.list)+1]] <- raw15
  }
  if(is.element(2016, years)){
    res16 <- resource_show(id = "ca2fcfb8-b9aa-4e4b-8e36-c5df34a01fbb", as = "table")
    raw16 <- fetch(res16$url) %>%
      mutate_all(as.character) %>%
      rename(`Sale.Price` = `Sale.Price.`)
    df.list[[length(df.list)+1]] <- raw16
  }
  if(is.element(2017, years)){
    res17 <- resource_show(id = "c0ca342b-e3de-4b7c-8852-205e294b00d8", as = "table")
    raw17 <- fetch(res17$url) %>%
      mutate_all(as.character)
    df.list[[length(df.list)+1]] <- raw17
  }
  raw <- bind_rows(df.list)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  d.output <- raw

  if(!missing(shape) | spatial == TRUE){
    d.spatial <- raw %>%
      mutate(`Key.No.` = stringr::str_pad(`Key.No.`, pad = "0", width = 10, side = "left"),
             uniqueID = 1:nrow(raw)) %>%
      inner_join(mai[,c("TAXKEY", "x", "y")],
                 by = c("Key.No." = "TAXKEY")) %>%
      group_by(uniqueID) %>%
      filter(row_number() == 1) %>%
      select(-uniqueID) %>%
      sf::st_as_sf(coords = c("x", "y"),
                   crs = 32054)

    missing.cases <- nrow(raw) - nrow(d.spatial)
    print(paste(missing.cases, "cases unable to be geocoded. Use include_missing = TRUE to include them in output."))

    if(!missing(shape)){
      d.spatial <- d.spatial %>%
        st_transform(crs = st_crs(shape)) %>%
        st_intersection(shape)
    }

    if(spatial == FALSE){
      d.spatial <- st_set_geometry(d.spatial, NULL)
    }

    if(include_missing == TRUE & missing.cases > 0){
      d.missing <- anti_join(raw, d.spatial)
      d.spatial <- bind_rows(d.spatial, d.missing)
    }
    d.output <- d.spatial
  }
  d.output
}
