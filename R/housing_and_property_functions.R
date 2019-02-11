# This file builds functions which retrieve housing and property data
# It requires ckanr and tidyverse

# This file builds functions which retrieve crime and public safety data
# It requires ckanr and tidyverse

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
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_mai()
#' get_mai(spatial = TRUE)

get_mai <- function(spatial, shape, include_missing) {
  # make spatial false by default
  if(missing(spatial)){
    spatial = FALSE
  }
  if(missing(include_missing)){
    include_missing = FALSE
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "9f905487-720e-4f30-ae70-b5ec8a2a65a1", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw$TAXKEY <- str_pad(raw$TAXKEY, width = 10, side = "left", pad = "0")
  raw

  d.final <- raw

  if(spatial == TRUE | !missing(shape)){
    list.spatial <- list()
    raw$uniqueID <- 1:length(raw$TAXKEY)

    # match by taxkey
    spatial1 <- inner_join(raw, mai[,c("TAXKEY", "x", "y")]) %>%
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

# Get all current liquor licenses in the city
get_LiquorLicenses <- function() {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "45c027b5-fa66-4de2-aa7e-d9314292093d", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
}

# Get residential & commericial permit work data
get_WorkPermits <- function() {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "828e9630-d7cb-42e4-960e-964eae916397", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
}

# Get vacant buildings data
get_VacantBuildings <- function() {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "46dca88b-fec0-48f1-bda6-7296249ea61f", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
}

# Get delinquent real estate data
get_DelinquentRealEstate <- function() {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "a1291cf2-2c11-4a04-90ac-e86e0a03a52e", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
}

# Get the locations of lead service lines
get_LeadService <- function() {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "c8c72ec0-8331-4ccb-949b-bd284d0054db", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
}

# Get city-owned improved properties sold
get_ImprovedSales <- function(start_year, end_year) {
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
  raw
}
