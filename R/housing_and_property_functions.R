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

# This file builds functions which retrieve current liquor licenses
# It requires ckanr and tidyverse

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
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_LiquorLicenses()
#' get_LiquorLicenses(spatial = TRUE)
get_LiquorLicenses <- function(spatial, shape, include_missing) {
  # set default
  if(missing(spatial)){
    spatial = FALSE
  }

  if(missing(include_missing)){
    include_missing = FALSE
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "45c027b5-fa66-4de2-aa7e-d9314292093d", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
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
    spatial1 <- inner_join(raw, mai[,c("TAXKEY", "x", "y")]) %>%
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

# Get residential & commericial permit work data
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
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_WorkPermits()
#' get_WorkPermits(spatial = TRUE)

get_WorkPermits <- function(start_date, end_date, spatial, shape, include_missing) {
  if(missing(start_date)){
    start_date = as.Date("2013-01-01")
  }
  if(missing(end_date)){
    end_date = Sys.Date()
  }
  if(missing(spatial)){
    spatial = FALSE
  }
  if(missing(include_missing)){
    include_missing = FALSE
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "828e9630-d7cb-42e4-960e-964eae916397", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
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

    mai <- mai %>%
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
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_VacantBuildings()
#' get_VacantBuildings(spatial = TRUE)
#'
# Get vacant buildings data
get_VacantBuildings <- function(start_date, end_date, spatial, shape, include_missing) {
  # set defaults
  if(missing(spatial)){
    spatial = FALSE
  }
  if(missing(include_missing)){
    include_missing = FALSE
  }
  if(missing(start_date)){
    start_date = as.Date("2010-01-01")
  }
  if(missing(end_date)){
    end_date = Sys.Date()
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "46dca88b-fec0-48f1-bda6-7296249ea61f", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
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
    d.mai <- mai[,c("TAXKEY", "x", "y")] %>%
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

# Get delinquent real estate data

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
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_DelinquentRealEstate()
#' get_DelinquentRealEstate(spatial = TRUE)

get_DelinquentRealEstate <- function(shape, spatial, include_missing) {
  # set defaults
  if(missing(spatial)){
    spatial = FALSE
  }
  if(missing(include_missing)){
    include_missing = FALSE
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "a1291cf2-2c11-4a04-90ac-e86e0a03a52e", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw <- raw %>%
    mutate(`Tax.Key..` = stringr::str_pad(`Tax.Key..`, pad = "0", width = 10, side = "left"))
  d.output <- raw

  if(spatial == TRUE | !missing(shape)){
    d.spatial <- raw %>%
      mutate(uniqueID = 1:nrow(raw)) %>%
               inner_join(mai[,c("TAXKEY", "x", "y")],
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

# Get the locations of lead service lines
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

get_LeadService <- function(shape, spatial, include_missing){
  # set defaults
  if(missing(spatial)){
    spatial = FALSE
  }
  if(missing(include_missing)){
    include_missing = FALSE
  }

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

    mai <- mai %>%
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

# Get city-owned improved properties sold

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

get_ImprovedSales <- function(start_year, end_year, shape, spatial, include_missing) {
  # set defaults
  if(missing(start_year)){
    start_year = 2014
  }
  if(missing(end_year)){
    end_year = as.numeric(stringr::str_sub(Sys.Date(), 1, 4))
  }
  if(missing(spatial)){
    spatial = FALSE
  }
  if(missing(include_missing)){
    include_missing = FALSE
  }

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
