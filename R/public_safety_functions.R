# This file builds functions which retrieve crime and public safety data
# It requires ckanr and tidyverse

#' Retrieve the Wisconsin Incident Based Report (WIBR) dataset
#'
#' \code{get_wibrs} returns a data.frame containing the complete WIBRS crime data
#'   for the requested time period (if specified) and the selected geography
#'   (if specified).
#'
#'  Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/wibr}
#'
#' @param start_date The first date to be included. Must be coercible to class Date.
#' Defaults to first date available.
#' @param end_date The last date to be included. Must be coercible to class Date.
#' Defaults to last date available.
#' @param make_spatial Logical. If TRUE the output is class sf. Defaults to FALSE.
#' @param shape An object of class sf. If included, the output will be filtered using
#' st_intersection
#' @param include_missing Logical. If TRUE values not geocoded will be added to the output. Defaults to FALSE.
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import sf
#' @importFrom ckanr resource_show
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_wibrs()
#' get_wibrs(start_date = as.Date("2018-01-01"), end_date = as.Date("2018-02-01"))

# Get WIBRS crime data
get_wibrs <- function(start_date, end_date, make_spatial, shape, include_missing) {
  if(missing(make_spatial) & missing(shape)){
    make_spatial = FALSE
  } else{
    make_spatial = TRUE
  }

  if(missing(include_missing)){
    include_missing = FALSE
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "87843297-a6fa-46d4-ba5d-cb342fb2d3bb", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))

  # Add date field
  raw <- raw %>%
    mutate(ReportedDate = stringr::str_sub(ReportedDateTime, 1, 10))

  # Create start_date and end_date if missing
  if(missing(start_date)){
    start_date = min(as.Date(raw$ReportedDate))
  }
  if(missing(end_date)){
    end_date = max(as.Date(raw$ReportedDate))
  }

  # Filter by start and end dates
  date.filtered <- raw %>%
    filter(ReportedDate >= as.Date(start_date),
           ReportedDate <= as.Date(end_date)) %>%
    mutate(address_match = stringr::str_to_upper(Location),
           address_match = stringr::str_replace(address_match, "MILWAUKEE, WISCONSIN", "MILWAUKEE, WI"),
           address_match = stringr::str_remove_all(address_match, ","),
           #       address_match = word(address_match, 1, -2),
           address_match = stringr::str_remove_all(address_match, "#"),
           address_match = stringr::str_remove(address_match, "MILWAUKEE WI"),
           address_match = stringr::str_squish(address_match),
           address_match = stringr::str_replace(address_match, " ST ST", " ST"))

  # join to MAI
  mai <- mai %>%
    mutate(address_all = paste(HSE_NBR, DIR, STREET, STTYPE, SFX, UNIT_NBR),
           address_all = stringr::str_replace_all(address_all, " NA ", " "),
           address_all = replace(address_all, stringr::str_sub(address_all, -3) == " NA",
                                 stringr::str_sub(address_all[stringr::str_sub(address_all, -3) == " NA"], 1, -4)),
           address_all = stringr::str_to_upper(address_all),
           address_all = stringr::str_squish(address_all),
           address_all = replace(address_all, !is.na(other_address), other_address[!is.na(other_address)]),
           address_NoSTTYPE = paste(HSE_NBR, DIR, STREET),
           address_NoSTTYPE = stringr::str_replace_all(address_NoSTTYPE, " NA ", " "),
           address_NoSTTYPE = replace(address_NoSTTYPE, stringr::str_sub(address_NoSTTYPE, -3) == " NA",
                                      stringr::str_sub(address_NoSTTYPE[stringr::str_sub(address_NoSTTYPE, -3) == " NA"], 1, -4)),
           address_NoSTTYPE = stringr::str_to_upper(address_NoSTTYPE),
           address_NoSTTYPE = stringr::str_squish(address_NoSTTYPE))

  # Join simple addresses
  has.address <- date.filtered[date.filtered$address_match != "",]
  no.address <- date.filtered[date.filtered$address_match == "",]

  join.list <- list()
  join1 <- inner_join(has.address, mai[,c("address_all","x","y")],
                      by = c("address_match" = "address_all"))
  missing <- anti_join(has.address, join1)
  join.list[[(length(join.list) + 1)]] <- join1

  # Join, trying MAI addresses without street type
  if(nrow(missing) > 0){
    join2 <- inner_join(missing, mai[,c("address_NoSTTYPE", "x", "y")],
                        by = c("address_match" = "address_NoSTTYPE"))
    missing <- anti_join(missing, join2)
    join.list[[(length(join.list) + 1)]] <- join2
    if(nrow(missing) > 0){
      # Join, source addresses with no street type
      missing$address_NoSTTYPE <- remove_STTYPE(missing)

      join3 <- inner_join(missing, mai[,c("address_NoSTTYPE", "x", "y")]) %>%
        group_by(IncidentNum) %>%
        filter(row_number() == 1)
      missing <- anti_join(missing, join3)
      join.list[[(length(join.list) + 1)]] <- join3
    }
  }

  # try coordinates
  try.coords <- bind_rows(no.address, missing)
  has.coords <- try.coords %>%
    filter(!is.na(RoughX),
           !is.na(RoughY)) %>%
    rename(x = RoughX, y = RoughY)
  join.list[[(length(join.list) + 1)]] <- has.coords

  all.joined <- bind_rows(join.list)

  d <- all.joined

  # Make data.frame of missing coordinates if include_missing = T
  if(include_missing == TRUE){
    missing.coords <- try.coords %>%
      filter(!is.na(RoughX),
             !is.na(RoughY))
  }

  # If make_spatial is TRUE, then convert to SF
  if(make_spatial == TRUE){
    d <- d %>%
      sf::st_as_sf(coords = c("x", "y"),
                   crs = 32054)

    # If shape is specified, perform intersection with it
    if(!missing(shape)){
      d <- d %>%
        st_transform(crs = st_crs(shape)) %>%
        st_intersection(shape)
    }
    print(paste(length(date.filtered$IncidentNum[is.na(date.filtered$RoughX)]), "out of",
                length(date.filtered$IncidentNum),
                "incidents not assigned coordinates. Use include_missing = TRUE to view them"))
  }

  complete <- d

  # If include_missing was specified, then add missing values back in
  if(include_missing == TRUE){
    complete <- bind_rows(d, missing.coords)
  }

  complete
}

#' Retrieve the Traffic Accident dataset
#'
#' \code{get_TrafficAccidents} returns a data.frame containing a list of traffic accidents
#'   for the requested time period (if specified).
#'
#'  Currently, geocoding is not available
#'   for this resource because locations are not given as standard addresses. Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/trafficaccident}
#'
#' @param start_date The first date to be included. Must be coercible to class Date.
#' Defaults to first date available.
#' @param end_date The last date to be included. Must be coercible to class Date.
#' Defaults to last date available.
#' @param include_missingDate Logical. If TRUE values without a valid date will be included. Defaults to FALSE.
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import sf
#' @importFrom ckanr resource_show
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_TrafficAccidents()
#' get_TrafficAccidents(start_date = as.Date("2018-01-01"), end_date = as.Date("2018-02-01"))

# Get traffic accidents
get_TrafficAccidents <- function(start_date, end_date, include_missingDate) {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "8fffaa3a-b500-4561-8898-78a424bdacee", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))

  # Split datetime into two variables
  raw2 <- raw %>%
    tidyr::separate(col = "CASEDATE", sep = " ", into = c("date", "time")) %>%
    mutate(date = as.Date(date))

  raw3 <- raw2 %>%
    filter(!is.na(date))

  # Filter by date and time, default to full range of data
  if(missing(start_date)){
    start_date <- min(raw3$date)
  }
  if(missing(end_date)){
    end_date <- max(raw3$date)
  }

  date.filtered <- raw3 %>%
    filter(date >= as.Date(start_date),
           date <= as.Date(end_date))

  missing.date <- length(raw2$CASENUMBER) - length(raw3$CASENUMBER)
  print(paste(missing.date, "Incidents are missing properly formatted dates.",
              "Set include_missingDate = TRUE to include them."))

  if(missing(include_missingDate)){
    include_missingDate = FALSE
  }

  if(include_missingDate == TRUE){
    no.date <- anti_join(raw2, raw3)
    date.filtered <- bind_rows(date.filtered, no.date)
  }

  date.filtered
}

#' Retrieve the Fire Incident dataset
#'
#' \code{get_FireIncidents} returns a data.frame containing a list of fire dept incidents
#'   for the requested time period (if specified).
#'
#'  Geocoding is based on the coordinates provided in the file. Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/mfdprimary/}
#'
#' @param start_date The first date to be included. Must be coercible to class Date.
#' Defaults to first date available.
#' @param end_date The last date to be included. Must be coercible to class Date.
#' Defaults to last date available.
#' @param make_spatial Logical. If TRUE the output is class sf. Defaults to FALSE.
#' @param shape An object of class sf. If included, the output will be filtered using
#' st_intersection
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import sf
#' @importFrom ckanr resource_show
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_FireIncidents()
#' get_FireIncidents(start_date = as.Date("2018-01-01"), end_date = as.Date("2018-02-01"))

# Get Milwaukee Fire Department actions
get_FireIncidents <- function(start_date, end_date, spatial, shape) {
  # Make spatial have default of false
  if(missing(spatial)){
    spatial = FALSE
  }
  if(missing(start_date)){
    start_date = as.Date("2015-01-01")
  }
  if(missing(end_date)){
    end_date = as.Date(Sys.Date())
  }

  years.list <- as.numeric(stringr::str_sub(start_date, 1, 4)):as.numeric(stringr::str_sub(end_date, 1, 4))
  raw.list <- list()

  start <- Sys.time()
  # download 2015
  if(2015 %in% years.list == TRUE){
    raw2015 <- readr::read_csv("https://data.milwaukee.gov/dataset/b61a89f7-1604-4b3a-b465-42f5a1e232bc/resource/97509bea-dfa4-435e-aa75-5499adf7bbe0/download/og_2015_primary.csv",
                               col_types = readr::cols(.default = "c")) %>%
      filter(`Case Number` != "Case Number") %>%
      tidyr::separate(col = "Date Received", into = c("DateReceived", "TimeReceived"),
                      sep = " ") %>%
      tidyr::separate(col = "DateReceived", into = c("month", "day", "year"),
                      sep = "/") %>%
      mutate(DateReceived = paste(year, month, day, sep = "-")) %>%
      select(-year, -month, -day)
    raw.list[[length(raw.list)+1]] <- raw2015
  }
  if(2016 %in% years.list == TRUE){
    raw2016 <- readr::read_csv("https://data.milwaukee.gov/dataset/b61a89f7-1604-4b3a-b465-42f5a1e232bc/resource/5d2d6ce8-e4f6-4c41-a800-31e4eface37f/download/og_2016_primary.csv",
                               col_types = readr::cols(.default = "c")) %>%
      filter(`Case Number` != "Case Number") %>%
      tidyr::separate(col = "Date Received", into = c("DateReceived", "TimeReceived"),
                      sep = " ") %>%
      tidyr::separate(col = "DateReceived", into = c("month", "day", "year"),
                      sep = "/") %>%
      mutate(DateReceived = paste(year, month, day, sep = "-")) %>%
      select(-year, -month, -day)
    raw.list[[length(raw.list)+1]] <- raw2016
  }
  if(2017 %in% years.list == TRUE){
    raw2017 <- readr::read_csv("https://data.milwaukee.gov/dataset/b61a89f7-1604-4b3a-b465-42f5a1e232bc/resource/25a3fb9e-161c-416e-82da-6837c704b643/download/og_2017_primary.csv",
                               col_types = readr::cols(.default = "c")) %>%
      filter(`Case Number` != "Case Number") %>%
      tidyr::separate(col = "Date Received", into = c("DateReceived", "TimeReceived"),
                      sep = " ") %>%
      tidyr::separate(col = "DateReceived", into = c("month", "day", "year"),
                      sep = "/") %>%
      mutate(DateReceived = paste(year, month, day, sep = "-")) %>%
      select(-year, -month, -day)
    raw.list[[length(raw.list)+1]] <- raw2017
  }
  if(2018 %in% years.list == TRUE){
    raw2018 <- readr::read_csv("https://data.milwaukee.gov/dataset/c62f2c78-da2e-48d8-9215-37fb11c143f1/resource/ed310d17-2a6d-4334-9102-ff20f4462743/download/mfdprimary.csv",
                               col_types = readr::cols(.default = "c")) %>%
      filter(`Case Number` != "Case Number") %>%
      tidyr::separate(col = "Date Received", into = c("DateReceived", "TimeReceived"),
                      sep = " ")
    raw.list[[length(raw.list)+1]] <- raw2018
  }

  raw <- bind_rows(raw.list)

  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))

  # fix variable names
  names(raw) <- stringr::str_replace_all(names(raw), " ", "")

  # Split datetime into two variables
  raw2 <- raw %>%
    mutate(DateReceived = as.Date(DateReceived))

  # filter out missing dates
  raw3 <- raw2 %>%
    filter(!is.na(DateReceived))

  date.filtered <- raw3 %>%
    filter(DateReceived >= as.Date(start_date),
           DateReceived <= as.Date(end_date))
  d.output <- date.filtered

  # make spatial
  if(!missing(shape) | spatial == TRUE){
    date.filtered <- st_as_sf(date.filtered, coords = c("Longitude", "Latitude"),
                              crs = 4326)

    # filter by shape
    if(!missing(shape)){
      date.filtered <- date.filtered %>%
        st_transform(crs = st_crs(shape)) %>%
        st_intersection(shape)
    }

    # remove spatial attributes if spatial == FALSE
    if(spatial == FALSE){
      date.filtered <- st_set_geometry(date.filtered, NULL)
    }
    d.output <- date.filtered
  }

  d.output
}


#' Retrieve the Emergency Calls for Service (EMS) dataset
#'
#' \code{get_ems} returns a data.frame containing a list of fire dept incidents
#'   for the requested time period (if specified).
#'
#'  No geocoding is available as the only geographies provided are zip codes and aldermanic districts. Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/mfdems}
#'
#' @param start_date The first date to be included. Must be coercible to class Date.
#' Defaults to first date available.
#' @param end_date The last date to be included. Must be coercible to class Date.
#' Defaults to last date available.
#' @param include_missingDate Logical. If TRUE values without a valid date will be included. Defaults to FALSE.
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import sf
#' @importFrom ckanr resource_show
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_ems()
#' get_ems(start_date = as.Date("2018-01-01"), end_date = as.Date("2018-02-01"))

# Get emergency medical services trips
get_ems <- function(start_date, end_date, include_missingDate) {
  if(missing(start_date)){
    start_date <- as.Date("2015-01-01")
  }
  if(missing(end_date)){
    end_date <- Sys.Date()
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  start <- Sys.time()
  years <- as.numeric(stringr::str_sub(start_date, 1, 4)):as.numeric(stringr::str_sub(end_date, 1, 4))
  df.list <- list()
  if(is.element(2015, years)){
    res15 <- resource_show(id = "9ab724f8-2e43-420e-9c86-e757acacb412", as = "table")
    raw15 <- fetch(res15$url) %>%
      mutate_all(as.character)
    df.list[[length(df.list)+1]] <- raw15
  }
  if(is.element(2016, years)){
    res16 <- resource_show(id = "7844c7a6-8308-4d32-b46b-69603ebb5767", as = "table")
    raw16 <- fetch(res16$url) %>%
      mutate_all(as.character)
    df.list[[length(df.list)+1]] <- raw16
  }
  if(is.element(2017, years)){
    res17 <- resource_show(id = "ebe5461b-7302-47a2-bd54-2204758249b8", as = "table")
    raw17 <- fetch(res17$url) %>%
      mutate_all(as.character)
    df.list[[length(df.list)+1]] <- raw17
  }
  if(is.element(2018, years)){
    res18 <- resource_show(id = "662070a0-eadc-4328-b0f5-e8e73531c8e2", as = "table")
    raw18 <- fetch(res18$url) %>%
      mutate_all(as.character)
    df.list[[length(df.list)+1]] <- raw18
  }
  raw <- bind_rows(df.list) %>%
    filter(`Incident.Date` != "Incident Date") #  Remove apparent header rows in data
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))

  # filter out missing dates
  raw2 <- raw %>%
    filter(!is.na(Incident.Date))

  date.filtered <- raw2 %>%
    filter(Incident.Date >= as.Date(start_date),
           Incident.Date <= as.Date(end_date))

  missing.date <- length(raw$Case.Number) - length(raw2$Case.Number)
  print(paste(missing.date, "Incidents are missing properly formatted dates.",
              "Set include_missingDate = TRUE to include them."))

  if(missing(include_missingDate)){
    include_missingDate = FALSE
  }

  if(include_missingDate == TRUE){
    no.date <- anti_join(raw2, raw3)
    date.filtered <- bind_rows(date.filtered, no.date)
  }

  date.filtered
}

