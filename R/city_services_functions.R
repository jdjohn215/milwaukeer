# This file builds city services functions including those for
# garbage violations and call center data

#' Retrieve the resolved (abated) garbage violations dataset
#'
#' \code{get_GarbageViolations} returns a data.frame containing the complete Garbage Violations dataset
#'   for the requested time period (if specified) and the selected geography
#'   (if specified).
#'
#'  Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/garbageviolationsabatedcurrent}
#'
#' @param start_date The first date to be included. Must be coercible to class Date.
#' Defaults to first date available.
#' @param end_date The last date to be included. Must be coercible to class Date.
#' Defaults to last date available.
#' @param spatial Logical. If TRUE the output is class sf. Defaults to FALSE.
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
#' get_GarbageViolations()
#' get_GarbageViolations(start_date = as.Date("2018-01-01"), end_date = as.Date("2018-02-01"))

get_GarbageViolations <- function(start_date, end_date, shape, include_missing, spatial) {
  # Make missings default value false
  if(missing(include_missing)){
    include_missing = FALSE
  }
  if(missing(spatial)){
    spatial = FALSE
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  start <- Sys.time()
  res.historical <- resource_show(id = "665d15b0-71b7-45cd-9864-4648fec06219", as = "table")
  raw.historical <- fetch(res.historical$url) %>%
    mutate_all(as.character)
  res.current <- resource_show(id = "9424b207-86b9-4914-9fb9-4ed47f37e260", as = "table")
  raw.current <- fetch(res.current$url) %>%
    mutate_all(as.character)
  raw <- bind_rows(raw.historical, raw.current) %>%
    mutate(DateOpened = as.Date(DateOpened),
           DateAbated = as.Date(DateAbated))
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  if(missing(start_date))
    start_date <- min(raw$DateOpened)
  if(missing(end_date))
    end_date <- max(raw$DateOpened)
  date.sliced <- raw %>%
    filter(DateOpened >= as.Date(start_date),
           DateOpened <= as.Date(end_date)) %>%
    mutate(address_match = stringr::str_to_upper(Address),
           address_match = stringr::str_replace(address_match, "MILWAUKEE, WISCONSIN", "MILWAUKEE, WI"),
           address_match = stringr::str_remove_all(address_match, ","),
           address_match = stringr::word(address_match, 1, -2),
           address_match = stringr::str_remove_all(address_match, "#"),
           address_match = stringr::str_remove(address_match, "MILWAUKEE WI"),
           address_match = stringr::str_squish(address_match),
           address_match = stringr::str_replace(address_match, " ST ST", " ST"),
           uniqueID = 1:length(DateOpened))
  d.final <- date.sliced

  if(!missing(shape) | spatial == T){
    # join to MAI
    mai <- mai %>%
      mutate(address_all = paste(HSE_NBR, DIR, STREET, STTYPE, SFX, UNIT_NBR),
             address_all = stringr::str_replace_all(address_all, " NA ", " "),
             address_all = replace(address_all, stringr::str_sub(address_all, -3) == " NA",
                                   stringr::str_sub(address_all[stringr::str_sub(address_all, -3) == " NA"], 1, -4)),
             address_all = stringr::str_to_upper(address_all),
             address_all = stringr::str_squish(address_all),
             address_NoSTTYPE = paste(HSE_NBR, DIR, STREET),
             address_NoSTTYPE = stringr::str_replace_all(address_NoSTTYPE, " NA ", " "),
             address_NoSTTYPE = replace(address_NoSTTYPE, stringr::str_sub(address_NoSTTYPE, -3) == " NA",
                                        stringr::str_sub(address_NoSTTYPE[stringr::str_sub(address_NoSTTYPE, -3) == " NA"], 1, -4)),
             address_NoSTTYPE = stringr::str_to_upper(address_NoSTTYPE),
             address_NoSTTYPE = stringr::str_squish(address_NoSTTYPE))

    # Join simple addresses
    join.list <- list()
    join1 <- inner_join(date.sliced, mai[,c("address_all","x","y")], by = c("address_match" = "address_all"))
    missing <- anti_join(date.sliced, join1)
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

        join3 <- inner_join(missing, mai[,c("address_NoSTTYPE", "x", "y")])
        missing <- anti_join(missing, join3)
        join.list[[(length(join.list) + 1)]] <- join3
        if(nrow(missing) > 0){
          # Attempt to geocode using city address then DIME geocoder
          join4 <- geocode_address(batch = missing, fields = "address_match") %>%
            filter(x != "error") %>%
            mutate(x = as.numeric(x),
                   y = as.numeric(y))
          missing <- anti_join(missing, join4)
          join.list[[(length(join.list) + 1)]] <- join4
        }
      }
    }

    all.joined <- bind_rows(join.list) %>%
      group_by(uniqueID) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      select(-uniqueID)
    print(paste(length(missing$address_match), "cases missing out of",
                length(date.sliced$Address)))

    if(!missing(shape)){
      # Make sf object
      all.joined <- all.joined %>%
        st_as_sf(crs = 32054, coords = c("x", "y")) %>%
        st_transform(st_crs(shape)) %>%
        # Filter spatially
        st_intersection(shape)

      if(spatial == FALSE){
        all.joined <- st_set_geometry(all.joined, NULL)
      }
    }

    if(missing(shape)){
      all.joined <- all.joined %>%
        st_as_sf(crs = 32054, coords = c("x", "y"))
    }

    if(include_missing == TRUE){
      all.joined <- bind_rows(all.joined, missing)
    }

    all.joined %>%
      select(-address_match, -address_NoSTTYPE)

    d.final <- all.joined
  }

  d.final
}


# Get call center date, filtered by date

#' Retrieve the call center data dataset
#'
#' \code{get_CallCenter} returns a data.frame containing the complete Call Center dataset
#'   for the requested time period (if specified) and the selected geography
#'   (if specified).
#'
#'  Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/callcenterdatacurrent}
#'
#' @param start_date The first date to be included. Must be coercible to class Date.
#' Defaults to first date available.
#' @param end_date The last date to be included. Must be coercible to class Date.
#' Defaults to last date available.
#' @param spatial Logical. If TRUE the output is class sf. Defaults to FALSE.
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
#' get_CallCenter()
#' get_CallCenter(start_date = as.Date("2018-01-01"), end_date = as.Date("2018-02-01"))

get_CallCenter <- function(start_date, end_date, spatial, shape, include_missing) {

  # Set defaults
  if(missing(spatial)){
    spatial = FALSE
  }
  if(missing(include_missing)){
    include_missing = FALSE
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  start <- Sys.time()
  df.list <- list()
  # If start_date is missing, then fetch historical file
  if(missing(start_date)){
    res.historical <- resource_show(id = "abdfe983-e856-40cd-bee2-85e78454344a", as = "table")
    raw.historical <- fetch(res.historical$url) %>%
      mutate_all(as.character)
    df.list[[length(df.list)+1]] <- raw.historical
  }
  # If start_date is present, then check if start_date precedes 2018, if yes, fetch historical
  if(!missing(start_date)){
    if(as.Date(start_date) < as.Date("2018-01-01")){
      res.historical <- resource_show(id = "abdfe983-e856-40cd-bee2-85e78454344a", as = "table")
      raw.historical <- fetch(res.historical$url) %>%
        mutate_all(as.character)
      df.list[[length(df.list)+1]] <- raw.historical
    }
  }
  # If end_date is missing, then fetch current file
  if(missing(end_date)){
    res.current <- resource_show(id = "bf2b508a-5bfa-49da-8846-d87ffeee020a", as = "table")
    raw.current <- fetch(res.current$url) %>%
      mutate_all(as.character)
    df.list[[length(df.list)+1]] <- raw.current
  }
  # If end_date is present, then check if end_date is in 2018 or 19, if yes, fetch current
  if(!missing(end_date)){
    if(as.Date(end_date) > as.Date("2017-12-31")){
      res.current <- resource_show(id = "bf2b508a-5bfa-49da-8846-d87ffeee020a", as = "table")
      raw.current <- fetch(res.current$url) %>%
        mutate_all(as.character)
      df.list[[length(df.list)+1]] <- raw.current
    }
  }
  # Create raw by combining files
  raw <- bind_rows(df.list) %>%
    mutate(CREATION_DATE = as.Date(stringr::str_sub(CREATIONDATE, 1, 10)),
           CLOSED_DATE = as.Date(stringr::str_sub(CLOSEDDATETIME, 1, 10)),
           CREATION_TIME = stringr::str_sub(CREATIONDATE, -8),
           CLOSED_TIME = stringr::str_sub(CLOSEDDATETIME, -8)) %>%
    select(-CREATIONDATE, -CLOSEDDATETIME)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  # If missing start_date, create it from dataframe
  if(missing(start_date))
    start_date <- min(raw$CREATION_DATE)
  # If missing end_date, create it from dataframe
  if(missing(end_date))
    end_date <- max(raw$CREATION_DATE)

  # Slice dataframe by start and finish date
  date.filtered <- raw %>%
    filter(CREATION_DATE >= as.Date(start_date),
           CREATION_DATE <= as.Date(end_date))

  final.data <- date.filtered

  if(!missing(shape) | spatial == TRUE){
    date.filtered <- date.filtered %>%
      mutate(address_match = stringr::str_to_upper(OBJECTDESC),
             address_match = stringr::str_replace(address_match, "MILWAUKEE, WISCONSIN", "MILWAUKEE, WI"),
             address_match = stringr::str_remove_all(address_match, ","),
             #       address_match = word(address_match, 1, -2),
             address_match = stringr::str_remove_all(address_match, "#"),
             address_match = stringr::str_remove(address_match, "MILWAUKEE WI"),
             address_match = stringr::str_squish(address_match),
             address_match = stringr::str_replace(address_match, " ST ST", " ST"),
             uniqueID = 1:length(address_match))

    # join to MAI
    mai <- mai %>%
      mutate(address_all = paste(HSE_NBR, DIR, STREET, STTYPE, SFX, UNIT_NBR),
             address_all = stringr::str_replace_all(address_all, " NA ", " "),
             address_all = replace(address_all, stringr::str_sub(address_all, -3) == " NA",
                                   stringr::str_sub(address_all[stringr::str_sub(address_all, -3) == " NA"], 1, -4)),
             address_all = stringr::str_to_upper(address_all),
             address_all = stringr::str_squish(address_all),
             address_all = replace(address_all, other_address != "none", other_address[other_address != "none"]),
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
          group_by(uniqueID) %>%
          filter(row_number() == 1)
        missing <- anti_join(missing, join3)
        join.list[[(length(join.list) + 1)]] <- join3

        if(nrow(missing) > 0){
          # Join, source addresses without unit character between HouseNR & SDIR
          join4 <- missing %>%
            mutate(address_match2 = paste(stringr::word(address_match, 1),
                                          stringr::word(address_match, 3, -1)))
          join4 <- inner_join(join4, mai[,c("address_all", "x", "y")],
                              by = c("address_match2" = "address_all")) %>%
            group_by(uniqueID) %>%
            filter(row_number() == 1)
          missing <- anti_join(missing, join4)
          join.list[[(length(join.list) + 1)]] <- join4
        }
      }
    }
    # Join together all the geocoded records
    all.joined <- bind_rows(join.list) %>%
      group_by(uniqueID) %>%
      filter(row_number() == 1)

    # If shape is specified, perform intersection with it
    if(!missing(shape)){
      # first, make spatial, then intersect
      all.joined <- all.joined %>%
        sf::st_as_sf(coords = c("x", "y"),
                     crs = 32054) %>%
        st_transform(crs = st_crs(shape)) %>%
        st_intersection(shape)

      # Remove spatial attributes if spatial == FALSE
      if(spatial == FALSE){
        all.joined <- st_set_geometry(all.joined, NULL)
      }
    }

    # If shape is not specified, but spatial == TRUE
    if(missing(shape) & spatial == TRUE){
      all.joined <- all.joined %>%
        sf::st_as_sf(coords = c("x", "y"),
                     crs = 32054)
    }

    # If include_missing == TRUE, add missing records
    if(include_missing == TRUE){
      all.joined <- bind_rows(all.joined, missing)
    }

    final.data <- all.joined
  }
  final.data
}
