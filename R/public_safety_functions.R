# This file builds functions which retrieve crime and public safety data
# It requires ckanr and tidyverse

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


# Get traffic accidents
get_TrafficAccidents <- function() {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "8fffaa3a-b500-4561-8898-78a424bdacee", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
}

# Get Milwaukee Fire Department actions
get_FireIncidents <- function() {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "ed310d17-2a6d-4334-9102-ff20f4462743", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
}

# Get emergency medical services trips
get_ems <- function(start_year, end_year) {
  ckanr_setup(url = "https://data.milwaukee.gov")
  start <- Sys.time()
  years <- start_year:end_year
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
  raw
}

