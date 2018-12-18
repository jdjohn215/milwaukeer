# This file builds city services functions including those for
# garbage violations and call center data

get_GarbageViolations <- function(start_date, end_date, shape, include_missing) {
  # Make missings default value false
  if(missing(include_missing)){
    include_missing = FALSE
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
    mutate(address_match = str_to_upper(Address),
           address_match = str_replace(address_match, "MILWAUKEE, WISCONSIN", "MILWAUKEE, WI"),
           address_match = str_remove_all(address_match, ","),
           address_match = word(address_match, 1, -2),
           address_match = str_remove_all(address_match, "#"),
           address_match = str_remove(address_match, "MILWAUKEE WI"),
           address_match = str_squish(address_match),
           address_match = str_replace(address_match, " ST ST", " ST"))

  # join to MAI
  mai <- mai %>%
    mutate(address_all = paste(HSE_NBR, DIR, STREET, STTYPE, SFX, UNIT_NBR),
           address_all = str_replace_all(address_all, " NA ", " "),
           address_all = replace(address_all, str_sub(address_all, -3) == " NA",
                                 str_sub(address_all[str_sub(address_all, -3) == " NA"], 1, -4)),
           address_all = str_to_upper(address_all),
           address_all = str_squish(address_all),
           address_NoSTTYPE = paste(HSE_NBR, DIR, STREET),
           address_NoSTTYPE = str_replace_all(address_NoSTTYPE, " NA ", " "),
           address_NoSTTYPE = replace(address_NoSTTYPE, str_sub(address_NoSTTYPE, -3) == " NA",
                                      str_sub(address_NoSTTYPE[str_sub(address_NoSTTYPE, -3) == " NA"], 1, -4)),
           address_NoSTTYPE = str_to_upper(address_NoSTTYPE),
           address_NoSTTYPE = str_squish(address_NoSTTYPE))

  # Join simple addresses
  join1 <- inner_join(date.sliced, mai[,c("address_all","x","y")], by = c("address_match" = "address_all"))
  missing1 <- anti_join(date.sliced, join1)

  # Join, trying MAI addresses with street type
  join2 <- inner_join(missing1, mai[,c("address_NoSTTYPE", "x", "y")],
                      by = c("address_match" = "address_NoSTTYPE"))
  missing2 <- anti_join(missing1, join2)

  # Join, source addresses with no street type
  missing2$address_NoSTTYPE <- remove_STTYPE(missing2)

  join3 <- inner_join(missing2, mai[,c("address_NoSTTYPE", "x", "y")])
  missing3 <- anti_join(missing2, join3)

  # Attempt to geocode using city address then DIME geocoder
  join4 <- geocode_address(batch = missing3, fields = "address_match") %>%
    filter(x != "error") %>%
    mutate(x = as.numeric(x),
           y = as.numeric(y))
  missing4 <- anti_join(missing3, join4)

  all.joined <- bind_rows(join1, join2, join3, join4)
  print(paste(length(missing4$address_match), "cases missing out of",
              length(date.sliced$Address)))

  if(!missing(shape)){
    # Make sf object
    all.joined <- all.joined %>%
      st_as_sf(crs = 32054, coords = c("x", "y")) %>%
      st_transform(st_crs(shape)) %>%
      # Filter spatially
      st_intersection(shape)
  }

  if(include_missing == TRUE){
    all.joined <- bind_rows(all.joined, missing3)
  }

  all.joined
}

# Get call center date, filtered by date
get_CallCenter <- function(start_date, end_date) {
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
  # If end_date is present, then check if end_date is in 2018, if yes, fetch current
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
    mutate(CREATION_DATE = as.Date(str_sub(CREATIONDATE, 1, 10)),
           CLOSED_DATE = as.Date(str_sub(CLOSEDDATETIME, 1, 10)),
           CREATION_TIME = str_sub(CREATIONDATE, -8),
           CLOSED_TIME = str_sub(CLOSEDDATETIME, -8)) %>%
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
  date.sliced <- raw %>%
    filter(CREATION_DATE >= as.Date(start_date),
           CREATION_DATE <= as.Date(end_date))
}
