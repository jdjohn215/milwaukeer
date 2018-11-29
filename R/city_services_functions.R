# This file builds city services functions including those for
# garbage violations and call center data

get_GarbageViolations <- function(start_date, end_date) {
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
           DateOpened <= as.Date(end_date))
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
