# This file builds functions which retrieve crime and public safety data
# It requires ckanr and tidyverse

# Get WIBRS crime data
get_wibrs <- function() {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "87843297-a6fa-46d4-ba5d-cb342fb2d3bb", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
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

