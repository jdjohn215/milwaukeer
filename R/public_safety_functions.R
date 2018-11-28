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
get_ems <- function() {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "662070a0-eadc-4328-b0f5-e8e73531c8e2", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
}

