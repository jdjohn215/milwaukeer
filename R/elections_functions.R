# This file builds functions which retrieve elections and campaign data
# It requires ckanr and tidyverse

# Get polling places
get_PollingPlace <- function() {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "b50aed9e-2893-483c-a56a-3c51530c2cc9", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
}
