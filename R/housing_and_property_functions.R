# This file builds functions which retrieve housing and property data
# It requires ckanr and tidyverse

# Get MPROP data
## right now this function just grabs the current file
## in the future, build in an ability to grab historical files
get_mprop <- function() {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "0a2c7f31-cd15-4151-8222-09dd57d5f16d", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
}

# Get the city's master address index
get_mai <- function() {
  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "9f905487-720e-4f30-ae70-b5ec8a2a65a1", as = "table")
  start <- Sys.time()
  raw <- fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
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
