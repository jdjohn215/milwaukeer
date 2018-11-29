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
