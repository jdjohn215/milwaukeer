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
get_FireIncidents <- function(start_date = NULL, end_date = NULL,
                              spatial = FALSE, shape) {

  # make default dates
  if(is.null(start_date)){
    start_date = as.Date("2015-01-01")
  }
  if(is.null(end_date)){
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
    raw2018 <- readr::read_csv("https://data.milwaukee.gov/dataset/b61a89f7-1604-4b3a-b465-42f5a1e232bc/resource/18d887be-4254-4386-b9b4-f1a8c016db38/download/mfdprimaryarchive-1.csv",
                               col_types = readr::cols(.default = "c")) %>%
      filter(`Case Number` != "Case Number") %>%
      tidyr::separate(col = "Date Received", into = c("DateReceived", "TimeReceived"),
                      sep = " ")
    raw.list[[length(raw.list)+1]] <- raw2018
  }
  if(2019 %in% years.list == TRUE){
    raw2018 <- readr::read_csv("https://data.milwaukee.gov/dataset/c62f2c78-da2e-48d8-9215-37fb11c143f1/resource/ed310d17-2a6d-4334-9102-ff20f4462743/download/mfdprimary.csv",
                               col_types = readr::cols(.default = "c")) %>%
      filter(`Case Number` != "Case Number") %>%
      tidyr::separate(col = "Date Received", into = c("DateReceived", "TimeReceived"),
                      sep = " ")
    raw.list[[length(raw.list)+1]] <- raw2019
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
