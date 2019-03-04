#' Retrieve the Emergency Calls for Service (EMS) dataset
#'
#' \code{get_ems} returns a data.frame containing a list of fire dept incidents
#'   for the requested time period (if specified).
#'
#'  No geocoding is available as the only geographies provided are zip codes and aldermanic districts. Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/mfdems}
#'
#' @param start_date The first date to be included. Must be coercible to class Date.
#' Defaults to first date available.
#' @param end_date The last date to be included. Must be coercible to class Date.
#' Defaults to last date available.
#' @param include_missingDate Logical. If TRUE values without a valid date will be included. Defaults to FALSE.
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import sf
#' @importFrom ckanr resource_show
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_ems()
#' get_ems(start_date = as.Date("2018-01-01"), end_date = as.Date("2018-02-01"))

# Get emergency medical services trips
get_ems <- function(start_date = NULL, end_date = NULL, include_missingDate = FALSE) {
  if(is.null(start_date)){
    start_date <- as.Date("2015-01-01")
  }
  if(is.null(end_date)){
    end_date <- as.Date(Sys.Date())
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  start <- Sys.time()
  years <- as.numeric(stringr::str_sub(start_date, 1, 4)):as.numeric(stringr::str_sub(end_date, 1, 4))
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

  # filter out missing dates
  raw2 <- raw %>%
    filter(!is.na(Incident.Date))

  date.filtered <- raw2 %>%
    filter(Incident.Date >= as.Date(start_date),
           Incident.Date <= as.Date(end_date))

  missing.date <- length(raw$Case.Number) - length(raw2$Case.Number)
  print(paste(missing.date, "Incidents are missing properly formatted dates.",
              "Set include_missingDate = TRUE to include them."))

  if(missing(include_missingDate)){
    include_missingDate = FALSE
  }

  if(include_missingDate == TRUE){
    no.date <- anti_join(raw2, raw3)
    date.filtered <- bind_rows(date.filtered, no.date)
  }

  date.filtered
}

