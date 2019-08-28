#' Retrieve the Traffic Accident dataset
#'
#' \code{get_TrafficAccidents} returns a data.frame containing a list of traffic accidents
#'   for the requested time period (if specified).
#'
#'  Currently, geocoding is not available
#'   for this resource because locations are not given as standard addresses. Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/trafficaccident}
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
#' @importFrom ckanr ckan_fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_TrafficAccidents()
#' get_TrafficAccidents(start_date = as.Date("2018-01-01"), end_date = as.Date("2018-02-01"))

# Get traffic accidents
get_TrafficAccidents <- function(start_date = NULL, end_date = NULL,
                                 include_missingDate = FALSE) {

  # set default dates
  if(is.null(start_date)){
    start_date = as.Date("2006-01-01")
  }
  if(is.null(end_date)){
    end_date = as.Date(Sys.Date())
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "8fffaa3a-b500-4561-8898-78a424bdacee", as = "table")
  start <- Sys.time()
  raw <- ckan_fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))

  # Split datetime into two variables
  raw2 <- raw %>%
    tidyr::separate(col = "CASEDATE", sep = " ", into = c("date", "time")) %>%
    mutate(date = as.Date(date))

  raw3 <- raw2 %>%
    filter(!is.na(date))

  date.filtered <- raw3 %>%
    filter(date >= as.Date(start_date),
           date <= as.Date(end_date))

  missing.date <- length(raw2$CASENUMBER) - length(raw3$CASENUMBER)
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
