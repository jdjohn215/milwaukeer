#' Retrieve the Wisconsin Incident Based Report (WIBR) dataset
#'
#' \code{get_wibrs} returns a data.frame containing the complete WIBRS crime data
#'   for the requested time period (if specified) and the selected geography
#'   (if specified).
#'
#'  Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/wibr}
#'
#' @param start_date The first date to be included. Must be coercible to class Date.
#' Defaults to first date available.
#' @param end_date The last date to be included. Must be coercible to class Date.
#' Defaults to last date available.
#' @param spatial Logical. If TRUE the output is class sf. Defaults to FALSE.
#' @param shape An object of class sf. If included, the output will be filtered using
#' st_intersection
#' @param include_missing Logical. If TRUE values not geocoded will be added to the output. Defaults to FALSE.
#' @return A dataframe.
#' @export
#' @import dplyr
#' @import sf
#' @importFrom ckanr resource_show
#' @importFrom ckanr ckan_fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_wibrs()
#' get_wibrs(start_date = as.Date("2018-01-01"), end_date = as.Date("2018-02-01"))

# Get WIBRS crime data
get_wibrs <- function(start_date= NULL, end_date = NULL,
                      spatial = FALSE, shape = NULL, include_missing = FALSE) {

  # Set default dates
  if(is.null(start_date)){
    start_date = as.Date("2005-01-01")
  }
  if(is.null(end_date)){
    end_date = as.Date(Sys.Date())
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  res <- resource_show(id = "87843297-a6fa-46d4-ba5d-cb342fb2d3bb", as = "table")
  start <- Sys.time()
  raw <- ckan_fetch(res$url)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))

  # Add date field
  raw <- raw %>%
    mutate(ReportedDate = stringr::str_sub(ReportedDateTime, 1, 10))

  # Filter by start and end dates
  date.filtered <- raw %>%
    filter(ReportedDate >= as.Date(start_date),
           ReportedDate <= as.Date(end_date)) %>%
    mutate(address_match = stringr::str_to_upper(Location),
           address_match = stringr::str_replace(address_match, "MILWAUKEE, WISCONSIN", "MILWAUKEE, WI"),
           address_match = stringr::str_remove_all(address_match, ","),
           #       address_match = word(address_match, 1, -2),
           address_match = stringr::str_remove_all(address_match, "#"),
           address_match = stringr::str_remove(address_match, "MILWAUKEE WI"),
           address_match = stringr::str_squish(address_match),
           address_match = stringr::str_replace(address_match, " ST ST", " ST"))

  # join to MAI
  mai <- milwaukeer::mai %>%
    mutate(address_all = paste(HSE_NBR, DIR, STREET, STTYPE, SFX, UNIT_NBR),
           address_all = stringr::str_replace_all(address_all, " NA ", " "),
           address_all = replace(address_all, stringr::str_sub(address_all, -3) == " NA",
                                 stringr::str_sub(address_all[stringr::str_sub(address_all, -3) == " NA"], 1, -4)),
           address_all = stringr::str_to_upper(address_all),
           address_all = stringr::str_squish(address_all),
           address_all = replace(address_all, !is.na(other_address), other_address[!is.na(other_address)]),
           address_NoSTTYPE = paste(HSE_NBR, DIR, STREET),
           address_NoSTTYPE = stringr::str_replace_all(address_NoSTTYPE, " NA ", " "),
           address_NoSTTYPE = replace(address_NoSTTYPE, stringr::str_sub(address_NoSTTYPE, -3) == " NA",
                                      stringr::str_sub(address_NoSTTYPE[stringr::str_sub(address_NoSTTYPE, -3) == " NA"], 1, -4)),
           address_NoSTTYPE = stringr::str_to_upper(address_NoSTTYPE),
           address_NoSTTYPE = stringr::str_squish(address_NoSTTYPE))

  # Join simple addresses
  has.address <- date.filtered[date.filtered$address_match != "",]
  no.address <- date.filtered[date.filtered$address_match == "",]

  join.list <- list()
  join1 <- inner_join(has.address, mai[,c("address_all","x","y")],
                      by = c("address_match" = "address_all"))
  missing <- anti_join(has.address, join1)
  join.list[[(length(join.list) + 1)]] <- join1

  # Join, trying MAI addresses without street type
  if(nrow(missing) > 0){
    join2 <- inner_join(missing, mai[,c("address_NoSTTYPE", "x", "y")],
                        by = c("address_match" = "address_NoSTTYPE"))
    missing <- anti_join(missing, join2)
    join.list[[(length(join.list) + 1)]] <- join2
    if(nrow(missing) > 0){
      # Join, source addresses with no street type
      missing$address_NoSTTYPE <- remove_STTYPE(missing)

      join3 <- inner_join(missing, mai[,c("address_NoSTTYPE", "x", "y")]) %>%
        group_by(IncidentNum) %>%
        filter(row_number() == 1)
      missing <- anti_join(missing, join3)
      join.list[[(length(join.list) + 1)]] <- join3
    }
  }

  # try coordinates
  try.coords <- bind_rows(no.address, missing)
  has.coords <- try.coords %>%
    filter(!is.na(RoughX),
           !is.na(RoughY)) %>%
    rename(x = RoughX, y = RoughY)
  join.list[[(length(join.list) + 1)]] <- has.coords

  all.joined <- bind_rows(join.list)

  d <- all.joined

  # Make data.frame of missing coordinates if include_missing = T
  if(include_missing == TRUE){
    missing.coords <- try.coords %>%
      filter(!is.na(RoughX),
             !is.na(RoughY))
  }

  # If spatial is TRUE, then convert to SF
  if(!is.null(shape) | spatial == TRUE){
    d <- d %>%
      sf::st_as_sf(coords = c("x", "y"),
                   crs = 32054)

    # If shape is specified, perform intersection with it
    if(!is.null(shape)){
      d <- d %>%
        st_transform(crs = st_crs(shape)) %>%
        st_intersection(shape)
    }
    print(paste(length(date.filtered$IncidentNum[is.na(date.filtered$RoughX)]), "out of",
                length(date.filtered$IncidentNum),
                "incidents not assigned coordinates. Use include_missing = TRUE to view them"))
  }

  if(!is.null(shape) & spatial == FALSE){
    d <- st_set_geometry(d, NULL)
  }

  complete <- d

  # If include_missing was specified, then add missing values back in
  if(include_missing == TRUE){
    complete <- bind_rows(d, missing.coords)
  }

  complete
}
