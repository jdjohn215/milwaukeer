#' Retrieve the resolved (abated) garbage violations dataset
#'
#' \code{get_GarbageViolations} returns a data.frame containing the complete Garbage Violations dataset
#'   for the requested time period (if specified) and the selected geography
#'   (if specified).
#'
#'  Refer to the data dictionary for variable descriptions:
#'   \url{https://data.milwaukee.gov/dataset/garbageviolationsabatedcurrent}
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
#' @importFrom ckanr fetch
#' @importFrom ckanr ckanr_setup
#'
#' @examples
#' get_GarbageViolations()
#' get_GarbageViolations(start_date = as.Date("2018-01-01"), end_date = as.Date("2018-02-01"))

get_GarbageViolations <- function(start_date = NULL, end_date = NULL, shape,
                                  include_missing = FALSE, spatial = FALSE) {
  # set defaults
  if(is.null(start_date)){
    start_date = as.Date("2016-01-01")
  }
  if(is.null(end_date)){
    end_date = as.Date(Sys.Date())
  }

  ckanr_setup(url = "https://data.milwaukee.gov")
  start <- Sys.time()
  res.historical <- resource_show(id = "665d15b0-71b7-45cd-9864-4648fec06219", as = "table")
  raw.historical <- fetch(res.historical$url) %>%
    mutate_all(as.character)
  # res.current <- resource_show(id = "9424b207-86b9-4914-9fb9-4ed47f37e260", as = "table")
  # raw.current <- fetch(res.current$url) %>%
  #   mutate_all(as.character)
  raw <- raw.historical %>%
    mutate(DateOpened = as.Date(DateOpened),
           DateAbated = as.Date(DateAbated))
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))

  date.sliced <- raw %>%
    filter(DateOpened >= as.Date(start_date),
           DateOpened <= as.Date(end_date)) %>%
    mutate(address_match = stringr::str_to_upper(Address),
           address_match = stringr::str_replace(address_match, "MILWAUKEE, WISCONSIN", "MILWAUKEE, WI"),
           address_match = stringr::str_remove_all(address_match, ","),
           address_match = stringr::word(address_match, 1, -2),
           address_match = stringr::str_remove_all(address_match, "#"),
           address_match = stringr::str_remove(address_match, "MILWAUKEE WI"),
           address_match = stringr::str_squish(address_match),
           address_match = stringr::str_replace(address_match, " ST ST", " ST"),
           uniqueID = 1:length(DateOpened))
  d.final <- date.sliced

  if(!missing(shape) | spatial == T){
    # join to MAI
    mai <- milwaukeer::mai %>%
      mutate(address_all = paste(HSE_NBR, DIR, STREET, STTYPE, SFX, UNIT_NBR),
             address_all = stringr::str_replace_all(address_all, " NA ", " "),
             address_all = replace(address_all, stringr::str_sub(address_all, -3) == " NA",
                                   stringr::str_sub(address_all[stringr::str_sub(address_all, -3) == " NA"], 1, -4)),
             address_all = stringr::str_to_upper(address_all),
             address_all = stringr::str_squish(address_all),
             address_NoSTTYPE = paste(HSE_NBR, DIR, STREET),
             address_NoSTTYPE = stringr::str_replace_all(address_NoSTTYPE, " NA ", " "),
             address_NoSTTYPE = replace(address_NoSTTYPE, stringr::str_sub(address_NoSTTYPE, -3) == " NA",
                                        stringr::str_sub(address_NoSTTYPE[stringr::str_sub(address_NoSTTYPE, -3) == " NA"], 1, -4)),
             address_NoSTTYPE = stringr::str_to_upper(address_NoSTTYPE),
             address_NoSTTYPE = stringr::str_squish(address_NoSTTYPE))

    # Join simple addresses
    join.list <- list()
    join1 <- inner_join(date.sliced, mai[,c("address_all","x","y")], by = c("address_match" = "address_all"))
    missing <- anti_join(date.sliced, join1)
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

        join3 <- inner_join(missing, mai[,c("address_NoSTTYPE", "x", "y")])
        missing <- anti_join(missing, join3)
        join.list[[(length(join.list) + 1)]] <- join3
        if(nrow(missing) > 0){
          # Attempt to geocode using city address then DIME geocoder
          join4 <- geocode_address(batch = missing, fields = "address_match") %>%
            filter(x != "error") %>%
            mutate(x = as.numeric(x),
                   y = as.numeric(y))
          missing <- anti_join(missing, join4)
          join.list[[(length(join.list) + 1)]] <- join4
        }
      }
    }

    all.joined <- bind_rows(join.list) %>%
      group_by(uniqueID) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      select(-uniqueID)
    print(paste(length(missing$address_match), "cases missing out of",
                length(date.sliced$Address)))

    if(!missing(shape)){
      # Make sf object
      all.joined <- all.joined %>%
        st_as_sf(crs = 32054, coords = c("x", "y")) %>%
        st_transform(st_crs(shape)) %>%
        # Filter spatially
        st_intersection(shape)

      if(spatial == FALSE){
        all.joined <- st_set_geometry(all.joined, NULL)
      }
    }

    if(missing(shape)){
      all.joined <- all.joined %>%
        st_as_sf(crs = 32054, coords = c("x", "y"))
    }

    if(include_missing == TRUE){
      all.joined <- bind_rows(all.joined, missing)
    }

    all.joined %>%
      select(-address_match, -address_NoSTTYPE)

    d.final <- all.joined
  }

  d.final
}
