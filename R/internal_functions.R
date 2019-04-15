# This script includes the package's interal functions
# In the future, figure out how to make this appropriately internal

# This function return a vector of an address excluding ...
# the street type and everything after it.
# It returns a dataframe
remove_STTYPE <- function(data){
  temp <- data %>%
    # Add extra space at the end of string
    mutate(address_match = paste0(address_match, " "),
           row = 1:length(address_match))

  # make vector of street types surrounded by spaces
  street.types <- c(" AV ", " DR ", " ST ", " RD ", " BL ", " PL ",
                    " LA ", " CR ", " WA ", " CT ", " TR ", " PK ",
                    " BI ", " WY ")

  # make empty list
  position.list <- list()

  # iterate through possible street types
  for(i in seq_along(street.types)){
    positions <- stringr::str_locate(temp$address_match, pattern = street.types[i])
    positions <- data.frame(positions)
    positions$row <- 1:length(positions$start)
    position.list[[i]] <- positions
  }

  # combine position data.frames for each possible street type
  # if multiple patterns are identified, choose the last one
  position <- bind_rows(position.list) %>%
    filter(!is.na(start)) %>%
    group_by(row) %>%
    filter(start == max(start))

  # join to the original temp file by row
  # if start position is missing, replace with end of string
  temp <- left_join(temp, position) %>%
    mutate(start = replace(start, is.na(start), nchar(address_match[is.na(start)])),
           end = replace(end, is.na(end), nchar(address_match[is.na(end)])))

  # Create variable with address string cut off just before street type
  temp$address_NoSTTYPE <- stringr::str_sub(temp$address_match,
                                   start = 1, end = temp$start)

  # output the address_NoSTTYPE column
  stringr::str_squish(temp$address_NoSTTYPE)
}

# This function runs address strings through the city of Milwaukee's...
# address-then-DIME geocoder.

#' Retrieve the call center data dataset
#'
#' \code{get_CallCenter} this function runs address strings through the city of Milwaukee's
#' address-then-DIME geocoder
#'
#' @param batch the data.frame
#' @param fields the character vector of the address
#' @return A dataframe.
#' @export
#'
geocode_address <- function(batch, fields){
  prefix <- "http://maps2.milwaukee.gov/ArcGIS/rest/services/geocode/MAIthenDIME_geocode/GeocodeServer/findAddressCandidates?Street="
  suffix <-  "&SingleLine=&outFields=Loc_name&outSR=&f=json"
  dummy <- "error"
  e1 <- "null or bad html returned from geocoder"
  psuedo <- data.frame(dummy,
                       data.frame(dummy, dummy),
                       e1,
                       data.frame(dummy))
  psuedo[, c(1:5)] = apply(psuedo[, c(1:5)], 2, function(x) as.character(x))


  for (i in 1:dim(batch)[1]){
    result <- list()
    result$candidates <- psuedo

    add <- paste(batch[i, fields], collapse="+")
    add <- stringr::str_replace_all(add, "[[:punct:]]", " ") # remove punctuation, except " "
    add <- stringr::str_replace_all(add, "[ ]+", "+") # to make sure all spaces are single "+"
    url <- paste(prefix, add, suffix, sep="")

    html <- RCurl::getURL(url)           # !need to handle the null address field case

    try(result <- jsonlite::fromJSON(html))   # consider using tryCatch to capture particular error message

    if(is.null(result$candidates)){
      result$candidates <- psuedo
    } else if(!is.data.frame(result$candidates)){
      result$candidates <- psuedo
    } else if(!is.list(result)){
      result$candidates <- psuedo
    }

    batch$x[i] <-  result$candidates[1,2][1]
    batch$y[i] <-  result$candidates[1,2][2]
  }

  batch
}
