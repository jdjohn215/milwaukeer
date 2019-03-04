#' Retrieve a specified geography as a simple features object
#'
#' \code{get_geography} returns a data.frame containing the complete WIBRS crime data
#'   for the requested time period (if specified) and the selected geography
#'   (if specified).
#'
#'  Refer to the linked descriptions for more information:
#'
#' @param name The name of the desired geography. Options are:
#' \describe{
#' \item{police.districts}{\href{https://data.milwaukee.gov/dataset/milwaukee-police-district}{Milwaukee Police Department districts}}
#' \item{mpd.squad.areas}{\href{https://data.milwaukee.gov/dataset/milwaukee-police-department-squad-areas}{Milwaukee Police Department squad areas}}
#' \item{neighborhoods}{the shapes of neighborhoods defined by the \href{https://data.milwaukee.gov/dataset/neighborhoods}{Department of City Development}}
#' \item{mpd.reporting.districts}{\href{https://data.milwaukee.gov/dataset/milwaukee-police-department-reporting-districts}{Milwaukee Police Department reporting units}}
#' \item{voting.wards}{\href{https://data.milwaukee.gov/dataset/voting-wards}{Voting wards}}
#' \item{redevelopment.plans}{\href{https://data.milwaukee.gov/dataset/redevelopment-plans}{redevelopment plans}}
#' \item{snow.plowing.areas}{\href{https://data.milwaukee.gov/dataset/snow-removal}{Snow removal routes and plowing areas}}
#' \item{streetcar.stops}{\href{https://data.milwaukee.gov/dataset/street-car-stops}{Locations of streetcar stops}}
#' \item{city.maintained.land}{\href{https://data.milwaukee.gov/dataset/city-maintained-parcels}{Parcel outlines of city-owned properties}}
#' \item{garbage.routes}{\href{https://data.milwaukee.gov/dataset/garbage-recycling-collection}{Garbage routes}}
#' \item{recycling.summer.routes}{\href{https://data.milwaukee.gov/dataset/garbage-recycling-collection}{Summer recycling routes}}
#' \item{recycling.winter.routes}{\href{https://data.milwaukee.gov/dataset/garbage-recycling-collection}{Winter recycling routes}}
#' \item{sanitation.districts}{\href{https://data.milwaukee.gov/dataset/garbage-recycling-collection}{Sanitation districts}}
#' \item{street.sweeping}{\href{https://data.milwaukee.gov/dataset/street-sweeping}{Street sweeping routes}}
#' \item{bids}{\href{https://data.milwaukee.gov/dataset/business-improvement-districts-bid}{Business Improvement Districts}}
#' \item{strategic.action.plans}{\href{https://data.milwaukee.gov/dataset/strategic-action-plans}{Strategic action plans}}
#' \item{adult.family.homes}{\href{https://data.milwaukee.gov/dataset/community-based-residential-facilities-within-city-of-milwaukee}{Adult family homes}}
#' \item{leaf.collection}{\href{https://data.milwaukee.gov/dataset/leaf-collection}{Leaf collection areas}}
#' \item{aldermanic.districts}{\href{https://data.milwaukee.gov/dataset/aldermanic-districts}{Outlines of aldermanic districts}}
#' \item{child.group.homes}{\href{https://data.milwaukee.gov/dataset/child-residential-facilities-within-city-of-milwaukee}{Locations of child residential facilities}}
#' \item{liquor.licenses}{\href{https://data.milwaukee.gov/dataset/alcohol-licenses}{Parcel centroids associated with liquor licenses}}
#' \item{schools}{\href{https://data.milwaukee.gov/dataset/schools-elementary-secondary}{Parcels identified as elementary or secondary schools}}
#'
#' }
#'
#' Defaults to first date available.
#' @return A dataframe of class sf.
#' @export
#' @importFrom sf st_read
#'
#' @examples
#' get_geography(name = "neighborhoods")

get_geography <- function(name){
  if(name == "street.sweeping"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/DPW/DPW_sanitation/MapServer/19/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "police.districts"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/MPD/MPD_geography/MapServer/2/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "street.sweeping"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/DPW/DPW_sanitation/MapServer/19/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "voting.wards"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/election/election_geography/MapServer/2/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "snow.plowing.areas"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/DPW/DPW_sanitation/MapServer/15/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "leaf.collection"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/DPW/DPW_sanitation/MapServer/17/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "streetcar.stops"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/DPW/DPW_streetcar/MapServer/0/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "mpd.squad.areas"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/MPD/MPD_geography/MapServer/1/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "mpd.reporting.districts"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/MPD/MPD_geography/MapServer/0/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "bids"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/planning/special_districts/MapServer/3/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "bids"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/planning/special_districts/MapServer/3/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "strategic.action.plans"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/planning/special_districts/MapServer/7/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "adult.family.homes"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/regulation/CLA/MapServer/1/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "garbage.routes"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/DPW/DPW_sanitation/MapServer/9/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "recycling.summer.routes"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/DPW/DPW_sanitation/MapServer/11/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "recycling.winter.routes"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/DPW/DPW_sanitation/MapServer/12/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "sanitation.districts"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/DPW/DPW_sanitation/MapServer/0/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "neighborhoods"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/planning/special_districts/MapServer/4/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "redevelopment.plans"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/planning/special_districts/MapServer/6/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "city.sidewalks"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/DPW/DPW_operations/MapServer/0/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "city.maintained.land"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/DPW/DPW_operations/MapServer/1/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "shelter.care"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/regulation/CLA/MapServer/2/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "child.group.homes"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/regulation/CLA/MapServer/3/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "aldermanic.districts"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/election/alderman/MapServer/3/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "targeted.investment.neighborhoods"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/planning/special_districts/MapServer/10/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "liquor.licenses"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/regulation/license/MapServer/1/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }
  if(name == "schools"){
    sf.output <- st_read('https://maps2.milwaukee.gov/arcgis/rest/services/property/parcels_mprop/MapServer/20/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
  }

  sf.output
}
