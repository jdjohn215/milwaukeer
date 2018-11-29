# Get MPROP data
## right now this function just grabs 2016-2018
## in the future, build in an ability to grab historical files
## which are in difficult to unzip file formats like zipped xlsx

get_mprop <- function(start_year, end_year) {
  ckanr_setup(url = "https://data.milwaukee.gov")
  start <- Sys.time()
  years <- start_year:end_year
  df.list <- list()
  if(is.element(2016, years)){
    res16 <- resource_show(id = "5f67a8e0-7ce9-4b47-b72e-2a7fe0753f3f", as = "table")
    raw16 <- fetch(res16$url) %>%
      mutate_all(as.character) %>%
      mutate(year = 2016)
    df.list[[length(df.list)+1]] <- raw16
  }
  if(is.element(2017, years)){
    res17 <- resource_show(id = "b0dfa9e3-27d6-42f5-8614-52ac0485f7d5", as = "table")
    raw17 <- fetch(res17$url) %>%
      mutate_all(as.character) %>%
      rename(TAX_RATE_CD = TAX_RATE_C,
             HOUSE_NR_LO = HOUSE_NR_L,
             HOUSE_NR_HI = HOUSE_NR_H,
             HOUSE_NR_SFX = HOUSE_NR_S,
             C_A_EXM_TYPE = C_A_EXM_TY,
             C_A_EXM_LAND = C_A_EXM_LA,
             C_A_EXM_IMPRV = C_A_EXM_IM,
             C_A_EXM_TOTAL = C_A_EXM_TO,
             P_A_EXM_TYPE = P_A_EXM_TY,
             P_A_EXM_LAND = P_A_EXM_LA,
             P_A_EXM_IMPRV = P_A_EXM_IM,
             P_A_EXM_TOTAL = P_A_EXM_TO,
             LAST_VALUE_CHG = LAST_VALUE,
             REASON_FOR_CHG = REASON_FOR,
             CONVEY_DATE = CONVEY_DAT,
             CONVEY_TYPE = CONVEY_TYP,
             OWNER_NAME_1 = OWNER_NAME,
             OWNER_NAME_2 = OWNER_NA_1,
             OWNER_NAME_3 = OWNER_NA_2,
             OWNER_MAIL_ADDR = OWNER_MAIL,
             OWNER_CITY_STATE = OWNER_CITY,
             LAST_NAME_CHG = LAST_NAME_,
             NEIGHBORHOOD = NEIGHBORHO,
             EXM_ACREAGE = EXM_ACREAG,
             EXM_PER_CT_LAND = EXM_PER_CT,
             EXM_PER_CT_IMPRV = EXM_PER__1,
             AIR_CONDITIONING = AIR_CONDIT,
             POWDER_ROOMS = POWDER_ROO,
             PARKING_TYPE = PARKING_TY,
             LAND_USE_GP = LAND_USE_G,
             GEO_ZIP_CODE = GEO_ZIP_CO,
             GEO_ALDER_OLD = GEO_ALDER_,
             GEO_BI_MAINT = GEO_BI_MAI,
             DPW_SANITATION = DPW_SANITA,
             RAZE_STATUS = RAZE_STATU) %>%
      mutate(year = 2017)
    df.list[[length(df.list)+1]] <- raw17
  }
  if(is.element(2018, years)){
    res18 <- resource_show(id = "0a2c7f31-cd15-4151-8222-09dd57d5f16d", as = "table")
    raw18 <- fetch(res18$url) %>%
      mutate_all(as.character) %>%
      mutate(year = 2018)
    df.list[[length(df.list)+1]] <- raw18
  }
  raw <- bind_rows(df.list)
  end <- Sys.time()
  fetchTime <- difftime(end, start, units = "secs")
  print(paste("Download time:", round(fetchTime, 2), "seconds."))
  raw
}
