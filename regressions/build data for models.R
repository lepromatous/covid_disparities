library(RSocrata)
library(sf)
library(rgdal)
library(leaflet)
library(tidyverse)
library(janitor)
library(data.table)
library(vroom)
library(albersusa)
library(Hmisc)
library(spdep)
library(shiny)
library(scales)
library(spatialreg)
library(tidycensus)
library(feather)
sf::sf_use_s2(FALSE)

setwd("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/github/COVID disparities/extra data")
# ==============================================================================
# ==============================================================================
# BASE COVID DATA
# ==============================================================================
# ==============================================================================

#### This pulls latest available date. Will use this later. 
urlz <- paste0("https://data.cdc.gov/resource/8xkx-amqh.json?$select=date&$limit=1")
tokenz<-'chCxsk4zel6QXbaemotF65C9L'
pull(read.socrata(
  urlz,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!")) -> dt



# ==============================================================================
# ==============================================================================
# FUNCTION to be able to subset date easily. 
# ==============================================================================
# ==============================================================================
yo <- function(datez=Sys.Date()-1,complete.sub = 90){

  #datez="2021-06-25"
  #=======================================================================
  # COVID hesitancy data: https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw
  #=======================================================================
  
  hesitancy <- arrow::read_feather("hesitancy.feather")
  
  
  
  
  # ======================================================================
  # COVID data: https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
  # ======================================================================

  # Start by pulling old data - made this previously so its not so slow using only socrata
  # pulled data on 1-19-2022
  covid <- arrow::read_feather("covid.feather")

  # Now pull new data
  # make url for socrata
  urlz <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date>='2022-01-20'"
  tokenz<-'chCxsk4zel6QXbaemotF65C9L'
  
read.socrata(
    urlz,
    app_token = tokenz,
    #####
    email     = "tim.wiemken@gmail.com",
    password  =  "ThisIsNotAGoodP@ssw0rd!!!"
  ) %>%
  tibble()-> covid2 
covid2 <- covid2[,-c(47:51)]
  
  
  # merge these two COVID sets (old and new) and do some cleaning. 
  covid3 <- rbind(covid, covid2)
  covid <- subset(covid3, covid3$date == datez)
  rm(covid2)
  rm(covid3)
  

  
  # ===========================================================================
  # PULL NYT DATA 
  # ===========================================================================
  
  nyt <- vroom::vroom("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  nyt<-nyt[,c(1,4,5,6)]
  nyt$fips <- stringr::str_pad(nyt$fips, pad="0", side="left", width=5)
  library(data.table)
  nyt<-setDT(nyt)
  nyt <- setorder(nyt, date)
  nyt <- nyt[date == datez, .SD, by=c("fips", "date")]

  # COUNTY POPULATIONS OVERALL TO MAKE RATES
  library(vroom)
  pops <- vroom("pop_2019.csv")
  pops$fips <- stringr::str_pad(pops$fips, pad="0", side="left", width=5)
  nyt <- merge(nyt, pops, by="fips")
  nyt$case_rate <- (nyt$cases / nyt$pop)*10000
  nyt$death_rate <- (nyt$deaths / nyt$pop)*10000
  nyt$deaths_per_case <- (nyt$deaths / nyt$cases)*100

  
  
  # ============================================================
  # =========== SOME DATA CLEANING =============================
  # ============================================================
  hesitancy %>%
    clean_names() %>%
    mutate(estimated_hesitant = round(as.numeric(estimated_hesitant)*100,1),
           fips = stringr::str_pad(fips, side="left", pad="0", width=5)) -> hesitancy
  
  covid %>%
    clean_names() %>%
    mutate(across(all_of(names(covid)[c(3, 6:15)]), as.numeric),
           fips = stringr::str_pad(fips, side="left", pad="0", width=5)) -> covid
  
  # # ============================================================
  # # =========== DROP UNMATCHED COUNTIES IN COVID================
  # # ============================================================
  # rmz1<-setdiff(covid$fips, hesitancy$fips)
  # covid <- subset(covid, covid$fips%nin%rmz1)
  # rmz2 <- setdiff(covid$fips, nyt$fips)
  # covid <- subset(covid, covid$fips%nin%rmz2)
  # 
  # # ============================================================
  # # =========== DROP FIPS THAT DONT HAVE DATA PER CDC===========
  # # ============================================================
  # rmz3 <- covid$fips[covid$recip_state %in% c("HI", "TX") | covid$completeness_pct==0]
  
  # ============================================================
  # =========== PULL MAPS AND MERGE ============================
  # ============================================================
  library(albersusa)
  library(sf)
  counties <- counties_sf()
  counties %>%
    clean_names() -> counties
  
  state.map <- usa_sf()
  state.map %>%
    clean_names() %>%
    st_transform(5070) -> state.map
  
  # covid and hesitancy
  df <- merge(covid, hesitancy, by="fips")
  rm(covid)
  rm(hesitancy)
  gc()
  
  df %>%
    group_by(recip_state) %>%
    mutate(mean_state_hesitancy = round(mean(estimated_hesitant, na.rm=T),1),
           covid_uptake_mean_state = round(mean(series_complete_12pluspop_pct, na.rm=T),1)) %>%
    ungroup() -> df
  
  # map with df
  df.sf <- merge(counties, df, by="fips")
  rm(counties)
  rm(df)
  gc()
  
  # case/death with all
  df.sf <- merge(df.sf, nyt, by="fips")
  rm(nyt)
  gc()
  
  
  # ============================================================
  # =========== Merge Census data 
  # ============================================================
  census_demog <- vroom::vroom("census_demog.csv")
  
  df.sf <- merge(df.sf, census_demog, by.x="fips", by.y="GEOID", all.x=T)
  rm(census_demog)
  gc()
  
  # ============================================================
  # =========== Rural urban codes and mod per Prener============
  # ============================================================
  ruca <- vroom::vroom("NCHSURCodes2013.csv")
  ruca %>%
    mutate(
      ., ruca = case_when(
    ruca == 1 ~ "Large Metro, Central",
    ruca == 2 ~ "Large Metro, Fringe",
    ruca %in% c(3,4) ~ "Small Metro",
    ruca %in% c(5,6) ~ "Non-Metro",
    TRUE ~ as.character(ruca))
    )->ruca
  ruca <- ruca[,-3]
  
  df.sf <- merge(df.sf, ruca, by="fips", all.x=T)
  rm(ruca)
  gc()
  
  # ============================================================
  # =========== County Health Rankings, overall and sub ========
  # ============================================================
 vroom::vroom("2021_chr_overall.csv") %>%
    janitor::clean_names() %>%
    filter(!is.na(county))  -> chr_full
  df.sf <- merge(df.sf, chr_full, by="fips")
  rm(chr_full)
  gc()
  
  vroom::vroom("2021_chr_subs.csv") %>%
    janitor::clean_names() %>%
    filter(!is.na(county))  -> chr_sub
  df.sf <- merge(df.sf, chr_sub, by="fips")
  rm(chr_sub)
  gc()

  df.sf$chr_outcome_quartile <- ifelse(df.sf$chr_outcome_quartile=="NR", NA,
                                       ifelse(df.sf$chr_outcome_quartile=="5", NA,
                                              df.sf$chr_outcome_quartile))
  df.sf$chr_factor_quartile <- ifelse(df.sf$chr_factor_quartile=="NR", NA,
                                       ifelse(df.sf$chr_factor_quartile=="5", NA,
                                              df.sf$chr_factor_quartile))
  # =======================================================================
  # County populations by age groups for modeling (offsets)
  # =======================================================================
read_feather("county_pop_age.feather") %>%
    janitor::clean_names() -> county_pops
  df.sf <- merge(df.sf, county_pops, by="fips")
  rm(county_pops)
  gc()
  
  # ============================================================
  # =========== ADD BROADSTREET ADI ============================
  # ============================================================
  adi <- vroom::vroom("adi")
  adi %>%
    filter(year==2020) %>%
    rename(
      fips = 3,
      adi =8
    )  -> adi
  adi <- adi[,c("fips", "adi")]
  df.sf <- merge(df.sf, adi, by="fips", all.x=T)
  
  ### clean the rest
  rm(adi)
  rm(dt)
  rm(tokenz)
  rm(urlz)
  
  prener <- vroom::vroom("county_svi_expanded.csv")
  names(prener) <-tolower(names(prener))
  prener %>%
    rename(
      fips = "geoid"
    ) -> prener
  
  prener$fips[nchar(prener$fips)==4]<-stringr::str_pad(prener$fips[nchar(prener$fips)==4], pad="0", side="left", width=5)
  
  df.sf <- merge(df.sf, prener, by="fips", all.x=T)
  
  
  
  
#   naat <- vroom::vroom("https://s3.amazonaws.com/quartzdata/datasets/test-burden.csv")
#   naat %>%
#     rename(
#       fips = "fips_code"
#     ) %>%
#     mutate(
#       fips = stringr::str_pad(fips, side="left", pad="0", width=5)
#     ) -> naat
#   
# table(df.sf$state.x)  
#   df.sf <- merge(df.sf, naat, by="fips", all.x=T)
  
  ####make HHS region for clustering
  df.sf$hhs_region <- NULL
  df.sf$hhs_region[df.sf$state.x %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont")] <- 1
  df.sf$hhs_region[df.sf$state.x %in% c("New Jersy", "New York", "Puerto Rico", "Virgin Islands")] <- 2
  df.sf$hhs_region[df.sf$state.x %in% c("Delaware", "District of Columbia", "Maryland", "Pennsylvania", "Virginia", "West Virginia")] <- 3
  df.sf$hhs_region[df.sf$state.x %in% c("Alabamaa", "Florida", "Georgia", "Kentucky", "Mississippi", "North Carolina", "South Carolina", "Tennessee")] <- 4
  df.sf$hhs_region[df.sf$state.x %in% c("Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin")] <- 5
  df.sf$hhs_region[df.sf$state.x %in% c("Arkansas", "Louisiana", "New Mexico", "Oklahoma", "Texas")] <- 6
  df.sf$hhs_region[df.sf$state.x %in% c("Iowa", "Kansas", "Missouri", "Nebraska")] <- 7
  df.sf$hhs_region[df.sf$state.x %in% c("Colorado", "Montana", "North D", "South D", "Utah", "Wyoming")] <- 8
  df.sf$hhs_region[df.sf$state.x %in% c("Arizona", "California", "Hawaii", "Nevada")] <- 9
  df.sf$hhs_region[df.sf$state.x %in% c("Alaska", "Idaho", "Oregon", "Washington")] <- 10
  
  
  df.sf$hhs_region <- factor(df.sf$hhs_region, 
                          levels=seq(1:10), 
                          labels = c("Region 1 - Boston",
                                     "Region 2 - New York",
                                     "Region 3 - Philadelphia",
                                     "Region 4 - Atlanta",
                                     "Region 5 - Chicago",
                                     "Region 6 - Dallas",
                                     "Region 7 - Kansas City",
                                     "Region 8 - Denver",
                                     "Region 9 - San Francisco",
                                     "Region 10 - Seattle"))
  
  
  # # Find tertiles
  # vTert = quantile(df.sf$total_test_results_reported_7_day_count_change_per_100K, c(0:3/3), na.rm=T)
  # 
  # # classify values
  # df.sf$naat_tertile = with(df.sf, 
  #                        cut(total_test_results_reported_7_day_count_change_per_100K, 
  #                            vTert, 
  #                            include.lowest = T, 
  #                            labels = c("Low", "Medium", "High")))
  # 
  ### svi tert
  df.sf$social_vulnerability_index <- as.numeric(as.character(df.sf$social_vulnerability_index))
  vTert = quantile(df.sf$social_vulnerability_index, c(0:3/3), na.rm=T)
  
  # classify values
  df.sf$svi_tert = with(df.sf, 
                            cut(social_vulnerability_index, 
                                vTert, 
                                include.lowest = T, 
                                labels = c("Low", "Medium", "High")))
  
  
  pharm <- vroom::vroom("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/extra data/ncpdp_by_fips_cd_11FEB2022.csv")
  pharm %>%
    rename(
      fips = 1,
      pharm_count =2
    ) %>%
    mutate(
      fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
    )-> pharm
  
  df.sf <- merge(df.sf, pharm, by="fips", all.x=T)
  
  df.sf$ncpd <- df.sf$pharm_count/df.sf$pop_18plus*1000

    # =======================================================================
  # END
  # =======================================================================
  return(df.sf) 
}




# =======================================================================
# Example Run
# =======================================================================
#f <- yo(datez="2022-01-08")

