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

#setwd("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/covidvax")

#tidycensus::census_api_key("1bea7542b64a438650b457bd6609c1d7bd75cbaa", install=T)
### pull current date for covid data##
urlz <- paste0("https://data.cdc.gov/resource/8xkx-amqh.json?$select=date&$limit=1")
tokenz<-'chCxsk4zel6QXbaemotF65C9L'
pull(read.socrata(
  urlz,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!")) -> dt



#### begin function
yo <- function(datez=Sys.Date()-1,complete.sub = 90){
  sf::sf_use_s2(FALSE)
  #=======================================================================
  # hesitancy
  # COVID hesitancy data: https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw

  hesitancy <- arrow::read_feather("hesitancy.feather")
  
  # ======================================================================
  # COVID data: https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
  # ======================================================================
  #####
  
  # df1 <- read.fst("df1.fst")
  # df2 <- read.fst("df2.fst")
  # df3 <- read.fst("df3.fst")
  # 
  covid <- arrow::read_feather("covid.feather")
  # rm(df1)
  # rm(df2)
  # rm(df3)
  
  ## make url for socrata
  urlz <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date>='2022-01-20'"
  tokenz<-'chCxsk4zel6QXbaemotF65C9L'
  
  covid2 <- read.socrata(
    urlz,
    app_token = tokenz,
    #####
    email     = "tim.wiemken@gmail.com",
    password  =  "ThisIsNotAGoodP@ssw0rd!!!"
  )
  
  #write.csv(covid, "~/Desktop/covid.csv", row.names=F, na="")
  
  # 
  covid3 <- rbind(covid, covid2)
  covid <- subset(covid3, covid3$date == datez)
  rm(covid2)
  rm(covid3)
  
  if(nrow(covid)==0){stop("Data not avaiable for date selected, please choose an earlier date.")}
  
  # ============================================================
  # PULL NYT DATA ==============================================
  # ============================================================
  
  covid_cases_deaths <- vroom("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  covid_cases_deaths<-covid_cases_deaths[,c(1,4,5,6)]
  covid_cases_deaths$fips <- stringr::str_pad(covid_cases_deaths$fips, pad="0", side="left", width=5)
  
  covid_cases_deaths<-setDT(covid_cases_deaths)
  covid_cases_deaths <- setorder(covid_cases_deaths, date)
  covid_cases_deaths <- covid_cases_deaths[date == datez, .SD, by=c("fips", "date")]
  #pops <- vroom::vroom("/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/covidvax/pop_2019.csv")
  pops <- vroom("pop_2019.csv")
  pops$fips <- stringr::str_pad(pops$fips, pad="0", side="left", width=5)
  covid_cases_deaths <- merge(covid_cases_deaths, pops, by="fips")
  covid_cases_deaths$cases <- (covid_cases_deaths$cases / covid_cases_deaths$pop)*10000
  covid_cases_deaths$deaths <- (covid_cases_deaths$deaths / covid_cases_deaths$pop)*10000
  covid_cases_deaths$deaths_per_case <- (covid_cases_deaths$deaths / covid_cases_deaths$cases)*100
  covid_cases_deaths<-covid_cases_deaths
  
  
  # ============================================================
  # 2019 County Population Tidycensus 8/5/2021==================
  # ============================================================
  
  
  
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
  
  # ============================================================
  # =========== DROP UNMATCHED COUNTIES IN COVID================
  # ============================================================
  rmz1<-setdiff(covid$fips, hesitancy$fips)
  covid <- subset(covid, covid$fips%nin%rmz1)
  rmz2 <- setdiff(covid$fips, covid_cases_deaths$fips)
  covid <- subset(covid, covid$fips%nin%rmz2)
  
  # ============================================================
  # =========== DROP FIPS THAT DONT HAVE DATA PER CDC===========
  # ============================================================
  rmz3 <- covid$fips[covid$recip_state %in% c("HI", "TX") | covid$completeness_pct==0]
  
  # ============================================================
  # =========== PULL MAPS AND MERGE ============================
  # ============================================================
  counties <- counties_sf()
  counties %>%
    clean_names() -> counties
  
  state.map <- usa_sf()
  state.map %>%
    clean_names() %>%
    st_transform(5070) -> state.map
  
  # covid and hesitancy
  df <- merge(covid, hesitancy, by="fips")
  
  df %>%
    group_by(recip_state) %>%
    mutate(mean_state_hesitancy = round(mean(estimated_hesitant, na.rm=T),1),
           covid_uptake_mean_state = round(mean(series_complete_12pluspop_pct, na.rm=T),1)) %>%
    ungroup() -> df
  
  # map with df
  df.sf <- merge(counties, df, by="fips")
  
  # case/death with all
  df.sf <- merge(df.sf, covid_cases_deaths, by="fips")
  
  # ============================================================
  # =========== Merge Census data ==============================
  # ============================================================
  #census_demog <- vroom::vroom("/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/covidvax/census_demog.csv")
  
  census_demog <- vroom::vroom("census_demog.csv")
  
  df.sf <- merge(df.sf, census_demog, by.x="fips", by.y="GEOID", all.x=T)
  
  
  # ============================================================
  # =========== Missing FIPS for maps  =========================
  # ============================================================
  df.missing.fips <- subset(counties, counties$fips%in%c(rmz1, rmz2, rmz3))
  df.subs <- subset(df.sf, df.sf$completeness_pct<complete.sub)
  df.missing.fips2 <- subset(counties, counties$fips%in%df.subs$fips)
  df.missing.fips <- rbind(df.missing.fips2, df.missing.fips)
  #df.missing.fips3 <- subset(df.sf, is.na(df.sf[,xvar]) | is.na(df.sf[,yvar]))
  df.missing.fips <- df.missing.fips[,names(df.missing.fips)]

  # ============================================================
  # =========== Non MISSING FIPS for ANALYSIS =========================
  # ============================================================
  df.sf <- df.sf[df.sf$fips %nin% df.missing.fips$fips,]
  
  ruca <- vroom::vroom("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/covidvax/regressions/other data/NCHSURCodes2013.csv")
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
  
  return(df.sf) 
}


df <- yo(datez="2022-01-08")

