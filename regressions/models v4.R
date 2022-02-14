library(tidyverse)
library(arrow)
library(feather)


setwd("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/github/COVID disparities/extra data")


#### hesitancy
hesitancy <- arrow::read_feather("hesitancy.feather")
hesitancy %>%
  mutate(
    fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
  )-> hesitancy



### ncpd
pharm <- vroom::vroom("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/extra data/ncpdp_by_fips_cd_11FEB2022.csv")
pharm %>%
  rename(
    fips = 1,
    pharm_count =2
  ) %>%
  mutate(
    fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
  )-> pharm


df <- merge(hesitancy, pharm, by="fips", all=T)

### svi
test <- vroom::vroom("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/extra data/prener.csv")
svi <- test[,c("GEOID", "SVI")]
svi %>%
  rename(
    fips =1,
    svi = 2
  ) -> svi
svi$svi <- as.numeric(svi$svi)
svi$svi_quant <- ifelse(svi$svi<25,1, 
                        ifelse(svi$svi >=25 & svi$svi<50,2,
                               ifelse(svi$svi >=50 & svi$svi<75,3,4)))

svi %>%
  mutate(
    fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
  )-> svi


df <- merge(df, svi, by="fips", all=T)


#### pull covid data
# make url for socrata
library(RSocrata)
urlz.jun <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2021-06-26'"
urlz.dec <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2021-12-25'"
urlz.jan <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2022-01-25'"

tokenz<-'chCxsk4zel6QXbaemotF65C9L'

read.socrata(
  urlz.jun,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!"
  ) %>%
  tibble()-> covid.jun
covid.jun %>%
  mutate(
    fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
  )-> covid.jun


read.socrata(
  urlz.dec,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!"
) %>%
  tibble()-> covid.dec
covid.dec %>%
  mutate(
    fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
  )-> covid.dec

read.socrata(
  urlz.jan,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!"
) %>%
  tibble()-> covid.jan
covid.jan %>%
  mutate(
    fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
  )-> covid.jan

#### nyt jun
nyt <- vroom::vroom("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
nyt<-nyt[,c(1,4,5,6)]
nyt$fips <- stringr::str_pad(nyt$fips, pad="0", side="left", width=5)
# COUNTY POPULATIONS OVERALL TO MAKE RATES
library(vroom)
pops <- vroom("pop_2019.csv")
pops$fips <- stringr::str_pad(pops$fips, pad="0", side="left", width=5)
nyt <- merge(nyt, pops, by="fips")

library(data.table)
nyt<-setDT(nyt)
nyt <- setorder(nyt, date)

nyt.jun <- nyt[date == "2021-06-25", .SD, by=c("fips", "date")]
nyt.dec <- nyt[date == "2021-12-25", .SD, by=c("fips", "date")]
nyt.jan <- nyt[date == "2022-01-25", .SD, by=c("fips", "date")]

nyt.jun$case_rate <- (nyt.jun$cases / nyt.jun$pop)*10000
nyt.jun$death_rate <- (nyt.jun$deaths / nyt.jun$pop)*10000

nyt.dec$case_rate <- (nyt.dec$cases / nyt.dec$pop)*10000
nyt.dec$death_rate <- (nyt.dec$deaths / nyt.dec$pop)*10000

nyt.jan$case_rate <- (nyt.jan$cases / nyt.jan$pop)*10000
nyt.jan$death_rate <- (nyt.jan$deaths / nyt.jan$pop)*10000



df.jun <- merge(covid.jun, df, by="fips", all.x=T)
df.dec <- merge(covid.dec, df, by="fips", all.x=T)
df.jan <- merge(covid.jan, df, by="fips", all.x=T)

df.jun <- merge(df.jun, nyt.jun, by="fips", all.x=T)
df.dec <- merge(df.dec, nyt.dec, by="fips", all.x=T)
df.jan <- merge(df.jan, nyt.jan, by="fips", all.x=T)

df.jun$census2019_18pluspop <- as.numeric(df.jun$census2019_18pluspop)
df.dec$census2019_18pluspop <- as.numeric(df.dec$census2019_18pluspop)
df.jan$census2019_18pluspop <- as.numeric(df.jan$census2019_18pluspop)
df.jun$estimated_hesitant <- as.numeric(df.jun$estimated_hesitant)
df.dec$estimated_hesitant <- as.numeric(df.dec$estimated_hesitant)
df.jan$estimated_hesitant <- as.numeric(df.jan$estimated_hesitant)


mod_p_jan<-MASS::glm.nb(series_complete_18plus ~ factor(svi_quant) + 
               estimated_hesitant +
               death_rate +
               case_rate + pharm_count +  offset(log(census2019_18pluspop)), 
             data=df.jan)

mod_clean_p_jan <- broom::tidy(mod_p_jan)
mod_clean_p_jan$rr <- round(exp(mod_clean_p_jan$estimate),2)
ci_p_jan <- exp(confint(mod_p_jan))
mod_clean_p_jan$ci.lower <- ci_p_jan[,1]
mod_clean_p_jan$ci.upper <- ci_p_jan[,2]
mod_clean_p_jan <- mod_clean_p_jan[-1,c(1,6,7,8,5)]
mod_clean_p_jan %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals =5
  ) -> gt_primary_jan
gt_primary_jan <- gt_primary_jan$`_data`

               
               
               
               
               