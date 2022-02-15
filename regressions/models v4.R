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
urlz.apr <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2021-04-25'"
urlz.jun <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2021-06-26'"
urlz.oct <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2021-10-25'"
urlz.dec <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2021-12-25'"
urlz.jan <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2022-01-25'"

tokenz<-'chCxsk4zel6QXbaemotF65C9L'

read.socrata(
  urlz.apr,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!"
) %>%
  tibble()-> covid.apr
covid.apr %>%
  mutate(
    fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
  )-> covid.apr


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
  urlz.oct,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!"
) %>%
  tibble()-> covid.oct
covid.oct %>%
  mutate(
    fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
  )-> covid.oct

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

nyt.apr <- nyt[date == "2021-04-25", .SD, by=c("fips", "date")]
nyt.jun <- nyt[date == "2021-06-25", .SD, by=c("fips", "date")]
nyt.oct <- nyt[date == "2021-10-25", .SD, by=c("fips", "date")]
nyt.dec <- nyt[date == "2021-12-25", .SD, by=c("fips", "date")]
nyt.jan <- nyt[date == "2022-01-25", .SD, by=c("fips", "date")]

nyt.apr$case_rate <- (nyt.apr$cases / nyt.apr$pop)*10000
nyt.apr$death_rate <- (nyt.apr$deaths / nyt.apr$pop)*10000

nyt.jun$case_rate <- (nyt.jun$cases / nyt.jun$pop)*10000
nyt.jun$death_rate <- (nyt.jun$deaths / nyt.jun$pop)*10000

nyt.oct$case_rate <- (nyt.oct$cases / nyt.oct$pop)*10000
nyt.oct$death_rate <- (nyt.oct$deaths / nyt.oct$pop)*10000

nyt.dec$case_rate <- (nyt.dec$cases / nyt.dec$pop)*10000
nyt.dec$death_rate <- (nyt.dec$deaths / nyt.dec$pop)*10000

nyt.jan$case_rate <- (nyt.jan$cases / nyt.jan$pop)*10000
nyt.jan$death_rate <- (nyt.jan$deaths / nyt.jan$pop)*10000


df.apr <- merge(covid.apr, df, by="fips", all.x=T)
df.jun <- merge(covid.jun, df, by="fips", all.x=T)
df.oct <- merge(covid.oct, df, by="fips", all.x=T)
df.dec <- merge(covid.dec, df, by="fips", all.x=T)
df.jan <- merge(covid.jan, df, by="fips", all.x=T)

df.apr <- merge(df.apr, nyt.apr, by="fips", all.x=T)
df.jun <- merge(df.jun, nyt.jun, by="fips", all.x=T)
df.oct <- merge(df.oct, nyt.oct, by="fips", all.x=T)
df.dec <- merge(df.dec, nyt.dec, by="fips", all.x=T)
df.jan <- merge(df.jan, nyt.jan, by="fips", all.x=T)

df.apr$census2019_18pluspop <- as.numeric(df.apr$census2019_18pluspop)
df.jun$census2019_18pluspop <- as.numeric(df.jun$census2019_18pluspop)
df.oct$census2019_18pluspop <- as.numeric(df.oct$census2019_18pluspop)
df.dec$census2019_18pluspop <- as.numeric(df.dec$census2019_18pluspop)
df.jan$census2019_18pluspop <- as.numeric(df.jan$census2019_18pluspop)

df.apr$series_complete_18plus <- as.numeric(df.apr$series_complete_18plus)
df.jun$series_complete_18plus <- as.numeric(df.jun$series_complete_18plus)
df.oct$series_complete_18plus <- as.numeric(df.oct$series_complete_18plus)
df.dec$series_complete_18plus <- as.numeric(df.dec$series_complete_18plus)
df.jan$series_complete_18plus <- as.numeric(df.jan$series_complete_18plus)

#df.jun$booster_doses_18plus <- as.numeric(df.jun$booster_doses_18plus)
df.dec$booster_doses_18plus <- as.numeric(df.dec$booster_doses_18plus)
df.jan$booster_doses_18plus <- as.numeric(df.jan$booster_doses_18plus)

df.apr$estimated_hesitant5 <- (as.numeric(df.apr$estimated_hesitant)*100)/5
df.jun$estimated_hesitant5 <- (as.numeric(df.jun$estimated_hesitant)*100)/5
df.oct$estimated_hesitant5 <- (as.numeric(df.oct$estimated_hesitant)*100)/5
df.dec$estimated_hesitant5 <- (as.numeric(df.dec$estimated_hesitant)*100)/5
df.jan$estimated_hesitant5 <- (as.numeric(df.jan$estimated_hesitant)*100)/5

df.apr$case_rate1000 <- as.numeric(df.apr$case_rate)/1000
df.jun$case_rate1000 <- as.numeric(df.jun$case_rate)/1000
df.oct$case_rate1000 <- as.numeric(df.oct$case_rate)/1000
df.dec$case_rate1000 <- as.numeric(df.dec$case_rate)/1000
df.jan$case_rate1000 <- as.numeric(df.jan$case_rate)/1000

df.apr$death_rate10 <- as.numeric(df.apr$death_rate)/10
df.jun$death_rate10 <- as.numeric(df.jun$death_rate)/10
df.oct$death_rate10 <- as.numeric(df.oct$death_rate)/10
df.dec$death_rate10 <- as.numeric(df.dec$death_rate)/10
df.jan$death_rate10 <- as.numeric(df.jan$death_rate)/10

df.apr$pharm_rate <- as.numeric(df.apr$pharm_count)/df.apr$census2019_18pluspop*1000
df.jun$pharm_rate <- as.numeric(df.jun$pharm_count)/df.jun$census2019_18pluspop*1000
df.oct$pharm_rate <- as.numeric(df.oct$pharm_count)/df.oct$census2019_18pluspop*1000
df.dec$pharm_rate <- as.numeric(df.dec$pharm_count)/df.dec$census2019_18pluspop*1000
df.jan$pharm_rate <- as.numeric(df.jan$pharm_count)/df.jan$census2019_18pluspop*1000

df.apr <- subset(df.jun, as.numeric(df.apr$completeness_pct)>=90)
df.jun <- subset(df.jun, as.numeric(df.jun$completeness_pct)>=90)
df.oct <- subset(df.jun, as.numeric(df.oct$completeness_pct)>=90)
df.dec <- subset(df.dec, as.numeric(df.dec$completeness_pct)>=90)
df.jan <- subset(df.jan, as.numeric(df.jan$completeness_pct)>=90)





################################################################################
################################################################################
### Primay series MODELS
################################################################################
################################################################################
### apr

mod_p_apr<-MASS::glm.nb(series_complete_18plus ~ factor(svi_quant) + 
                          estimated_hesitant5 +
                          death_rate10 +
                          case_rate1000 + pharm_rate +  offset(log(census2019_18pluspop)), 
                        data=df.apr)

mod_clean_p_apr <- broom::tidy(mod_p_apr)
mod_clean_p_apr$rr <- round(exp(mod_clean_p_apr$estimate),2)
ci_p_apr <- exp(confint(mod_p_apr))
mod_clean_p_apr$ci.lower <- ci_p_apr[,1]
mod_clean_p_apr$ci.upper <- ci_p_apr[,2]
mod_clean_p_apr <- mod_clean_p_apr[-1,c(1,6,7,8,5)]
mod_clean_p_apr %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals =5
  ) -> gt_primary_apr
gt_primary_apr <- gt_primary_apr$`_data`
gt_primary_apr

### jun

mod_p_jun<-MASS::glm.nb(series_complete_18plus ~ factor(svi_quant) + 
                          estimated_hesitant5 +
                          death_rate10 +
                          case_rate1000 + pharm_rate +  offset(log(census2019_18pluspop)), 
                        data=df.jun)

mod_clean_p_jun <- broom::tidy(mod_p_jun)
mod_clean_p_jun$rr <- round(exp(mod_clean_p_jun$estimate),2)
ci_p_jun <- exp(confint(mod_p_jun))
mod_clean_p_jun$ci.lower <- ci_p_jun[,1]
mod_clean_p_jun$ci.upper <- ci_p_jun[,2]
mod_clean_p_jun <- mod_clean_p_jun[-1,c(1,6,7,8,5)]
mod_clean_p_jun %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals =5
  ) -> gt_primary_jun
gt_primary_jun <- gt_primary_jun$`_data`
gt_primary_jun



### oct

mod_p_oct<-MASS::glm.nb(series_complete_18plus ~ factor(svi_quant) + 
                          estimated_hesitant5 +
                          death_rate10 +
                          case_rate1000 + pharm_rate +  offset(log(census2019_18pluspop)), 
                        data=df.oct)

mod_clean_p_oct <- broom::tidy(mod_p_oct)
mod_clean_p_oct$rr <- round(exp(mod_clean_p_oct$estimate),2)
ci_p_oct <- exp(confint(mod_p_oct))
mod_clean_p_oct$ci.lower <- ci_p_oct[,1]
mod_clean_p_oct$ci.upper <- ci_p_oct[,2]
mod_clean_p_oct <- mod_clean_p_oct[-1,c(1,6,7,8,5)]
mod_clean_p_oct %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals =5
  ) -> gt_primary_oct
gt_primary_oct <- gt_primary_oct$`_data`
gt_primary_oct

### dec


mod_p_dec<-MASS::glm.nb(series_complete_18plus ~ factor(svi_quant) + 
                          estimated_hesitant5 +
                          death_rate10 +
                          case_rate1000 + pharm_rate + offset(log(census2019_18pluspop)), 
                        data=df.dec)

mod_clean_p_dec <- broom::tidy(mod_p_dec)
mod_clean_p_dec$rr <- round(exp(mod_clean_p_dec$estimate),2)
ci_p_dec <- exp(confint(mod_p_dec))
mod_clean_p_dec$ci.lower <- ci_p_dec[,1]
mod_clean_p_dec$ci.upper <- ci_p_dec[,2]
mod_clean_p_dec <- mod_clean_p_dec[-1,c(1,6,7,8,5)]
mod_clean_p_dec %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals =5
  ) ->  gt_primary_dec
gt_primary_dec <- gt_primary_dec$`_data`
gt_primary_dec


### jan


mod_p_jan<-MASS::glm.nb(series_complete_18plus ~ factor(svi_quant) + 
               estimated_hesitant5 +
               death_rate10 +
               case_rate1000 + pharm_rate +  offset(log(census2019_18pluspop)), 
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
gt_primary_jan
               
               


################################################################################
################################################################################
### BOOSTER MODELS
################################################################################
################################################################################
### MUST REMOVE ZEROS FROM DENOM
df.dec_boost <- subset(df.dec, df.dec$series_complete_18plus!=0)
df.jan_boost <- subset(df.jan, df.jan$series_complete_18plus!=0)



mod_b_dec<-MASS::glm.nb(booster_doses_18plus ~ factor(svi_quant) + 
                          estimated_hesitant5 +
                          death_rate10 +
                          case_rate1000 + pharm_rate +  offset(log(series_complete_18plus)), 
                        data=df.dec_boost)

mod_clean_p_dec <- broom::tidy(mod_b_dec)
mod_clean_p_dec$rr <- round(exp(mod_clean_p_dec$estimate),2)
ci_p_dec <- exp(confint(mod_b_dec))
mod_clean_p_dec$ci.lower <- ci_p_dec[,1]
mod_clean_p_dec$ci.upper <- ci_p_dec[,2]
mod_clean_p_dec <- mod_clean_p_dec[-1,c(1,6,7,8,5)]
mod_clean_p_dec %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals =5
  ) -> gt_boost_dec
gt_boost_dec <- gt_boost_dec$`_data`
gt_boost_dec


#### JAN BOOSTER

mod_b_jan<-MASS::glm.nb(booster_doses_18plus ~ factor(svi_quant) + 
                          estimated_hesitant5 +
                          death_rate10 +
                          case_rate1000 + pharm_rate +  offset(log(series_complete_18plus)), 
                        data=df.jan_boost)

mod_clean_p_jan <- broom::tidy(mod_b_jan)
mod_clean_p_jan$rr <- round(exp(mod_clean_p_jan$estimate),2)
ci_p_jan <- exp(confint(mod_b_jan))
mod_clean_p_jan$ci.lower <- ci_p_jan[,1]
mod_clean_p_jan$ci.upper <- ci_p_jan[,2]
mod_clean_p_jan <- mod_clean_p_jan[-1,c(1,6,7,8,5)]
mod_clean_p_jan %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals =5
  ) -> gt_boost_jan
gt_boost_jan <- gt_boost_jan$`_data`
gt_boost_jan



#################################################################################
##################################################################################
library(sf)
library(sp)
library(spdep)

# prior to this, you'll have fit a model you can test, which I'll call model below
# you also need an sf object of your county-level data, which I'll call df_sf below
counties <- tigris::counties(cb=T)

#### make DF.  change df. jan to whatever.  remove unknown fips
df_sf <- merge(counties, df.jan, by.x="GEOID", by.y="fips", all.y=T)
df_sf <- subset(df_sf,df_sf$GEOID!="00UNK")
df_sf$rate <- df_sf$booster_doses_18plus/df_sf$series_complete_18plus *100000


# convert to sp object
df_sp <- as_Spatial(df_sf)

# calculate spatial weights matrix
queens <- poly2nb(df_sp, queen = TRUE)
weights <- nb2listw(queens, style="W", zero.policy = TRUE)

# test residuals
lm.morantest(mod_p_jan, weights, alternative="two.sided", zero.policy = TRUE)

# spatial diagnostics
lm.LMtests(mod_p_jan, weights, test = "all", zero.policy = TRUE)

### ?everything is significant so fit lag and error models
#rm(list=ls()[! ls() %in% c("df_sp","weights")])


library(spatialreg)
out.lag <- spatialreg::lagsarlm(rate ~ factor(svi_quant) + 
                                  estimated_hesitant5 +
                                  death_rate10 +
                                  case_rate1000 +
                                  pharm_rate,
                                data=df_sp@data,
                                list = weights,
                                zero.policy = T)
impacts(out.lag, listw = weights)

out.error <- spatialreg::errorsarlm(rate ~ factor(svi_quant) + 
                                      estimated_hesitant +
                                      death_rate +
                                      case_rate +
                                      pharm_rate,
                                    data=df_sp@data,
                                    list = weights, 
                                    zero.policy=T)

summary(out.lag)
summary(out.error)


               
               