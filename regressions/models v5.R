library(tidyverse)
library(arrow)
library(feather)
#https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation

setwd("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/github/COVID disparities/extra data")

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


df <- merge(pharm, svi, by="fips", all=T)


#### pull covid data
# make url for socrata

# urlz.apr <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2021-04-25'"
# urlz.jun <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2021-06-26'"
# urlz.oct <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2021-10-25'"
# urlz.dec <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2021-12-25'"
# urlz.jan <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date='2022-01-25'"

# urlz <-"https://data.cdc.gov/resource/8xkx-amqh.json"
# 
# tokenz<-'chCxsk4zel6QXbaemotF65C9L'
# 
# read.socrata(
#   urlz,
#   app_token = tokenz,
#   email     = "tim.wiemken@gmail.com",
#   password  =  "ThisIsNotAGoodP@ssw0rd!!!"
# ) %>%
#   tibble()-> covid
# covid %>%
#   mutate(
#     fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
#   )-> covid
# 
# arrow::write_feather(covid[,c("date", "fips", "completeness_pct", "series_complete_18plus", 
#                               "booster_doses_18plus", "census2019_18pluspop")], "~/Desktop/covidvax.feather")
# 
covid <- arrow::read_feather("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/extra data/covidvax.feather")
df <- merge(covid, df, by="fips", all.y=T)

chr <- vroom::vroom("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/extra data/county health 2021.csv")
chr %>%
  mutate(
    fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
  )-> chr
df <- merge(df, chr, by="fips", all.x=T)





df$census2019_18pluspop <- as.numeric(df$census2019_18pluspop)

df$series_complete_18plus <- as.numeric(df$series_complete_18plus)

df$booster_doses_18plus <- as.numeric(df$booster_doses_18plus)

df$pharm_rate <- (as.numeric(df$pharm_count)/df$census2019_18pluspop*100000)/10
### rate per 100k
df$pcp_rate <- df$pcp_rate/10

df <- subset(df, as.numeric(df$completeness_pct)>=80)

df$primary_rate <- df$series_complete_18plus/df$census2019_18pluspop *100000
df$booster_rate <- df$booster_doses_18plus/df$series_complete_18plus *100000

#df.pre <- subset(df, df$date <

df.pre <- subset(df, df$date=="2021-10-01")
df.post <- subset(df, df$date=="2022-02-01")

df.subtractme <- df.pre[,c("fips", "series_complete_18plus")]
df.post <- merge(df.post, df.subtractme, by="fips", suffixes=c("post", "pre"))
df.post$series_complete_18plus <- df.post$series_complete_18pluspost-df.post$series_complete_18pluspre
df.post <- df.post[,names(df.post)%nin%c("series_complete_18pluspost", "series_complete_18pluspre")]
df.post$series_complete_18plus[df.post$series_complete_18plus<0] <-0

df.all <- subset(df, df$date=="2022-02-22")

################################################################################
################################################################################
### Primary series MODELS
################################################################################
################################################################################
### pre
library(rms)
mod_p<-MASS::glm.nb(series_complete_18plus ~ factor(svi_quant) + 
                         pharm_rate +  pcp_rate + pharm_rate*pcp_rate + 
                      offset(log(census2019_18pluspop)), 
                        data=df.pre)

mod_p_clean <- broom::tidy(mod_p)
mod_p_clean$rr <- round(exp(mod_p_clean$estimate),2)
ci_p <- exp(confint(mod_p))
mod_p_clean$ci.lower <- ci_p[,1]
mod_p_clean$ci.upper <- ci_p[,2]
mod_p_clean <- mod_p_clean[-1,c(1,6,7,8,5)]
mod_p_clean %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals =5
  ) -> pre_primary
pre_primary




### POST
mod_p<-MASS::glm.nb(series_complete_18plus ~ factor(svi_quant) + 
                      pharm_rate +  pcp_rate +  pharm_rate*pcp_rate + 
                      offset(log(census2019_18pluspop)), 
                    data=df.post)

mod_p_clean <- broom::tidy(mod_p)
mod_p_clean$rr <- round(exp(mod_p_clean$estimate),2)
ci_p <- exp(confint(mod_p))
mod_p_clean$ci.lower <- ci_p[,1]
mod_p_clean$ci.upper <- ci_p[,2]
mod_p_clean <- mod_p_clean[-1,c(1,6,7,8,5)]
mod_p_clean %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals =5
  ) -> post_primary
post_primary          
               

################################################################################
################################################################################
### BOOSTER MODELS
################################################################################
################################################################################
#### post BOOSTER
df.post <- subset(df.post, !is.na(df.post$booster_doses_18plus))
df.post <- subset(df.post, df.post$series_complete_18plus!=0)
df.post$int <- df.post$pharm_rate*df.post$pcp_rate

## must remove one outlier for model to run.
mod_b<-MASS::glm.nb(booster_doses_18plus ~ factor(svi_quant) + 
                      pharm_rate +  pcp_rate  + int + 
                      offset(log(series_complete_18plus)),
                        data=df.post[df.post$int<500,])

mod_b_clean <- broom::tidy(mod_b)
mod_b_clean$rr <- round(exp(mod_b_clean$estimate),2)
ci_b <- exp(confint(mod_b))
mod_b_clean$ci.lower <- ci_b[,1]
mod_b_clean$ci.upper <- ci_b[,2]
mod_b_clean <- mod_b_clean[-1,c(1,6,7,8,5)]
mod_b_clean %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals =5
  ) -> post_boost
post_boost





#############################################################################
#############################################################################
#############################################################################
### all time - end feb 1 primary series
library(rms)
mod_p<-MASS::glm.nb(series_complete_18plus ~ factor(svi_quant) + 
                      pharm_rate +  pcp_rate + pharm_rate*pcp_rate + 
                      offset(log(census2019_18pluspop)), 
                    data=df.all)

mod_p_clean <- broom::tidy(mod_p)
mod_p_clean$rr <- round(exp(mod_p_clean$estimate),2)
ci_p <- exp(confint(mod_p))
mod_p_clean$ci.lower <- ci_p[,1]
mod_p_clean$ci.upper <- ci_p[,2]
mod_p_clean <- mod_p_clean[-1,c(1,6,7,8,5)]
mod_p_clean %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals =5
  ) -> pre_primary
pre_primary



###### booster
#### post BOOSTER
df.all <- subset(df.all, !is.na(df.all$booster_doses_18plus))
df.all <- subset(df.all, df.all$series_complete_18plus!=0)
df.all$int <- df.all$pharm_rate*df.all$pcp_rate

## must remove one outlier for model to run.
mod_b<-MASS::glm.nb(booster_doses_18plus ~ factor(svi_quant) + 
                      pharm_rate +  pcp_rate  + int + 
                      offset(log(series_complete_18plus)),
                    data=df.all[df.all$int<500,])

mod_b_clean <- broom::tidy(mod_b)
mod_b_clean$rr <- round(exp(mod_b_clean$estimate),2)
ci_b <- exp(confint(mod_b))
mod_b_clean$ci.lower <- ci_b[,1]
mod_b_clean$ci.upper <- ci_b[,2]
mod_b_clean <- mod_b_clean[-1,c(1,6,7,8,5)]
mod_b_clean %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals =5
  ) -> post_boost
post_boost




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


               
               