library(gt)
library(vroom)
library(tidyverse)


setwd("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/covid_disparities/main data/")
df.primary.dec <- vroom::vroom("primary18_dec25.csv")
df.primary.dec$social_vulnerability_index5 <- df.primary.dec$social_vulnerability_index/5
df.primary.jan <- vroom::vroom("primary18_jan25.csv")
df.primary.jan$social_vulnerability_index5 <- df.primary.jan$social_vulnerability_index/5
df.boost.dec <- vroom::vroom("boost18_dec25.csv")
df.boost.dec$social_vulnerability_index5 <- df.boost.dec$social_vulnerability_index/5
df.boost.jan <- vroom::vroom("boost18_jan25.csv")
df.boost.jan$social_vulnerability_index5 <- df.boost.jan$social_vulnerability_index/5









##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### MODEL 1: Primary Series, December 2021
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
mod_p_dec <- MASS::glm.nb(series_complete_18plus ~ social_vulnerability_index + 
                  estimated_hesitant +
                  death_rate_100k +
                  case_rate_100k + 
                    #factor(naat_tertile) +
                  offset(log(pop_18plus)), 
                data=df.primary.dec)
    
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
    decimals=5
  ) -> gt_primary_dec
gt_primary_dec <- gt_primary_dec$`_data`

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### MODEL 2: Booster, December 2021
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

mod_b_dec <- MASS::glm.nb(booster_doses_18plus ~ social_vulnerability_index + 
                            estimated_hesitant +
                            death_rate_100k +
                            case_rate_100k + 
                            #factor(naat_tertile) +
                            offset(log(series_complete_18plus)), 
                          data=df.boost.dec)

mod_clean_b_dec <- broom::tidy(mod_b_dec)
mod_clean_b_dec$rr <- round(exp(mod_clean_b_dec$estimate),2)
ci_b_dec <- exp(confint(mod_b_dec))
mod_clean_b_dec$ci.lower <- ci_b_dec[,1]
mod_clean_b_dec$ci.upper <- ci_b_dec[,2]
mod_clean_b_dec <- mod_clean_b_dec[-1,c(1,6,7,8,5)]
mod_clean_b_dec %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals=5
  )-> gt_boost_dec
gt_boost_dec <- gt_boost_dec$`_data`






##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### MODEL 3: Primary Series, January 2022
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
mod_p_jan <- MASS::glm.nb(series_complete_18plus ~ social_vulnerability_index + 
                            estimated_hesitant +
                            death_rate_100k +
                            case_rate_100k + 
                            factor(naat_tertile) +
                            offset(log(pop_18plus)), 
                          data=df.primary.jan)

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


##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### MODEL 4: Booster, January 2022
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

mod_b_jan <- MASS::glm.nb(booster_doses_18plus ~ social_vulnerability_index + 
                            estimated_hesitant +
                            death_rate_100k +
                            case_rate_100k + 
                            factor(naat_tertile) +
                            offset(log(series_complete_18plus)), 
                          data=df.boost.jan)

mod_clean_b_jan <- broom::tidy(mod_b_jan)
mod_clean_b_jan$rr <- round(exp(mod_clean_b_jan$estimate),2)
ci_b_jan <- exp(confint(mod_b_jan))
mod_clean_b_jan$ci.lower <- ci_b_jan[,1]
mod_clean_b_jan$ci.upper <- ci_b_jan[,2]
mod_clean_b_jan <- mod_clean_b_jan[-1,c(1,6,7,8,5)]
mod_clean_b_jan %>%
  gt::gt() %>%
  gt::fmt_number(
    columns = 2:5, 
    decimals=5
  ) -> gt_boost_jan
gt_boost_jan <- gt_boost_jan$`_data`













##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### MODEL - SPATIAL CLUSTERING - JANUARY DATA
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
df.boost.jan$naat_tertile <- factor(df.boost.jan$naat_tertile)
df.sp.jan <- df.boost.jan[,c("fips", "social_vulnerability_index", 
                             "estimated_hesitant", "death_rate_100k", 
                             "case_rate_100k", "naat_tertile",
                             "series_complete_18plus", "booster_doses_18plus", "svi_tert")]

### need to create rate for outcome b/c ount outcomes in lag models dont work
df.sp.jan$rate <- df.sp.jan$booster_doses_18plus/df.sp.jan$series_complete_18plus *100000
mod_b_jan2 <- lm(rate ~ social_vulnerability_index + 
                            estimated_hesitant +
                            death_rate_100k +
                            case_rate_100k + 
                            factor(naat_tertile),
                          data=df.sp.jan)
library(sf)
library(sp)
library(spdep)

# prior to this, you'll have fit a model you can test, which I'll call model below
# you also need an sf object of your county-level data, which I'll call df_sf below
counties <- tigris::counties(cb=T)
df_sf <- merge(counties, df.sp.jan, by.x="GEOID", by.y="fips", all.y=T)

# convert to sp object
df_sp <- as_Spatial(df_sf)

# calculate spatial weights matrix
queens <- poly2nb(df_sp, queen = TRUE)
weights <- nb2listw(queens, style="W", zero.policy = TRUE)

# test residuals
lm.morantest(mod_b_jan2, weights, alternative="two.sided", zero.policy = TRUE)

# spatial diagnostics
lm.LMtests(mod_b_jan2, weights, test = "all", zero.policy = TRUE)

### everythign is significant so fit lag and error models

library(spatialreg)
out.lag <- spatialreg::lagsarlm(rate ~ factor(svi_tert) + 
                                  estimated_hesitant +
                                  death_rate_100k +
                                  case_rate_100k + 
                                  factor(naat_tertile),
                                data=df_sp@data,
                                list = weights,
                                zero.policy = T)
out.error <- spatialreg::errorsarlm(rate ~ factor(svi_tert) + 
                                  estimated_hesitant +
                                  death_rate_100k +
                                  case_rate_100k + 
                                  factor(naat_tertile),
                                data=df_sp@data,
                                list = weights, 
                                zero.policy=T)

summary(out.lag)
summary(out.error)



     