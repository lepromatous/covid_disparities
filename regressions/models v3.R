library(gt)
library(vroom)


setwd("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/covid_disparities/main data/")
df.primary.dec <- vroom::vroom("primary18_dec25.csv")
df.primary.jan <- vroom::vroom("primary18_jan25.csv")
df.boost.dec <- vroom::vroom("boost18_dec25.csv")
df.boost.jan <- vroom::vroom("boost18_jan25.csv")









# modelme <- function(dat, outcome, offsetz){
#   library(MASS)
#       p1 <- paste(paste(outcome, " ~ social_vulnerability_index"), 
#                 "pct_births",
#                 "estimated_hesitant",
#                 "death_rate_100k",
#                 "case_rate_100k", sep=" + ")
#     
#     frm <- as.formula(paste(p1, " + offset(log(", offsetz,"))", sep=""))
#                                   
#       mod <- MASS::glm.nb(frm,
#                       data=dat)
#       mod_clean <- broom::tidy(mod)
#       mod_clean$rr <- round(exp(mod_clean$estimate),2)
#       mod_clean$ci.lower <- exp(confint(mod))[,1]
#       mod_clean$ci.upper <- exp(confint(mod))[,2]
#   
#   return(mod_clean)
# }


##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### MODEL 1: Primary Series, December 2021
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
mod_p_dec <- MASS::glm.nb(series_complete_18plus ~ social_vulnerability_index + 
                  estimated_hesitant +
                  death_rate_100k +
                  case_rate_100k + 
                    factor(naat_tertile) +
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
    decimals=3
  )

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### MODEL 2: Booster, December 2021
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

mod_b_dec <- MASS::glm.nb(booster_doses_18plus ~ social_vulnerability_index + 
                            estimated_hesitant +
                            death_rate_100k +
                            case_rate_100k + 
                            factor(naat_tertile) +
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
  )





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

     

############# MERGE JAN TABLES
fullTable = rbind(gt_primary_jan, gt_boost_jan)
gtTable = gt(fullTable) %>%
  tab_row_group(
    label = md("**Primary Series Uptake**"),
    rows = 1:6
  ) %>% 
  tab_row_group(
    label = md("**Booster Uptake**"),
    rows = 7:12
  )

gtTable %>%
  fmt_number(
    columns = 2:5,
    decimals = 4
  ) %>%
  cols_label(
    term = "Variable",
    rr = "Rate Ratio",
    ci.lower = "Lower 95% CI",
    ci.upper = "Upper 95% CI",
    p.value = "P-Value"
    ) %>%
  tab_header(
    title = md("Output of Negative Binomial Models")
    ) %>%
  tab_footnote(
    footnote = md("Booster offset is population 18+ fully vaccinated"),
    cells_row_groups(groups = "**Booster Uptake**")
  ) %>%
  tab_footnote(
    footnote = md("Primary Series offset is USA population 18+"),
    cells_row_groups(groups = "**Primary Series Uptake**")
  ) -> out

str(out)

### rename rows
out$`_data`$term <- c("Social Vulnerability Index", "Estimated Hesitancy (%)", 
                      "COVID-19 Death Rate/100k", "COVID-19 Case Rate/100k", "NAAT Tertile 2 vs 1", "NAAT Tertile 3 vs 1",
                      "Social Vulnerability Index", "Estimated Hesitancy (%)", 
                      "COVID-19 Death Rate/100k", "COVID-19 Case Rate/100k","NAAT Tertile 2 vs 1", "NAAT Tertile 3 vs 1")
out  
