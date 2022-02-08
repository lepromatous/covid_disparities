###############################################################################
###############################################################################
######## Make Data v2 #########################################################
###############################################################################
###############################################################################



getdata <- function(datezz="2022-01-18", num.var, denom.var){
  source("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/github/COVID disparities/covid_disparities/regressions/build data for models.R")
  df2 <- yo(datez=datezz)

  outs <- df2[,c("fips","series_complete_yes", "series_complete_5plus", "series_complete_12plus", "series_complete_18plus", 
                 "series_complete_65plus", "booster_doses", "booster_doses_18plus",
                 "booster_doses_50plus", "booster_doses_65plus",
                 "pop_5plus", "pop_12plus", "pop_18plus", 
                 "pop_50plus", "pop_65plus")]
  ivs <- df2[,c("census_area", "recip_state", "completeness_pct", 
                "svi_ctgy", "social_vulnerability_index", "metro_status", "estimated_hesitant", "pop", "cases",
                "deaths", "deaths_per_case","chr_outcome_quartile", "chr_factor_quartile",
                "chr_heatlh_behav_quartile", "chr_clinical_care_quartile", "chr_ses_quartile", 
                # "chr_phys_enviro_quartile","total_housing", 
                # "total_households", "inc_median", "inc_per_cap", "pct_poverty", 
                # "pct_uemploy", "pct_lths", "pct_over65", "pct_under18", "pct_disable", 
                # "pct_single", "pct_race_white", "pct_race_black", "pct_race_native", 
                # "pct_race_aapi", "pct_race_other", "pct_race_nonwhite", "pct_latino", 
                # "pct_english_lwell", "pct_multi_unit", "pct_mobile", "pct_vacany", 
                # "pct_renter", "pct_crowded", "pct_no_veh", "pct_group", "pct_births", 
                # "pct_ss", "pct_ssi", "pct_tanf", "pct_snap", "pct_public_health", 
               # "pct_no_health", 
                "naat_tertile", "hhs_region", "rpl_theme1", "rpl_theme2",
                "rpl_theme3", "rpl_theme4"
                 )]
  
  df <- data.frame(outs, ivs)
  #sf::st_geometry(df) <- NULL
  df <- df[,!names(df)%in% "geometry"]
  df <- df[,!names(df)%in% "geometry.1"]
  

  df<- df[,c(1, 17,19,21,27:32, 2:16,18,20,22:26, 33:ncol(df))]
  
  
  
  df %>%
    mutate(across(c(11:ncol(df)), as.numeric)) %>%
    mutate(across(c(2:10), as.factor)) %>%
    #drop_na() %>%
    filter(
      booster_doses_18plus !=0,
      series_complete_18plus !=0
    ) -> df
  
  df$case_rate_100k <- df$cases / df$pop *100000
  df$death_rate_100k <- df$deaths / df$pop *100000
  
  df <- df[,c("fips", "svi_ctgy", "metro_status", num.var, 
              denom.var, 
              "census_area", 
              "completeness_pct", "social_vulnerability_index", "estimated_hesitant", 
              "deaths_per_case", 
              # "total_housing", 
              # "total_households", "inc_median", "inc_per_cap", "pct_poverty", 
              # "pct_uemploy", "pct_lths", "pct_over65", "pct_under18", "pct_disable", 
              # "pct_single", "pct_race_white", "pct_race_black", "pct_race_native", 
              # "pct_race_aapi", "pct_race_other", "pct_race_nonwhite", "pct_latino", 
              # "pct_english_lwell", "pct_multi_unit", "pct_mobile", "pct_vacany", 
              # "pct_renter", "pct_crowded", "pct_no_veh", "pct_group", "pct_births", 
              # "pct_ss", "pct_ssi", "pct_tanf", "pct_snap", "pct_public_health", 
              # "pct_no_health", 
              "case_rate_100k", "death_rate_100k", "naat_tertile", "hhs_region",
              "rpl_theme1", "rpl_theme2",
              "rpl_theme3", "rpl_theme4")]
  
  
  
  
  return(df)
}

df.boost.dec <- getdata(datezz = "2021-12-25", num.var = "booster_doses_18plus", denom.var = "series_complete_18plus")
write.csv(df.boost.dec, "~/Desktop/boost18_dec25.csv", row.names=F, na="")

df.boost.jan <- getdata(datezz = "2022-01-25", num.var = "booster_doses_18plus", denom.var = "series_complete_18plus")
write.csv(df.boost.jan, "~/Desktop/boost18_jan25.csv", row.names=F, na="")


df.primary.dec <- getdata(datezz = "2021-12-25", num.var = "series_complete_18plus", denom.var = "pop_18plus")
write.csv(df.primary.dec, "~/Desktop/primary18_dec25.csv", row.names=F, na="")

df.primary.jan <- getdata(datezz = "2022-01-25", num.var = "series_complete_18plus", denom.var = "pop_18plus")
write.csv(df.primary.jan, "~/Desktop/primary18_jan25.csv", row.names=F, na="")

