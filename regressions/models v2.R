#### READ SOURCE TO MAKE DATA 

source("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/COVID disparities/covid_disparities/regressions/build data for models.R")
df <- yo(datez="2022-01-08")

df$social_vulnerability_index <- as.numeric(as.character(df$social_vulnerability_index))
df$social_vulnerability_index10 <- as.numeric(df$social_vulnerability_index)/10
df$adi10 <- as.numeric(df$adi)/10

df$pct_female <- as.numeric(df$pct_female)/5
#### merge in other stuff - may be specific to date. 

#### things to add
## air pollution (CHR), race, housing segregation (CHR), prevalence of comorbid/social hx
## rate STI, mobility (google)

#################################################################################################################
#################################################################################################################
####### Primary  ################################################################################################
#################################################################################################################
#################################################################################################################

### case, death, ACS insurance, NPI, 
#### 5+
library(MASS)
out_5_2dose <- MASS::glm.nb(series_complete_5plus ~ social_vulnerability_index10 + 
                      pct_female + death_rate + factor(ruca) + factor(chr_outcome_quartile) +
                        offset(log(pop_5plus)), data=df)
broom::tidy(out_5_2dose)
1-exp(coef(out_5_2dose))
summary(out_18_2dose)

#### 18+
out_18_2dose <- MASS::glm.nb(series_complete_18plus ~ social_vulnerability_index10 + 
                      pct_female + death_rate + factor(ruca) + factor(chr_outcome_quartile)  + 
                        offset(log(pop_18plus)), data=df)
broom::tidy(out_18_2dose)
1-exp(coef(out_18_2dose))

#### 65+
out_65_2dose <- MASS::glm.nb(series_complete_65plus ~ social_vulnerability_index10 + 
                      pct_female + death_rate + factor(ruca) + factor(chr_outcome_quartile) +
                        offset(log(pop_65plus)), data=df)
broom::tidy(out_65_2dose)
1-exp(coef(out_65_2dose))


#################################################################################################################
#################################################################################################################
####### BOOSTERS  ###############################################################################################
#################################################################################################################
#################################################################################################################
#### 18
names(df)
library(MASS)
out_18_boosterdose <- MASS::glm.nb(booster_doses_18plus ~ social_vulnerability_index10 + 
                              pct_female + death_rate + factor(ruca) + factor(chr_outcome_quartile) +
                              offset(log(pop_18plus)), data=df)
broom::tidy(out_18_boosterdose)
1-exp(coef(out_18_boosterdose))

#### 18+
out_50_boosterdose <- MASS::glm.nb(booster_doses_50plus ~ social_vulnerability_index10 + 
                               pct_female + death_rate + factor(ruca) + factor(chr_outcome_quartile)  +
                               offset(log(pop_50plus)), data=df)
broom::tidy(out_50_boosterdose)
1-exp(coef(out_50_boosterdose))

#### 65+
out_65_boosterdose <- MASS::glm.nb(booster_doses_65plus ~ social_vulnerability_index10 + 
                               pct_female + death_rate + factor(ruca) + factor(chr_outcome_quartile) + 
                               offset(log(pop_65plus)), data=df)
broom::tidy(out_65_boosterdose)
1-exp(coef(out_65_boosterdose))



forestplotme <- function(model, title){

df <- data.frame("name" = c("Social Vulnerability Index, 10 Point Increment",
                            "Percent Female in County, 5 Percent Increment",
                            "County Death Rate per 100,000",
                            "Large Metro Area, Fringe vs Large Metro, Central",
                            "Non-Metro Area, Fringe vs Large Metro, Central",
                            "Small Metro Area, Fringe vs Large Metro, Central",
                            "Community Health Outcome Ranking, 2nd Quartile vs 1st",
                            "Community Health Outcome Ranking, 3rd Quartile vs 1st",
                            "Community Health Outcome Ranking, 4th Quartile vs 1st"),
                 "beta" = model$coefficients[-1],
                 "se" = coef(summary(model))[-1, "Std. Error"],
                 "pvalue" = coef(summary(model))[-1, "Pr(>|z|)"])

      #https://nightingalehealth.github.io/ggforestplot/articles/ggforestplot.html
      library(ggforestplot)
      ggforestplot::forestplot(
        df = df,
        name = name,
        estimate = beta,
        se = se,
        pvalue=pvalue,
        psignif = 0.05,
        logodds=T,
        xlab = "Rate Ratio",
        title = title
      ) -> out
return(out)
}


forestplotme(model=out_18_2dose, title="2-Dose Series 18+")
forestplotme(model=out_18_boosterdose, title="Booster Dose Series 18+")

forestplotme(model=out_65_2dose, title="2-Dose Series 65+")
forestplotme(model=out_65_boosterdose, title="Booster Dose Series 65+")






