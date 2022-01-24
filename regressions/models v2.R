#### READ SOURCE TO MAKE DATA 

source("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/covidvax/regressions/build data for models.R")
df <- yo(datez="2022-01-08")



#### merge in other stuff - may be specific to date. 

#################################################################################################################
#################################################################################################################
####### Primary  ################################################################################################
#################################################################################################################
#################################################################################################################

### case, death, ACS insurance, NPI, 

#### 5+
library(MASS)
out_5_2dose <- MASS::glm.nb(series_complete_5plus ~ as.numeric(social_vulnerability_index) + 
                      pct_female + deaths + factor(ruca) + factor(chr_outcome_quartile) +
                        offset(log(pop_5plus)), data=df)
summary(out_5_2dose)

#### 18+
out_18_2dose <- MASS::glm.nb(series_complete_18plus ~ as.numeric(social_vulnerability_index) + 
                      pct_female + deaths + factor(ruca) + factor(chr_outcome_quartile) + 
                        factor(chr_factor_quartile) + offset(log(pop_18plus)), data=df)
summary(out_18_2dose)

#### 65+
out_65_2dose <- MASS::glm.nb(series_complete_65plus ~ as.numeric(social_vulnerability_index) + 
                      pct_female + deaths + factor(ruca) + factor(chr_outcome_quartile) + 
                        factor(chr_factor_quartile) +  offset(log(pop_65plus)), data=df)
summary(out_65_2dose)



#################################################################################################################
#################################################################################################################
####### BOOSTERS  ###############################################################################################
#################################################################################################################
#################################################################################################################
