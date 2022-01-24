source("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/covidvax/regressions/build data for models.R")

#### merge in other stuff - may be specific to date. 

#################################################################################################################
#################################################################################################################
####### Primary  ################################################################################################
#################################################################################################################
#################################################################################################################

### case, death, ACS insurance, NPI, 

#### 5+
library(MASS)
out <- MASS::glm.nb(series_complete_5plus ~ as.numeric(social_vulnerability_index) + 
                      pct_female + deaths + factor(ruca) + offset(log(pop)), data=df)

#### 18+
out <- MASS::glm.nb(series_complete_5plus ~ as.numeric(social_vulnerability_index) + 
                      pct_female + deaths + factor(ruca) + offset(log(pop)), data=df)

#### 65+
mod.lm.primary65 <- lm(as.numeric(series_complete_65pluspop_pct)  ~ as.numeric(social_vulnerability_index) + 
                         pct_female + deaths + factor(ruca), data=df)
summary(mod.lm.primary65)



#################################################################################################################
#################################################################################################################
####### BOOSTERS  ###############################################################################################
#################################################################################################################
#################################################################################################################
#### OF ALL FULLY VAXXED
mod.lm.boost.all <- lm(as.numeric(booster_doses_vax_pct)  ~ as.numeric(social_vulnerability_index) + 
                         pct_female + deaths + factor(ruca), data=df)
summary(mod.lm.boost.all)

#### OF 18+ FULLY VAXXED
mod.lm.boost18 <- lm(as.numeric(booster_doses_18plus_vax_pct)  ~ as.numeric(social_vulnerability_index) + 
                       pct_female + deaths + factor(ruca), data=df)
summary(mod.lm.boost18)

#### OF 65+ FULLY VAXXED
mod.lm.boost65 <- lm(as.numeric(booster_doses_65plus_vax_pct)  ~ as.numeric(social_vulnerability_index) + 
                       pct_female + deaths + factor(ruca), data=df)
summary(mod.lm.boost65)


library(MASS)
out <- MASS::glm.nb(booster_doses_18plus ~ as.numeric(social_vulnerability_index) + 
                      pct_female + deaths + factor(ruca) + offset(log(pop)), data=df)
summary(out)
