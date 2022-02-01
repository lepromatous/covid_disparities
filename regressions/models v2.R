





library(MASS)
nb.18 <- glm.nb(booster_doses_18plus ~ social_vulnerability_index + 
                  pct_births + 
                  estimated_hesitant +
                  death_rate_100k +
                  case_rate_100k +
                  offset(log(series_complete_18plus)),
                data=df.boost18)
mod.nb.18 <- broom::tidy(nb.18)
mod.nb.18$rr <- round(exp(mod.nb.18$estimate),2)
mod.nb.18$ci.lower <- exp(confint(nb.18))[,1]
mod.nb.18$ci.upper <- exp(confint(nb.18))[,2]

View(mod.nb.18)



#sapply(df.boost18[,nmz], function(x) (cor.test(x, df.boost18$social_vulnerability_index)$estimate))




#### merge in other stuff - may be specific to date. 

#### things to add
## air pollution (CHR), race, housing segregation (CHR), prevalence of comorbid/social hx
## rate STI, mobility (google)

#################################################################################################################
#################################################################################################################
####### Primary  ################################################################################################
#################################################################################################################
#################################################################################################################

### lasso regression to pick varz

require(glmnet) || install.packages("glmnet")
library(glmnet)








### case, death, ACS insurance, NPI, 
#### 5+
library(MASS)
out_5_2dose <- MASS::glm.nb(series_complete_5plus ~ social_vulnerability_index10 + 
                      pct_female + death_rate + factor(ruca) + factor(chr_outcome_quartile) +
                        offset(log(pop_5plus)), data=df)
broom::tidy(out_5_2dose)
1-exp(coef(out_5_2dose))
summary(out_5_2dose)

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







# #### find lambda
# cv <- mpath::cv.glmregNB(booster_doses_18plus ~ ., data=df.boost18, offset=log(series_complete_18plus), nfolds=10,
#             plot.it=TRUE, se=TRUE, n.cores=8, trace=FALSE,
#             parallel=T)
# 
# test <- mpath::glmregNB(booster_doses_18plus ~ ., data=df.boost18, offset=log(series_complete_18plus), nlambda = 100, alpha=1,
#                         parallel=TRUE, n.cores=8, lambda = cv$lambda.optim)
# 
# ### get vars > 3 decomal zero
# noquote(row.names(test$beta)[round(test$beta, 4)>0])
# 
# 
# nmz <- c(names(df.boost18)[grep("pct_", names(df.boost18))], "social_vulnerability_index", "estimated_hesitant")
# for(i in 1:length(nmz)){
#   df.boost18[,nmz[i]] <- df.boost18[,nmz[i]]/10
# }
# 
# out2 <- lm(booster_doses_18plus ~ social_vulnerability_index + metro_status + chr_factor_quartile + 
#              chr_ses_quartile + estimated_hesitant + pct_poverty +
#              pct_over65 + pct_latino + pct_english_lwell + pct_multi_unit + pct_vacany + 
#              pct_no_veh + pct_group + pct_ssi + pct_snap + pct_public_health + death_rate_100k, data=df.boost18)
# regclass::VIF(out2)
# 



