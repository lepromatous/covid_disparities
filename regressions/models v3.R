library(vroom)
df.primary.dec <- vroom::vroom("/Users/timothywiemken/Desktop/primary18_dec25.csv")
df.primary.jan <- vroom::vroom("/Users/timothywiemken/Desktop/primary18_jan25.csv")
df.boost.dec <- vroom::vroom("/Users/timothywiemken/Desktop/boost18_dec25.csv")
df.boost.jan <- vroom::vroom("/Users/timothywiemken/Desktop/boost18_jan25.csv")


modelme <- function(dat, outcome, offsetz){
  library(MASS)
      p1 <- paste(paste(outcome, " ~ social_vulnerability_index"), 
                "pct_births",
                "estimated_hesitant",
                "death_rate_100k",
                "case_rate_100k", sep=" + ")
    
    frm <- as.formula(paste(p1, " + offset(log(", offsetz,"))", sep=""))
                                  
      mod <- MASS::glm.nb(frm,
                      data=dat)
      mod_clean <- broom::tidy(mod)
      mod_clean$rr <- round(exp(mod_clean$estimate),2)
      mod_clean$ci.lower <- exp(confint(mod))[,1]
      mod_clean$ci.upper <- exp(confint(mod))[,2]
  
  return(mod_clean)
}

modelme(dat = df.boost.dec, 
        outcome = "booster_doses_18plus", 
        offsetz = "series_complete_18plus",
        )

