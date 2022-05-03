library(feather)
library(tidyverse)
library(stringr)
library(janitor)
library(tidyquant)
library(ggtext)

###############################################################################
###############################################################################
##########Primary Series Plot ###########################
###############################################################################
###############################################################################
df <- arrow::read_feather("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/extra data/covidvax.feather")

df$fips <- stringr::str_pad(df$fips, side="left", pad="0", width=5) 

test <- vroom::vroom("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/extra data/prener.csv")
svi <- test[,c("GEOID", "SVI")]
svi %>%
  rename(
    fips = 1,
    svi = 2
  ) -> svi
svi$svi <- as.numeric(svi$svi)

svi %>%
  mutate(
    svi_quant = case_when(
      svi <25 ~ 1,
      svi >=25 & svi <50 ~ 2,
      svi >=50 & svi <75 ~ 3,
      TRUE ~ 4
    ),
    tertiles = ntile(svi, 3),
    tertiles = if_else(tertiles == 1, 'Low', if_else(tertiles == 2, 'Medium', 'High'))
  ) -> svi

# svi$svi_quant <- ifelse(svi$svi<25,1, 
#                         ifelse(svi$svi >=25 & svi$svi<50,2,
#                                ifelse(svi$svi >=50 & svi$svi<75,3,4)))

svi %>%
  mutate(
    fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
  )-> svi

df <- merge(df, svi, by="fips", all.x=T)

#df <- subset(df, as.numeric(df$completeness_pct)>=80)

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### completeness modeling
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

### start with 90% of days over 80% complete
df %>%
  filter(
    fips !="00UNK"
  ) %>%
  group_by(fips) %>%
  mutate(
    comp.days = ifelse(completeness_pct>=80,1,0)
  ) %>%
  ungroup() -> df

df %>%
  filter(comp.days==1) %>%
  group_by(fips) %>%
  summarise(
    n()
  ) -> total.days.complete

df %>%
  group_by(fips) %>%
  summarise(
    n()
  ) -> total.days.reporting

keeps <- merge(total.days.complete, total.days.reporting, by="fips", all.y=T)
keeps$pct_complete_days <- keeps$`n().x`/ keeps$`n().y` *100

##### USE THIS VECTOR FOR SUBSETTING IN MODELS v5.R
keep.fips <- keeps$fips[keeps$pct_complete_days>=80]