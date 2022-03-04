library(feather)
library(tidyverse)
library(stringr)
library(janitor)
library(tidyquant)
library(ggtext)
#library(arrow)
library(vroom)



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


df %>%
  filter(df$fips %in% keep.fips) -> df
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 





df %>%

  tibble() %>%
  
  janitor::clean_names() %>%
  
  dplyr::select(.,
                fips, date, completeness_pct, series_complete_18plus, booster_doses_18plus, census2019_18pluspop, svi, svi_quant
  ) %>%
  
  mutate_at(c("completeness_pct", "series_complete_18plus", "booster_doses_18plus", "census2019_18pluspop", "svi"), as.numeric)%>%

  group_by(svi_quant, date) %>%
  
  summarise(
    total_primary_uptake = sum(series_complete_18plus, na.rm=T),
    total_pop = sum(census2019_18pluspop, na.rm=T),
    total_booster = sum(booster_doses_18plus, na.rm=T)
  ) %>%
  
  ungroup() -> out

# out %>%
#   group_by(svi_quant) %>%
#   summarise(
#     max(total_pop)
#   )







### manual verification of this being correct
#write.csv(df[df$date=="2021-10-01" & df$svi<25,], "~/Desktop/files.csv", row.names=F, na="")
#View(out[out$svi_quant==1,])



out %>%
  #group_by(svi_quant) %>%
  mutate(
    primary_rate = total_primary_uptake/total_pop*100,
    booster_rate = total_booster / total_primary_uptake*100
  ) -> plotme

plotme <- subset(plotme, !is.na(plotme$svi_quant))

plotme$svi_quant <- factor(plotme$svi_quant, levels=c(1,2,3,4), labels=c("First Quartile (Least Vulnerable)", "Second Quartile", "Third Quartile", "Fourth Quartile (Most Vulnerable)"))


### quantitiative diffs
plotme %>%
  filter(svi_quant %in%c("First Quartile (Least Vulnerable)", "Fourth Quartile (Most Vulnerable)")) %>%
  arrange(date, svi_quant) %>%
  select(svi_quant, date, primary_rate, booster_rate)->plotme.quant
plotme.quant$booster_rate[is.nan(plotme.quant$booster_rate)] <- NA


# plotme.quant %>%
#   #filter(date >= "2021-08-10") %>%
#   pivot_wider(
#     data=.,
#     names_from = svi_quant,
#     values_from = c("primary_rate", "booster_rate")
#   ) %>%
#   rename(
#     date = 1,
#     p.1 = 2,
#     p.4 = 3,
#     b.1 = 4,
#     b.4 = 5
#   ) %>%
#   mutate(
#     diff.primary = p.1 - p.4,
#     diff.booster = b.1 - b.4
#   ) %>%
#   summarise(
#     median.primary = median(diff.primary, na.rm=T),
#     median.boost = median(diff.booster, na.rm=T)
#   )
# ### 4.5 absolute vs 10.8%;  1.5% pre august; -1.9 post

median(plotme.quant$primary_rate[plotme.quant$svi_quant=="First Quartile (Least Vulnerable)"], na.rm=T) - 
median(plotme.quant$primary_rate[plotme.quant$svi_quant=="Fourth Quartile (Most Vulnerable)"], na.rm=T)
#1.2%
median(plotme.quant$booster_rate[plotme.quant$svi_quant=="First Quartile (Least Vulnerable)" & plotme.quant$date>="2021-12-16"], na.rm=T) - 
  median(plotme.quant$booster_rate[plotme.quant$svi_quant=="Fourth Quartile (Most Vulnerable)"& plotme.quant$date>="2021-12-16"], na.rm=T)
### 11.3 percent



col <- RColorBrewer::brewer.pal(n=4, "Set1")
ggplot() +
  geom_ma(data=plotme, aes(x=date, y=primary_rate, colour=svi_quant),  n=7, linetype=1, size=0.9) +
  #geom_line(data=plotme, aes(x=date, y=primary_rate, colour=svi_quant), linetype=1, size=0.9) +
  scale_y_continuous(limits=c(0,80), labels=function(x) paste0(x, "%")) +
  scale_color_manual(values = col) +
  labs(
    colour = "Social Vulnerability Index",
    x="\nDate of Primary Series Completion ('Fully Vaccinated')",
    y="Percent of SVI Category Population Fully Vaccinated\n"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line("lightgray", size=0.2)
  ) 








###############################################################################
###############################################################################
##########BOOSTER PLOT  ###########################
###############################################################################
###############################################################################
# df <- read_feather("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/covid_disparity.feather")
# 
# df$fips <- stringr::str_pad(df$fips, side="left", pad="0", width=5) 
#   
#   df %>%
#     tibble() %>%
#     
#     janitor::clean_names() %>%
#     
#     dplyr::select(.,
#           fips, date, completeness_pct, series_complete_18plus, booster_doses_18plus, census2019_18pluspop, svi, svi_quant
#      ) %>%
#     
#     mutate_at(c("completeness_pct", "series_complete_18plus", "booster_doses_18plus", "census2019_18pluspop", "svi"), as.numeric)%>%
#     
#     group_by(fips) %>%
#     
#     arrange(date) %>%
#     
#     ungroup() %>%
#     
#     group_by(svi_quant, date) %>%
#     
#     summarise(
#       total_primary_uptake = sum(series_complete_18plus, na.rm=T),
#       total_pop = sum(census2019_18pluspop, na.rm=T),
#       total_booster = sum(booster_doses_18plus, na.rm=T)
#       ) %>%
#     
#     ungroup()-> out
# 
#   
#   out %>%
#     group_by(svi_quant) %>%
#     mutate(
#       primary_rate = total_primary_uptake/total_pop*100,
#       booster_rate = total_booster / total_primary_uptake*100
#     ) -> plotme
#   
#   plotme <- subset(plotme, !is.na(plotme$svi_quant))
#   
#   plotme$svi_quant <- factor(plotme$svi_quant, levels=c(1,2,3,4), labels=c("First Quartile", "Second Quartile", "Third Quartile", "Fourth Quartile"))
#   
  plotme_boost <- subset(plotme, plotme$date>="2021-12-16")
  
  col <- RColorBrewer::brewer.pal(n=4, "Set1")
  ggplot() +
    
    geom_ma(data=plotme, aes(x=date, y=primary_rate, colour=svi_quant),  n=7, linetype=1, size=0.9) +
    #geom_line(data=plotme_boost, aes(x=date, y=booster_rate, colour=svi_quant), linetype=1, size=0.9) +
        scale_y_continuous(limits=c(0,80), labels=function(x) paste0(x, "%")) +
    scale_color_manual(values = col) +
    labs(
      colour = "Social Vulnerability Index",
      x="\nDate of Booster",
      y="Percent of SVI Category Fully Vaccinated Population with Booster\n"
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line("lightgray", size=0.2)
    ) 
    

  
  #########################################################################
  #########################################################################
  ########## COMBINE PLOTS
  #########################################################################
  #########################################################################
  dual.plot1 <- plotme[,c("date", "primary_rate", "svi_quant")]
  dual.plot1$primary <- 1
  dual.plot1 %>%
    rename(
      rate = "primary_rate"
    ) -> dual.plot1
  
  dual.plot2 <- plotme_boost[,c("date", "booster_rate", "svi_quant")]
  dual.plot2$primary <- 0
  dual.plot2 %>%
    rename(
      rate = "booster_rate"
    ) -> dual.plot2
  
  
  
  dual.plot <- data.frame(rbind(dual.plot1, dual.plot2))
  dual.plot$primary <- factor(dual.plot$primary)
    
  col <- RColorBrewer::brewer.pal(n=4, "Set1")
  ggplot() +
    geom_ma(data=dual.plot[dual.plot$primary==1,], aes(x=date, y=rate, colour=svi_quant), n=7, linetype=1, size=0.6) +
    geom_ma(data=dual.plot[dual.plot$primary==0,], aes(x=date, y=rate, colour=svi_quant), n=7, linetype=2, size=0.6) +
    #geom_line(data=dual.plot[dual.plot$primary==1,], aes(x=date, y=rate, colour=svi_quant), linetype=1, size=0.6) +
    #geom_line(data=dual.plot[dual.plot$primary==0,], aes(x=date, y=rate, colour=svi_quant), linetype=2, size=0.6) +
    scale_y_continuous(limits=c(0,80), n.breaks=10, labels=function(x) paste0(x, "%")) +
    scale_color_manual(values = col) +
    labs(
      colour = "Social Vulnerability Index",
      x="\nDate of Vaccination (Primary Series Completion or Booster)",
      y="Cumulative Percent of SVI Category Fully Vaccinated or Boosted\n (7-Day Moving Average)\n"
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line("lightgray", size=0.2)
    )  +
    # geom_rect(
    #   aes(xmin=as.POSIXct("2021-12-16") , xmax=as.POSIXct("2022-02-10"), ymin=20, ymax=60), alpha=0.2
    # ) +
    geom_richtext(aes(x=as.POSIXct("2022-01-13"), y=55),
                  label = "**Boosters**",
                  col = "Black",
                  size=3.5) +
    geom_richtext(aes(x=as.POSIXct("2021-07-30"), y=63),
                  label = "**Primary Series**",
                  col = "Black",
                  size=3.5)  -> p
  
    # annotate("text", x=as.POSIXct("2022-01-14"), y=55, label="Boosters", size=4) +
    # annotate("text", x=as.POSIXct("2021-07-30"), y=63, label="Primary Series", size=4)
    # 
#ggsave("~/Desktop/fig1.png")


#plotly::ggplotly(p)




############ farid stuff
# 
# df %>%
#   filter(date=="2021-10-10") %>%
#   group_by(factor(svi_quant)) %>%
#   summarise(
#     out = format(sum(as.numeric(census2019_18pluspop), na.rm=T), big.mark=","), 
#     n(),
#     median(as.numeric(completeness_pct), na.rm=T)
#   )


df %>%
  group_by(svi_quant) %>%
  median()

