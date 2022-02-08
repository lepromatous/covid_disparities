

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
    title = md("Output of Negative Binomial Models"),
    subtitle = md("*Week ending January 25, 2022*")
      ) %>%
  tab_footnote(
    footnote = md("Booster offset is population 18+ fully vaccinated"),
    cells_row_groups(groups = "**Booster Uptake**")
  ) %>%
  tab_footnote(
    footnote = md("Primary Series offset is USA population 18+"),
    cells_row_groups(groups = "**Primary Series Uptake**")
  ) -> out


### rename rows
out$`_data`$term <- c("SVI (5 Pct Increase)", "Estimated Hesitancy (%)", 
                      "COVID-19 Death Rate/100k", "COVID-19 Case Rate/100k", "NAAT Tertile 2 vs 1", "NAAT Tertile 3 vs 1",
                      "SVI (5 Pct Increase)", "Estimated Hesitancy (%)", 
                      "COVID-19 Death Rate/100k", "COVID-19 Case Rate/100k","NAAT Tertile 2 vs 1", "NAAT Tertile 3 vs 1")
gtsave(out, "~/Desktop/jan.pdf")





############# MERGE dec TABLES

fullTable = rbind(gt_primary_dec, gt_boost_dec)
gtTable = gt(fullTable) %>%
  tab_row_group(
    label = md("**Primary Series Uptake**"),
    rows = 1:4
  ) %>% 
  tab_row_group(
    label = md("**Booster Uptake**"),
    rows = 5:8
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
    title = md("Output of Negative Binomial Models"),
    subtitle = md("*Week ending December 25, 2021*")
  ) %>%
  tab_footnote(
    footnote = md("Booster offset is population 18+ fully vaccinated"),
    cells_row_groups(groups = "**Booster Uptake**")
  ) %>%
  tab_footnote(
    footnote = md("Primary Series offset is USA population 18+"),
    cells_row_groups(groups = "**Primary Series Uptake**")
  ) -> out


### rename rows
out$`_data`$term <- c("SVI (5 Pct Increase)", "Estimated Hesitancy (%)", 
                      "COVID-19 Death Rate/100k", "COVID-19 Case Rate/100k", 
                      "SVI (5 Pct Increase)", "Estimated Hesitancy (%)", 
                      "COVID-19 Death Rate/100k", "COVID-19 Case Rate/100k")
gtsave(out, "~/Desktop/dec.pdf")

