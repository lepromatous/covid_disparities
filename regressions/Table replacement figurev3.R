df <- vroom::vroom("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-manufacturer.csv")


df %>%
  group_by(location) %>%
  filter(max(date)) -> df2


class(df$date)
