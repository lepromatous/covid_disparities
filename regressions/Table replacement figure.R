# df <- vroom::vroom("~/Desktop/data_nb.csv")
# df %>%
#   separate(., col=apr, sep="-", into=c("apr.lower", "apr.upper")) %>%
#   separate(., col=apr.lower, sep=" ", into=c("apr.rr", "apr.lower"))%>%
#   separate(., col=jun, sep="-", into=c("jun.lower", "jun.upper"))%>%
#   separate(., col=jun.lower, sep=" ", into=c("jun.rr", "jun.lower"))%>%
#   separate(., col=oct, sep="-", into=c("oct.lower", "oct.upper"))%>%
#   separate(., col=oct.lower, sep=" ", into=c("oct.rr", "oct.lower"))%>%
#   separate(., col=dec, sep="-", into=c("dec.lower", "dec.upper"))%>%
#   separate(., col=dec.lower, sep=" ", into=c("dec.rr", "dec.lower"))%>%
#   separate(., col=jan, sep="-", into=c("jan.lower", "jan.upper"))%>%
#   separate(., col=jan.lower, sep=" ", into=c("jan.rr", "jan.lower"))%>%
#   separate(., col=dec.b, sep="-", into=c("dec.b.lower", "dec.b.upper"))%>%
#   separate(., col=dec.b.lower, sep=" ", into=c("dec.b.rr", "dec.b.lower"))%>%
#   separate(., col=jan.b, sep="-", into=c("jan.b.lower", "jan.b.upper"))%>%
#   separate(., col=jan.b.lower, sep=" ", into=c("jan.b.rr", "jan.b.lower")) -> test
#                                                
# test %>%
#   mutate(across(
#     everything(),
#     ~gsub(")", "", .)
#   )) -> test
#   
# test %>%
#   mutate(across(
#     everything(),
#     ~gsub("\\(", "", .)
#   )) -> test
# 
# 
# vz <- names(test[,2:22])
# test %>%
#   mutate(
#     across(all_of(vz),
#     as.numeric
#   )) -> test
# 
# 
# test[] %>%
#   pivot_longer(., cols=c(2:22), names_to = "month") -> test
# write.table(test[grep(".upper", test$month),], "~/Desktop/file.csv", sep=",", row.names=F)

df <- readxl::read_excel("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/extra data/data_nbplot.xlsx")
df$yearmon <- factor(as.yearmon(df$yearmon))
df$vax.factor <- factor(df$vax, levels=c(0,1), labels=c("Primary Series", "Booster"))
df$svi <- factor(df$svi, levels=c(2,3,4), labels=c("Quartile 2", "Quartile 3", "Quartile 4"))


colz <- RColorBrewer::brewer.pal(n=3, "Set1")
ggplot(data=df, 
       aes(x=yearmon, y=rr, ymin=lower, ymax=upper, colour=svi, linetype=vax.factor)) + 
           geom_point(position = position_dodge(.5)) +
           geom_linerange(position = position_dodge(.5)) +
  geom_hline(yintercept=1, colour="black", linetype=1, size=0.3) + 
  scale_color_manual(values=colz) +
  scale_y_continuous(limits=c(0.7, 1.1), n.breaks=5) + 
  labs(
    colour = "Social Vulnerability Index",
    x = "\nDate",
    y = "Risk Ratio and 95% Confidence Interval\n for Primary Series or Booster Uptake\n",
    linetype = "Vaccine Type"
  ) +
  theme(
    panel.background =element_blank(),
    axis.line = element_line("gray80", size=0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line("gray90", size=0.2),
    panel.grid.minor.y = element_line("gray90", size=0.2),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.position = "top") + 
  guides(linetype=guide_legend(direction='horizontal'),
         colour=guide_legend(direction='horizontal'))
ggsave("~/Desktop/plot.png")  
  
  
  


