df <- readxl::read_excel("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/manuscript/Regression output2.xlsx")
df$yearmon <- factor(ifelse(as.character(df$yearmon)=="2021-10-01", "Dec 2020 - Sep 2021", "Nov 2021 - Feb 2022"),
                     levels = c("Dec 2020 - Sep 2021", "Nov 2021 - Feb 2022"))
df$vax.factor <- factor(df$vax, levels=c(0,1), labels=c("Primary Series", "Booster"))
df$svi <- factor(df$svi, levels=c(1, 2,3,4), labels=c("Quartile 1 (Reference, Least Vulnerable)", "Quartile 2", "Quartile 3", "Quartile 4 (Most Vulnerable)"))


colz <- RColorBrewer::brewer.pal(n=4, "Set1")
ggplot(data=df, 
       aes(x=yearmon, y=rr, ymin=lower, ymax=upper, colour=svi, linetype=vax.factor)) + 
  geom_point(position = position_dodge(.5)) +
  geom_linerange(position = position_dodge(.5)) +
  geom_hline(yintercept=1, colour="black", linetype=1, size=0.3) + 
  scale_color_manual(values=colz) +
  scale_y_continuous(limits=c(0.5, 1.35), n.breaks=11) + 
  labs(
    colour = "Social Vulnerability Index",
    x = "",
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





