df <- readxl::read_excel("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/manuscript/Regression output3.xlsx")
df$vax.factor <- factor(df$vax, levels=c(0,1), labels=c("Primary Series", "Booster"))
df$svi <- factor(df$svi, levels=c(1, 2,3,4), labels=c("Quartile 1 (Reference, Least Vulnerable)", "Quartile 2", "Quartile 3", "Quartile 4 (Most Vulnerable)"))
df$yearmon <- "2020-12-16 - 2022-02-01"

colz <- RColorBrewer::brewer.pal(n=4, "Set1")
ggplot(data=df, 
       aes(x=vax.factor, y=rr, ymin=lower, ymax=upper, colour=svi)) + 
  geom_point(position = position_dodge(.5), size=1.7) +
  geom_linerange(position = position_dodge(.5), size=0.8) +
  geom_hline(yintercept=1, colour="black", linetype=1, size=0.4) + 
  scale_color_manual(values=colz) +
  scale_y_continuous(limits=c(0.7, 1.05), n.breaks=7) + 
  labs(
    colour = "Social Vulnerability Index",
    x = "",
    y = "Adjusted Risk Ratio and 95% Confidence Interval\n for Primary Series or Booster Uptake\n"
    ) +
  theme(
    panel.background =element_blank(),
    axis.line = element_line("gray80", size=0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line("gray90", size=0.2),
    panel.grid.minor.y = element_line("gray90", size=0.2),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.position = "top",
    axis.text = element_text(size=13),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size=13)) + 
  guides(linetype=guide_legend(direction='horizontal'),
         colour=guide_legend(direction='horizontal'))
ggsave("~/Desktop/fig2.png")  





