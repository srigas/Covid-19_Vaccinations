# World level time-series
plot3 <- ggplot(world_dt, aes(x=date)) +
  geom_line(aes(y = fully_vaccinated_ratio,
  linetype = "Fully"), size=.9, color="#486393") +
  geom_line(aes(y = partially_vaccinated_ratio,
  linetype = "Partially"), size=.9, color="#486393") +
  labs(x="",y="Vaccinated Population Ratio (%)") +
  scale_linetype_manual(name = "", values=c("Fully"="solid",
  "Partially"="dashed")) + theme_grey(base_size = 16)