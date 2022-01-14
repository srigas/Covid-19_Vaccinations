# Time-series for Greece, with World and Europe for comparison
greece_dt <- vaccs[country_region=='Greece',list("date"=date,
  "fully_gr"=fully_vaccinated_ratio,
  "partially_gr"=partially_vaccinated_ratio)]

europe_ts <- vaccs[continent_name=='Europe',
  list("part_eu"=sum(people_partially_vaccinated),
  "full_eu"=sum(people_fully_vaccinated)), by = .(date)]

# Create ratios for Europe
eur_pop <- cont_pops[continent_name=='Europe']$populations
europe_ts$fully_eu <- round(100*europe_ts$full_eu/eur_pop,digits=1)
europe_ts$partially_eu <- round(100*europe_ts$part_eu/eur_pop,digits=1)

active_dt <- merge(greece_dt,europe_ts,by="date")
active_dt <- merge(active_dt,world_dt,by="date")

# Check for bad values to avoid what happened with World series
for (i in 1:(nrow(active_dt)-1)) {
  if (any(active_dt[i+1]-active_dt[i] < 0)) {
      active_dt[i+1] <- active_dt[i]
  }
}

plot10 <- ggplot(active_dt, aes(x=date)) +
  geom_line(aes(y = fully_gr, color = "Greece", linetype="Fully"), size=.9) +
  geom_line(aes(y = fully_eu, color = "Europe", linetype="Fully"), size=.9) +
  geom_line(aes(y = fully_vaccinated_ratio, color = "World", linetype="Fully"),
  size=.9) + geom_line(aes(y = partially_gr, color = "Greece",
  linetype="Partially"), size=.9) + geom_line(aes(y = partially_eu,
  color = "Europe", linetype="Partially"), size=.9) +
  geom_line(aes(y = partially_vaccinated_ratio, color = "World",
  linetype="Partially"), size=.9) + 
  labs(x="",y="Vaccinated Population Ratio (%)") +
  scale_color_manual(name = "",
  values = c("Greece" = "#486393", "Europe" = "#6196a3",
  "World" = "#818fa3")) + scale_linetype_manual(name = "",
  values=c("Fully"="solid", "Partially"="dashed")) + ylim(0, 75) +
  theme_grey(base_size = 16)