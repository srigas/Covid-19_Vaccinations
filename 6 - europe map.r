# Continent level maps: europe
world <- map_data('world')

europe_curr <- curr_dt[continent_name=='Europe']

# Create the input for the european map - partially vac only
wdt <- world[world$region %in% europe_curr$country_region,]
wdt$value <- europe_curr$fully_vaccinated_ratio[match(wdt$region,
  europe_curr$country_region)]

plot9 <- ggplot(wdt, aes(x=long, y=lat, group = group, fill = value)) + 
  geom_polygon(colour = "white", size = 0.05) + theme_bw()  + 
  scale_fill_continuous(low = "#d1def2", high = "#486393") +
  labs(fill = "Ratio (%)" , x="", y="") +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + 
  coord_map(xlim = c(-20, 50), ylim = c(30, 70)) + 
  theme(panel.border =  element_blank())