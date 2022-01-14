# Get info for final available date of each country
curr_dt <- vaccs[order(-date)]
curr_dt <- unique(curr_dt, by = "country_region")

library(maps)

# These are the names recognized by the library
curr_dt$country_region[curr_dt$country_region == "US"] <- "USA"
curr_dt$country_region[curr_dt$country_region == "United Kingdom"] <- "UK"

world <- map_data('world')

# Create the input for the worldmap - fully vac
wdt <- world[world$region %in% curr_dt$country_region,]
wdt$value <- curr_dt$fully_vaccinated_ratio[match(wdt$region,
  curr_dt$country_region)]

# Plot the world map
plot1 <- ggplot(wdt, aes(x=long, y=lat, group = group, fill = value)) + 
  geom_polygon(colour = "white", size = 0.05) +
  theme_bw()  + 
  scale_fill_continuous(low = "#d1def2", high = "#486393") +
  labs(fill = "Ratio (%)" , x="", y="") +
  scale_y_continuous(breaks=c()) + 
  scale_x_continuous(breaks=c()) + 
  coord_map(xlim = c(-170, 170), ylim = c(-50, 100)) + 
  theme(panel.border =  element_blank())

# Create the input for the worldmap - partially vac
wdt <- world[world$region %in% curr_dt$country_region,]
wdt$value <- curr_dt$partially_vaccinated_ratio[match(wdt$region,
  curr_dt$country_region)]

# Plot the world map
plot2 <- ggplot(wdt, aes(x=long, y=lat, group = group, fill = value)) + 
  geom_polygon(colour = "white", size = 0.05) + theme_bw() + 
  scale_fill_continuous(low = "#d1def2", high = "#486393") +
  labs(fill = "Ratio (%)" , x="", y="") +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + 
  coord_map(xlim = c(-170, 170), ylim = c(-50, 100)) + 
  theme(panel.border =  element_blank())