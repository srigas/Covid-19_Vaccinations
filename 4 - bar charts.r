# Bar plots with color coding depending on continent
color_mapping <- c("Europe" = "#486393", "Asia" = "#92b0e5",
  "Americas" = "#6196a3", "Oceania" = "#276dc2", "Africa" = "#818fa3")

top_full_vac <- head(curr_dt[order(-fully_vaccinated_ratio)],n=10)
bot_par_vac <- head(curr_dt[order(partially_vaccinated_ratio)],n=10)

plot4 <- ggplot(top_full_vac, aes(x=reorder(country_region,
  -fully_vaccinated_ratio), y=fully_vaccinated_ratio,
  fill=continent_name)) + geom_bar(stat="identity", width=.8) +
  scale_fill_manual(values = color_mapping) +
  labs(fill = "Continent" , x="", y="Fully Vaccinated Ratio (%)") + 
  theme(axis.text.x = element_text(angle=45, vjust = 0.6))

plot5 <- ggplot(bot_par_vac, aes(x=reorder(country_region,
  -partially_vaccinated_ratio), y=partially_vaccinated_ratio,
  fill=continent_name)) + geom_bar(stat="identity", width=.8) +
  scale_fill_manual(values = color_mapping) +
  labs(fill = "Continent" , x="", y="Partially Vaccinated Ratio (%)") + 
  theme(axis.text.x = element_text(angle=45, vjust = 0.6))