# Continent-wise comparison of having received at least 1 dose per season

# Continent total population, as judged by the curr_dt table
cont_pops <- curr_dt[,list("populations"=sum(population)),by=continent_name]

# First season results
first_season <- vaccs[date <= as.Date('2021-4-28')]
end_of_first <- unique(first_season[order(-date)], by = "country_region")
continental_first <- end_of_first[,
  list("partial_sum"=sum(people_partially_vaccinated)), by = .(continent_name)]
results_first <- merge(continental_first, cont_pops, all=FALSE)
results_first$partial_ratio =
  round(100*results_first$partial_sum/results_first$populations, digits=1)

# Second season results
second_season <- vaccs[date > as.Date('2021-4-28') & 
  date <= as.Date('2021-9-14')]
end_of_second <- unique(second_season[order(-date)], by = "country_region")
continental_second <- end_of_second[,
  list("partial_sum"=sum(people_partially_vaccinated)), by = .(continent_name)]
results_second <- merge(continental_second, cont_pops, all=FALSE)
results_second$partial_ratio =
  round(100*results_second$partial_sum/results_second$populations, digits=1)

# Third season results
third_season <- vaccs[date > as.Date('2021-9-14')]
end_of_third <- unique(third_season[order(-date)], by = "country_region")
continental_third <- end_of_third[,
  list("partial_sum"=sum(people_partially_vaccinated)), by = .(continent_name)]
results_third <- merge(continental_third, cont_pops, all=FALSE)
results_third$partial_ratio =
  round(100*results_third$partial_sum/results_third$populations, digits=1)

library(gridExtra)

# Lollipop Charts
plot6 <- ggplot(results_first, aes(x=continent_name, y=partial_ratio)) +
  geom_point(size=3, color="#486393") +
  geom_segment(aes(x=continent_name, xend=continent_name,
  y=0, yend=partial_ratio), color="#486393") + ylim(0, 75) +
  labs(x="First Season",y="Partially Vaccinated Population Ratio (%)") +
  theme(axis.text.x = element_text(angle=45, vjust=0.6))

plot7 <- ggplot(results_second, aes(x=continent_name, y=partial_ratio)) +
  geom_point(size=3, color="#486393") +
  geom_segment(aes(x=continent_name, xend=continent_name,
  y=0, yend=partial_ratio), color="#486393") +
  labs(x="Second Season",y="") + ylim(0, 75) +
  theme(axis.text.x = element_text(angle=45, vjust=0.6))

plot8 <- ggplot(results_third, aes(x=continent_name, y=partial_ratio)) +
  geom_point(size=3, color="#486393") +
  geom_segment(aes(x=continent_name, xend=continent_name,
  y=0, yend=partial_ratio), color="#486393") +
  labs(x="Third Season",y="") + ylim(0, 75) +
  theme(axis.text.x = element_text(angle=45, vjust=0.6))

grid.arrange(plot6,plot7,plot8, nrow = 1, top='')