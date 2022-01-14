# Compare with other European countries of similar population (10% margin)
greek_ppl <- curr_dt$population[curr_dt$country_region=='Greece']

sim_ct <- vaccs[continent_name == 'Europe' & population < 1.1*greek_ppl &
  population > 0.9*greek_ppl]

print(unique(sim_ct$country_region))

similar_ppl <- sim_ct[country_region=='Greece',list(date=date,
  full_gr=fully_vaccinated_ratio,part_gr=partially_vaccinated_ratio)]

extra_ct <- sim_ct[country_region=='Portugal',list(date=date,
  full_pr=fully_vaccinated_ratio,part_pr=partially_vaccinated_ratio)]

similar_ppl <- merge(similar_ppl,extra_ct,by="date")

extra_ct <- sim_ct[country_region=='Sweden',list(date=date,
  full_se=fully_vaccinated_ratio,part_se=partially_vaccinated_ratio)]

similar_ppl <- merge(similar_ppl,extra_ct,by="date")

extra_ct <- sim_ct[country_region=='Belarus',list(date=date,
  full_be=fully_vaccinated_ratio,part_be=partially_vaccinated_ratio)]

similar_ppl <- merge(similar_ppl,extra_ct,by="date")

extra_ct <- sim_ct[country_region=='Czechia',list(date=date,
  full_cz=fully_vaccinated_ratio,part_cz=partially_vaccinated_ratio)]

similar_ppl <- merge(similar_ppl,extra_ct,by="date")

extra_ct <- sim_ct[country_region=='Hungary',list(date=date,
  full_hg=fully_vaccinated_ratio,part_hg=partially_vaccinated_ratio)]

similar_ppl <- merge(similar_ppl,extra_ct,by="date")

# Check for bad values to avoid what happened with World series
for (i in 1:(nrow(similar_ppl)-1)) {
  if (any(similar_ppl[i+1]-similar_ppl[i] < 0)) {
      similar_ppl[i+1] <- similar_ppl[i]
  }
}

cmap <- c("Greece" = "#486393", "Portugal" = "#276dc2", "Sweden" = "#6196a3",
  "Belarus" = "#818fa3", "Czechia" = "#225a63", "Hungary" = "#92b0e5")

plot13 <- ggplot(similar_ppl, aes(x=date)) +
  geom_line(aes(y = part_gr), size=.9, color = "#486393") +
  geom_line(aes(y = part_pr), size=.9, color = "#276dc2") +
  geom_line(aes(y = part_se), size=.9, color = "#6196a3") +
  geom_line(aes(y = part_be), size=.9, color = "#818fa3") +
  geom_line(aes(y = part_cz), size=.9, color = "#225a63") +
  geom_line(aes(y = part_hg), size=.9, color = "#92b0e5") +
  labs(x="",y="Partially Vaccinated Population Ratio (%)") +
  theme_grey(base_size = 16)

plot14 <- ggplot(similar_ppl, aes(x=date)) +
  geom_line(aes(y = full_gr, color = "Greece"), size=.9) +
  geom_line(aes(y = full_pr, color = "Portugal"), size=.9) +
  geom_line(aes(y = full_se, color = "Sweden"), size=.9) +
  geom_line(aes(y = full_be, color = "Belarus"), size=.9) +
  geom_line(aes(y = full_cz, color = "Czechia"), size=.9) +
  geom_line(aes(y = full_hg, color = "Hungary"), size=.9) +
  labs(x="",y="Fully Vaccinated Population Ratio (%)") +
  scale_color_manual(name = "", values = cmap) +
  theme_grey(base_size = 16)