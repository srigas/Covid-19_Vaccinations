# Greece's World level ranking
tot_ct <- curr_dt[,.N]

rank_part <- curr_dt[order(
  -partially_vaccinated_ratio)][country_region=='Greece',which=TRUE]

rank_full <- curr_dt[order(
  -fully_vaccinated_ratio)][country_region=='Greece',which=TRUE]

paste0("Globally, Greece ranks ",rank_part,"/",tot_ct,
  " for partially vaccinated ratio and ",rank_full,"/",tot_ct,
  " for fully vaccinated ratio.")

# Greece's European level ranking
europe_curr <- curr_dt[continent_name=='Europe']
tot_ct <- europe_curr[,.N]

rank_part <- europe_curr[order(
  -partially_vaccinated_ratio)][country_region=='Greece',which=TRUE]

rank_full <- europe_curr[order(
  -fully_vaccinated_ratio)][country_region=='Greece',which=TRUE]

paste0("In Europe, Greece ranks ",rank_part,"/",tot_ct,
  " for partially vaccinated ratio and ",rank_full,"/",tot_ct,
  " for fully vaccinated ratio.")

# Diverging plots for Greece compared to other european countries
gr_full <- europe_curr[country_region=='Greece']$fully_vaccinated_ratio
europe_curr$fully <- round((europe_curr$fully_vaccinated_ratio -
  gr_full)/gr_full,digits=1)
europe_curr$fully_type <- ifelse(europe_curr$fully < 0, "below", "above")

active_dt <- europe_curr[order(fully)]
active_dt$country_region <- factor(active_dt$country_region,
  levels = active_dt$country_region)

plot11 <- ggplot(active_dt, aes(x=country_region, y=fully, label=fully)) +
  geom_bar(stat='identity', aes(fill=fully_type), width=.5) +
  scale_fill_manual(name="Percentage (%) of",
  labels = c("Higher Ratio", "Lower Ratio"),
  values = c("above"="#486393", "below"="#6196a3")) +
  labs(x="",y="Relative percentages for Full Vaccination Ratio") +
  coord_flip() + theme_grey(base_size = 16)

gr_partial <- europe_curr[country_region=='Greece']$partially_vaccinated_ratio
europe_curr$partially <- round((europe_curr$partially_vaccinated_ratio -
  gr_partial)/gr_partial,digits=1)
europe_curr$partially_type <- ifelse(europe_curr$partially < 0,
  "below", "above")

active_dt <- europe_curr[order(partially)]
active_dt$country_region <- factor(active_dt$country_region,
  levels = active_dt$country_region)

plot12 <- ggplot(active_dt, aes(x=country_region, y=partially,
  label=partially)) + geom_bar(stat='identity',
  aes(fill=partially_type), width=.5) + coord_flip() +
  scale_fill_manual(name="Percentage (%) of",
  labels = c("Higher Ratio", "Lower Ratio"),
  values = c("above"="#486393", "below"="#6196a3")) +
  labs(x="",y="Relative percentages for Partial Vaccination Ratio") +
  theme_grey(base_size = 16)