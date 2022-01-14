# Required libraries
library(data.table); library(ggplot2); library(coronavirus)

# Uncomment to update the coronavirus dataset
# Last updated: 08/01/2022
#update_dataset(silence = FALSE)

# Create the data.table from the vaccine dataset
vaccs <- setDT(copy(covid19_vaccine))

# Custom function for what follows
custom_max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

# calculate the total population of all countries present in the 
# dataset and sum it to get the world population
world_pop <- vaccs[country_region!='World',
  list(mpop=custom_max(population)),by=country_region][,sum(mpop)]

# Update the population column for World data
vaccs[country_region=='World',population:=world_pop]

# Creates the two required new columns by updating the data.table
vaccs[, `:=`(fully_vaccinated_ratio=
  round((100*people_fully_vaccinated/population),digits=1),
  partially_vaccinated_ratio=
  round((100*people_partially_vaccinated/population),digits=1))]

# Create a new datatable only for world data
world_dt <- vaccs[country_region=='World',
  .(fully_vaccinated_ratio,partially_vaccinated_ratio),by=date]

# Clear all na values from the vaccs dt, including World data
vaccs <- na.omit(vaccs, cols=c("fully_vaccinated_ratio", 
  "partially_vaccinated_ratio", "population"))

# Keep only relevant columns
vaccs <- vaccs[country_region!='World'
  & fully_vaccinated_ratio <= 100 & partially_vaccinated_ratio <= 100,
  .(country_region,people_partially_vaccinated,people_fully_vaccinated,
  lat,long,population,continent_name,fully_vaccinated_ratio,
  partially_vaccinated_ratio),by=date]

# Fix these NA values
vaccs[country_region=='Kosovo',continent_name:='Europe']
vaccs[country_region=='Sudan',continent_name:='Africa']

# At this point there are no NA values in the data table.
# it can be seen by running vaccs[is.na(any_column_name)]
# it is an empty datatable

# Also switch to "Americas" instead of North and South
vaccs[continent_name=='South America' | continent_name=='North America',
  continent_name:='Americas']