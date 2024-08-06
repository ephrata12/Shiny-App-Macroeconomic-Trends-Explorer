library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(janitor)
library(tidyselect)
library(tidyr)
library(lubridate)
library(magrittr)
library(rvest)

# GDP growth rate data
world_map <- maps::map("world", plot = FALSE, fill = TRUE) %>%
  st_as_sf()

growth_rate_data <- read_csv("data/growth_rate.csv", skip=4, col_select = c("Country Name", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"))

old_names <- c("Bahamas, The", "Cabo Verde", "Czechia", "Congo, Dem. Rep.", "Egypt, Arab Rep.", "Gambia, The", "Korea, Rep.",
               "Kyrgyz Republic", "Iran, Islamic Rep.", "Lao PDR", "Moldova", "Congo, Rep.",
               "Russian Federation", "Slovak Republic", "Syrian Arab Republic",
               "Turkiye", "United States",
               "United Kingdom", "Venezuela, RB", "British Virgin Islands", "Virgin Islands (U.S.)", "	
Yemen, Rep.")
new_names <- c("Bahamas", "Cape Verde", "Czech Republic", "Democratic Republic of the Congo", "Egypt", "Gambia",
               "South Korea", "Kyrgyzstan", "Iran", "Laos", "Moldovia", "Republic of Congo", "Russia", "Slovakia",
               "Syria", "Turkey", "USA", "UK", "Venezuela", "Virgin Islands, British", "S
Virgin Islands, US", "Yemen")

for (i in 1:length(old_names)){
  growth_rate_data$`Country Name`[growth_rate_data$`Country Name` == old_names[i]] <- new_names[i]
}

growth_rate_leaflet <- growth_rate_data %>%
  inner_join(world_map, by = c("Country Name"="ID")) %>%
  rename(country_name = `Country Name`) %>%
  pivot_longer(cols=c(`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`),
               names_to='year',
               values_to='growth_rate') %>%
  st_as_sf() %>%
  st_transform('+proj=longlat +datum=WGS84')


# Generating csv file with wrangled data
saveRDS(growth_rate_leaflet, "data/growth_rate_leaflet.rds")

## GDP Indicator Correlation -Ebony 

dev_indicators0 <- read_csv("data/dev_indicators.csv", na = c(".."))
dev_indicators0 <- dev_indicators0

for (year in 2000:2022) {
  old_name <- paste0(year, " [YR", year, "]")
  new_name <- as.character(year)
  names(dev_indicators0)[names(dev_indicators0) == old_name] <- new_name
}

start_year <- 2000
end_year <- 2022
dev_indicators0 <- dev_indicators0 %>%
  filter(if_any(`2000`:`2022`, ~ !is.na(.)))

dev_indicators1 <- dev_indicators0 %>%
  select(-c(`Country Code`, `Series Code`)) %>% 
  rename("Country" = "Country Name",
         "variable" = "Series Name"
  )

dev_indicators1 <- dev_indicators1 %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "year",
               values_to = "value")

dev_indicators <- dev_indicators1 %>% 
  pivot_wider( names_from = variable,
               values_from = value)

dev_indicators <- dev_indicators %>% 
  rename(
    "gdp" = "GDP (constant 2015 US$)",
    "Education" = "Educational attainment, at least completed upper secondary, population 25+, total (%) (cumulative)",
    "Depletion" = "Adjusted savings: natural resources depletion (% of GNI)",
    "Labor" = "Labor force, total"
  )

write_csv(dev_indicators, "data/dev_indicators_clean.csv")

# Load the dataset
inflation_rate_data <- read.csv("data/inflation_rate.csv", na.strings = "..")

inflation_rate_clean <- inflation_rate_data %>%
  # Rename columns 
  rename_with(~str_replace(., "X(\\d+)\\.\\.YR\\1\\.", "\\1"), starts_with("X20")) %>%
  rename(country = Country.Name) %>%
  
  # Transform the data from wide to long format
  pivot_longer(cols = starts_with("20"), 
               names_to = "year",         
               values_to = "inflationRate") %>%
  
  # Filter out rows with missing inflationRate values
  filter(!is.na(inflationRate)) %>%
  
  # Round the inflationRate column to 3 decimal places
  mutate(inflationRate = round(inflationRate, 3)) %>%
  
  # Select the desired columns for the final data frame
  select(country, year, inflationRate)

write_csv(inflation_rate_clean, "data/inflation_rate_clean.csv")
