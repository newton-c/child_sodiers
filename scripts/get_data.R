library(countrycode)
library(tidyverse)
library(Amelia)
library(haven)
library(readxl)
library(WDI)

set.seed(977634)

load(unzip("data/ged211-RData.zip"))


ged <- GEDEvent_v21_1 %>%
    filter(type_of_violence != 1) %>%
    select(year, active_year, conflict_new_id, conflict_name, dyad_new_id,
           dyad_name, side_a, side_b, country, region, best) %>%
    group_by(year, active_year, conflict_new_id, conflict_name, dyad_new_id,
             dyad_name, side_a, side_b, country, region) %>%
    summarize(deaths = sum(best)) %>%
    mutate(cumulative_deaths = cumsum(deaths),
           total_active_years = cumsum( active_year))

# Child sodlier
load("data/Replication_Forced.RData")
cs <- table
rm(table)

dyad_years <- cs %>%
    select(dyadid, endyear, ccodecow, Csdum, Csindex, nr_anystrategy,
           duration, bdeaths) %>%
    group_by(dyadid, endyear, ccodecow) %>%
    summarise(Csdum = max(Csdum), Csindex = max(Csindex),
              resources = max(nr_anystrategy),
              duration = max(duration)) %>%
    rowwise() %>%
    mutate(year = list(seq(1990, 2020, by = 1))) %>%
    unnest(cols = c(year)) %>%
    ungroup()

# Homicide data
homc <- read_xlsx("data/homicide_country_download.xlsx") %>%
    filter(Gender == "Total (all ages)",
           Indicator == "Homicide: # of victims",
           Unit == "Count") %>%
    rename(year = Year, hom_count = Value) %>%
    select(country, year, hom_count)

homr <- read_xlsx("data/homicide_country_download.xlsx") %>%
    filter(Gender == "Total (all ages)",
           Indicator == "Homicide: # of victims",
           Unit != "Count") %>%
    rename(year = Year, hom_rate = Value) %>%
    select(country, year, hom_rate)

hom <- inner_join(homr, homc)
hom$ccode <- countrycode(sourcevar = hom$country, origin = "country.name",
                         destination = "cown")
hom <- filter(hom, !is.na(ccode))

dat <- left_join(dyad_years, hom, by = c("ccodecow" = "ccode", "year" = "year"))
dat <- dat %>%
    mutate(hom_count = as.numeric(hom_count),
    #mutate(hom_rate = as.numeric(hom_rate),
           postcon = ifelse(year >= endyear, 1, 0))

# World Bank Data
wb_data <- WDI(indicator = c('popslums' = 'EN.POP.SLUM.UR.ZS',
                             'education' = 'SE.XPD.TOTL.GD.ZS'),
               start = 1990, end = 2020)

wb_data$ccodecow <- countrycode(sourcevar = wb_data$iso3c, origin = "iso3c",
                             destination = "cown") 
wb_data <- select(wb_data, ccodecow, year, popslums, education) %>%
           filter(!is.na(ccodecow) & !is.na(year)) %>%
           unique()

dat_total <- left_join(dat, wb_data) %>% unique()

#test
rstanarm::stan_glm.nb(hom_count ~ resources + Csdum,
                      data = subset(dat, postcon == 1))
