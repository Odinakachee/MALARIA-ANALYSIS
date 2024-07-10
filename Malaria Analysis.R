#MALARIA ANALYSIS

install.packages('tidyverse')
library(tidyverse)

malaria_inc <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-13/malaria_inc.csv')

view(malaria_inc)

malaria_inc_processed <- malaria_inc %>%
  set_names(c('country', 'code', 'year', 'incidence')) %>%
  mutate(incidence = incidence/1000)
view(malaria_inc_processed)

malaria_inc_processed %>%
  filter(country%in% sample(unique(country), 6)) %>%
  ggplot(aes(year, incidence, color = country)) +
  geom_line()

malaria_inc_processed %>%
  filter(country%in% sample(unique(country), 6)) %>%
  ggplot(aes(year, incidence, color = country)) +
  geom_line() +
  scale_y_continuous(labels = scales :: percent_format())


malaria_spread <- malaria_inc_processed %>%
  mutate(year = paste0('Y', year)) %>%
  spread(year, incidence)

view(malaria_spread)


malaria_spread %>%
  filter(country != 'Turkey',
         !is.na(code)) %>%
  mutate(current = Y2015, change = Y2015 - Y2000) %>%
  ggplot(aes(current,  change)) +
  geom_point() +
  geom_text(aes(label = code), vjust = 1, hjust = 1) +
  scale_x_continuous(labels = scales:: percent_format()) +
  scale_y_continuous(labels = scales:: percent_format())


#to check NIgeria change in malaria occurrence

malaria_inc_processed %>%
  filter(country == 'Nigeria') %>%
  ggplot(aes(year, incidence, color = country)) +
  geom_line() +
  scale_y_continuous(labels = scales :: percent_format())

#ETHIOPIA
malaria_inc_processed %>%
  filter(code == 'ETH') %>%
  ggplot(aes(year, incidence, color = country)) +
  geom_line() +
  scale_y_continuous(labels = scales :: percent_format())

malaria_inc_processed %>%
  filter(code == 'TLS') %>%
  ggplot(aes(year, incidence, color = country)) +
  geom_line() +
  scale_y_continuous(labels = scales :: percent_format())


#thursday 4th

install.packages('maps')
library(maps)


world <- map_data('world') %>%
  filter(region != 'Antarctica')

maps :: iso3166 %>%
  select(a3, mapname)

#merging the three data to get the longitude and latitude 

malaria_inc_processed %>%
  filter(incidence < 1) %>%
  inner_join(maps::iso3166 %>%
               select(a3, mapname), by = c(code = 'a3'))%>%
  inner_join(world, by = c(mapname = 'region')) %>% 
ggplot(aes(long, lat, group = group, fill = incidence)) +
  geom_polygon()+
  scale_fill_gradient2(low = 'blue', high = 'red', midpoint = 0.20, labels =  scales::percent_format())+
  coord_flip()+
  facet_wrap(~year)
  
#malaria death cases

malaria_deaths <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-13/malaria_deaths.csv')

view(malaria_deaths)
malaria_deaths_processed <- malaria_deaths %>%
  setNames(c('country', 'code', 'year', 'deaths'))

malaria_deaths_processed %>%
  filter(country %in% sample(unique(country), 6)) %>%
  ggplot(aes(year, deaths, color = country)) +
  geom_line() +
  labs(y = 'deaths per 100,000') +
  scale_y_continuous(labels =  scales:: percent_format())


install.packages('fuzzyjoin')
install.packages('stringr')

library(stringr)
library(fuzzyjoin)

malaria_country_data <- malaria_deaths_processed %>%
  inner_join(maps :: iso3166 %>%
               select(a3, mapname), by = c(code = 'a3')) %>%
  mutate(mapname = str_remove(mapname, '//C.*'))

malaria_map_data <- map_data('world') %>%
  filter(region != 'Antarctica') %>%
  tbl_df() %>%
  inner_join(malaria_country_data, by =  c(region = 'mapname'))

malaria_map_data %>%
  ggplot(aes(long, lat, group = group, fill = deaths)) +
  geom_polygon() +
  scale_fill_gradient2(low = 'blue', high = 'red', midpoint = 100) +
  theme_void() +
  labs(title = 'Malaria deaths over time around the world',
       fill = 'deaths per 10,000')

install.packages('countrycode')
library(countrycode)
install.packages('gganimate')
library(gganimate)


animation <- malaria_map_data %>%
  mutate(continent = countrycode(code, 'iso3c', 'continent')) %>%
filter(continent == 'Africa') %>%
  ggplot(aes(long, lat, group = group, fill = deaths)) +
  geom_polygon() +
  scale_fill_gradient2(low = 'blue', high = 'red', midpoint = 100) +
  theme_void() +
  transition_manual(year) +
  labs(title = 'Malaria deaths over time in Africa ((current_frame))', fill = 'deaths per 100,000')

animate(animation, nframes = 300, fps = 10, renderer = gifski_renderer('test.gif'))




