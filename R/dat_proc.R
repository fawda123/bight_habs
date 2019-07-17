# datproc

# front matter ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(here)

prj <- 4326 # wgs84

# master data proc --------------------------------------------------------

wqdat <- read_excel('data/raw/DA retro_Carondata_May 2019.xlsx', sheet = 'Master List', 
                  col_types = c('numeric', 'numeric', 'text', 'date', 'text', 'text', 'numeric', 'text', 'text', 'text',
                                'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
                                'numeric', 'numeric', 'numeric', 'text'),
                  range = 'A1:V4575') %>% 
  dplyr::select(-`Serial number`, -`Already Published?`) %>% 
  mutate(
    Lat = gsub('N$', '', Lat),
    Lat = case_when(
      grepl('\\s', Lat) ~ strsplit(Lat, '\\s') %>% map(., function(x) paste0(x[1], gsub('^0\\.', '.', as.character(as.numeric(x[2])/60)))) %>% unlist,
      T ~ Lat
    ),
    Lat = as.numeric(Lat),
    Long= gsub('W$', '', Long),
    Long = case_when(
      grepl('\\s', Long) ~ strsplit(Long, '\\s') %>% map(., function(x) paste0(x[1], gsub('^0\\.', '.', as.character(as.numeric(x[2])/60)))) %>% unlist,
      T ~ Long
    ),
    Long = as.numeric(Long),
    Long = ifelse(Long > 0, -1 * Long, Long), 
    Month = month(`Full Date`, label = T, abbr = F),
    Year = year(`Full Date`)
  ) 

save(wqdat, file = 'data/wqdat.RData', compress = 'xz')


# time series data --------------------------------------------------------

# note that wq dat has some of the sites but the data here are more complete

tsdat <- read_csv(here('data/raw', 'SCCOOS shore stations_2008_2019.csv'), na = c('NaN', 'na', 'NA'), col_types = cols()) %>% 
  rename(
    Lat = latitude, 
    Long = longitude, 
    `Water Temperature (C)` = `Water Temperature (<U+00B0>C)`
  ) %>% 
  unite(date, year, month, day, sep = '-', remove = F) %>% 
  mutate(
    date = ymd(date),
    qrt = quarter(date),
    qrt = factor(qrt, levels = c('1', '2', '3', '4'), labels = c('JFM', 'AMJ', 'JAS', 'OND'), ordered = T)
  )
                    
save(tsdat, file = here('data', 'tsdat.RData'), compress = 'xz')
