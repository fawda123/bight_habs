# datproc

# front matter ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(sf)

prj <- 4326 # wgs84

# master data proc --------------------------------------------------------

dat <- read_excel('data/raw/DA retro_Carondata_March 2019.xlsx', sheet = 'Master List', 
                  col_types = c('numeric', 'numeric', 'text', 'date', 'text', 'text', 'numeric', 'text', 'text', 'text',
                                'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
                                'numeric', 'numeric', 'numeric', 'text'),
                  range = 'A1:V4575') %>% 
  dplyr::select(-`Serial number`, -`Project ID`, -`Station ID`, -`Already Published?`) %>% 
  mutate(
    Lat = gsub('N$', '', Lat),
    Lat = case_when(
      grepl('\\s', Lat) ~ strsplit(Lat, '\\s') %>% map(., function(x) paste0(x[1], gsub('^\\0\\.', '', as.character(as.numeric(x[2])/60)))) %>% unlist,
      T ~ Lat
    ), 
    Lat = as.numeric(Lat),
    Long= gsub('W$', '', Long),
    Long = case_when(
      grepl('\\s', Long) ~ strsplit(Long, '\\s') %>% map(., function(x) paste0(x[1], gsub('^\\0\\.', '', as.character(as.numeric(x[2])/60)))) %>% unlist,
      T ~ Long
    ),
    Long = as.numeric(Long), 
    Long = ifelse(Long > 0, -1 * Long, Long)
  ) 

datgeo <- dat %>% 
  filter(!is.na(Lat)) %>% 
  st_as_sf(., coords = c('Long', 'Lat'), crs = prj)
