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

save(wqdat, file = here('data' , 'wqdat.RData'), compress = 'xz')


# time series data --------------------------------------------------------

# note that wq dat has some of the sites but the data here are more complete

tsdat <- read_csv(here('data/raw', 'SCCOOS shore stations_2008_2019.csv'), na = c('NaN', 'na', 'NA'), col_types = cols()) %>% 
  unite(date, year, month, day, sep = '-', remove = F) %>% 
  filter(!location %in% 'Monterey Wharf') %>% 
  rename(
    Lat = latitude, 
    Long = longitude, 
    temp = `Water Temperature (<U+00B0>C)`,
    chla = `Chlorophyll (mg/m3)`,
    da = `Domoic Acid (ng/mL)`,
    pntot = `Pseudo-nitzschia Total Cells (cells/L)`,
    pndel = `Pseudo-nitzschia delicatissima group (cells/L)`,
    pnser = `Pseudo-nitzschia seriata group (cells/L)`,
    phosphorus = `Phosphate (uM)`,
    silicate = `Silicate (uM)`, 
    nitrate = `Nitrate (uM)`,
    nitrite = `Nitrite (uM)`,
    ammonia = `Ammonia (uM)`,
    depth = `depth (m)`
  ) %>% 
  mutate(
    location = reorder(location, -Lat), # location as factor by latitude
    date = ymd(date),
    dec_time = decimal_date(date),
    qrt = quarter(date),
    qrt = factor(qrt, levels = c('1', '2', '3', '4'), labels = c('JFM', 'AMJ', 'JAS', 'OND'), ordered = T), 
    din = ifelse(
      rowSums(cbind(is.na(nitrate), is.na(nitrite), is.na(ammonia))) == 3, NA, 
      rowSums(cbind(nitrate, nitrite, ammonia), na.rm = T)
      ), # din, sum all even if NA, unless all NA, then NA
    ntop = din / phosphorus, # nitrogen to phosphorus ratio
    ntop = ifelse(is.infinite(ntop), NA, ntop),
    siton = silicate / din, # silicate to nitrogen ratio
    siton = ifelse(is.infinite(siton), NA, siton),
    temp = ifelse(temp < 5, NaN, temp) # remove some temp outliers
  ) %>% 
  dplyr::select(-depth)

# get temp anomalies by fitting seasonal model and taking difference
tsdat <- tsdat %>% 
  group_by(location) %>% 
  mutate(
    tempseas = predict(lm(temp ~ sin(2*pi*dec_time) + cos(2*pi*dec_time), na.action = na.exclude)), 
    tempanom = temp - tempseas
  ) %>% 
  ungroup

# upwelling index data
# https://oceanview.pfeg.noaa.gov/products/upwelling/dnld

# get upwelling data for stations 10, 11
upwell <- tibble(
  stat = c(10, 11), 
  url = c(
    'https://oceanwatch.pfeg.noaa.gov/products/PFELData/upwell/daily/p10dayac.all',
    'https://oceanwatch.pfeg.noaa.gov/products/PFELData/upwell/daily/p11dayac.all'
    )
  ) %>% 
  group_by_all() %>% 
  mutate(
    ind = map(url, function(x){
      
      datin <- readLines(x) 
      
      out <- datin %>% 
        .[-c(1:6)] %>% 
        tibble(dat = .) %>% 
        separate(dat, c('date', 'ind'), sep = '\\s+') %>% 
        mutate(
          date = ymd(date),
          ind = ifelse(ind == -9999, NA, ind), 
          ind = as.numeric(ind)
        )
      
      return(out)
      
    })
  ) %>% 
  ungroup %>% 
  select(-url)

save(upwell, file = here('data', 'upwell.RData'), compress = 'xz')

# locations of upwelling estimates
locs <- tibble(
    lat = c(60, 60, 57, 54, 51, 48, 45, 42, 39, 36, 33, 30, 27, 24, 21),
    lon = c(149, 146, 137, 134, 131, 125, 125, 125, 125, 122, 119, 119, 116, 113, 107),
    stat = seq(1:15)
  ) %>% 
  mutate(lon = -1 * lon) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = prj)

# habs monitoring station locations
# st_nearest_feature gets index of nearest station in locs
tsstat <- tsdat %>% 
  select(location, Long, Lat) %>% 
  unique %>% 
  st_as_sf(., coords = c('Long', 'Lat'), crs = prj) %>% 
  mutate(
    stat = st_nearest_feature(., locs)
  ) %>% 
  st_set_geometry(NULL)

# join ts and station data with upwelling data
tsstat <- tsstat %>% 
  left_join(upwell, by = 'stat') %>% 
  unnest %>% 
  select(-stat) %>% 
  rename(upwell = ind)

# join tsstat to tsdat
tsdat <- tsdat %>% 
  left_join(tsstat, by = c('date', 'location'))

save(tsdat, file = here('data', 'tsdat.RData'), compress = 'xz')
