#### Multisource analysis for Cityscape ---------------------------------------

#dependencies
library(tidyverse)
library(lubridate)
library(yaml)
library(DBI)
library(sf)
library(curl)
library(reldist)

#set wd to project base folder
setwd("R:/Project/natrent-data-shop")

#store credentials at base dir of natrent-city-sub as YAML
cred <- read_yaml("./natrent0.yaml")

#create a database connection
natrent <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "natrent",
  host = "natrent0.csde.washington.edu",
  user = names(cred),
  password = cred[[names(cred)]],
  bigint = "numeric"
)

#metros of interest
metros <- c("42660", "38060", "37980", "17140", "41700")

#shapefile for counties in metros of interest
county <- st_read(natrent, 
                  query = "SELECT a.namelsad, a.cbsafp, a.geoid AS fips, b.stusps AS state_abb,
                                  CONCAT(a.namelsad, ' ', b.stusps) AS container,
                                  ST_TRANSFORM(a.geometry, 5070) AS geometry
                           FROM county17 AS a
                           JOIN state17 AS b ON a.statefp = b.statefp
                           WHERE cbsafp IN ('42660', '38060', '37980', '17140', '41700')")

#query metro information and join to county sf
county <- natrent %>% 
  tbl("cbsa17") %>%
  select(metro_name = name, cbsafp) %>%
  collect() %>%
  right_join(county) %>%
  st_as_sf()


#hrs to download
hrs <- as.double(Sys.Date() - as.Date("2020-09-30"), units = "hours")

#source ESRI geocoding wrapper
source("./arcpyWrapper.R")


#### Query apartments.com ------------------------------------------------------

#query recent data for metros of interest, dedupe lightly
apts <- natrent %>%
  tbl("geocoded") %>%
  filter(source == "Apartments.com", listing_date >= "2020-10-01", 
         cbsafp %in% metros) %>%
  collect() %>%
  distinct(countyfp, cbsafp, beds, baths, sqft, rent_mid, geo_address, .keep_all = TRUE) %>%
  rename(fips = countyfp)  %>%
  left_join(county %>% select(fips, namelsad) %>% st_drop_geometry())
  

#### Query forrent.com ---------------------------------------------------------

#query data and parse fields into appropriate data types, dedupe lightly
forrent <- natrent %>% 
  tbl("extra_markets") %>% 
  filter(website == "forrent.com") %>%
  collect() %>%
  mutate(scraped_time = parse_datetime(scraped_time),
         date = date(scraped_time),
         is_bldg = str_detect(bedrooms, "-"),
         beds = str_replace(bedrooms, "Studio", "0"),
         baths = NA,
         sqft = NA) %>%
  mutate_if(is_character,
            function(x){ifelse(x == "not available on this site", NA, x)}) %>%
  rename(address = location_identifier) %>%
  distinct(beds, baths, sqft, rent, address, .keep_all = TRUE)

#building summary
forrent %>% group_by(is_bldg) %>% tally %>% mutate(prop = n/sum(n))

#prepare the building data
forrent_bldg <- forrent %>%
  filter(is_bldg) %>%
  mutate(beds = str_remove(beds, " Beds"),
         rent = str_remove_all(rent, "$|,")) %>%
  separate(rent, c("rent_lower", "rent_upper"), sep = "-|-", remove = FALSE) %>%
  separate(beds, c("beds_lower", "beds_upper"), sep = "-|-", remove = FALSE) %>%
  mutate_at(vars(ends_with("lower"), ends_with("upper")), parse_number) %>%
  filter(!is.na(beds_lower) & !is.na(beds_upper)) %>%
  mutate(bldg_id = row_number())
  
#function to handle linear imputation of beds/rent between upper and lower values
build_forrent_grid <- function(row){
  grid <- tibble(bldg_id = forrent_bldg$bldg_id[row],
                 beds = forrent_bldg$beds_lower[row]:forrent_bldg$beds_upper[row])
  slope <- (forrent_bldg$rent_upper[row] - forrent_bldg$rent_lower[row])/(length(unique(grid$beds))-1)
  grid$rent <- forrent_bldg$rent_lower[row] + slope * (grid$beds - min(grid$beds))
  grid$beds <- as.character(grid$beds)
  grid
}

#pass each row of forrent_bldg to build_forrent_grid
forrent_bldg_grid <- map(1:nrow(forrent_bldg), build_forrent_grid)

#reduce resulting list into a single data frame
forrent_bldg_grid <- reduce(forrent_bldg_grid, bind_rows)

#append building information onto the data
forrent_bldg_grid <- forrent_bldg_grid %>%
  left_join(forrent_bldg %>% select(-rent, -beds, -baths))

#now clean up the unit data to the right column types
forrent <- forrent %>%
  filter(!is_bldg) %>%
  mutate_at(vars(rent), parse_number) %>%
  mutate(rent_lower = NA, rent_upper = NA,
         beds_lower = NA, beds_upper = NA)

#now reassemble the unit and building data into single df
forrent <- bind_rows(forrent, forrent_bldg_grid) %>%
  mutate(beds = parse_number(beds))

#forrent <- arcpy_geocode(forrent, run_fields = "address")


#### Query homes.com -----------------------------------------------------------

#query data and parse fields into appropriate data types, dedupe lightly
homes <- natrent %>% 
  tbl("extra_markets") %>% 
  filter(website == "homes.com") %>%
  collect() %>%
  filter(rent != "$--")%>%
  mutate(scraped_time = parse_datetime(scraped_time),
         date = date(scraped_time),
         #rent = parse_number(rent),,
         is_bldg = str_detect(rent, "\\+"),
         rent = parse_number(rent),
         beds = str_replace(bedrooms, "Studio", "0"),
         beds = str_extract(beds, "[0-9-.]{1,5} Bed[s]{0,1}"),
         beds = parse_number(beds),
         baths = str_extract(bedrooms, "[0-9-.]{1,5} Bath[s]{0,1}"),
         baths = parse_number(baths),
         sqft = str_remove(bedrooms, ","),
         sqft = str_extract(sqft, "[0-9-+]{1,6} Sqft"),
         sqft = parse_number(sqft)) %>%
  mutate_if(is_character,
            function(x){ifelse(x == "not available on this site", NA, x)}) %>%
  rename(address = location_identifier) %>%
  distinct(beds, baths, sqft, rent, address, .keep_all = TRUE)

#building summary
homes %>% group_by(is_bldg) %>% tally %>% mutate(prop = n/sum(n))

#homes <- arcpy_geocode(homes, run_fields = "address")


#### Query realtor.com ---------------------------------------------------------

#query data and parse fields into appropriate data types, dedupe lightly
realtor <- natrent %>% 
  tbl("extra_markets") %>% 
  filter(website == "realtor.com") %>%
  collect() %>%
  mutate(scraped_time = parse_datetime(scraped_time),
         date = date(scraped_time),
         is_bldg = str_detect(bedrooms, "-"),
         rent = parse_number(rent),
         beds = str_replace(bedrooms, "Studio", "0"),
         beds = parse_number(beds),
         baths = NA,
         sqft = NA) %>%
  mutate_if(is_character,
            function(x){ifelse(x == "not available on this site", NA, x)}) %>%
  rename(address = location_identifier) %>%
  distinct(beds, baths, sqft, rent, address, .keep_all = TRUE) %>%
  mutate(address = str_replace(address, "\n", " "))

#building summary
realtor %>% group_by(is_bldg) %>% tally %>% mutate(prop = n/sum(n))

#realtor <- arcpy_geocode(realtor, run_fields = "address")


#### Query rent.com ------------------------------------------------------------

#query data and parse fields into appropriate data types, dedupe lightly
rent <- natrent %>% 
  tbl("extra_markets") %>% 
  filter(website == "rent.com") %>%
  collect() %>%
  mutate(scraped_time = parse_datetime(scraped_time),
         date = date(scraped_time),
         availablity = str_extract(availablity, "[0-9]{1,3} Units Available"),
         is_bldg = str_detect(bedrooms, "-"),
         rent = parse_number(rent),
         beds = str_replace(bedrooms, "Studio", "0"),
         beds = str_extract(beds, "[0-9-]{3} Beds"),
         beds = parse_number(beds),
         baths = NA,
         sqft = NA) %>%
  mutate_if(is_character,
            function(x){ifelse(x == "not available on this site", NA, x)}) %>%
  rename(address = location_identifier) %>%
  distinct(beds, baths, sqft, rent, address, .keep_all = TRUE) %>%
  mutate(address = str_replace(address, "\n", " "))

#building summary
rent %>% group_by(is_bldg) %>% tally %>% mutate(prop = n/sum(n))

#rent <- arcpy_geocode(rent, run_fields = "address")


#### Load craigslist -----------------------------------------------------------

#curl_fetch_disk("http://helena-backend.us-west-2.elasticbeanstalk.com/datasets/6023",
#                "./raw/craigslist.csv")

#load data, prepare columns to appropriate type and lightly dedplue
cl <- vroom::vroom("./raw/craigslist.csv", 
               col_names = c("date", "title", "rent", "size_stats", 
                             "neighborhood", "address", "gmaps_url",
                             "beds", "baths", "sqft", "hu_char", "text",
                             "post_id", "posted_date", "updated_date", "container",
                             "url", "program_iteration")) %>%
  mutate(beds = parse_number(str_remove(beds, "BR")),
         baths = parse_number(str_remove(baths, "Ba")),
         sqft = parse_number(str_remove(sqft, "\nft\n2")),
         rent = parse_number(str_remove_all(rent, "\\$,")),
         month_str = str_extract(date, paste(month.abb, collapse = "|")),
         month = str_pad(match(month_str, month.abb), 2, "left", "0"),
         day = str_sub(date, 5, 6),
         year = str_extract(posted_date, "202[0-9]"),
         time = str_extract(date, "[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2} A{0,1}P{0,1}M"),
         date = as.Date(paste(year, month, day, sep = "-"))) %>%
  arrange(desc(date)) %>%
  distinct(beds, baths, sqft, rent, gmaps_url, .keep_all = T)

#parse the gmaps url coordinates, convert data frame to sf
cl <- cl %>%
  mutate(lat_lng = str_split_fixed(gmaps_url, "search/", n = 2)[, 2]) %>%
  separate(lat_lng, into = c("lat", "lng"), sep = ",") %>%
  mutate_at(vars(lat, lng), function(x){parse_number(x)}) %>%
  st_as_sf(coords = c("lng", "lat"), na.fail = FALSE) %>%
  st_set_crs(4326)

#point in polygon intersect listings with counties, filter to counties of interest
cl <- cl %>%
  st_transform(crs = st_crs(county)) %>%
  st_join(county %>% select(-container)) %>%
  filter(cbsafp %in% metros)


#### Load gosection8 -----------------------------------------------------------

#curl_fetch_disk("http://helena-backend.us-west-2.elasticbeanstalk.com/datasets/6000",
#                "./raw/gosection8.csv")

#load gosection8, clean up columns and dedu
gosection8 <- read_csv("./raw/gosection8.csv", 
                       col_names = c("rent", "beds_bath", "text", "address", 
                                     "availability", "container", "url", "program_iteration"))%>%
  mutate(rent = parse_number(rent),
         beds = str_replace(beds_bath, "Studio|Efficiency", "0"),
         beds = parse_number(str_extract(beds, "[0-9]\nBed")),
         baths = parse_number(str_extract(beds_bath, "[0-9.]{1,3} Bath")),
         unit_type = str_split_fixed(beds_bath, "\n", 3)[,3],
         unit_type = ifelse(unit_type == "", NA, unit_type),
         sqft = NA) %>%
  filter(container %in% county$container) %>%
  distinct(beds, baths, sqft, rent, address, .keep_all = TRUE) %>%
  left_join(st_drop_geometry(county))

#gosection8 <- arcpy_geocode(gosection8, run_fields = "address")

#gosection8 <- st_read("./geo/gosection8.shp") %>%
#  st_transform(crs = st_crs(county)) %>%
#  st_join(county) %>%
#  filter(cbsafp %in% metros)


#### Load trulia ---------------------------------------------------------------

#curl_fetch_disk("http://helena-backend.us-west-2.elasticbeanstalk.com/datasets/5874",
#                "./raw/trulia.csv")

#load data, distinguish bldg and unit ads, clean columns, filter to counties of interest and 
#lightly dedupe
trulia <- read.csv("./raw/trulia.csv", nrows = 50531,
                   col.names = c("tags", "rent", "beds", "baths", "sqft",
                                 "line_1", "line_2", "url", "program_iteration")) %>%
  mutate(container = NA) %>%
  bind_rows(read.csv("./raw/trulia.csv", skip = 50531,
                     col.names = c("tags", "rent", "beds", "baths", "sqft",
                                   "line_1", "line_2", "container", "url",
                                   "program_iteration"))) %>%
  mutate(beds = str_replace_all(beds, "Studio", "0"),
         beds = str_extract(beds, "[0-9-]{1,3}bd"),
         baths = str_remove_all(baths, "ba"),
         sqft  = str_remove_all(sqft, " sqft"),
         is_bldg = str_detect(rent, "-|-"),
         rent = str_remove_all(rent, "/mo|\\$"),
         #line_1 = str_replace(line_1, "(?<=\\#)(.*?)(?=\\,)", "\\,"),
         #line_1 = str_replace(line_1, "(?<=UNIT)(.*?)(?=\\,)", "\\,"),
         #line_1 = str_remove_all(line_1, "UNIT,|\\#,"),
         line_1 = str_split_fixed(line_1, " \\#", n = 2)[,1],
         address = paste(line_1, line_2, sep = ", "),
         fips = str_pad(parse_number(str_split_fixed(url, "for_rent/", n = 2)[, 2]), 
                        width = 5, side = "left", pad = "0")) %>%
  mutate_at(vars(baths, sqft), parse_number) %>%
  filter(fips %in% county$fips) %>%
  distinct(beds, baths, sqft, rent, address, .keep_all = TRUE) %>%
  left_join(county %>% select(-container) %>% st_drop_geometry())

#process building data
trulia_bldg <- trulia %>%
  filter(is_bldg) %>%
  separate(rent, c("rent_lower", "rent_upper"), sep = "-|-", remove = FALSE) %>%
  separate(beds, c("beds_lower", "beds_upper"), sep = "-|-", remove = FALSE) %>%
  mutate_at(vars(ends_with("lower"), ends_with("upper")), parse_number) %>%
  filter(!is.na(beds_lower) & !is.na(beds_upper)) %>%
  mutate(bldg_id = row_number())

#function to handle linear imputation of beds/rent between upper and lower values
build_trulia_grid <- function(row){
  grid <- tibble(bldg_id = trulia_bldg$bldg_id[row],
                 beds = trulia_bldg$beds_lower[row]:trulia_bldg$beds_upper[row])
  slope <- (trulia_bldg$rent_upper[row] - trulia_bldg$rent_lower[row])/(length(unique(grid$beds))-1)
  grid$rent <- trulia_bldg$rent_lower[row] + slope * (grid$beds - min(grid$beds))
  grid$beds <- as.character(grid$beds)
  grid
}

#create list of data frames with interpolated values
trulia_bldg_grid <- map(1:nrow(trulia_bldg), build_trulia_grid)

#reduce list into a single data frame
trulia_bldg_grid <- reduce(trulia_bldg_grid, bind_rows)

#append building information onto the data
trulia_bldg_grid <- trulia_bldg_grid %>%
  left_join(trulia_bldg %>% select(-rent, -beds, -baths))

#now clean up the unit data to the right column types
trulia <- trulia %>%
  filter(!is_bldg) %>%
  mutate_at(vars(rent), parse_number) %>%
  mutate(rent_lower = NA, rent_upper = NA,
         beds_lower = NA, beds_upper = NA)

#now reassemble the unit and building data into single df
trulia <- bind_rows(trulia, trulia_bldg_grid) %>%
  mutate(beds = parse_number(beds))
  
#trulia <- arcpy_geocode(trulia, run_fields = "address")


#### Load zillow ---------------------------------------------------------------

#curl_fetch_disk("http://helena-backend.us-west-2.elasticbeanstalk.com/datasets/6044",
#                "./raw/zillow.csv")

#load data, distinguish building and unit data, filter to counties of interest
zillow <- read_csv("./raw/zillow.csv", 
                   col_names = c("rent", "bedrooms", "address", "unit_type",
                                 "container", "url", "program_iteration")) %>%
  mutate(is_bldg = is.na(rent),
         unit_type = str_remove(unit_type, " for rent")) %>%
  filter(container %in% county$container)

#munge the building data to long structure where advertised rents by bed = 1 row
zillow_bldg <- zillow %>%
  filter(is_bldg) %>%
  mutate(rent_0B = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\nStudio"),
         rent_1B = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\n1 bd"),
         rent_2b = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\n2 bd"),
         rent_3b = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\n3 bd"),
         rent_4b = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\n4 bd"),
         rent_5b = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\n5 bd"),
         rent_6b = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\n6 bd"),
         rent_7b = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\n7 bd"),
         rent_8b = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\n8 bd"),
         rent_9b = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\n9 bd"),
         rent_10b = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\n10 bd"),
         rent_11b = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\n11 bd"),
         rent_12b = str_extract(bedrooms, "\\$[0-9,]{3,6}\\+{0,1}\\n12 bd")) %>%
  select(-rent) %>%
  pivot_longer(-c(address, unit_type, container, url, program_iteration, is_bldg, bedrooms),
               names_to = "beds",
               values_to = "rent") %>%
  mutate_at(vars(beds, rent), parse_number) %>%
  filter(!is.na(rent)) %>%
  mutate(sqft = NA,
         bldg_name = str_trim(str_split_fixed(address, "\\|", n = 2)[, 1]),
         address = str_trim(str_split_fixed(address, "\\|", n = 2)[, 2]))

#prepare the unit data
zillow <- zillow %>%
  filter(!is_bldg) %>%
  mutate(rent = parse_number(rent),
         beds = str_replace_all(bedrooms, "Studio", "0"),
         beds = parse_number(beds),
         sqft = parse_number(str_extract(bedrooms, "[0-9,]{3,6}\\nsqft")),
         bldg_name = NA,
         address = str_replace(address, "(?<=\\#)(.*?)(?=\\,)", "\\,"),
         address = str_replace(address, "(?<=UNIT)(.*?)(?=\\,)", "\\,"),
         address = str_remove_all(address, "UNIT,|\\#,"))

#join the building and unit data back together
zillow <- bind_rows(zillow, zillow_bldg) %>%
  mutate(baths = NA) %>%
  distinct(beds, baths, sqft, rent, address, .keep_all = TRUE) %>%
  left_join(county)

#zillow <- arcpy_geocode(zillow, run_fields = "address")


#### Load padmapper ------------------------------------------------------------

#curl_fetch_disk("http://helena-backend.us-west-2.elasticbeanstalk.com/datasets/6188",
#                "./raw/padmapper.csv")

#load the padmapper data, distinguish building and unit ads, filter to counties of interest
pad <- read_csv("./raw/padmapper.csv",
                col_names = c("rent", "bedrooms", "name_loc", "name",
                              "rent_inv_0B", "rent_inv_1B", "rent_inv_2B", 
                              "rent_inv_3B",   "rent_inv_4B", "text", "container", 
                              "url", "program_iteration")) %>%
  mutate(is_bldg = str_detect(rent, "\\+"),
         bedrooms = str_replace(bedrooms, "Studio", "0"),
         name_loc = str_trim(str_remove(name_loc, "info"))) %>%
  mutate_if(is_character, function(x){str_trim(str_replace_all(x, "\n", " "))}) %>%
  filter(container %in% county$container)

#process the building data to a clean form where each advertised rent by bed = 1 row w/inv counts
pad_bldg <- pad %>%
  filter(is_bldg) %>%
  mutate(loc = str_trim(str_remove(name_loc, name))) %>%
  mutate_at(vars(starts_with("rent_inv")), 
            function(x){str_remove_all(x, "Studios |[1-4] Bedrooms | View")}) %>%
  mutate_at(vars(starts_with("rent_inv")),
            list(inv = ~ str_split_fixed(., " RENT", n = 2)[, 1],
                 rent = ~ str_remove_all(str_split_fixed(., " RENT ", n = 2)[, 2], "\\$|,"))) %>%
  rename_at(vars(ends_with( "_rent")), 
            ~ paste("rent", gsub("rent_inv_|_rent", "", .), sep = "_")) %>%
  rename_at(vars(ends_with( "_inv")), 
            ~ paste("inv", gsub("rent_inv_|_inv", "", .), sep = "_")) %>%
  select(-starts_with("rent_inv"), -rent) %>%
  filter(!is.na(name)) %>%
  pivot_longer(-c(name, loc, text, container, url, program_iteration, is_bldg, 
                  name_loc, bedrooms),
               names_to = "var_beds", values_to = "value") %>%
  separate(var_beds, into = c("var", "beds"), sep = "_") %>%
  distinct(name, text, container, url, program_iteration, is_bldg,
           loc, beds, var, .keep_all = TRUE) %>%
  pivot_wider(id_cols = c(name, text, container, url, program_iteration, is_bldg,
                          loc, beds, bedrooms, name_loc),
              names_from = var,
              values_from = value) %>%
  mutate_at(vars(beds, inv), parse_number) %>%
  filter(!is.na(inv)) %>%
  mutate(baths = NA, 
         line_1 = NA, 
         line_2 = NA,
         address = loc)

#process the unit data
pad <- pad %>%
  filter(!is_bldg) %>%
  select(-starts_with("rent_inv")) %>%
  mutate(rent = str_remove_all(rent, "\\$|,"),
         beds = str_replace(bedrooms, "Studio", "0"),
         beds = parse_number(str_extract(beds, "[0-9]{1,2} Bedroom")),
         baths = parse_number(str_extract(bedrooms, "[0-9]{1,2} Bathroom"))) %>%
  mutate(loc = str_replace_all(name_loc, "\\*", ""),
         line_1 = str_trim(str_split_fixed(name, "\\#|UNIT|APT", n = 2)[,1]),
         line_2 = str_trim(str_remove(name_loc, str_remove(name, "\\*"))),
         inv = 1,
         address = paste(line_1, line_2, sep = ", "))

#combine the building and unit data back together
pad <- bind_rows(pad, pad_bldg) %>%
  mutate(sqft = NA,
         rent_mult = ifelse(str_detect(rent, 'K'), 1e3, 1), 
         rent = rent_mult * parse_number(str_remove(rent, 'K|M'))) %>%
  distinct(beds, baths, sqft, rent, address, .keep_all = TRUE)

#geocode the data
pad <- arcpy_geocode(pad, run_fields = "address")

#append the metro info and filter to cases within 5 metros of interest
pad <- pad %>%
  st_as_sf(coords = c("X", "Y"), remove = FALSE) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(county)) %>%
  st_join(county %>% select(-container)) %>%
  filter(cbsafp %in% metros) %>%
  st_drop_geometry()


#### Load 2018 ACS estimates ---------------------------------------------------

acs_sum <- read_csv("./census/msa_contractrent_2bed_acs_2018.csv", n_max = 5) %>%
  rename(metro_name = msa) %>%
  left_join(county %>% st_drop_geometry() %>% distinct(metro_name, cbsafp)) %>%
  rename_at(vars(starts_with("pct_")), function(x){str_replace(x, "pct_", "q")}) %>%
  mutate(cat_beds = "2", source = "2018 ACS", n = NA)


#### Combine viable sources for rent estimation, summarize ---------------------

#prepare a subset of each source for combined listing table
cl_sum <- cl %>%
  st_drop_geometry() %>%
  select(cbsafp, fips, namelsad, beds, rent) %>%
  mutate(source = "Craigslist",  inv = 1) 

apts_sum <- apts %>%
  rename(rent = rent_mid) %>%
  select(cbsafp, fips, namelsad, beds, rent) %>%
  mutate(source = "Apartments.com", inv = 1)   

gos8_sum <- gosection8 %>%
  select(cbsafp, fips, namelsad, beds, rent) %>%
  mutate(source = "GoSection8", inv = 1) 

trulia_sum <- trulia %>%
  select(cbsafp, fips, namelsad, beds, rent) %>%
  mutate(source = "Trulia", inv = 1)

zillow_sum <- zillow %>%
  select(cbsafp, fips, namelsad, beds, rent) %>%
  mutate(source = "Zillow", inv = 1)

pad_sum <- pad %>%
  select(cbsafp, fips, namelsad, beds, rent, inv) %>%
  mutate(source = "Padmapper")

forrent_sum <- pad %>%
  select(cbsafp, fips, namelsad, beds, rent) %>%
  mutate(source = "Forrent.com", inv = 1)

#combine the listings and prepare for analysis
listings <- bind_rows(cl_sum, apts_sum, gos8_sum, trulia_sum, zillow_sum, 
                      pad_sum, forrent_sum) %>%
  filter(!is.na(beds), !is.na(rent), 
         rent > 50, rent < 15000) %>%
  mutate(cat_beds = case_when(
    beds == 0 ~ "0",
    beds == 1 ~ "1",
    beds == 2 ~ "2",
    beds == 3 ~ "3",
    beds >= 4 ~ "4+"
  )) %>%
  left_join(county %>% st_drop_geometry() %>% distinct(cbsafp, metro_name))

#compute rent dist stats for each bedroom size, append ACS 2B distribution
sum_tbl <- listings %>%
  group_by(cbsafp, metro_name, source, cat_beds) %>% 
  summarize(n = sum(inv),
            q5 = wtd.quantile(rent, .05, weight = inv),
            q10 = wtd.quantile(rent, .10, weight = inv),
            q25 = wtd.quantile(rent, .25, weight = inv),
            q40 = wtd.quantile(rent, .40, weight = inv),
            q50 = wtd.quantile(rent, .50, weight = inv),
            q75 = wtd.quantile(rent, .75, weight = inv),
            q90 = wtd.quantile(rent, .90, weight = inv),
            q95 = wtd.quantile(rent, .95, weight = inv)) %>%
  bind_rows(acs_sum) %>%
  mutate(iqr = q75 - q25)


#### Save summary table to disk ------------------------------------------------

write_csv(listings, "./output/multisource_listing_table.csv")
write_csv(sum_tbl, "./output/multisource_summary_table.csv")

