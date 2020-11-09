#### Wrapper and related elements for geocoding/geoparsing ---------------------
#### natrent @ UW

#functions to handle/support the geocoding process using ArcPy as primary geocoder
#for national rental data scraping with Smartystreets as primary geoparser for ads
#where we need to extract addresses from the listed text

arcpy_rev_geocode <- function(sf, debug = FALSE){
  
  #save some values for the geocoding script call
  LISTSRC <- config$SOURCEABB
  LISTLOC <- config$SCRIPT_ID
  PYPATH <- config$PYPATH
  rev_geocode_script <- paste0(config$PATH, "/scripts/arcpyRevGeocode.py")
  input_shp <- paste0(config$PATH, "/data/geo/ESRI/", LISTSRC, "_", LISTLOC, "_rev_geocode_arcpy.shp")
  output_shp <- paste0(config$PATH, "/data/geo/ESRI/", LISTSRC, "_", LISTLOC, "_rev_geocoded")
  
  #if there are files from failed runs in the ESRI working folder, remove them
  file.remove(Sys.glob(paths = paste0(config$PATH, "/data/geo/ESRI/", LISTSRC, "_", LISTLOC, "_*")))
  
  #reduce the table to unique lat/long combinations to be geocoded
  rev_geocode_coords <- sf %>%
    select(gmaps_lat, gmaps_lng, geometry) %>%
    distinct(gmaps_lat, gmaps_lng, geometry)
    
  #make sure the config's PYPATH is appropriate for ArcPy
  if(!grepl("x64", config$PYPATH)){
    stop("Need to use 64-bit Python and ArcGIS Geoprocessing Tools")
  }
  
  #write the geocode table to storage with the corresponding name
  st_write(rev_geocode_coords, dsn = input_shp, 
           driver = "ESRI Shapefile", quiet = !debug)
  
  #give us three tries to get past ArcGIS gremlines
  gc_tries <- 1
  gc_result <- NULL
  sleep_time <- 20
  
  #Python2.7 script to use ArcPy.GeocodeAddresses_geocoding
  while(gc_tries <= 3 && is.null(gc_result)){
    gc_result <- try(system(paste(PYPATH, rev_geocode_script, input_shp, output_shp)))
    
    if(inherits(gc_result, "try-error")){
      
      #a lot of observed bugs result from what looks like licensing issues
      #i.e. ArcGIS thinks we are using more licenses than we are allowed
      Sys.sleep(sleep_time)
      
      #reset the gc_result object for another run, mark that we used an attempt
      gc_result <- NULL
      gc_tries <- gc_tries + 1
    }
  }
  
  #read in the point data shapefile that was produced
  outcome <- try(read_sf(paste0(output_shp, ".shp"),
                         layer = paste0(LISTSRC, "_", LISTLOC, "_rev_geocoded"),
                         stringsAsFactors = FALSE),
                 silent = !debug)

  #end function call based on whether outcome exists / was unsuccessful
  if(!inherits(outcome, "try-error")){
    
    #turn outcome object into data.frames
    outcome <- st_drop_geometry(outcome)
    
    #clean up the resulting outcome df a bit
    outcome <- outcome %>%
      rename_at(vars(starts_with("REV_")), tolower) %>%
      rename(rev_address = rev_addres, rev_zip = rev_postal) %>%
      mutate_at(c("rev_address", "rev_city", "rev_region"), toupper) 
    
    #now join this back to the original, undeduplicated table
    sf <- suppressWarnings(suppressMessages(left_join(sf, outcome)))
    
    #return the tbl that now has geocode fields
    sf
    
    #if we had three unsuccessful runs and/or couldn't read in the shp we expected
  } else{
    
    #stop and print the last observed error
    stop(paste("ArcGIS reverse geocoding was stopped after 3 unsuccessful attempts.\n\nLast Error:\n", 
               as.character(outcome)))
  }
}

arcpy_geocode <- function(tbl, run_fields = NULL, debug = FALSE){
  
  #path to python
  PYPATH <- "C:/Python27/ArcGISx6410.7/python.exe"

  #geocode script loc
  geocode_script <- "./arcpyGeocode.py"
  
  #grab the object name of what was passed as tbl
  source_name <- deparse(substitute(tbl))

  #remove all files from prior runs
  file.remove(Sys.glob(paste0("./geo/", source_name, ".*")))
  
  #filenames for run
  input_tbl <- paste0("./geo/", source_name, ".csv")
  output_shp <- paste0("./geo/", source_name, ".shp")
  
  #make sure we have the right arguments to proceed
  if(is.null(run_fields)){
    stop("Need to specify the address, city and state fields as ordered character vector.")
  }
  
  #make sure the run_field columns are character to avoid join errors
  tbl[unique(run_fields)] <- lapply(tbl[unique(run_fields)], as.character)

  #reduce the table to unique combinations to be geocoded
  geocode_tbl <- tbl %>%
    select(all_of(run_fields)) %>%
    distinct()
  
  #write the geocode table to storage with the corresponding name
  write_csv(geocode_tbl, input_tbl, na = " ")
  
  run_fields_input <- paste(run_fields, collapse = " ")
  
  #give us three tries to get past ArcGIS gremlines
  gc_tries <- 1
  gc_result <- NULL
  sleep_time <- 20
  
  #Python2.7 script to use ArcPy.GeocodeAddresses_geocoding
  #cat(paste("Command:", PYPATH, geocode_script, input_tbl, output_shp, run_fields_input))
  
  while(gc_tries <= 3 && is.null(gc_result)){
    gc_result <- try(system(paste(PYPATH, geocode_script, input_tbl, output_shp, 
                                  run_fields_input)))
    
    if(inherits(gc_result, "try-error")){
      
      #a lot of observed bugs result from what looks like licensing issues
      #i.e. ArcGIS thinks we are using more licenses than we are allowed
      Sys.sleep(sleep_time)
      
      #reset the gc_result object for another run, mark that we used an attempt
      gc_result <- NULL
      gc_tries <- gc_tries + 1
    }
  }
  
  #read in the point data shapefile that was produced
  outcome <- try(read_sf(output_shp,
                         stringsAsFactors = FALSE),
                 silent = !debug)
  
  #end function call based on whether outcome exists / was unsuccessful
  if(!inherits(outcome, "try-error")){
    
    #if it does exist, we only need the tabular data
    outcome$geometry <- NULL
    
    #rename input fields
    colnames(outcome[(ncol(outcome)-length(run_fields)+1):ncol(outcome)]) <- run_fields
    
    #make sure the run_field columns are character to avoid join errors
    outcome[unique(run_fields)] <- lapply(outcome[unique(run_fields)], as.character)
    
    #silently join this back to the geocoding table that was input
    geocode_tbl <- suppressWarnings(suppressMessages(left_join(geocode_tbl, outcome)))
    
    #now join this back to the original, undeduplicated table
    tbl <- suppressWarnings(suppressMessages(left_join(tbl, geocode_tbl, by = run_fields)))
    
    #return the tbl that now has geocode fields
    tbl
    
    #if we had three unsuccessful runs and/or couldn't read in the shp we expected
  } else{
    
    #stop and print the last observed error
    stop(paste("ArcGIS geocoding was stopped after 3 unsuccessful attempts.\n\nLast Error:\n", 
               as.character(outcome)))
  }
}

smarty_geoparse <- function(tbl, debug = FALSE){
  #set the arguments to pass via cmd
  LISTSRC <- config$SOURCEABB
  LISTLOC <- config$SCRIPT_ID
  #LISTLOC <- substr(config$LOCABB, 1, 3)
  PYPATH <- config$PYPATH
  geoparse_script <- paste0(config$PATH, "/scripts/addressExtraction.py")
  input_tbl <- paste0(config$PATH, "/data/geo/Smarty/", LISTSRC, "_", LISTLOC, "_to_geoparse_smarty.csv")
  output_tbl <- paste0(config$PATH, "/data/geo/Smarty/", LISTSRC, "_", LISTLOC, "_craigslist_data_processed.csv")
  #parse_report <- paste0(config$PATH, "/log/reports/", today, "/", LISTSRC, "_", LISTLOC, ".txt")
  
  #if there are files from failed runs in the Smarty working folders, remove them
  file.remove(Sys.glob(paths = paste0(config$PATH, "/data/geo/Smarty/", LISTSRC, "_", LISTLOC, "*")))
  
  geoparse_tbl <- tbl %>% select(address1, neigh1, city1, region1, google_maps_url, listing_text)
  
  #save the temp table to storage
  write_csv(geoparse_tbl, input_tbl)
  
  #announce we are sending flat file to addressExtraction.py
  report("Starting API calls to Smartystreets for address validation/extraction...")
  
  #Python2.7 script to use Smartystreets Python SDK
  suppressMessages(system(command=paste(PYPATH, geoparse_script, input_tbl, output_tbl, LISTSRC, LISTLOC, today)))
  
  #read in the result
  outcome <- read_csv(output_tbl, col_types = cols())
  
  #rename columns related to geocoding
  outcome <- outcome %>%
    rename(match_type = address_category, 
           lng = lon,
           match_address = l1,
           match_address2 = l2) %>%
    #need to make the zipcode 5 digits long since to match Arc format
    mutate(zip5 = substr(str_extract(match_address2, '(?<!\\d)\\d{5}(?:[ -]\\d{4})?\\b'), 1, 5),
           match_address2 = str_replace(match_address2, pattern = '(?<!\\d)\\d{5}(?:[ -]\\d{4})?\\b', zip5),
           #program_iteration = as.character(program_iteration),
           temp_add = paste(match_address, match_address2),
           #dummy = "<NA>"
           ) %>%
    select(-zip5)
  
  #implement a call to arcpyGeocode() to improve on the coordinate precision
  arc_processed <- arcpyGeocode(outcome, 
                                run_fields = "temp_add",
                                stage = "5a")
  
  arc_processed <- arc_processed %>% 
    mutate(address1 = as.character(address1),
           neigh1 = as.character(neigh1),
           city1 = as.character(city1),
           region1 = as.character(region1),
           google_maps_url = as.character(google_maps_url),
           listing_text = as.character(listing_text))
  
  result_tbl <- suppressWarnings(suppressMessages(left_join(tbl, arc_processed) %>% distinct()))
  
  #clean the temporary files
  file.remove(Sys.glob(paths = paste0(config$PATH, "/data/geo/ESRI/", LISTSRC, "_", LISTLOC, "*")))
  
  #return the outcome table
  result_tbl
}
