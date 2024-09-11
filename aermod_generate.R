# function to read in CSV, output AERMOD input file

## to do:
  ## go by facility, use a filler value of 1 for emissions rate. results will be multiplied by fraction later
  ## find out format of receptor locations and rewrite them into something AERMOD can use
  ## curious what restraints AERMOD has on formatting location data (limit the number of entries?)
  ## what's an example of the meteorology data that we use?




get_facility_title <- function(facility_name) {
  require(stringr) 

  
  
  facility_name %>% 
    str_remove_all("\"\"") %>% 
    str_remove_all(" -") %>% 
    str_replace_all(" ", "_") %>% 
    str_remove_all("\\/")
  
}

write_source_input <- function(input_row, individual = TRUE, all = TRUE, source = "point", elev = T,
                               source_data = point_sources_sc,
                               title_two = "TESTING",
                               model_opt = "CONC FASTALL",
                               aver_time = "ANNUAL",
                               pollut_id = "OTHER",
                               run_or_not = "RUN",
                               urban_src = "ALL",
                               source_type = "POINT",
                               surf_file = "/pln5/CMAQ/CMAQv5.0.2/UTIL/MMIFv3.4/AERMOD.2018.NRCA60.sfc",
                               prof_file = " /pln5/CMAQ/CMAQv5.0.2/UTIL/MMIFv3.4/AERMOD.2018.NRCA60.pfl",
                               surf_data =  "99999  2018  WRF I78J62",
                               uair_data =  "99999  2018  WRF I78J62",
                               site_data = "99999  2018  WRF I78J62",
                               prof_base = "0.0  METERS",
                               start_end = "2018 01 01 2018 12 31",
                               rect_table = "ALLAVE 1ST",
                               summ_file = "PLACEHOLDER",
                               post_file = "PLACEHOLDER",
                               plot_file = "PLACEHOLDER"
                               ) {
  
  require(aermod)
  require(oce)
  require(sf)
  require(data.table)
  
  
  facilities <- unique(source_data$FACILITY_NAME) # get all the unique facility names
  
  # nested loops: facility -> pollutant
  lapply(facilities, \(f) {
    
    facility_title <- get_facility_title(f) # extract title

    print(facility_title)
    
    # isolate individual facility
    facility_data <- source_data %>% 
      select(!c(POLLNAME, POLL, ANN_VALUE)) %>% 
      filter(FACILITY_NAME == f) %>% 
      unique() %>% 
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
    
    # get number of stacks
    stack_number <- facility_data %>% 
      pull(STACK_ID) %>%
      unique() %>% 
      length()
    
    facility_data$stack_number <- stack_number
    
    # generate receptors
    facility_receptors <- generate_receptors(row = facility_data[1,])

    
    lon <- st_coordinates(facility_receptors)[,1]
    lat <- st_coordinates(facility_receptors)[,2]
    
    utm_coords <- oce::lonlat2utm(longitude = lon, latitude = lat)
    
    ELEV <- facility_receptors$ELEV
    
    XPNTS = utm_coords$easting
    YPNTS = utm_coords$northing
    
    # write file
    
    ## control pathway
    sink(paste0(facility_title, ".INP"))
    cat("CO STARTING\n")
    cat(paste0("CO TITLEONE ", facility_title, "\n"))
    cat(paste0("CO TITLETWO ", title_two, "\n"))
    cat(paste0("CO MODELOPT ", model_opt, "\n"))
    cat(paste0("CO AVERTIME ", aver_time, "\n"))
    cat(paste0("CO POLLUTID ", pollut_id, "\n"))
    cat(paste0("CO RUNORNOT ", run_or_not, "\n"))
    cat("CO FINISHED\n\n")
    
    ## source
    cat("SO STARTING\n")
    ### source location
    for(i in 1:nrow(facility_data)) {
      
      source_id = paste0("STACK", i)
      
      cat(paste0("SO LOCATION ", source_id, "\t", source_type, "\t", facility_data$easting[i], "\t", facility_data$northing[i], "\t", facility_data$STKHGT[i], "\n"))
      
    }
    cat("\n")
    
    ### source parameters
    for(j in 1:nrow(facility_data)) {
      
      source_id = paste0("STACK", j)
      
      cat(paste0("SO SRCPARAM ", source_id, "\t", 1, "\t", facility_data$STKHGT[j], "\t", facility_data$STKTEMP[j], "\t", facility_data$STKFLOW[j], "\t", facility_data$STKDIAM[j], "\n"))

    }
    ### source groups
    for(k in 1:nrow(facility_data)) {
      source_id = paste0("STACK", k)
      
      cat(paste0("SO SRCGROUP ", paste(rep(source_id, times = 2), collapse = " "), "\n"))
    }
    
    cat("SO FINISHED\n\n")
    
    ## receptors
    cat("RE STARTING\n")
    
    for(m in 1:length(XPNTS)) {
      cat(paste0("RE DISCCART   ", XPNTS[m], "   ", YPNTS[m], "   ", ifelse(elev, ELEV[m], ""), ifelse(elev, paste0("   ", ELEV[m]), ""), "\n"))
    }
    cat("RE DISCCART CAR1 END\n\n")
    
    # meterology pathway;
    cat("ME STARTING\n")
    cat(paste0("   SURFFILE ", surf_file, "\n"))
    cat(paste0("   PROFFILE ", prof_file, "\n"))
    cat(paste0("   SURFDATA ", surf_data, "\n"))
    cat(paste0("   UAIRDATA ", uair_data, "\n"))
    cat(paste0("   PROFBASE ", prof_base, "\n"))
    cat(paste0("   STARTEND ", start_end, "\n"))
    cat("ME FINISHED\n\n")
    
    # output pathway
    cat("OU STARTING")
    cat(paste0("   RECTABLE ", rect_table, "\n"))
    cat(paste0("   SUMMFILE ", summ_file, "\n"))
    cat(paste0("   PLOTFILE ", plot_file, "\n"))
    cat("OU FINISHED")
    
    sink()
    
  })
}
 
write_source_input()  
  
  
    
  