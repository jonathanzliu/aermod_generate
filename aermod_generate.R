# function to read in CSV, output AERMOD input file

## to do:
  ## find out format of receptor locations and rewrite them into something AERMOD can use
  ## curious what restraints AERMOD has on formatting location data (limit the number of entries?)
  ## what's an example of the meteorology data that we use?



require(data.table)

write_aermod_input <- function(input_row, individual = TRUE, all = TRUE) {
  
  filename <- input_row$TITLEONE
  
  entries <- str_split(input_row, ";")
  names(entries) <- names(input_row)
  
  # control pathway
  sink(paste0(filename, ".INP"))
  cat("CO STARTING\n")
  cat(paste0("   TITLEONE ", entries$TITLEONE, "\n"))
  cat(paste0("   TITLETWO ", entries$TITLETWO, "\n"))
  cat(paste0("   MODELOPT ", entries$MODELOPT, "\n"))
  cat(paste0("   AVERTIME ", entries$AVERTIME, "\n"))
  cat(paste0("   POLLUTID ", entries$POLLUTID, "\n"))
  cat(paste0("   RUNORNOT ", entries$RUNORNOT, "\n"))
  cat("CO FINISHED\n\n")
  
  # source pathway
  cat("SO STARTING\n")
  for(i in 1:length(entries$source_id)) {
    cat(paste0("   LOCATION ", entries$source_id[i], "\t", entries$source_type[i], "\t", entries$X[i], "\t", entries$Y[i], "\t", entries$elev_m[i], "\n"))
  }
  for(j in 1:length(entries$source_id)) {
    cat(paste0("   SRCPARAM ", entries$source_id[j], "\t", entries$ER[j], "\t", entries$hgt_m[j], "\t", entries$temp_k[j], "\t", entries$vel_ms[j], "\t", entries$dia_m[j], "\n"))
  }
  
  cat(paste0("   INCLUDED ", entries$INCLUDED), "\n")
  for(i in 1:length(entries$source_id)) {
    cat(paste0("   SRCGROUP ", paste(rep(entries$source_id[i], each = 2), collapse = " ")), "\n")
  }
  cat("\n")
  
  # receptor pathway
  cat("RE STARTING\n")
  cat(paste0("   INCLUDED ", entries$INCLUDED, "\n"))
  cat("RE FINISHED\n\n")
  
  # meterology pathway;
  cat("ME STARTING\n")
  cat(paste0("   SURFFILE ", entries$SURFFILE, "\n"))
  cat(paste0("   PROFFILE ", entries$PROFFILE, "\n"))
  cat(paste0("   SURFDATA ", entries$SURFDATA, "\n"))
  cat(paste0("   UAIRDATA ", entries$UAIRDATA, "\n"))
  cat(paste0("   PROFBASE ", entries$PROFBASE, "\n"))
  cat("ME FINISHED\n\n")
  
  # output pathway
  cat("OU STARTING")
  cat(paste0("   RECTABLE ", entries$RECTABLE, "\n"))
  cat(paste0("   SUMMFILE ", entries$SUMMFILE, "\n"))
  cat(paste0("   PLOTFILE ", entries$PLOTFILE, "\n"))
  cat("OU FINISHED")
  
  sink()
  
}

generate_airmod <- function(csv_file) {
  
  # read in the file
  df <- fread(csv_file)
  
  # get the number of rows
  rows <- nrow(df)
  
  # loop through rows to generate textfiles
  for(i in 1:rows) {
    
    write_aermod_input(df[i,])
    
  }
  
  
}
