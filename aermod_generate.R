# function to read in CSV, output AERMOD input file

require(data.table)

write_aermod_input <- function(input_row, individual = TRUE, all = TRUE) {
  
  filename <- input_row$TITLEONE
  
  entries <- str_split(input_row, ";")
  names(entries) <- names(input_row)
  
  # control pathway
  sink(paste0(filename, ".txt"))
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
  cat(paste0("   LOCATION ", entries$source_id, "\t", entries$source_type, "\t", entries$X, "\t", entries$Y, "\t", entries$elev_m, "\n"))
  cat(paste0("   SRCPARAM ", entries$source_id, "\t", entries$ER, "\t", entries$hgt_m, "\t", entries$temp_k, "\t", entries$vel_ms, "\t", entries$dia_m, "\n"))
  
  cat(paste0("   INCLUDED ", entries$INCLUDED), "\n")
  for(i in 1:length(entries$source_id)) {
    cat(paste0("   SRCGROUP ", paste(rep(entries$source_id[i], each = 2), collapse = " ")), "\n\n")
  }
  
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
  
  
  
}