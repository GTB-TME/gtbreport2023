load_gtb <- function(df_name,               #basename of data file/data.frame
                     convert_dots = TRUE,   #convert . in data dataname to _
                     convert_dashes = FALSE #convert _ in data dataname to .
                     ){

  # Convenience function to load a data object saved as an rda file from the GTB database
  # and returns it as a data object (dataframe or data table)
  #
  # Usage  my_cty_dataframe <- load_gtb("cty")
  #
  #

  snapshot_files <-   c(
    'tb',
    'sty',
    'tpt',
    'tx',
    'drnew',
    'drret',
    'drhnew',
    'drhret',
    'drfq',
    'drroutine',
    'agg',
    'monthly',
    'covid',
    'ltbi',
    'vrgtb'
  )

  # Find out if the requested file is part of a snapshot
  is_snapshot_file <- length(grep(df_name, snapshot_files, fixed = TRUE))

  if (is_snapshot_file >= 1) {

    # Use function latest_snapshot_date() to identify the folder name with the latest GTB snapshot data
    # and load it into the local envirnment
    load(paste0(paste0(here::here("data/gtb"),
                       "/snapshot_",
                       latest_snapshot_date(),
                       "/",
                       df_name,
                       ".rda")),
         envir = environment())

  } else {

    if(df_name %in% c('est','global','regional')){ #est handled differently due to different location
      fn <- here::here('inc_mort/analysis/')
      fn <- paste0(fn,df_name,'.rda')
      if(!file.exists(fn)) stop(paste0('Estimate ',df_name,' not ready yet!'))
      load(fn,envir = environment())
    } else {
      ## Use the location of the "other" set of GTB data files (not country-reported data)
      ## and load it into the local envirnment
      load(paste0(here::here("data/gtb/other"),
                  "/",
                  df_name,
                  ".rda"),
           envir = environment())
    }

  }

  # Use get() to work with the recently loaded object by name
  # and refer to it as df
  df <- get(df_name,
            envir = environment())

  # If requested, undo PG's conversion of underscores to dots in variable names
  if (convert_dots == TRUE) {
    names(df) <- gsub('[.]', '_', names(df))
  }

  ## If needed, convert underscores to dots in variable names
  if(convert_dashes == TRUE){
    names(df) <- gsub('_','\\.',names(df))
  }

  # Return the data object
  return(df)
}


latest_snapshot_date <- function(){

  # Get the date of the most recent GTB data snapshot

  # Get list of GTB data subfolders
  snapshots <- list.dirs(path = here::here("data/gtb"),
                         full.names = FALSE,
                         recursive = FALSE)

  # Restrict the list to snapshot folders
  snapshots <- snapshots[grep(pattern = "snapshot_", snapshots)]

  # Extract the date part of the folder names and find the folder with the maximum date
  # This will be the folder with the latest set of snapshot files
  latest_snapshot <- max(as.Date(substr(snapshots, 10, 20)))

  return(latest_snapshot)
}



