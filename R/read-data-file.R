#' Read in the iscam data file
#'
#' @details This file was checked out back in time because it had been
#' changed to work on Petrale and had to be changed back to work on Arrowtooth.
#' The command run was:
#' git checkout 815275c~1 -- R/read-data-file.R
#' The command to bring it back should be:
#' git checkout 424f727 -- R/read-data-file.R
#'
#' @param file Filename (full path)
#' @param verbose Say more
#'
#' @return A list representing the contents of the iscam data file
#' @importFrom stringr str_split
#' @export
read_data_file <- function(file = NULL,
                           verbose = FALSE){

  if(!file.exists(file)){
    warning("Data file ", basename(file)," not found in ", dirname(file), ". Is your ",
            iscam.starter.file, " set up correctly? Setting data to NA.")
    return(NA)
  }
  data <- readLines(file, warn=FALSE)
  tmp <- list()
  ind <- 0

  # Remove any empty lines
  data <- data[data != ""]

  # remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  # Get the element number for the "Gears" names if present
  dat <- grep("^#.*Gears:.+", data)
  tmp$has_gear_names <- FALSE
  if(length(dat > 0)){
    gear_names_str <- gsub("^#.*Gears:(.+)", "\\1", data[dat])
    gear_names <- strsplit(gear_names_str, ",")[[1]]
    tmp$gear_names <- gsub("^[[:blank:]]+", "", gear_names)
    tmp$has_gear_names <- TRUE
  }

  # Get the element number for the "GearAbbrevs" names if present
  dat <- grep("^#.*GearAbbrevs:.+", data)
  tmp$has_gear_abbrevs <- FALSE
  if(length(dat > 0)){
    gear_abbrevs_str <- gsub("^#.*GearAbbrevs:(.+)", "\\1", data[dat])
    gear_abbrevs <- strsplit(gear_abbrevs_str, ",")[[1]]
    tmp$gear_abbrevs <- gsub("^[[:blank:]]+", "", gear_abbrevs)
    tmp$has_gear_abbrevs <- TRUE
  }

  # Get the element number for the "FleetGears" names if present
  dat <- grep("^#.*FleetGears:.+",data)
  tmp$has_fleet_gear_names <- FALSE
  if(length(dat >0)){
    # The gear names were in the file
    fleet_gear_names_str <- gsub("^#.*FleetGears:(.+)", "\\1", data[dat])
    fleet_gear_names <- strsplit(fleet_gear_names_str, ",")[[1]]
    tmp$fleet_gear_names <- gsub("^[[:blank:]]+", "", fleet_gear_names)
    tmp$has_fleet_gear_names <- TRUE
  }

  # Get the element number for the "FleetAbbrevs" names if present
  dat <- grep("^#.*FleetAbbrevs:.+", data)
  tmp$has_fleet_abbrevs <- FALSE
  if(length(dat > 0)){
    # The gear abbreviations were in the file. These are the survey_abbrev column in the survey index data
    fleet_abbrevs_str <- gsub("^#.*FleetAbbrevs:(.+)", "\\1", data[dat])
    fleet_abbrevs <- strsplit(fleet_abbrevs_str, ",")[[1]]
    tmp$fleet_abbrevs <- gsub("^[[:blank:]]+", "", fleet_abbrevs)
    tmp$has_fleet_abbrevs <- TRUE
  }

  # Get the element number for the "IndexGears" names if present
  dat <- grep("^#.*IndexGears:.+",data)
  tmp$has_index_gear_names <- FALSE
  if(length(dat >0)){
    # The gear names were in the file
    index_gear_names_str <- gsub("^#.*IndexGears:(.+)", "\\1", data[dat])
    index_gear_names <- strsplit(index_gear_names_str, ",")[[1]]
    tmp$index_gear_names <- gsub("^[[:blank:]]+", "", index_gear_names)
    tmp$has_index_gear_names <- TRUE
  }

  # Get the element number for the "IndexAbbrevs" names if present
  dat <- grep("^#.*IndexAbbrevs:.+", data)
  tmp$has_index_abbrevs <- FALSE
  if(length(dat > 0)){
    # The gear abbreviations were in the file. These are the survey_abbrev column in the survey index data
    index_abbrevs_str <- gsub("^#.*IndexAbbrevs:(.+)", "\\1", data[dat])
    index_abbrevs <- strsplit(index_abbrevs_str, ",")[[1]]
    tmp$index_abbrevs <- gsub("^[[:blank:]]+", "", index_abbrevs)
    tmp$has_index_abbrevs <- TRUE
  }

  # Get the element number for the "AgeGears" names if present (gears with age comp data)
  dat <- grep("^#.*AgeGears:.+",data)
  tmp$has_age_gear_names <- FALSE
  if(length(dat >0)){
    # The gear names were in the file
    age_gear_names_str <- gsub("^#.*AgeGears:(.+)", "\\1", data[dat])
    age_gear_names <- strsplit(age_gear_names_str, ",")[[1]]
    tmp$age_gear_names <- gsub("^[[:blank:]]+", "", age_gear_names)
    tmp$has_age_gear_names <- TRUE
  }

  # Get the element number for the "AgeAbbrevs" names if present (gears with age comp data)
  dat <- grep("^#.*AgeAbbrevs:.+",data)
  tmp$has_age_gear_names <- FALSE
  if(length(dat >0)){
    # The gear names were in the file
    age_gear_abbrevs_str <- gsub("^#.*AgeAbbrevs:(.+)", "\\1", data[dat])
    age_gear_abbrevs <- strsplit(age_gear_abbrevs_str, ",")[[1]]
    tmp$age_gear_abbrevs <- gsub("^[[:blank:]]+", "", age_gear_abbrevs)
    tmp$has_age_gear_abbrevs <- TRUE
  }

  ## Get the element number for the "CatchUnits" if present
  dat <- grep("^#.*CatchUnits:.+", data)
  if(length(dat > 0)){
    catch.units.str <- gsub("^#.*CatchUnits:(.+)", "\\1", data[dat])
    tmp$catch.units <- gsub("^[[:blank:]]+", "", catch.units.str)
  }

  ## Get the element number for the "IndexUnits" if present
  dat <- grep("^#.*IndexUnits:.+", data)
  if(length(dat > 0)){
    index.units.str <- gsub("^#.*IndexUnits:(.+)", "\\1", data[dat])
    tmp$index.units <- gsub("^[[:blank:]]+", "", index.units.str)
  }

  ## Save the number of specimens per year (comment at end of each age comp
  ##  line), eg. #135 means 135 specimens contributed to the age proportions for
  ##  that year
  age.n <- vector()
  ## Match age comp lines which have N's as comments
  tmp$has.age.comp.n <- FALSE
  pattern <- "^[[:digit:]]{4}[[:space:]]+[[:digit:]][[:space:]]+[[:digit:]][[:space:]]+[[:digit:]][[:space:]]+[[:digit:]].*#([[:digit:]]+).*"
  dat <- data[grep(pattern, data)]
  if(length(dat) > 0){
    for(n in 1:length(dat)){
      age.n[n] <- sub(pattern, "\\1", dat[n])
    }
  }
  ## age.n is now a vector of values of N for the age comp data.
  ## The individual gears have not yet been parsed out, this will
  ##  happen later when the age comps are read in.

  ## Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  ## Remove the lines that start with #.
  dat <- data[-dat]

  ## Remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  ## Remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  ## Now we have a nice bunch of string elements which are the inputs for iscam.
  ## Here we parse them into a list structure
  ## This is dependent on the current format of the DAT file and needs to
  ##  be updated whenever the DAT file changes format
  tmp$num.areas <- as.numeric(dat[ind <- ind + 1])
  tmp$num.groups <- as.numeric(dat[ind <- ind + 1])
  tmp$num.sex <- as.numeric(dat[ind <- ind + 1])
  tmp$start.yr <- as.numeric(dat[ind <- ind + 1])
  tmp$end.yr <- as.numeric(dat[ind <- ind + 1])
  tmp$start.age <- as.numeric(dat[ind <- ind + 1])
  tmp$end.age <- as.numeric(dat[ind <- ind + 1])
  tmp$num.gears <- as.numeric(dat[ind <- ind + 1])
  tmp$prop.female <- as.numeric(dat[ind <- ind + 1])

  ## Gear allocation
  tmp$gear.alloc  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  if(!tmp$has_gear_names){
    tmp$gear_names <- 1:length(tmp$gear.alloc)
  }

  ## Age-schedule and population parameters
  tmp$linf      <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$k         <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$to        <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$lw.alpha  <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$lw.beta   <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$age.at.50.mat <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$sd.at.50.mat  <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$use.mat   <- as.numeric(dat[ind <- ind + 1])
  tmp$mat.vec   <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+|,")[[1]])

  ## Catch data
  tmp$num.catch.obs <- as.numeric(dat[ind <- ind + 1])
  tmp$catch         <- matrix(NA, nrow = tmp$num.catch.obs, ncol = 7)

  for(row in 1:tmp$num.catch.obs){
    tmp$catch[row,] <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  }
  colnames(tmp$catch) <- c("year", "gear", "area", "group", "sex", "type", "value")

  ## Abundance indices are a ragged object and are stored as a list of matrices
  tmp$num.indices     <- as.numeric(dat[ind <- ind + 1])
  tmp$num.index.obs   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$survey.type <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  ##nrows <- sum(tmp$nitnobs)
  tmp$indices <- list()
  for(index in 1:tmp$num.indices){
    nrows <- tmp$num.index.obs[index]
    ncols <- 8
    tmp$indices[[index]] <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$indices[[index]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    }
    colnames(tmp$indices[[index]]) <- c("iyr","it","gear","area","group","sex","wt","timing")
  }

  ## Age composition data are a ragged object and are stored as a list of matrices
  tmp$num.age.gears <- as.numeric(dat[ind <- ind + 1])
  ##if(!tmp$hasAgeGearNames){
  ##  tmp$ageGearNames <- 1:length(tmp$nagears)
  ##}

  tmp$num.age.gears.vec       <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$num.age.gears.start.age <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$num.age.gears.end.age   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$eff                     <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$age.comp.flag           <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$dm_num_samp             <- as.numeric(dat[ind <- ind + 1])
  tmp$dm_use_single_num_samp  <- as.logical(as.numeric(dat[ind <- ind + 1]))
  tmp$age.comps <- NULL

  ## One list element for each gear (tmp$nagears)
  ## Check to see if there are age comp data
  if(tmp$num.age.gears.vec[1] > 0){
    tmp$age.comps <- list()
    for(gear in 1:tmp$num.age.gears){
      nrows <- tmp$num.age.gears.vec[gear]
      ## 5 of the 6 here is for the header columns
      ncols <- tmp$num.age.gears.end.age[gear] - tmp$num.age.gears.start.age[gear] + 7
      tmp$age.comps[[gear]] <- matrix(NA, nrow = nrows, ncol = ncols)

      for(row in 1:nrows){
        tmp$age.comps[[gear]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
      }
      colnames(tmp$age.comps[[gear]]) <- c("year",
                                           "sample_size",
                                           "gear",
                                           "area",
                                           "group",
                                           "sex",
                                           tmp$num.age.gears.start.age[gear]:tmp$num.age.gears.end.age[gear])
    }
  }

  ## Build a list of age comp gear N's
  tmp$age.gears.n <- list()
  start <- 1
  for(ng in 1:length(tmp$num.age.gears.vec)){
    end <- start + tmp$num.age.gears.vec[ng] - 1
    tmp$age.gears.n[[ng]] <- age.n[start:end]
    start <- end + 1
  }

  ## Empirical weight-at-age data
  tmp$num.weight.tab <- as.numeric(dat[ind <- ind + 1])
  tmp$num.weight.obs <- as.numeric(dat[ind <- ind + 1])
  tmp$waa <- NULL

  if(tmp$num.weight.obs > 0){
    ## Parse the weight-at-age data
    nrows       <- tmp$num.weight.obs
    ncols       <- tmp$end.age - tmp$start.age + 6
    tmp$weight.at.age <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$weight.at.age[row,] <-
        as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
    }
    colnames(tmp$weight.at.age) <- c("year",
                                     "gear",
                                     "area",
                                     "group",
                                     "sex",
                                     tmp$start.age:tmp$end.age)
  }

  ## Annual Mean Weight data
  ## Catch data
  tmp$num.mean.weight <- as.numeric(dat[ind <- ind + 1])
  tmp$num.mean.weight.obs <- as.numeric(dat[ind <- ind + 1])
  if(tmp$num.mean.weight.obs >0){
    tmp$mean.weight.data  <- matrix(NA, nrow = sum(tmp$num.mean.weight.obs), ncol = 7)
    for(row in 1:sum(tmp$num.mean.weight.obs)){
      tmp$mean.weight.data[row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    }
    colnames(tmp$mean.weight.data) <- c("year",
                                        "meanwt",
                                        "gear",
                                        "area",
                                        "group",
                                        "sex",
                                        "timing")
  }
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}
