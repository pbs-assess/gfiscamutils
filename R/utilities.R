#' Verify that models object is a list of iscam models and that models is the same length as models_names
#'
#' @param models a list of iscam model objects
#' @param models_names a vector of names for the models
#'
#' @export
verify_models <- function(models,
                          models_names){
  if(length(models) != length(models_names)){
    stop("models_names must be the same length as models.", call. = FALSE)
  }
  for(i in seq_along(models)){
    if(class(models[[i]]) != model.class){
      stop("Model ", i, " in the list is not of the type ", model.class, call. = FALSE)
    }
  }
}

#' Calculate column quantiles for a data matrix
#'
#' @param data a matrix
#' @param probs a vector of probabilities to be passed to [stats::quantile()]
#'
#' @export
#' @importFrom stats quantile
get.quants <- function(data,
                       probs){

  if(is.null(dim(data))){
    ## It is a single posterior, e.g. sbo
    quants <- quantile(data, probs)
  }else{
    ## It is a timeseries posterior, e.g. sbt
    quants <- apply(data, 2, quantile, probs)
  }
  quants
}

#' Get priors information from prior.str
#'
#' @param prior.str is a string like Lognormal(2.0,1.01)
#' @param dec.points number of decimal points to return
#' @param first.to.lower makes the first letter of the name of the prior lower case
#'
#' @return a vector of length 3, e.g.:"Lognormal", 2.0, 1.01
#' @export
#' @importFrom gfutilities f
split_prior_info <- function(prior.str,
                             dec.points = 1,
                             first.to.lower = FALSE){
  p <- strsplit(prior.str, "\\(")[[1]]
  if(first.to.lower){
    ## Make the name of the prior lower case
    p[1] <- paste0(tolower(substr(p[1], 1, 1)), substr(p[1], 2, nchar(p[1])))
  }
  p.type <- p[1]
  p <- strsplit(p[2], ",")[[1]]
  p.mean <- f(as.numeric(p[1]), dec.points)
  p.sd <- f(as.numeric(gsub(")", "", p[2])), dec.points)
  return(c(p.type, p.mean, p.sd))
}

#' Get the value at given rank
#'
#' @param vec a vector of values
#' @param rank 1=max, 2-second highest, etc.
#'
#' @return the value at the rank and the index where it was found
#' @export
get.age.prop <- function(vec, rank = 1){
  prop <- rev(sort(vec))
  prop <- prop[ran]
  age <- as.numeric(names(vec[vec == prop]))
  c(age, prop)
}

#' Get a pretty version of the parameter name
#'
#' @param name iscam parameter to pretty-up
#' @param addToQ an integer to the parameter name for the q's. This is necessary
#' because iscam sets the q parameter names to 1, 2, 3... regardless of the
#' gear number. i.e. if gear 1 is a trawl fishery and gear 2 is a survey,
#' iscam will call q1 the survey gear. We must add 1 to it to get q2 to
#' accurately portray the survey gear number
#'
#' @return an R expression which represents the pretty version of the parameter name
#' @export
get.latex.name <- function(name, addToQ = 0){

  if(name == "ro") return(expression("R"[0]))
  if(name == "rbar") return(expression(bar("R")))
  if(name == "rinit") return(expression(bar("R")[init]))
  if(name == "m") return(expression("M"))
  if(name == "bo") return(expression("B"[0]))
  if(name == "sbo") return(expression("SB"[0]))
  if(name == "vartheta") return(expression(vartheta))
  if(name == "rho") return(expression(rho))
  if(name == "bmsy") return(expression("B"[MSY]))
  if(name == "msy") return(expression("MSY"))
  if(name == "fmsy") return(expression("F"[MSY]))
  if(name == "umsy") return(expression("U"[MSY]))
  if(name == "ssb") return(expression("SSB"))
  if(name == "sel1") return(expression(hat(a)[1]))
  if(name == "selsd1") return(expression(hat(gamma)[1]))
  if(name == "sel2") return(expression(hat(a)[2]))
  if(name == "selsd2") return(expression(hat(gamma)[2]))
  if(name == "sel3") return(expression(hat(a)[3]))
  if(name == "selsd3") return(expression(hat(gamma)[3]))
  if(name == "sel4") return(expression(hat(a)[4]))
  if(name == "selsd4") return(expression(hat(gamma)[4]))
  if(name == "sel5") return(expression(hat(a)[5]))
  if(name == "selsd5") return(expression(hat(gamma)[5]))
  if(name == "log_ro") return(expression("ln(R"[0]*")"))
  if(name == "h") return(expression("h"))
  if(name == "m1") return(expression("M"[1]))
  if(name == "m2") return(expression("M"[2]))
  if(name == "log_m") return(expression("ln(M)"))
  if(name == "log_rbar") return(expression("ln("*bar("R")*")"))
  if(name == "log_rinit") return(expression("ln("*bar("R")[init]*")"))

  if(length(grep("^q[1-9]+$", name))){
    digit <- as.numeric(sub("^q([1-9]+)$", "\\1", name))
    return(substitute("q"[digit], list(digit = digit)))
  }

  if(length(grep("^log_q[1-9]+$", name))){
    digit <- as.numeric(sub("^log_q([1-9]+)$", "\\1", name))
    return(substitute("ln(q"[digit]*")", list(digit = digit)))
  }
  NULL
}

#' Draw a time series envelope on a device on which [plot.new()] has already been called
#'
#' @param yrs the years to plot
#' @param quants a 3-row matrix, where the middle row is the median and the other two are
#' the lower and upper values for some confidence interval.
#' @param col color of the envelope
#' @param first boolean. If TRUE, [plot.new()] will be called. If FALSE, [lines()] will be
#'  called.
#' @param opacity how opaque the envelope shading is. Percentage value
#' @param ... other graphical parameters
#'
#' @export
draw.envelope <- function(yrs,
                          quants,
                          col = "black",
                          first,
                          opacity = 75,
                          ...){

  lower  <- quants[1,]
  median <- quants[2,]
  upper  <- quants[3,]

  if(first){
    plot(yrs,
         median,
         type = "l",
         col = col,
         lty = 1,
         lwd = 2,
         ...)
    shade <- get.shade(col, opacity)
    poly.yrs <- c(yrs, rev(yrs))
    poly.ci    <- c(lower, rev(upper))
    polygon(poly.yrs, poly.ci, col = shade)
  }else{
    lines(yrs,
          median,
          type = "l",
          col = col,
          lty = 1,
          lwd = 2,
          ...)
    ## Upper and lower part of CI
    lines(yrs,
          lower,
          col = col,
          lty = 5,
          lwd = 1)
    lines(yrs,
          upper,
          col = col,
          lty = 5,
          lwd = 1)
  }
}

#' Extract the model class objects from the list of model lists,
#' and merge them into a single model list containing all the model
#' class objects
#'
#' @param ... one or more lists of iscam model objects
#'
#' @return an iscam model object list
#' @export
c.model.list <- function(...){

  lst <- list(...)
  ret.lst <- NULL
  ind <- 1
  for(i in 1:length(lst)){
    if(class(lst[[i]]) != model.lst.class){
      stop("List element ", i, " is not of the class '", model.lst.class, "'.")
    }
    for(j in 1:length(lst[[i]])){
      if(class(lst[[i]][[j]]) != model.class){
        stop("Sublist element ", j, " of list element ", i,
             " is not of the class '", model.class, "'.")
      }
      ret.lst[[ind]] <- lst[[i]][[j]]
      ind <- ind + 1
    }
  }
  class(ret.lst) <- model.lst.class
  ret.lst
}

#' Calculation of sigma and tau from rho and vartheta
#'
#' @param rho parameter rho from iscam model
#' @param vartheta parameter vartheta from iscam model
#'
#' @return a list of length 2, the calculated tau and sigma parameters
#' @export
calc.sig.tau <- function(rho, vartheta){

  tau <- sqrt((1 - rho) / vartheta)
  sigma <- sqrt(rho / vartheta)
  list(tau, sigma)
}

#' Calculate the number of rows and columns to have in a grid for plotting multiple figures
#'
#' @param num the number of figures
#'
#' @return a vector of length 2 representing the number of rows and columns
#' @export
get.rows.cols <- function(num){
  if(num <= 64 && num > 49){
    if(num <= 56){
      nside <- c(8,7)
    }else{
      nside <- c(8,8)
    }
  }else if(num <= 49 && num > 36){
    if(num <= 42){
      nside <- c(7,6)
    }else{
      nside <- c(7,7)
    }
  }else if(num <= 36 && num > 25){
    if(num <= 30){
      nside <- c(6,5)
    }else{
      nside <- c(6,6)
    }
  }else if(num <= 25 && num > 16){
    if(num <= 20){
      nside <- c(5,4)
    }else{
      nside <- c(5,5)
    }
  }else if(num <= 16 && num > 9){
    if(num <= 12){
      nside <- c(4,3)
    }else{
      nside <- c(4,4)
    }
  }else if(num <=  9 && num > 4){
    if(num <= 6){
      nside <- c(3,2)
    }else{
      nside <- c(3,3)
    }
  }else if(num <=  4 && num > 1){
    if(num == 2){
      nside <- c(2,1)
    }else{
      nside <- c(2,2)
    }
  }else{
    nside <- c(1,1)
  }
  return(nside)
}

# Function to transform from Season to Year
Season2Year <- function(dat) {
  # The herring 'season' column is a combination of the two fishery years: for
  # example, season '20123' indicates the years 2012 and 2013. The input *.dat
  # file for the analysis using ADMB requires this to be an acual year, which we
  # define as the second (i.e., later) year, 2013. This function takes in a
  # vector of seasons (dat), and outputs a vector of years (res).
  # Grab the first 4 characters
  chars <- substr(x = dat, start = 1, stop = 4)
  # Convert to numeric
  digits <- as.numeric(x = chars)
  # Add one to get the year
  res <- digits + 1
  # Return years (as an integer)
  return(as.integer(res))
} # End Season2Year function

# Calculate mean if there are non-NA values, return NA if all values are NA
#' @export
mean_na <- function(x, omit_na = TRUE) {
  # An alternate version to mean(x, na.rm=TRUE), which returns 0 if x is all NA.
  # This version retuns NA if x is all NA, otherwise it returns the mean.
  # If all NA, NA; otherwise, mean
  ifelse(all(is.na(x)),
    res <- NA,
    res <- mean(x, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End mean_na function

# Calculate sum if there are non-NA values, return NA if all values are NA
#' @export
sum_na <- function(x, omit_na = TRUE) {
  # An alternate version to sum(x, na.rm=TRUE), which returns 0 if x is all NA.
  # This version retuns NA if x is all NA, otherwise it returns the sum.
  # If all NA, NA; otherwise, sum
  ifelse(all(is.na(x)),
    res <- NA,
    res <- sum(x, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End sum_na function

# Calculate unique if there are non-NA values, return NA if all values are NA
#' @export
unique_na <- function(x) {
  # An alternate version to unique, which fails sometimes if there are no
  # values. This version retuns NA if x is all NA, otherwise it returns the
  # unique values.
  # If all NA, NA; otherwise, unique
  ifelse(all(is.na(x)),
    res <- NA,
    res <- unique(x)
  )
  # Return the result
  return(res)
} # End unique_na function

# Calculate weighted mean if there are non-NA values, return NA if all values
# are NA
#' @export
wt_mean_na <- function(x, w, omit_na = TRUE) {
  # An alternate version to weighted.mean(x, w, na.rm=TRUE), which returns 0 if
  # x is all NA. This version retuns NA if x is all NA, otherwise it returns the
  # weighted mean.
  # If all NA, NA; otherwise, weighted mean
  ifelse(all(is.na(x)),
    res <- NA,
    res <- weighted.mean(x, w, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End mean_na function

# Calculate maximum if there are non-NA values, return NA if all values are NA
#' @export
max_na <- function(x, omit_na = TRUE) {
  # An alternate version to max(x, na.rm=TRUE), which returns 0 if x is all NA.
  # This version retuns NA if x is all NA, otherwise it returns the maximum.
  # If all NA, NA; otherwise, maximum
  ifelse(all(is.na(x)),
    res <- NA,
    res <- max(x, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End max_na function

# Calculate minimum if there are non-NA values, return NA if all values are NA
#' @export
min_na <- function(x, omit_na = TRUE) {
  # An alternate version to min(x, na.rm=TRUE), which returns 0 if x is all NA.
  # This version retuns NA if x is all NA, otherwise it returns the minimum.
  # If all NA, NA; otherwise, minimum
  ifelse(all(is.na(x)),
    res <- NA,
    res <- min(x, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End min_na function

# Fill in NA values with rolling mean of previous values
#' @export
roll_mean_na <- function(dat, n, omit_na = TRUE) {
  # Update the NAs in a vector with the mean of the previous values. The number
  # of values used can be less than n for NAs near the start of the vector, and
  # will be a maximum of n for values further along the vector. The value will
  # remain NA if no non-NA values are available.
  # Loop over observations starting with the second observation
  for (i in 2:length(dat)) {
    # If the value is NA
    if (is.na(dat[i])) {
      # Get window for current index: up to n previous values
      muWindow <- (i - min(n, i - 1)):(i - 1)
      # Calculate the mean of the values in the rolling window
      dat[i] <- mean_na(dat[muWindow], omit_na = omit_na)
    } # End if value is NA
  } # End i loop over observations
  # Return the observations with NAs (mostly) replaced by the rolling mean
  return(dat)
} # End roll_mean_na function

# Calculate plot margin by expanding the range of x and y as a percentage
CalcXYLims <- function( x, y, pX=0.01, pY=0.01 ) {
  # Set the space between the points and the plot border manually as a specified
  # percentage of the range. Return a list with range for x and y.
  # Get range of x
  rx <- range( x )
  # Calculate difference of x range
  drx <- diff( rx )
  # Calculate new usr for x
  usrX <- c( rx[1]-pX*drx, rx[2]+pX*drx )
  # Get range of y
  ry <- range( y )
  # Calculate difference of y range
  dry <- diff( ry )
  # Calculate new usr for y
  usrY <- c( ry[1]-pY*dry, ry[2]+pY*dry )
  # Return new x and y limits (i.e., xlim and ylim, respectively)
  return( list(x=usrX, y=usrY) )
} # End CalcXYLims

# Paste strings nicely
PasteNicely <- function( x, intChars=", ", nChar="and " ) {
  # Get the length of the vector
  n <- length( x )
  # If there are more than two
  if( n > 2 ) {
    # Make a print-friendly vector
    x[n] <- paste( nChar, x[n], sep="" )
    # Get print friendly values
    res <- paste( x, collapse=intChars )
  } else {  # End if more than two, otherwise
    # Add a space
    nCharSp <- paste( " ", nChar, sep="" )
    # Get print friendly values
    res <- paste( x, collapse=nCharSp )
  }  # End if not more than two
  # Return the results
  return( res )
}  # End PasteNicely function

# Function to add a new column indicating the number of consecutive values
CountConsecutive <- function( vec ) {
  # Determine the number of consecutive values in a vector. For example,
  # indicate whether a series of years is sequential, or if there are say three
  # groups of sequential years.
  # Break up the data by groups with consecutive values
  dUniqueGrps <- split( x=vec, f=cumsum(c(1, diff(vec) != 1)) )
  # Put group id into the data
  for( g in 1:length(dUniqueGrps) ) {
    # Add the group ID to the table
    dUniqueGrps[[g]] <- 1:length(dUniqueGrps[[g]])
  }  # End g loop over groups
  # Unsplit the list
  NConsec <- as.vector( unlist(dUniqueGrps) )
  # Return the data with groups
  return( NConsec )
}  # End ConsecutiveGroup function

# Load herring areas
LoadAreaData <- function( where ) {
  # Herring areas are kept in two csv files which indicate areas, once of which
  # has coarse area information, and other has finer details. This function
  # merges these two files, and drops unnecessary rows and columns. In addition,
  # 'groups' are created for certain regions based on section numbers. The
  # output is a data frame with both coarse- and fine-scale area information for
  # the region(s) in question. There is an option to subset the sections if
  # desired (e.g., for the Central Coast)
  # Message re region
  cat( "Region(s): ", paste(region, collapse=", "), " (",
       paste(range(yrRange), collapse=":"), ")\n", sep="" )
  # Cross-walk table for SAR to region and region name
  regions <- read_csv(file=
                        "SAR, Region, RegionName, Major
          1, HG, Haida Gwaii, TRUE
          2, PRD, Prince Rupert District, TRUE
          3, CC, Central Coast, TRUE
          4, SoG, Strait of Georgia, TRUE
          5, WCVI, West Coast of Vancouver Island, TRUE
          6, A27, Area 27, FALSE
          7, A2W, Area 2 West, FALSE
          8, JS, Johnstone Strait, FALSE",
                      col_types=cols("i", "c", "c", "l") )
  # If region isn't JS, remove it
  if( region != "JS" )  regions <- filter( .data=regions, SAR != 8 )
  # Return the regions table to the main environment
  regions <<- regions
  # Possible regions by type (return to the main level)
  allRegions <<- list( major=as.character(regions$Region[regions$Major]),
                       minor=as.character(regions$Region[!regions$Major]) )
  # Error if region is incorrect
  if( !all(region %in% unlist(allRegions)) )  stop( "Possible regions are: ",
                                                    paste(unlist(allRegions), collapse=", "), call.=FALSE )
  # Establish connection with access
  accessDB <- odbcConnectAccess( access.file=file.path(where$loc, where$db) )
  # TODO: Sections 132 and 135 are also SoG sections -- how to resolve?
  # Manual fix: Johnstone Strait herring sections
  jsSections <<- c( 111, 112, 121:127, 131:136 )
  # If the region is Johnstone Strait
  if( region == "JS" ) {
    # Message
    cat( "Note overlap between JS and SoG: Sections 132 and 135\n")
    # Access the sections worksheet and wrangle
    sections <- sqlFetch( channel=accessDB, sqtable=where$fns$sections ) %>%
      filter( Section %in% jsSections ) %>%
      mutate( SAR=8 ) %>%
      full_join( y=regions, by="SAR" ) %>%
      filter( Region %in% region ) %>%
      select( SAR, Region, RegionName, Section ) %>%
      distinct( ) %>%
      as_tibble( )
  } else {  # End if Johnstone Strait, otherwise
    # Access the sections worksheet and wrangle
    sections <- sqlFetch( channel=accessDB, sqtable=where$fns$sections ) %>%
      filter( SAR != -1 ) %>%
      full_join( y=regions, by="SAR" ) %>%
      filter( Region %in% region ) %>%
      select( SAR, Region, RegionName, Section ) %>%
      distinct( ) %>%
      as_tibble( )
  }  # End if the region is not Johnstone Strait
  # Access the locations worksheet
  locDat <- sqlFetch( channel=accessDB, sqtable=where$fns$locations )
  # Grab the spatial info (X and Y)
  locSP <- locDat %>%
    transmute( X=ifelse(is.na(Location_Longitude), 0, Location_Longitude),
               Y=ifelse(is.na(Location_Latitude), 0, Location_Latitude))
  # Put X and Y into a spatial points object
  locPts <- SpatialPoints( coords=locSP, proj4string=CRS(inCRS) )
  # Convert X and Y from WGS to Albers
  locPtsAlb <- spTransform( x=locPts, CRSobj=CRS(outCRS) )
  # Extract spatial info
  dfAlb <- as_tibble( locPtsAlb )
  # Extract relevant location data
  locations <- locDat %>%
    cbind( dfAlb ) %>%
    rename( LocationCode=Loc_Code, LocationName=Location ) %>%
    mutate( Eastings=ifelse(is.na(Location_Longitude), Location_Longitude, X),
            Northings=ifelse(is.na(Location_Latitude), Location_Latitude, Y) ) %>%
    select( StatArea, Section, LocationCode, LocationName, Bed, Eastings,
            Northings ) %>%
    filter( Section %in% sections$Section ) %>%
    distinct( ) %>%
    as_tibble( )
  # Intialize an additional column for groups: NA
  locations$Group <- NA
  # Manually determine groups: Haida Gwaii
  locations$Group[locations$Section %in% c(21, 25)] <- "21+25"
  locations$Group[locations$Section %in% c(6)] <- "6"
  locations$Group[locations$Section %in% c(23)] <- "23"
  locations$Group[locations$Section %in% c(24)] <- "24"
  # Manually determine groups: Prince Rupert District
  locations$Group[locations$Section %in% c(31:33, 41:43, 51:53)] <- "No group"
  locations$Group[locations$Section %in% c(40, 49, 50, 59)] <- "No group"
  # Manually determine groups: Central Coast
  locations$Group[locations$Section %in% c(67, 71:78)] <- "6&7"
  locations$Group[locations$Section %in% c(70, 79)] <- "6&7"
  locations$Group[locations$Section %in% c(85, 86)] <- "8"
  # Manually determine groups: Strait of Georgia
  locations$Group[locations$Section %in% c(132, 135, 141)] <- "Lazo"
  locations$Group[locations$Section %in% c(142, 143, 171, 172)] <- "14&17"
  locations$Group[locations$Section %in% c(151, 152, 161:165, 280, 291, 292)] <-
    "ESoG"
  locations$Group[locations$Section %in% c(150, 160)] <- "ESoG"
  locations$Group[locations$Section %in% c(173, 181, 182, 191:193)] <- "SDodd"
  locations$Group[locations$Section %in% c(180, 190)] <- "SDodd"
  # Manually determine groups: West Coast Vancouver Island
  locations$Group[locations$Section %in% c(231)] <- "Alberni Inlet"
  locations$Group[locations$Section %in% c(232, 233)] <- "Barkley"
  #  locations$Group[locations$Section %in% c(230, 239)] <- "SA 23 Unkn"
  locations$Group[locations$Section %in% c(241)] <- "Tofino Inlet"
  locations$Group[locations$Section %in% c(242)] <- "Hesquiat"
  locations$Group[locations$Section %in% c(243)] <- "Hootla Kootla"
  locations$Group[locations$Section %in% c(244)] <- "Ahousaht"
  locations$Group[locations$Section %in% c(245)] <- "Vargas Island"
  #  locations$Group[locations$Section %in% c(240, 249)] <- "SA 24 Unkn"
  locations$Group[locations$Section %in% c(251, 252)] <- "Nootka"
  locations$Group[locations$Section %in% c(253)] <- "Nuchatlitz/Ehattesaht"
  #  locations$Group[locations$Section %in% c(250, 259)] <- "SA 25 Unkn"
  # Manually determine groups: Area 2 West
  locations$Group[locations$Section %in% c(1:5)] <- "No group"
  locations$Group[locations$Section %in% c(0)] <- "No group"
  # Manually determine groups: Area 27
  locations$Group[locations$Section %in% c(271:274)] <- "No Group"
  locations$Group[locations$Section %in% c(270)] <- "No Group"

  # If any groups are NA, check if *some* are missing (i.e., incomplete)
  if( any(is.na(locations$Group)) ) {
    # Get distinct rows
    grpU <- locations %>%
      select( StatArea, Section, Group ) %>%
      distinct( ) %>%
      arrange( StatArea, Section )
    # Get distinct rows with no missing groups
    grpUNA <- grpU %>%
      filter( is.na(Group) )
    # Check if none or all have groups
    noneOrAll <- nrow( grpU ) == nrow( grpUNA )
    # Message re some sections(s) missing group info
    if( !noneOrAll )  cat( "Incomplete `Group' info for Section(s): ",
                           paste(grpUNA$Section, collapse=", "), "\n", sep="" )
  }  # End if any groups are NA
  # Extract required data
  res <- locations %>%
    right_join( y=sections, by="Section" ) %>%
    filter( !is.na(StatArea), !is.na(Section) )  %>%
    select( SAR, Region, RegionName, StatArea, Group, Section, LocationCode,
            LocationName, Bed, Eastings, Northings ) %>%
    #      mutate( StatArea=formatC(StatArea, width=2, format="d", flag="0"),
    #          Section=formatC(Section, width=3, format="d", flag="0") ) %>%
    arrange( Region, StatArea, Group, Section, LocationCode ) %>%
    distinct( ) %>%
    droplevels( )
  # If not all sections are included
  if( !all(is.na(sectionSub)) ) {
    # Grab a subset of sections
    res <- res %>%
      filter(Section %in% sectionSub ) %>%
      droplevels( )
    # Message
    cat( "Sections: ", paste(sectionSub, collapse=", "), "\n", sep="" )
  }  # End if subsetting areas
  # Close the connection
  odbcClose( accessDB )
  # Return herring areas
  return( res )
}  # End LoadAreaData function

# Load shapefiles: land, stat areas, etc
LoadShapefiles <- function( where, a, bMax=5000 ) {
  # Load shapefiles for herring sections and aggegate sections to statistical
  # areas and region(s), and make tibbles to plot. In addition, load shapefile
  # for land (i.e., BC coast) and clip to the required extent. Returns a list of
  # shapefiles and tibbles.
  # Message
  cat( "Loading shapefiles... " )
  # Get area information
  aSm <- a %>%
    select( SAR, StatArea, Group, Section ) %>%
    distinct( ) %>%
    mutate( StatArea=formatC(StatArea, width=2, flag="0"),
            Section=formatC(Section, width=3, flag="0") ) %>%
    arrange( SAR, StatArea, Group, Section )
  # Load the Section shapefile (has Statistical Areas and Regions)
  secRaw <- readOGR( dsn=where$locSec, layer=where$fns$sections, verbose=FALSE )
  # Function to perform some light wrangling
  UpdateSections <- function( dat, keepAll ) {
    # Subset the sections to the region(s) in question, and perform some light
    # wrangling to get correct column names and IDs. Note that this would also
    # be the place to apply a slight spatial buffer to ensure that boundaries
    # are contiguous without overlapping.
    # Some light wrangling
    dat@data <- dat@data %>%
      mutate( StatArea=as.character(StatArea),
              Section=as.character(Section) ) %>%
      select( SAR, StatArea, Section )
    # If retain all the regions
    if( keepAll ) {
      # If the region is Johnstone Strait
      if( region == "JS" ) {
        # Update the JS SAR
        dat@data <- dat@data %>%
          mutate( SAR=ifelse(Section %in% jsSections & SAR == -1, 8, SAR) )
      }  # End if Johnstone Strait
      # Remove the non-SAR areas
      res <- dat[dat$SAR != -1, ]
    } else {  # End if retain all, otherwise
      # If the region is Johnstone Strait
      if( region == "JS" ) {
        # Subset to the right sections
        res <- dat[dat$Section %in% jsSections, ]
        # Update the SAR
        res$SAR <- 8
      } else {  # End if Johnstone Strait, otherwise
        # Subset to the right area
        res <- dat[dat$SAR %in% aSm$SAR, ]
      }  # End if the region is not Johnstone Strait
    }  # End if not retaining all
    # Return updated sections
    return( res )
  }  # End UpdateSections function
  # Update sections
  secSPDF <- UpdateSections( dat=secRaw, keepAll=FALSE )
  # Convert to data frame and select stat areas in question
  secDF <- secSPDF %>%
    fortify( region="Section" ) %>%
    rename( Eastings=long, Northings=lat, Section=group ) %>%
    as_tibble( )
  # Determine section centroids
  secCent <- gCentroid( spgeom=secSPDF, byid=TRUE )
  # Convert to data frame
  secCentDF <- secCent %>%
    as_tibble( ) %>%
    rename( Eastings=x, Northings=y ) %>%
    mutate( Section=formatC(secSPDF$Section, width=3, flag="0") ) %>%
    arrange( Section )
  # If 'Groups' has info, dissolve to Groups
  if( !(all(is.na(aSm$Group))) & region!="JS" & all(is.na(sectionSub)) ) {
    # First, remove NAs
    aSmC <- aSm %>%
      filter( !is.na(Group), !is.na(Section) ) %>%
      select( StatArea, Group, Section )
    # Merge groups information with sections
    secSPDF@data <- secSPDF@data %>%
      left_join( y=aSmC, by=c("StatArea", "Section") )
    # Dissolve to group
    grpSPDF <- aggregate( x=secSPDF, by=list(Temp=secSPDF$Group), FUN=unique )
    # Convert to data frame
    grpDF <- grpSPDF %>%
      fortify( region="Group" ) %>%
      rename( Eastings=long, Northings=lat, Group=group ) %>%
      as_tibble( )
    # Determine group centroids
    grpCent <- gCentroid( spgeom=grpSPDF, byid=TRUE )
    # Convert to data frame
    grpCentDF <- grpCent %>%
      as_tibble( ) %>%
      rename( Eastings=x, Northings=y ) %>%
      mutate( Group=grpSPDF$Group ) %>%
      arrange( Group )
  } else { # End if Groups has info, otherwise
    # No objects (null?)
    grpDF <- NULL
    grpCentDF <- NULL
  }  # End if Groups has no info
  # Dissolve to stat area
  saSPDF <- aggregate( x=secSPDF, by=list(Temp=secSPDF$StatArea), FUN=unique )
  # Convert to data frame and select stat areas in question
  saDF <- saSPDF %>%
    fortify( region="StatArea" ) %>%
    rename( Eastings=long, Northings=lat, StatArea=group ) %>%
    as_tibble( )
  # Determine stat area centroids
  saCent <- gCentroid( spgeom=saSPDF, byid=TRUE )
  # Convert to data frame
  saCentDF <- saCent %>%
    as_tibble( ) %>%
    rename( Eastings=x, Northings=y ) %>%
    mutate( StatArea=formatC(saSPDF$StatArea, width=2, flag="0") ) %>%
    filter( StatArea != "00" ) %>%
    arrange( StatArea )
  # Dissolve to region
  regSPDF <- aggregate( x=secSPDF, by=list(Temp=secSPDF$SAR), FUN=unique )
  # Convert to data frame and select region(s) in question
  regDF <- regSPDF %>%
    fortify( region="SAR" ) %>%
    rename( Eastings=long, Northings=lat, Region=group ) %>%
    as_tibble( )
  # Get a buffer around the region(s) in question
  buff <- gBuffer( spgeom=regSPDF, width=bMax, byid=FALSE )
  # Calculate the extent
  extBuff <- bbox( buff )
  # Convert the extent to a table
  extDF <- tibble( Eastings=extBuff[1, ], Northings=extBuff[2, ] )
  # Determine x:y aspect ratio (for plotting)
  xyRatio <- diff(extDF$Eastings) / diff(extDF$Northings)
  # Read the polygon data: land
  landSPDF <- readOGR( dsn=where$locLand, layer=where$fns$land, verbose=FALSE )
  # Clip the land to the buffer: big
  landCropSPDF <- crop( x=landSPDF, y=extBuff )
  # Convert to data frame
  landCropDF <- landCropSPDF %>%
    fortify( region="id" ) %>%
    rename( Eastings=long, Northings=lat ) %>%
    as_tibble( )
  # Update sections (keep all areas)
  secAllSPDF <- UpdateSections( dat=secRaw, keepAll=TRUE )
  # Dissolve to stat area
  saAllSPDF <- aggregate( x=secAllSPDF, by=list(Temp=secAllSPDF$StatArea),
                          FUN=unique )
  # Dissolve to region
  regAllSPDF <- aggregate( x=secAllSPDF, by=list(Temp=secAllSPDF$SAR),
                           FUN=unique )
  # Determine region centroids
  regCent <- gCentroid( spgeom=regAllSPDF, byid=TRUE )
  # Convert to data frame
  regCentDF <- regCent %>%
    as_tibble( ) %>%
    rename( Eastings=x, Northings=y ) %>%
    mutate( SAR=regAllSPDF$SAR, Region=unlist(allRegions) ) %>%
    arrange( SAR )
  # Convert to data frame and select all regions: sections
  secAllDF <- secAllSPDF %>%
    fortify( region="Section" ) %>%
    rename( Eastings=long, Northings=lat, Section=group ) %>%
    as_tibble( )
  # Convert to data frame and select all regions: statistical areas
  saAllDF <- saAllSPDF %>%
    fortify( region="StatArea" ) %>%
    rename( Eastings=long, Northings=lat, StatArea=group ) %>%
    as_tibble( )
  # Convert to data frame and select all regions: regions
  regAllDF <- regAllSPDF %>%
    fortify( region="SAR" ) %>%
    rename( Eastings=long, Northings=lat, Region=group ) %>%
    as_tibble( )
  # Get a buffer around the region(s) in question
  buffAll <- gBuffer( spgeom=regAllSPDF, width=bMax, byid=FALSE )
  # Calculate the extent
  extAllBuff <- bbox( buffAll )
  # Convert the extent to a table
  extAllDF <- tibble( Eastings=extAllBuff[1, ], Northings=extAllBuff[2, ] )
  # Determine x:y aspect ration (for plotting)
  xyAllRatio <- diff(extAllDF$Eastings) / diff(extAllDF$Northings)
  # Clip the land to the buffer: big
  landAllCropSPDF <- crop( x=landSPDF, y=extAllBuff )
  # Convert to data frame
  landAllCropDF <- landAllCropSPDF %>%
    fortify( region="id" ) %>%
    rename( Eastings=long, Northings=lat ) %>%
    as_tibble( )
  # Update progress message
  cat( "done\n" )
  # Return the data frames etc
  return( list(secDF=secDF, secCentDF=secCentDF,
               grpDF=grpDF, grpCentDF=grpCentDF,
               saDF=saDF, saCentDF=saCentDF,
               regSPDF=regSPDF, regDF=regDF, regCentDF=regCentDF,
               xyRatio=xyRatio, extDF=extDF,
               landCropSPDF=landCropSPDF, landCropDF=landCropDF,
               secAllDF=secAllDF, saAllDF=saAllDF, regAllDF=regAllDF,
               extAllDF=extAllDF, xyAllRation=xyAllRatio,
               landAllCropDF=landAllCropDF) )
}  # End LoadShapefiles function

# Function to make a circle
MakeCircle <- function( center=c(0,0), radius=1, nPts=100 ){
  # Vector of points
  tt <- seq( from=0, to=2*pi, length.out=nPts )
  # X values (math!)
  xx <- center[1] + radius * cos(tt)
  # Y values (and geometry!)
  yy <- center[2] + radius * sin(tt)
  # Return the data (x and y for a circle)
  return( tibble(X=xx, Y=yy) )
}  # End MakeCircle function

# Function to switch from 0/1 to No/Yes
YesNo <- function( x ) {
  # Input is a column/vector/etc of 0s and 1s, and output is No/Yes as an
  # ordered factor (Yes before No)
  # Update values: 0=No, 1=Yes, otherwise NA
  x <- ifelse( x == 0, "No", ifelse(x == 1, "Yes", x) )
  # If if's all Yes/No
  if( all(x %in% c("Yes", "No", NA)) ) {
    # Make an ordered factor
    xFac <- factor( x, levels=c("Yes", "No") )
  } else {  # End if Yes/No, otherwise
    # Just return the data
    xFac <- x
  }  # End if not all Yes/No
  # Return updated values
  return( xFac )
}  # End YesNo function

# Clip to the extent of a supplied sp object
ClipExtent <- function( dat, spObj, bufDist=NA, silent=FALSE ) {
  # Given a set of spatial points, dat, and an spatial polygons object, spObj,
  # determine which points fall inside (or within a given buffer) of the object.
  # Set the spatial X and Y for points that don't overlap (or lie within the
  # buffer) to NA, as they are 'outside' the area (i.e., we assume that the X
  # and Y for these points is wrong) so that we don't show them in charts. We
  # retain the points themselves, because the associated data is still useful.
  # Return the points as a data frame, with updated X and Y info.
  # Creat a buffer around the spatial object, if requested
  if( !is.na(bufDist) ) {
    # Make a buffer
    spObj <- gBuffer( spgeom=spObj, byid=TRUE, width=bufDist )
  }  # End if making a buffer
  # Get NAs (if any)
  isNA <- filter( .data=dat, is.na(Eastings), is.na(Northings) )
  # Message if there are any
  if( nrow(isNA) > 0 & !silent )
    cat( "Point(s) with missing spatial coordinates (NA):", nrow(isNA), "\n" )
  # Wrangle data
  samp <- dat %>%
    filter( !is.na(Eastings) & !is.na(Northings) )
  # If there are rows
  if( nrow(samp) > 0 ) {
    # Make a spatial points object
    spSamp <- SpatialPoints( coords=select(samp, Eastings, Northings),
                             proj4string=CRS(outCRS) )
    # Determine which points are outside the SAR
    inside <- over( x=spSamp, y=spObj )$SAR
    # If any points are outside the SAR
    if( any(is.na(inside)) ) {
      # Set the X and Y to NA
      samp <- samp %>%
        mutate( Eastings=ifelse(is.na(inside), NA, Eastings),
                Northings=ifelse(is.na(inside), NA, Northings) )
      # Message
      if( !silent) cat( "Point(s) outside SAR boundary: set X and Y to NA:",
                        length(inside[is.na(inside)]), "\n" )
    }  # End if any points are outside the SAR
  } else {  # End if there are rows, otherwise
    # Message
    if( !silent )  cat( "There are no geo-referenced points\n" )
  }  # End if there are no rows
  # Wrangle data
  res <- samp %>%
    bind_rows( isNA )
  # Return the data
  return( res )
}  # End ClipExtent function

# Get the decade (or other rounded value) from the year
GetDecade <- function( dat, r=10 ) {
  # Given a vector of years or dates, get the decade, and add an "s" to the end
  # for plot labels. Also deal with NAs.
  # Make sure it's the year
  yr <- as.numeric( format(dat, format="%Y") )
  # Round to nearest decade (or other value)
  decade <- paste( floor(yr/r)*r, "s", sep="" )
  # Fill in NAs
  decade <- ifelse( decade == "NAs", NA, decade )
  # Return the vector
  return( decade )
}  # End GetDecade function

# How to write a long table
WriteLongTable <- function( dat, fn ) {
  # Write the xtable (first time)
  print( x=dat, file=fn, tabular.environment='longtable', floating=FALSE,
         include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE,
         NA.string=NA, include.colnames=FALSE, hline.after=FALSE )
  # Load the xtable
  xTabLong <- readLines( con=fn, warn=FALSE )
  # Find the midrule
  isMid <- grep( pattern="midrule", x=xTabLong )
  # Remove the midrule
  xTabLong <- xTabLong[-isMid]
  # Remove the last line if it's empty
  if( xTabLong[length(xTabLong)] == "  " )
    xTabLong <- xTabLong[-length(xTabLong)]
  # Remove the last line return if there is one
  if( grepl(pattern="\\ ", x=xTabLong[length(xTabLong)]) )
    xTabLong[length(xTabLong)] <-
    gsub( pattern=" \\\\ ", replacement="", x=xTabLong[length(xTabLong)],
          fixed=TRUE )
  # Re-write the xtable
  writeLines( text=xTabLong, con=fn )
  # Add a bottomrule at the end
  #  write( x="\\bottomrule", file=fn, append=TRUE )
}  # End WriteLongTable function

# Convert line endings to Linux
ConvertLineEndings <- function( infile ) {
  # Grab the text
  txt <- readLines( con=infile )
  # Start a connection (binary)
  f <- file( description=infile, open="wb" )
  # Write file contents
  cat( txt, file=f, sep="\n" )
  # Close the connection
  close( con=f )
}  # End ConvertLineEndings function

# Bold and latex symbols
Bold2 <- function(x) {paste('\\textbf{',sanitize(x, type="latex"),'}', sep ='')}

# Change numbers into words (1:9)
#' @export
num_to_word <- function( x ) {
  # Get the list of numbers and words
  vec <- c( one=1, two=2, three=3, four=4, five=5, six=6, seven=7, eight=8,
            nine=9 )
  # Get the index corresponding to the number
  ind <- which( vec == x )
  # Get the name
  res <- names( vec[ind] )
  # Error if the name isn't there
  if( length(res)==0 ) stop( "Numbers can be from 1 to 9 only." )
  # Return the result
  res
}  # End function num_to_word

# Calculate percent change, difference, etc
DeltaPercent <- function( x, nYrs=1, type ) {
  # Numerator: difference
  top <- x - lag( x, n=nYrs )
  # Denominator: depends on the type
  if( type=="PctChange" ) bot <- lag( x, n=nYrs )            # Previous value
  if( type=="PctDiff" )   bot <- ( x + lag(x, n=nYrs) ) / 2  # Mean of values
  # Error if there's no denominator
  if( !exists("bot") )  stop( "No denominator; check 'type'" )
  # Calculate percent change
  res <- top / bot * 100
  # Return the result
  return( res )
}  # End DeltaPercent function

read.admb <- function(ifile){
  ret=read.fit(ifile)

  fn=paste(ifile,'.rep', sep='')
  A=read.rep(fn)
  A$fit=ret

  pfn=paste(ifile,'.psv',sep='')
  if(file.exists(pfn))
    A$post.samp=read.psv(pfn)

  return(A)
}

read.fit <- function(ifile){
  # __Example:
  #	file <-("~/admb/simple")
  #	A <- reptoRlist(file)
  #	Note there is no extension on the file name.

  ## The following is a contribution from:
  ## Anders Nielsen that reads the par & cor files.
  ret<-list()
  parfile<-as.numeric(scan(paste(ifile,'.par', sep=''),
                           what='', n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar<-as.integer(parfile[1])
  ret$nlogl<-parfile[2]
  ret$maxgrad<-parfile[3]
  file<-paste(ifile,'.cor', sep='')
  lin<-readLines(file)
  ret$npar<-length(lin)-2
  ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2])
  sublin<-lapply(strsplit(lin[1:ret$npar+2], ' '),function(x)x[x!=''])
  ret$names<-unlist(lapply(sublin,function(x)x[2]))
  ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
  ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))
  ret$cor<-matrix(NA, ret$npar, ret$npar)
  corvec<-unlist(sapply(1:length(sublin), function(i)sublin[[i]][5:(4+i)]))
  ret$cor[upper.tri(ret$cor, diag=TRUE)]<-as.numeric(corvec)
  ret$cor[lower.tri(ret$cor)] <- t(ret$cor)[lower.tri(ret$cor)]
  ret$cov<-ret$cor*(ret$std%o%ret$std)
  return(ret)
}

read.psv <- function(fn, nsamples = 10000){
  #This function reads the binary output from ADMB
  #-mcsave command line option.
  #fn = paste(ifile,'.psv',sep='')
  filen <- file(fn, "rb")
  nopar <- readBin(filen, what = integer(), n = 1)
  mcmc <- readBin(filen, what = numeric(), n = nopar * nsamples)
  mcmc <- matrix(mcmc, byrow = TRUE, ncol = nopar)
  close(filen)
  return(mcmc)
}

# A simple function for creating transparent colors
# Author: Nathan Stephens (hacks package)
colr <-function(col.pal=1,a=1){
  col.rgb<-col2rgb(col.pal)/255
  rgb(t(col.rgb),alpha=a)
}
