#' Make a catch xtable for latex document.
#'
#' Depends on the format of the catch.csv file. The function
#' requires that the input data frame 'catches' has at least
#' 3 columns called 'Year', 'CatchKG', and 'DiscardedKG'
#'
#' @param catches output of the [load.catches()] function
#' @param start.yr first year to show in table
#' @param end.yr last year to ahow in table
#' @param weight.factor value to divide the catches by
#' @param digits number of decimal points in table
#' @param areas a list of vectors of areas to group together, e.g.:
#'  areas <- list(c(3,4), c(5,6,7,8,9) means 3CD together, and 5ABCDE
#'  together as two groups. If NULL, a summary table of catch/discard
#'  by year will be produced
#' @param xcaption caption for table
#' @param xlabel label used as a reference in latex
#' @param font.size font size
#' @param space.size size of spaces between text in table
#' @param placement latex code for placement of the table in document
#'
#' @return an xtable in the proper format for catch and discards
#' @export
#' @importFrom gfutilities latex.bold latex.amp latex.mrow latex.mcol latex.cmidr latex.size.str get.align
#' @importFrom xtable xtable
make.catches.table <- function(catches,
                               start.yr,
                               end.yr,
                               weight.factor = 1000,
                               digits = 3,
                               areas = NULL,
                               xcaption = "default",
                               xlabel   = "default",
                               font.size = 9,
                               space.size = 10,
                               placement = "H"){

  yrs <- start.yr:end.yr
  catches <- catches[catches$Year %in% yrs,]

  agg <- function(d, nm){
    ## Aggregate the data frame d by Year for the column name given in nm
    ## Also applies the weight factor
    d <- d[, c("Year", nm)]
    ct <- aggregate(d[, nm],
                    list(d$Year),
                    sum,
                    na.rm = TRUE)
    colnames(ct) <- c("year", "catch")
    ct$catch <- ct$catch / weight.factor
    ct
  }

  if(!is.null(areas)){
    tab <- data.frame()
    header.vec <- NULL
    for(area.grp in 1:length(areas)){
      curr.area.grp <- areas[[area.grp]]
      tmp <- catches[catches$AreaCode %in% curr.area.grp,]

      ct <- agg(tmp, "CatchKG")
      d.ct <- agg(tmp, "DiscardedKG")

      yrs <- ct$year
      ## Bind the two amounts by year into table
      tot.catch <- cbind(yrs,
                         f(ct$catch, digits),
                         f(d.ct$catch, digits))
      colnames(tot.catch) <- c("Year",
                               "Landings",
                               "Discards")
      if(!nrow(tab)){
        tab <- tot.catch
      }else{
        tab <- merge(tab, tot.catch, by = "Year", all = TRUE)
      }

      ## Build the area descriptor
      ## If 3 and 4 are together, label 3CD, if only one of them label 3C or 3D
      ## If 5 - 9 are together, label 5ABCDE with 5=A, 6=B, 7=C, 8=D, and 9=E
      header <- NULL
      if(3 %in% curr.area.grp |
         4 %in% curr.area.grp){
        header <- paste0(header, "3")
      }
      if(3 %in% curr.area.grp){
        header <- paste0(header, "C")
      }
      if(4 %in% curr.area.grp){
        header <- paste0(header, "D")
      }
      if(5 %in% curr.area.grp |
         6 %in% curr.area.grp |
         7 %in% curr.area.grp |
         8 %in% curr.area.grp |
         9 %in% curr.area.grp){
        header <- paste0(header, "5")
      }
      if(5 %in% curr.area.grp){
        header <- paste0(header, "A")
      }
      if(6 %in% curr.area.grp){
        header <- paste0(header, "B")
      }
      if(7 %in% curr.area.grp){
        header <- paste0(header, "C")
      }
      if(8 %in% curr.area.grp){
        header <- paste0(header, "D")
      }
      if(9 %in% curr.area.grp){
        header <- paste0(header, "E")
      }
      header.vec <- c(header.vec, latex.bold(header))
    }
    ## Set up the column headers, i.e. Landings & Discards
    colnames(tab) <- c("",
                       rep(c(latex.bold("Landings"),
                             latex.bold("Discards")),
                           length(header.vec)))
    ## Add the extra header spanning multiple columns
    addtorow <- list()
    addtorow$pos <- list()
    addtorow$pos[[1]] <- -1
    addtorow$pos[[2]] <- nrow(tab)
    addtorow$command <-
      c(paste0("\\toprule ",
               latex.mrow(3, "*", latex.bold("Year")),
               latex.amp(),
               latex.mcol(length(header.vec) * 2,
                          "c",
                          latex.bold("Area")),
               latex.nline,
               latex.cmidr(paste0("2-", length(header.vec) * 2 + 1), "lr"),
               paste(sapply(1:length(header.vec),
                      function(x){paste0(latex.amp(),
                                         latex.mcol(2,
                                                    "c",
                                                    header.vec[x]))}),
                     collapse = ""),
               for(i in seq(2, (length(header.vec) * 2 + 1), by = 2)){
                 paste0(latex.cmidr(paste0(i, "-", i + 1), "lr"))
               },
               latex.nline),
        "\\bottomrule")
    size.string <- latex.size.str(font.size, space.size)
    print(xtable(tab,
                 caption = xcaption,
                 label = xlabel,
                 align = get.align(ncol(tab))),
          caption.placement = "top",
          include.rownames = FALSE,
          sanitize.text.function = function(x){x},
          size = size.string,
          add.to.row = addtorow,
          table.placement = placement,
          hline.after = c(0),
          NA.string = "--",
          booktabs = TRUE)
  }else{
    ct <- agg(catches, "CatchKG")
    d.ct <- agg(catches, "DiscardedKG")

    ## Bind the two amounts by year into table
    tab <- cbind(yrs,
                 f(ct$catch, digits),
                 f(d.ct$catch, digits))
    colnames(tab) <- c(latex.bold("Year"),
                       latex.bold("Landings"),
                       latex.bold("Discards"))

    size.string <- latex.size.str(font.size, space.size)
    print(xtable(tab,
                 caption = xcaption,
                 label = xlabel,
                 align = get.align(ncol(tab))),
          caption.placement = "top",
          include.rownames = FALSE,
          sanitize.text.function = function(x){x},
          size = size.string,
          table.placement = placement)
  }
}

#' Plot the catches in a stacked-barplot with legend
#'
#' @param catches output of the [load.catches()] function
#' @param gear.names names of the gears
#' @param leg.y.loc y-based location to place the legend
#' @param leg.cex text size for legend
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot aes geom_bar
make.catches.plot <- function(catches,
                              gear.names,
                              leg.y.loc = 430,
                              leg.cex = 1){
  old.par <- par()
  on.exit(par(old.par))
  j <- as.data.frame(catches$dat$catch)
  ## Make the gear catches into a list by gear
  k <- lapply(gear.names[,1],
              function(x){
                j[j$gear == x,]
              })
  ## Remove zero-row gears (e.g. survey gears)
  k <- k[sapply(k, nrow) > 0]
  g <- ggplot(j, aes(year, gear, value)) + geom_bar(aes(colour=gear))
  g
}
