#' Make a table of maturity data
#'
#' @param mat the maturity vectors data as loaded from maturity-vectors.csv
#' @param digits number of decimal places for the values
#' @param xcaption caption to appear in the calling document
#' @param xlabel the label used to reference the table in latex
#' @param font.size size of the font for the table
#' @param space.size size of the vertical spaces for the table
#' @param placement latex code for placement of the table in document
#'
#' @return
#' @export
#' @importFrom xtable xtable
#' @importFrom gfutilities latex.bold latex.amp latex.mcol latex.size.str f get.align
make.maturity.table <- function(mat,
                                digits = 3,
                                xcaption = "default",
                                xlabel   = "default",
                                font.size = 9,
                                space.size = 10,
                                placement = "H"){

  tab <- lapply(unique(mat$Stock),
              function(x){
                st <- mat[mat$Stock == x,]
                mdl <- lapply(unique(st$Model),
                            function(y){
                              st.mdl <- mat[mat$Stock == x & mat$Model == y,]
                              c(x, y, st.mdl$Maturity)
                            })
                do.call(rbind, mdl)
              })
  tab <- do.call(rbind, tab)
  age <- unique(mat$Age)
  ## Format table data
  tab[, 3:7] <- f(as.numeric(tab[, 3:7]), 4)
  tab[, 8:11] <- f(as.numeric(tab[, 8:11]), 1)

  colnames(tab) <- c(latex.bold("Stock"),
                     latex.bold("Model"),
                     latex.bold(age))
  addtorow <- list()
  addtorow$pos <- list(-1, 2, 4, 6, 8)
  addtorow$command <- c(paste0("\\toprule",
                               latex.amp(2),
                               latex.mcol(length(age),
                                          "c",
                                          latex.bold("Maturity at age")),
                               latex.nline),
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ")

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        add.to.row = addtorow,
        booktabs = TRUE)

}

#' Make maturity sensitivity table - Herring specific
#'
#' @param mat the data loaded from the maturity-sensitivity-results-table.csv file
#' @param which.model to make the table for, 1 = AM1, 2 = AM2
#' @param end.yr used for the end SB year in the table
#' @param digits number of decimal places for the values
#' @param xcaption caption to appear in the calling document
#' @param xlabel the label used to reference the table in latex
#' @param font.size size of the font for the table
#' @param space.size size of the vertical spaces for the table
#' @param placement latex code for placement of the table in document
#'
#' @return
#' @export
#' @importFrom xtable xtable
#' @importFrom gfutilities latex.bold latex.amp latex.mcol latex.size.str latex.cmidr f get.align
make.maturity.sens.table <- function(mat,
                                     which.model = 1,
                                     end.yr,
                                     digits = 3,
                                     xcaption = "default",
                                     xlabel   = "default",
                                     font.size = 9,
                                     space.size = 10,
                                     placement = "H"){

  if(which.model == 1){
    model <- "AM1"
  }else if(which.model == 2){
    model <- "AM2"
  }else{
    stop("which.model must be 1 or 2.")
  }

  tab <- mat[mat$Model == model,]
  base <- tab[tab$Maturity == "Base", -c(2,3)]
  sens <- tab[tab$Maturity == "Selectivity", -c(2,3)]
  tab <- cbind(base, sens)
  tab <- tab[,-6]

  tab[,1] <- c("HG",
               "PRD",
               "CC",
               "SOG",
               "WCVI")

  addtorow <- list()
  addtorow$pos <- list(0)
  addtorow$command <- paste0(latex.amp(),
                             latex.mcol(4,
                                        "c",
                                        latex.bold("Base Case")),
                             latex.amp(),
                             latex.mcol(4,
                                        "c",
                                        latex.mlc(c("Sensitivity Case",
                                                    "maturity set to selectivity"))),
                             latex.nline,
                             ## underscores
                             latex.cmidr("2-5", "lr"),
                             " ",
                             latex.cmidr("6-9", "lr"),
                             latex.bold("Stock"),
                             latex.amp(),
                             latex.bold("SB\\subscr{0}"),
                             latex.amp(),
                             latex.bold(paste0("SB\\subscr{", end.yr, "}")),
                             latex.amp(),
                             latex.bold("F\\subscr{MSY}"),
                             latex.amp(),
                             latex.bold("MSY"),
                             latex.amp(),
                             latex.bold("SB\\subscr{0}"),
                             latex.amp(),
                             latex.bold(paste0("SB\\subscr{", end.yr, "}")),
                             latex.amp(),
                             latex.bold("F\\subscr{MSY}"),
                             latex.amp(),
                             latex.bold("MSY"),
                             latex.nline)

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        booktabs = TRUE)
}
