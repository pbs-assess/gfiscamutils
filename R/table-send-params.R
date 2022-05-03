#' Create a table of parameter estimates for multiple models
#'
#' @description
#' Make a table of the sensitivity parameter information as found in
#' the CSV file in the data directory
#'
#' @rdname make.parameters.table
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc latex.cmidr
#' @importFrom xtable xtable
table_sens_params <- function(tab,
                              xcaption = "default",
                              xlabel   = "default",
                              font.size = 9,
                              space.size = 10,
                              placement = "H"){


  tab <- sub("\\|", ",", as.matrix(tab))
  colnames(tab) <- c(latex.bold("Scenario"),
                     latex.bold("Description"),
                     latex.bold("Parameters"))

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
        booktabs = TRUE)
}
