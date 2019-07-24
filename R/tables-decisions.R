#' Produce a decision xtable for the given iscam model
#'
#' @param model An iscal model object
#' @param digits Number of decimal places for the values in the table
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#'
#' @return An xtable
#' @export
#' @importFrom xtable xtable
#' @importFrom gfutilities f
make.decision.table <- function(model,
                                digits = 3,
                                xcaption = "default",
                                xlabel   = "default",
                                font.size = 10,
                                space.size = 11,
                                placement = "H"){

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  tab <- model$mcmccalcs$proj.dat
  ## Don't format the first column as it should be the TAC
  tab[, -1] <- f(tab[, -1], digits)
  ## Required to stop xtable from adding decimal points to TACs
  tab[, 1] <- f(tab[,1], 0)

  ## Remove fixed cutoffs from decision tables
  tab <- tab[,-c(5,6)]

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = rep("r", times=ncol(tab)+1)),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        booktabs = TRUE)
}
