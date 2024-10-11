#' Create a table comparing estimates of q across models
#'
#' @param models A list of iscam model objects (class [mdl_lst_cls])
#' @param model.names A vector of nice names (for plotting) for the `models`
#' @param digits Number of decimal points to show in table
#' @param xcaption Caption
#' @param xlabel Latex label
#' @param font.size Size of font in table
#' @param space.size Size of spaces between cells
#' @param placement See [xtable::print.xtable()]
#'
#' @description
#' Make a table of the values of q, including quantiles
#'
#' @return an [xtable::xtable()] object
#'
#' @export
table_sens_q <- function(models,
                         model.names,
                         digits = 3,
                         xcaption = "default",
                         xlabel   = "default",
                         font.size = 9,
                         space.size = 10,
                         placement = "H"){

  quants <- lapply(models, function(x){
    t(f(x$mcmccalcs$q.quants, digits))})
  tab <- do.call(cbind, quants)
  tab <- cbind(rownames(tab), tab)
  col.names <- colnames(tab)
  col.names[1] <- "$q_k$"
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  com <- paste0("\\toprule",
                latex.bold("Index"),
                latex.amp())
  for(i in 1:length(quants)){
    com <- paste0(com,
                  latex.mcol(ncol(quants[[i]]),
                             "c",
                             latex.bold(model.names[i])),
                  ifelse(i != length(quants), latex.amp(), ""))
  }
  com <- paste(com, latex.nline)
  addtorow$command <- com

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
        booktabs = TRUE)
}

