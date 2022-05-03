#' Create a table comparing estimates of q across models
#'
#' @description
#' Make a table of the values of q, including quantiles
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc latex.cmidr
#' @importFrom xtable xtable
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

