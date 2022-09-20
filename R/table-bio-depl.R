#' Create a table containing biomass and depletion estimates
#'
#' @description
#' Make a table with both spawning biomass and depletion in it.
#' Based on [table_ts_value()], but wider with both and extra headers
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc latex.cmidr
#' @importFrom xtable xtable
table_bio_depl <- function(model,
                           syr,
                           digits = 3,
                           xcaption = "default",
                           xlabel   = "default",
                           font.size = 9,
                           space.size = 10,
                           placement = "H",
                           tabular.environment = "tabular",
                           translate = FALSE){

  if(class(model) == mdl_lst_cls){
    model <- model[[1]]
    if(class(model) != msl_cls){
      stop("The structure of the model list is incorrect.")
    }
  }

  out.dat <- model$mcmccalcs$sbt.quants
  out.dat <- rbind(out.dat, model$mcmccalcs$depl.quants)

  tab <- f(t(out.dat), digits)
  tab <- cbind(rownames(tab), tab)
  tab <- tab[tab[,1] >= syr,]
  ## Remove the projection year (last row)
  tab <- tab[-nrow(tab),]

  col.names <- tr(colnames(tab))
  col.names[1] <- tr("Year")
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

  addtorow <- list()
  addtorow$pos <- list(-1, nrow(tab))
  addtorow$command <- c(paste0("\\toprule",
                               latex.amp(),
                               latex.mcol(4,
                                          "c",
                                          latex.bold(tr("Spawning biomass", translate))),
                               latex.amp(),
                               latex.mcol(4,
                                          "c",
                                          latex.bold(tr("Depletion", translate))),
                               latex.nline,
                               latex.cmidr("2-5", "lr"),
                               " ",
                               latex.cmidr("6-9", "lr")),
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
        booktabs = TRUE,
        tabular.environment = tabular.environment)
}

