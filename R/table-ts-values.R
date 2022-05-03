#' Create a table of time series estimates
#'
#' @description
#' Make a table for values such as biomass and recruitment
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc
#' @importFrom xtable xtable
#' @importFrom rosettafish en2fr
table_ts_values <- function(model,
                            type,
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

  if(type == 1){
    out.dat <- model$mcmccalcs$sbt.quants
  }else if(type == 2){
    out.dat <- model$mcmccalcs$recr.quants
  }else if(type == 3){
    out.dat <- model$mcmccalcs$f.mort.quants[[1]]
  }else if(type == 4){
    out.dat <- model$mcmccalcs$u.mort.quants[[1]]
  }else if(type == 5){
    out.dat <- model$mcmccalcs$depl.quants
  }else{
    stop("Type ", type, " not implemented.")
  }

  tab <- f(t(out.dat), digits)
  tab <- cbind(rownames(tab), tab)
  tab <- tab[tab[,1] >= syr,]
  col.names <- en2fr(colnames(tab), translate, allow_missing = TRUE)
  col.names[1] <- en2fr("Year", translate)
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

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
        booktabs = TRUE,
        tabular.environment = tabular.environment)
}

