#' Make a table of reference points
#'
#' @description
#' Make a table of reference points
#'
#' @rdname make.parameters.table
#' @param translate Logical. Translate to french if TRUE
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc
#' @importFrom xtable xtable
#' @importFrom rosettafish en2fr
table_ref_points <- function(model.am2,
                             digits = 3,
                             xcaption = "default",
                             xlabel   = "default",
                             font.size = 9,
                             space.size = 10,
                             placement = "H",
                             translate = FALSE){

  if(class(model.am2) == mdl_lst_cls){
    model <- model[[1]]
    if(class(model) != msl_cls){
      stop("The structure of the model.am2 list is incorrect.")
    }
  }

  tab.am2 <- model.am2$mcmccalcs$r.quants
  row.names <- tab.am2[,1]
  if( translate ) {
    row.names <- gsub( pattern="SB", replacement=en2fr("SB", translate),
                       x=row.names )
    row.names <- gsub( pattern="Proportion aged",
                       replacement=en2fr("Proportion aged", translate),
                       x=row.names )
  }
  col.names.am2 <- colnames(tab.am2)
  col.names.am2[1] <- paste0("\\textbf{", en2fr("Reference point", translate), "}")
  ## Remove latex rownames
  tab.am2 <- as.matrix(tab.am2[,-1])
  tab.am2 <- apply(tab.am2, c(1, 2) , as.numeric)
  ## Format the non-proportion data to digits
  n.row <- nrow(tab.am2)
  tab.am2.non <- tab.am2[-c(n.row - 1, n.row), ]
  tab.am2.non <- f(tab.am2.non, digits)
  ## Format the proportion-at-age to two digits only
  tab.am2.prop <- tab.am2[c(n.row - 1, n.row), ]
  tab.am2.prop <- f(tab.am2.prop, 2)
  tab.am2 <- rbind(tab.am2.non, tab.am2.prop)
  # Proportions don't actually have confidence bounds
  tab.am2[6,1] <- "-"
  tab.am2[6,3] <- "-"
  tab.am2[10,1] <- "-"
  tab.am2[10,3] <- "-"
  tab.am2[11,1] <- "-"
  tab.am2[11,3] <- "-"

  tab <- cbind(row.names,
               as.data.frame(tab.am2))

  colnames(tab) <- col.names.am2

  addtorow <- list()
  addtorow$pos <- list(-1, nrow(tab))
  addtorow$command <- c( "\\toprule", "\\bottomrule" )

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
        hline.after = c(0),
        add.to.row = addtorow,
        booktabs = TRUE)
}

