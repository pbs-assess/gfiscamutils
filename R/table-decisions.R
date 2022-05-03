#' Create a decision table for iSCAM models
#'
#' @description
#' Produce a decision table for the given iscam model
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param bo_refpts Vector of two proportional values for the limit reference
#' point and Upper stock reference. Values are 0.2B0 and 0.4B0 by default
#' @param bmsy_refpts Vector of two proportional values for the limit reference
#' point and Upper stock reference. Values are 0.4BMSY and 0.8BMSY by default
#' @param digits Number of decimal places for the values in the table
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
#' @importFrom gfutilities f
table_decisions <- function(model,
                            bo_refpts = c(0.2, 0.4),
                            bmsy_refpts = c(0.4, 0.8),
                            bold_header = FALSE,
                            digits = 2,
                            ...){

  if(class(model) != mdl_cls){
    if(class(model) != mdl_lst_cls){
      stop("`model` is not a gfiscamutils::mdl_cls class (",mdl_cls, ")")
    }
    stop("`model` is a `gfiscamutils::mdl_lst_cls` class (",mdl_lst_cls, ")\n",
         "  It should be a `gfiscamutils::mdl_cls` class (",mdl_cls, ")")
  }

  ctl_options <- model$proj$ctl.options %>%
    as_tibble(rownames = "variable") %>%
    rename(value = V1)
  start_yr <- ctl_options %>%
    filter(variable == "syrmeanm") %>%
    pull(value)
  end_yr_minus2 <- ctl_options %>%
    filter(variable == "nyrmeanm") %>%
    pull(value)
  end_yr_minus1 <- end_yr_minus2 + 1
  end_yr <- end_yr_minus1 + 1
  b_sy <- paste0("B", start_yr)
  b_ey <- paste0("B", end_yr)
  b_ey_minus1 <- paste0("B", end_yr_minus1)
  b_ey_minus2 <- paste0("B", end_yr_minus2)

  bo_raw <- model$mcmccalcs$params$bo
  bmsy_raw <- model$mcmccalcs$params$bmsy
  bo_refvals <- map(bo_refpts, ~{
    bo_raw * .x
  }) %>%
    `names<-`(bo_refpts)
  bmsy_refvals <- map(bmsy_refpts, ~{
    bmsy_raw * .x
  }) %>%
    `names<-`(bmsy_refpts)

  proj <- model$mcmccalcs$proj
  tab <- map(proj, ~{
    out <- c(.x$TAC[1],
             length(which(.x[, b_ey] < bo_refvals[[1]])) / nrow(.x),
             length(which(.x[, b_ey] < bo_refvals[[2]])) / nrow(.x),
             length(which(.x[, b_ey] < bmsy_refvals[[1]])) / nrow(.x),
             length(which(.x[, b_ey] < bmsy_refvals[[2]])) / nrow(.x),
             length(which(.x[, b_ey] < .x[, b_ey_minus1])) / nrow(.x))
    enframe(out) %>%
      mutate(name = c("TAC",
                      latex.mlc(c(paste0("P(B\\textsubscript{", end_yr, "}<"),
                                  paste0(bo_refpts[1], "B\\textsubscript{0})")),
                                make.bold = bold_header),
                      latex.mlc(c(paste0("P(B\\textsubscript{", end_yr, "}<"),
                                  paste0(bo_refpts[2], "B\\textsubscript{0})")),
                                make.bold = bold_header),
                      latex.mlc(c(paste0("P(B\\textsubscript{", end_yr, "}<"),
                                  paste0(bo_refpts[1], "B\\textsubscript{MSY})")),
                                make.bold = bold_header),
                      latex.mlc(c(paste0("P(B\\textsubscript{", end_yr, "}<"),
                                  paste0(bo_refpts[2], "B\\textsubscript{MSY})")),
                                make.bold = bold_header),
                      latex.mlc(c(paste0("P(B\\textsubscript{", end_yr, "}<"),
                                  paste0("B\\textsubscript{", end_yr_minus1, "})")),
                                make.bold = bold_header))) %>%
               pivot_wider(names_from = "name", values_from = "value")
  }) %>%
    bind_rows() %>%
    mutate_at(.vars = vars(-TAC), ~{f(.x, digits)}) %>%
    mutate(TAC = f(TAC, 0))

  if(fr()){
    names(tab)[names(tab) == "TAC"] <- latex.mlc(c("Prise",
                                                   "(milliers de t)"),
                                                 make.bold = bold_header)
  }else{
    names(tab)[names(tab) == "TAC"] <- latex.mlc(c("Catch",
                                                   "(thousand t)"),
                                                 make.bold = bold_header)
  }

  csas_table(tab,
             bold_header = FALSE,
             align = rep("r", ncol(tab)),
             col_names_align = rep("r", ncol(tab)),
             ...)
}
