#' Calculate the probabilities that biomasses are under or over given
#' reference points
#'
#' @description
#' Calculate the probabilities that biomasses are under or over given
#' reference points or that they decline or increase from one year to the
#' next
#'
#' @param d A data frame containing a column named `catch` and other columns
#' names as years of depletion
#' @param refpt The depletion value that is checked against in the data frame
#' `d`
#' @param prob_gt Stand for 'probability greater than'. If `TRUE`,
#' @param format If 'latex' modify column headers with latex code, if
#' 'html' modify column headers with html code
#'
#' @return
#' @export
calc_probs_biomass <- function(d,
                               refpt = 0.2,
                               prob_gt = TRUE,
                               format = c("latex", "html")){

  format <- match.arg(format)

  d_out <- d |> group_by(catch) |>
    summarize(across(everything(),
                     ~{
                       if(prob_gt){
                         sum(.x > refpt) / length(.x)
                       }else{
                         sum(.x < refpt) / length(.x)
                       }}))

  esc_open_prob <- ifelse(format == "html", "\\(P(", "P(")
  esc_bo <- ifelse(format == "html", "B_{0}", "B\\textsubscript{0}")
  esc_open_b <- ifelse(format == "html", "B_{", "B\\textsubscript{")
  esc_close_b <- "}"
  esc_close_prob <- ifelse(format == "html", "\\)", ")")

  nms <- names(d_out)
  nms[nms != "catch"] <- paste0(esc_open_prob,
                                esc_open_b,
                                nms[nms != "catch"],
                                ifelse(prob_gt,
                                       "} > ",
                                       "} < "),
                                refpt,
                                esc_bo,
                                esc_close_prob)
  names(d_out) <- nms

  d_out
}
