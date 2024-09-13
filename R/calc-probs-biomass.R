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
#' @param year_to_year If `TRUE`, calculate the probabilities of decline or
#' increase from one year to the next (depending on what `prob_gt` is, if
#' it is `TRUE`, it will be the probability of decreasing, if it is `FALSE`,
#' it will be the probability of increasing)
#' @param format If 'latex' modify column headers with latex code, if
#' 'html' modify column headers with html code
#' @param ... Absorb arguments intended for other functions
#'
#' @return A data frame containing N rows where N is the number of unique
#' catch levels in `d`
#' @export
calc_probs_biomass <- function(d,
                               refpt = 0.2,
                               prob_gt = TRUE,
                               year_to_year = FALSE,
                               format = c("latex", "html"),
                               ...){

  format <- match.arg(format)

  esc_open_prob <- ifelse(format == "html", "\\(P(", "P(")
  esc_bo <- ifelse(format == "html", "B_{0}", "B\\textsubscript{0}")
  esc_open_b <- ifelse(format == "html", "B_{", "B\\textsubscript{")
  esc_close_b <- "}"
  esc_close_prob <- ifelse(format == "html", "\\)", ")")

  if(year_to_year){

    catch_vals <- d$catch |>
      unique() |>
      sort() |>
      enframe(name = NULL, value = "catch")

    # Get inds of the years
    yrs_regex <- "^(2[0-9]{3})$"
    inds <- grep(yrs_regex, names(d))
    yrs <- grep(yrs_regex, names(d), value = TRUE) |>
      as.numeric()
    # Number of probs are the number of inds minus one
    yrs_start <- as.character(yrs[1:(length(inds) - 1)])
    yrs_end <- as.character(yrs[2:length(inds)])

    # Split projections into a list of data frames of projections, 1 for each
    # catch level
    d_lst <- d |>
      split(~catch)
    # Remove the `catch` column from the data frames in the list
    d_lst <- d_lst |>
      map(~{
        .x |>
          select(-catch)
      })

    # Calculate probability columns for dropping biomass year-to-year
    d_out <- map(d_lst, \(proj_tbl){
      # For each projection table at a given catch level ...
      k <- proj_tbl |>
        map2(seq_along(proj_tbl), ~{
          if(.y > 1){
            if(prob_gt){
              sum(.x > proj_tbl[[.y - 1]]) / length(.x)
            }else{
              sum(.x < proj_tbl[[.y - 1]]) / length(.x)
            }
          }else{
            NULL
          }
        })

      k[lengths(k) == 0] <- NULL
      k <- k |> map_dbl(~{.x})
      names(k) <- map(as.numeric(names(k)), ~{
        paste0(esc_open_prob,
               esc_open_b,
               .x,
               esc_close_b,
               ifelse(prob_gt,
                      " > ",
                      " < "),
               esc_open_b,
               .x - 1,
               esc_close_b,
               esc_close_prob)
      })
      k
    }) |>
      map_dfr(~{.x})

    d_out <- bind_cols(catch_vals, d_out)

  }else{

    d_out <- d |>
      group_by(catch) |>
      summarize(across(everything(),
                       ~{
                         if(prob_gt){
                           sum(.x > refpt) / length(.x)
                         }else{
                           sum(.x < refpt) / length(.x)
                         }}))

    nms <- names(d_out)
    nms[nms != "catch"] <- paste0(esc_open_prob,
                                  esc_open_b,
                                  nms[nms != "catch"],
                                  esc_close_b,
                                  ifelse(prob_gt,
                                         " > ",
                                         " < "),
                                  refpt,
                                  esc_bo,
                                  esc_close_prob)
    names(d_out) <- nms
  }

  d_out
}
