#' Calculate and insert columns containing arbitrary quantiles for a
#' particular column
#'
#' @description
#' Calculate and insert columns containing arbitrary quantiles for a
#' particular column
#'
#' @param df A [data.frame]
#' @param col A column name on which to perform the calculations. Must be in
#' `df` or an error will be thrown
#' @param probs A vector of quantile probabilities to pass to
#' [stats::quantile()]
#' @param include_mean If `TRUE`, include the mean in the output
#'
#' @return A [data.frame] with a new column for each value in the
#' `probs` vector
#' @importFrom purrr set_names partial
#' @export
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(purrr)
#' pq <- tribble(
#'   ~year, ~grp, ~val,
#'   2000,    1,  2.1,
#'   2001,    1,  3.4,
#'   2002,    1,  4.5,
#'   2003,    1,  5.6,
#'   2004,    1,  6.7,
#'   2000,    2,  3.1,
#'   2001,    2,  4.4,
#'   2002,    2,  5.5,
#'   2003,    2,  6.6,
#'   2004,    2,  8.7,
#'   2000,    3, 13.1,
#'   2001,    3, 14.4,
#'   2002,    3, 15.5,
#'   2003,    3, 16.6,
#'   2004,    3, 18.7)
#'
#' probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
#'
#' yrs <- sort(unique(pq$year))
#' df <- pq %>%
#'   group_by(year) %>%
#'   group_map(~ calc_quantiles(.x, col = "val", probs = probs)) %>%
#'   map_df(~{.x}) %>%
#'   mutate(year = yrs) %>%
#'   select(year, everything())
calc_quantiles <- function(df = NULL,
                           col = NULL,
                           probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                           include_mean = FALSE){

  stopifnot(col %in% names(df))
  stopifnot(class(df[[col]]) == "numeric")

  col_sym <- sym(col)
  out <- summarize_at(df,
                      vars(!!col_sym),
                      map(probs,
                          ~partial(quantile, probs = .x, na.rm = TRUE)) |>
                        set_names(probs))

  if(include_mean){
    out <- out |>
      mutate(avg = mean(df[[col]]))
  }
  out
}

#' Calculate quantiles across groups for a given column
#'
#' @description Calculate quantiles across groups for a given column
#'
#' @rdname calc_quantiles
#'
#' @param df A [data.frame] with columns with names given by `grp_cols`
#' and `col`
#' @param grp_cols A vector of column names to use for grouping the data
#' @param cols The column names to use as values to calculate quantiles for.
#' If `NULL`, all columns not in `grp_cols` will be calculated and the
#' quantiles will appear in a column
#' @param cols_rm A vector of names of columns to remove from the calculations.
#' Only used if `cols` is `NULL`
#' @param probs A vector of quantiles to pass to [stats::quantile()]
#' @param include_mean If TRUE, include the mean in the output
#' @param grp_names The column namesto use for labeling the grouped columns.
#' By default it is the same as the grouping columns (`grp_cols`).
#'
#' @return A [data.frame] containing the quantile values with one row per
#' group represented by `grp_cols`
#' @importFrom rlang sym syms
#' @importFrom dplyr group_map summarize_at summarize_all vars distinct
#' @importFrom dplyr full_join bind_cols
#' @importFrom tidyr pivot_longer
#' @export
calc_quantiles_by_group <- function(df = NULL,
                                    grp_cols = NULL,
                                    cols = NULL,
                                    cols_rm = "posterior",
                                    grp_names = grp_cols,
                                    probs = c(0.025, 0.5, 0.975),
                                    include_mean = FALSE){

  # Note that if any of these contain `NULL`, `all()` will return `TRUE`
  stopifnot(all(grp_cols %in% names(df)))
  stopifnot(all(cols %in% names(df)))

  grp_cols_sym <- syms(grp_cols)
  grp_names_sym <- syms(grp_names)

  if(is.null(cols)){
    cols <- names(df)[!names(df) %in% grp_cols]
    if(!is.null(cols_rm)){
      cols <- cols[!cols %in% cols_rm]
    }
  }
  grp_vals <- df |>
    select(!!!grp_names) |>
    distinct()

  # Re-level the gears so they match the calculated output rows
  lvls <- grp_vals$gear |> unique()
  grp_vals <- grp_vals |>
    mutate(gear = factor(gear)) |>
    mutate(gear = forcats::fct_relevel(gear, lvls))

  df <- df |>
    mutate(gear = factor(gear)) |>
    mutate(gear = forcats::fct_relevel(gear, lvls))

  lst_df <- map(cols, function(col){
    df |>
      group_by(!!!grp_cols_sym) |>
      group_map(~ calc_quantiles(.x,
                                 col = col,
                                 probs = probs,
                                 include_mean = include_mean)) |>
      map_df(~{.x}) |>
      bind_cols(grp_vals) |>
      select(!!!grp_names_sym, everything()) |>
      ungroup() |>
      pivot_longer(cols = -c(grp_names), names_to = "quants", values_to = col)
  })

  # Join all the tables together
  df <- lst_df[[1]]
  walk(lst_df[-1], ~{
    df <<- df |> dplyr::full_join(.x, c(grp_names, "quants"))
  })

  # Convert the quantiles to percentages, will produce NA warning if "avg"
  # is present even though there are no NAs in the output table
  df |>
    mutate(quants = ifelse(quants == "avg", "avg", paste0(round(as.numeric(quants) * 100, 2), "%")))
}
