#' Table of maturity estimates based on data only
#'
#' @param surv_samples A `survey_samples` list as output by
#' [gfdata::get_survey_samples()]
#' @param digits Number of decimal places for the values in the table
#' @param col_widths Widths for columns, except the Parameter column
#' @param return_df Logical. If `TRUE`, return the [data.frame] and not
#' the [csasdown::csas_table()]
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @importFrom gfplot fit_mat_ogive
#' @export
table_maturity <- function(surv_samples,
                           col_widths = NULL,
                           return_df = FALSE,
                           ...){

  mat <- fit_mat_ogive(surv_samples)
  nsex <- length(unique(mat$data$sex))
  nfish <- table(mat$data$sex)
  mat_params <- mat$mat_perc

  male <- grep("^m", names(mat_params))
  female <- grep("^f", names(mat_params))
  nsex <- ifelse(length(male) + length(female) == length(mat_params), 2, 1)

  prob_pat <- "^[m|f]\\.p(0\\.[0-9]+)$"
  probs <- gsub(prob_pat, "\\1", names(mat_params))

  if(nsex == 2){
    male_dat <- mat_params[male] |> unlist()
    probs <- gsub(prob_pat, "\\1", names(male_dat))
    male_dat <- male_dat[order(probs)]
    names(male_dat) <- paste0(round(as.numeric(sort(probs)) * 100, 0), "\\%")

    female_dat <- mat_params[female] |> unlist()
    probs <- gsub(prob_pat, "\\1", names(female_dat))
    female_dat <- female_dat[order(probs)]
    names(female_dat) <- paste0(round(as.numeric(sort(probs)) * 100, 0), "\\%")

    tab <- tibble(Sex = c(en2fr("Male"), en2fr("Female")))
    vals_df <- rbind(male_dat, female_dat)
    tab <- tab |>
      bind_cols(vals_df)

    nspec <- enframe(nfish, name = NULL) |>
      rename(`Number of specimens` = value)
    tab <- tab |>
      bind_cols(nspec)
  }else{
    female_dat <- mat_params[female] |> unlist()
    probs <- gsub(prob_pat, "\\1", names(female_dat))
    female_dat <- female_dat[order(probs)]
    nms <- paste0(round(as.numeric(sort(probs)) * 100, 0), "\\%")

    female_dat <- enframe(female_dat) |> t() |> as_tibble()
    female_dat <- female_dat[-1, ]
    names(female_dat) <- nms

    tab <- tibble(Sex = en2fr("Female"))
    tab <- tab |>
      bind_cols(female_dat)

    j <- tab |>
      mutate(`Number of specimens` = nfish)
  }

  out <- csas_table(tab,
                    format = "latex",
                    align = rep("r", ncol(tab)),
                    col_names_align = rep("r", ncol(tab)),
                    ...)

  if(!is.null(col_widths)){
    out <- out |>
      column_spec(2:ncol(tab), width = col_widths)
  }

  if(return_df){
    return(tab)
  }

  out
}
