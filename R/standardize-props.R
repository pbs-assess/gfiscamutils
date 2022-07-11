#' Update an iSCAM `dat` file by standardizing all the age comps
#'
#' @param fns A vector of the iSCAM `dat` filenames to process
#' @importFrom purrr walk map2 map_chr
#'
#' @return Nothing
standardize_comps <- function(fns){

  walk(fns, function(fn){
    if(!file.exists(fn)){
      stop("File '", fn, "' does not exist", call. = FALSE)
    }

    dat <- readLines(fn)
    pat <- "^([0-9]{4}\\s+[0-9]+\\s+[0-9]\\s+[0-9]\\s+[0-9]\\s+[0-9])\\s+(([0-9\\.]+\\s+)+)\\S+$"
    inds <- grep(pat, dat)
    comps <- dat[inds]
    headers <- trimws(gsub(pat, "\\1", comps))
    comp_strs <- trimws(gsub(pat, "\\2", comps))
    comp_lst <- str_split(comp_strs, "\\s+") |>
      map(~{as.numeric(.x)})

    # Standardize the comp list and re-build the line to go in the data file
    comps <- map2(headers, comp_lst, ~{
      .y <- round(.y / sum(.y), 6)
      paste(.x, " ", paste(.y, collapse = " "))
    }) |>
      map_chr(~{.x})

    if(length(inds) != length(comps)){
      stop("The calculated comps do not have the same number of lines as the ",
           "extracted comps",
           call. = FALSE)
    }
    dat[inds] <- comps
    writeLines(dat, fn)
  })
}
