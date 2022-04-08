#' Load the specially-formatted age fit or age residuals file. Loading uses
#' parallelism by gear
#'
#' @param fn The filename
#' @param type Either 'fits' or 'resids' for the age fits (A_hat in iscam)
#' or residuals (A_nu in iscam) respectively
#' @param gear_names A vector of gear names to use in the resulting data frame.
#' If `NULL`, the gear numbers set in the age comp section of the iscam
#' data file will be used
#' @param burnin The number of MCMC records to remove for burnin period
#' @param thin Remove every nth record for thinning of MCMC output
#'
#' @return The list of output data frames representing the age fits
#' by gear and year
#' @importFrom stringr str_split
#' @importFrom future plan
#' @importFrom furrr future_imap
#' @export
load_agefit <- function(fn,
                        type = c("fits", "resids"),
                        gear_names = NULL,
                        burnin = 1000,
                        thin = 1){

  type <- match.arg(type)

  d <- readLines(fn)
  d <- d[d != ""]
  label_inds <- grep("gear", d)
  labels <- d[label_inds]

  gears <- sort(unique(as.numeric(gsub(".*gear([0-9]+)", "\\1", d[label_inds]))))
  posts <- sort(unique(as.numeric(gsub("posterior([0-9]+).*", "\\1", d[label_inds]))))

  # Make a list of gears, each has a data frame with the posterior values with
  # years as the columns
  future::plan("multisession", workers = length(gears))
  lst <- furrr::future_imap(gears, ~{
    inds <- grep(paste0("gear", .x), d)
    start <- inds + 1
    # Each gear has a number of posteriors
    # Apply burnin and thinning
    start <- start[-seq_len(burnin)]
    start <- start[seq(1, length(start), by = thin)]
    p <- imap(start, ~{
      end <- label_inds[which(.x < label_inds)[1]] - 1
      if(is.na(end)){
        # End of the file is reached, use final line as end of chunk
        end <- length(d)
      }
      chunk <- d[.x:end]
      sex <- as.numeric(gsub("^[0-9]+ ([0-2]).*", "\\1", chunk))
      years <- as.numeric(gsub("^([0-9]+) .*", "\\1", chunk))
      chunk <- gsub("^[0-9]+ [0-2] +(.*)", "\\1", chunk)
      j <- map(sort(unique(sex)), function(.x, posterior){
        which_sex <- which(sex == .x)
        which_years <- years[which_sex]
        k <- map(chunk[which_sex], function(.x, sexx){
          tmp <- c(sexx, as.numeric(str_split(.x, pattern = " ")[[1]]))
        }, sexx = .x) %>%
          `names<-`(which_years) %>%
          do.call(rbind, .) %>%
          as.data.frame() %>%
          `names<-`(as.character(1:ncol(.))) %>%
          as_tibble(rownames = "year") %>%
          mutate(year = as.numeric(year)) %>%
          `names<-`(c("year", "sex", as.character(1:(ncol(.) - 2)))) %>%
          mutate(post = posterior)
      }, posterior = .y)
    }) %>%
      bind_rows %>%
      mutate(gear = gear_names[.y])
  })

  if(length(lst) == length(gear_names)){
    names(lst) <- gear_names
  }else{
    names(lst) <- gears
    warning("`gear_names` was not the same length as what was reported in the ",
            " age file ", fn, ". Using gear numbers as found in the data file instead.")
  }

  lst <- lst  %>%
    bind_rows %>%
    select(gear, post, year, sex, everything())

  lst
}
