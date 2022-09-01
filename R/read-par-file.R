#' Read in the contents of the iscam par file
#'
#' @param file Filename
#'
#' @return A list representing the contents of the iscam par file
#' @export
read_par_file <- function(file = NULL){

  if(!file.exists(file)){
    warning("Par file ", basename(file)," not found in ", dirname(file), ". Setting data to NA.")
    return(NA)
  }

  data <- readLines(file, warn = FALSE)
  tmp <- list()

  conv_check <- as.numeric(unlist(regmatches(data[1], gregexpr("[[:digit:]]+\\.*[[:digit:]]*", data[1]))))

  # Remove the first line from the par data since we already parsed it and saved the values
  data <- data[-1]

  # Commented lines signify that the following lines (until another commented line)
  # are parameter estimates for the parameter named in the comment
  param_nm_inds <- grep("^#", data)
  param_start_inds <- param_nm_inds + 1
  param_end_inds <- c(param_nm_inds[-1] - 1, length(data))
  tmp <- map(seq_along(param_nm_inds), ~{
    data[param_start_inds[.x]:param_end_inds[.x]]
  })
  names(tmp) <- gsub("# +", "", data[param_nm_inds])

  tmp$num_params <- as.integer(conv_check[1])
  tmp$obj_fun_val <- format(conv_check[2], digits = 6, scientific = FALSE)
  tmp$max_gradient <- format(conv_check[3], digits = 8, scientific = FALSE)
  tmp
}
