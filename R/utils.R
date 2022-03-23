# util function for printing training index to screen when verbose = TRUE
verbose_print <- function(verbose, index, total) {

  if (verbose == TRUE) {

    message(paste0("Trained ",
                   index,
                   "/",
                   total,
                   " models."))

  }

}
