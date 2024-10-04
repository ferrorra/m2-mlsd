RUS <- function(data, outcome, perc_min = 100) {
  datnrow <- nrow(data)
  if (nrow(na.omit(data)) < datnrow) {
    stop("Sorry, this dataset has missing value :(")
  }
  if (is.character(outcome)) {
    if (!(outcome %in% colnames(data))) {
      stop(paste("This dataset doesn't have a variable names", outcome))
    }
  } else {
    if (outcome < 1 | outcome > ncol(data)) {
      stop(paste("This dataset doesn't have a variable whose column number is", outcome))
    }
  }
  y <- data[, outcome]
  if (length(table(y)) != 2) {
    stop("Sorry, the outcome is not binary, I can't solve this problem :(")
  }
  if (table(y)[1] == table(y)[2]) {
    stop("Sorry, this dataset has been balanced and there is nothing I can do.")
  }
  if (!inherits(y, "character") & !inherits(y, "factor")) {
    warning("The outcome is a binary variable, but not a factor or character.")
  }
  min_cl <- names(table(y))[which.min(table(y))]
  min_ind <- which(y == min_cl)
  maj_ind <- sample(which(y != min_cl), length(min_ind) * perc_min/100, replace = FALSE)
  newdata <- data[c(min_ind, maj_ind), ]
  return(newdata)
}