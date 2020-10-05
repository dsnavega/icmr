# This file is part of icmr
#
# Copyright (C) 2020, David Senhora Navega
#
# icmr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icmr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icmr. If not, see <http://www.gnu.org/licenses/>.
#
# David Navega
# Laboratory of Forensic Anthropology
# Department of Life Sciences
# University of Coimbra
# Calçada Martim de Freitas, 3000-456, Coimbra
# Portugal

#' Simple Ordinary Least Squares
#'
#' @author David Senhora Navega
#' @noRd
#'
ols <- function(x, y, weights = NULL) {

  # Prepare Data
  data <- data.frame(x = x, y = y)
  n <- nrow(data)
  index <- which(!apply(is.na(data), 1, any))
  m <- length(index)

  data <- na.omit(data)
  A <- cbind(intercept = 1, input = data[[-ncol(data)]])
  b <- cbind(output = data[[ncol(data)]])

  # Weights
  if (is.null(weights)) {
    W <- diag(rep(x = 1, times = m))
  } else {
    W <- diag(weights)
  }

  # Compute OLS
  X <- solve(t(A) %*% W %*% A, t(A) %*% W %*% b)

  # Hat
  H <- diag(A %*% solve(t(A) %*% W %*% A) %*% t(A))

  # LOOCV Estimate
  estimate <- A %*% X
  estimate <- as.numeric((estimate - (b * H)) / (1 - H))
  value <- rep(x = NA, times = n)
  value[index] <- estimate

  object <- structure(
    .Data = list(coefficients = X, value = value),
    class = "ols"
  )

  return(object)

}

#' Predict for 'ols'
#'
#' @author David Senhora Navega
#' @noRd
#'
predict.ols <- function(object, x, ...) {
  value <- cbind(1, x) %*% object$coefficients
  return(value)
}

#' Iteratively Reweighted Least Squares
#'
#' @author David Senhora Navega
#' @noRd
#'
irls <- function(x, y, niteration = 50) {

  # Iteration (1)
  ols.model <- ols(x = x, y = y, weights = NULL)
  i <- 1
  old <- ols.model$coefficients

  while (i < niteration) {

    # Weights
    weights <- (1 / sqrt(ols.model$value))
    # Iteratively Reweighted Least Squares
    ols.model <- ols(x = x, y = y, weights = weights)
    new <- ols.model$coefficients

    # Stop if all coefficients change less than 1%
    if (all((abs(new - old) / old) < 0.01))
      break
    # Increase Count & Update Solution
    i <- i + 1
    old <- new

  }

  ols.model$niteration <- i
  class(ols.model) <- c("ols", "irls")

  return(ols.model)

}
