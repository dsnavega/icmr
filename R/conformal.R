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
# David Senhora Navega
# Laboratory of Forensic Anthropology
# Department of Life Sciences
# University of Coimbra
# Calçada Martim de Freitas, 3000-456, Coimbra
# Portugal

#' @author David Senhora Navega
#' @noRd
#'
is.conformal.layer <- function(x) {
  inherits(x = x, what = "conformal.layer")
}

#' @author David Senhora Navega
#' @noRd
#'
initialise.conformal.layer <- function(
  object, estimate.layer, variance.layer, control
) {

  if (!is.conformal.layer(object))
    stop("\n(-) object is not a 'conformal.regression.layer'.")

  if (is.estimate.layer(estimate.layer)) {
    loss <- abs(estimate.layer$regressor$diagnostics$residual)
  } else {
    stop("\n(-) 'estimate.layer' not supplied to 'conformal.layer'.")
  }

  if (is.variance.layer(variance.layer)) {
    variance <- variance.layer$regressor$value
  } else {
    stop("\n(-) 'variance.layer' not supplied to 'conformal.layer'.")
  }


  conformal.score <- loss / variance
  n <- length(variance)
  confidence <- seq(from = 0.5, 0.99, by = 0.01)
  conformal.factor <- sapply(confidence, function(confidence) {
    sort(conformal.score)[ceiling(n * confidence)]
  })
  names(conformal.factor) <- as.character(confidence)

  estimate.layer$regressor$diagnostics$residual <- NULL
  variance.layer$regressor$diagnostics$residual <- NULL

  layer <- structure(
    .Data = list(
      estimate.layer = estimate.layer,
      variance.layer = variance.layer,
      scaling = conformal.factor,
      alpha = control$alpha
    ),
    class = "conformal.layer"
  )

  return(layer)

}

#' @author David Senhora Navega
#' @noRd
#'
compute.conformal.layer <- function(object, x, alpha) {

  .truncate <- function(x, interval) pmax(pmin(x, interval[2]), interval[1])

  if (missing(alpha))
    alpha <- object$alpha

  confidence <- 1 - alpha

  if (any(confidence < 0.5) | any(confidence > 0.99))
    stop("\n(-) alpha must be a numeric value between 0.01 and 0.5.")

  estimate.layer <- object$estimate.layer
  variance.layer <- object$variance.layer

  truncate <- estimate.layer$truncate
  interval <- estimate.layer$interval

  scaling <- object$scaling[format(confidence, digits = 2)]

  if (missing(x)) {

    estimate <- compute(estimate.layer)
    variance <- compute(variance.layer)

    lower <- estimate - (variance * scaling)
    upper <- estimate + (variance * scaling)

    predicted <- cbind(estimate = estimate, lower = lower, upper = upper)
    colnames(predicted) <- c("estimate", "lower", "upper")

    if (truncate)
      predicted <- .truncate(predicted, interval)

    return(predicted)

  } else {

    estimate <- compute(estimate.layer, x = x)
    variance <- compute(variance.layer, x = estimate)

    lower <- estimate - (variance * scaling)
    upper <- estimate + (variance * scaling)
    predicted <- cbind(estimate = estimate, lower = lower, upper = upper)
    colnames(predicted) <- c("estimate", "lower", "upper")

    if (truncate)
      predicted <- .truncate(predicted, interval)

    return(predicted)

  }

}
