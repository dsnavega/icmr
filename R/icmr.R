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

#' Inductive Confidence Machine for Regression
#'
#' Reliable regression modeling using an Inductive Confidence Machine based on
#' Ridge Regression
#'
#' @author David Senhora Navega
#' @export
#' @import stats
#'
#' @param x a column-oriented matrix or a data.frame of numeric vector(s).
#' @param y a numeric vector
#' @param control a list of parameters created by \code{\link{icmr.control}}
#' @param weights a vector of non-negative weights.
#' @param kernel a logical stating if x is a kernel matrix.
#'
#' @return a icmr object
#'
icmr <- function(x, y, weights = NULL, kernel = F, control = icmr.control()) {

  # TIC
  start.clock <- Sys.time()

  # Exception Handling
  if (!is.matrix(x))
    stop("\n(-) x must be a numeric matrix.")

  if (any(is.na(x)))
    stop("\n(!) NA values not allowed in x.")

  if (any(is.na(y)))
    stop("\n(-) NA values not allowed in y.")

  if (!(is.vector(y) & is.numeric(y)))
    stop("\n(-) y must be a numeric vector.")

  if (NROW(x) != length(y))
    stop("\n(!) Number of observations (rows) in x do not match y length.")

  if (!is.icmr.control(control))
    stop("\n(-) control is not an object created by 'icmr.control().'")

  # Coerce to Matrix Form

  x <- rbind(x)
  y <- cbind(y)

  if (is.null(weights)) {
    weights <- rep(1, times = nrow(x))
  } else {

    if (!(is.vector(weights) & is.numeric(weights)))
      stop("\n(-) weights must be a numeric vector.")

    if (any(weights < 0))
      stop("\n(-) Negative weights not allowed.")

    if (length(weights) != nrow(x))
      stop("\n(-) weights must be a", nrow(x), "length vector.")

  }

  # Estimate Layer
  estimate.layer <- initialise(
    object = create.layer(type = "estimate"),
    x = x, y = y, weights = weights, kernel = kernel, control = control
  )

  # Variance Layer
  variance.layer <- initialise(
    object = create.layer("variance"),
    estimate = estimate.layer, x = x, weights = weights, kernel = kernel
  )

  # Conformal (Confidence Machine) Layer
  conformal.layer <- initialise(
    object = create.layer(type = "conformal"),
    estimate = estimate.layer, variance = variance.layer, control = control
  )

  # TOC
  stop.clock <- Sys.time()
  time <- difftime(time1 = stop.clock, time2 = start.clock)
  time.units <- attr(time, which = "units")
  time <- list(value = as.numeric(time), units = time.units)

  # Class Object
  object <- structure(
    .Data = list(
      conformal = conformal.layer,
      alpha = control$alpha,
      time = time
    ),
    class = "icmr"
  )

  return(invisible(object))

}

#' @author David Senhora Navega
#' @noRd
#'
is.icmr <- function(x) inherits(x = x, what = "icmr")

#' Predict method for Inductive Confidence Machine for Regression
#' @author David Senhora Navega
#' @export
#'
#' @param object an icmr object
#' @param x a numeric vector or matrix with the same variables used to
#' create the icmr object
#' @param alpha level of error tolerance or confidence for predictive intervals
#' computed when conformal = T. Note confidence = 1 - alpha, with alpha assuming
#' a value between 0.01 and 0.5. Default value is obtained from the icmr object.
#' @param conformal a logical stating if confidence interval should be computed
#' using conformal prediction framework. See Details.
#' @param ... ...
#'
#' @details
#' ...
#'
predict.icmr <- function(object, x, alpha = NULL, conformal = T, ...) {

  # Exception Handling
  if (!is.icmr(x = object))
    stop("\n(-) object is not a icmr model.")

  if (missing(alpha))
    alpha <- object$alpha

  if (is.null(alpha) | is.infinite(alpha) | is.na(alpha))
    alpha <- object$alpha

  if (alpha < 0.01  | alpha > 0.5)
    stop("\n(-) alpha must be a numeric value between 0.01 and 0.5.")

  if (!is.logical(conformal))
    stop("\n(-) 'interval' argument must be a logical.")

  if (missing(x)) {

    if (conformal) {
      prediction <- compute(object = object$conformal, alpha = alpha)
      return(prediction)
    } else {
      prediction <- list(
        estimate = compute(object = object$conformal$estimate.layer),
        variance = compute(object = object$conformal$variance.layer)
      )
      prediction <- lapply(prediction, as.numeric)
      return(prediction)
    }

  } else {

    if (conformal) {
      prediction <- compute(object = object$conformal, x = x, alpha = alpha)
      return(prediction)
    } else {
      conformal <- object$conformal
      estimate <- compute(object = conformal$estimate.layer, x = x)
      variance <- compute(object = conformal$variance.layer, x = estimate)
      prediction <- list(estimate = estimate, variance = variance)
      prediction <- lapply(prediction, as.numeric)
      return(prediction)
    }

  }

}

#' Print icmr
#' @author David Senhora Navega
#' @noRd
#'
#' @export
#'
print.icmr <- function(object) {
  cat("\n Inductive Confidence Machine: ")
  print(object$conformal$estimate.layer$regressor)
}

#' Control List for Inductive Confidence Machine for Regression
#'
#' @author David Navega
#' @export
#'
#' @param alpha a numeric defining the tolerance or confidence (1 - alpha) of
#' the predictive intervals of icmr model. Default is 0.05
#' @param interval a vector of two numeric values defining the domain of the
#' predictions generated by the icmr model for a continuous output.
#' Default is NULL which means that the interval is computed from the data.
#' @param delta a numeric value between 0 and 1 that control the extrapolation
#' of the icmr model outside of the bounds of the values known for continuous
#' output. 0 means no extrapolation. This parameter affects both the
#' point estimates and the prediction intervals. Default is 0.25.
#'
#' @return a list with the parameters given as arguments to the function call.
#'
icmr.control <- function(alpha = 0.05, interval = NULL, delta = 0.25) {

  if (alpha < 0.01  | alpha > 0.5)
    stop("\n(-) alpha must be a numeric value between 0.01 and 0.5.")

  if (delta < 0.0  | delta > 1)
    stop("\n(-) delta must be a numeric value between 0.0 and 1.")

  control.list <- structure(
    .Data = list(
      alpha = alpha,
      interval = interval,
      delta = delta,
      truncate = T  # Internal Usage
    ),
    class = "icmr.control"
  )

  return(control.list)

}

#' @author David Senhora Navega
#' @noRd
#'
is.icmr.control <- function(x) inherits(x = x, what = "icmr.control")
