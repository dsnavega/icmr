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
#' Reliable regression modelling using an Inductive Confidence Machine based on
#' Ridge Regression
#'
#' @author David Senhora Navega
#' @export
#' @import stats
#'
#' @param x a column-oriented matrix or a data.frame of numeric vector(s).
#' @param y a numeric vector
#' @param control a list of parameters created by \code{\link{icmr.control}}
#'
#' @return a icmr object
#'
icmr <- function(x, y, control = icmr.control()) {

  # TIC
  start.clock <- Sys.time()

  # Exception Handling
  if(!is.matrix(x))
    stop("\n(-) x must be a numeric matrix.")

  if (any(is.na(y)))
    stop("\n(-) No NA value is allowed in y.")

  if (!(is.vector(y) & is.numeric(y)))
    stop("\n(-) y must be a numeric vector.")

  if (NROW(x) != length(y))
    stop("\n(!) Number of observations (rows) in x do not match y length.")

  if (!is.icmr.control(control))
    stop("\n(-) control is not an object created by 'icmr.control().'")

  # Coerce to Matrix Form
  x <- cbind(x)
  y <- cbind(y)

  # Processing Layer
  if (control$process) {

    if (any(is.na(x)))
      warning("\n(!) NA values detected in x. Mean imputation applied.")

    processing.layer <- initialise(
      object = create.layer(type = "processing"),
      x = x
    )
    x <- compute(object = processing.layer, x = x)

  } else {

    processing.layer <- NULL

    if (any(is.na(x)))
      message <- c(
        "\n(-) NA values detected (x).",
        "\n Consider process = T as a control parameter. See Details of ?icmr."
      )
    stop(paste0(message))

  }

  # Estimate Layer
  estimate.layer <- initialise(
    object = create.layer(type = "estimate"),
    x = x, y = y,
    control = control
  )

  # Variance Layer
  variance.layer <- initialise(
    object = create.layer("variance"),
    estimate.layer = estimate.layer
  )

  # Conformal (Confidence Machine) Layer
  conformal.layer <- initialise(
    object = create.layer(type = "conformal"),
    estimate.layer = estimate.layer,
    variance.layer = variance.layer,
    control = control
  )

  # TOC
  stop.clock <- Sys.time()
  time <- difftime(time1 = stop.clock, time2 = start.clock)
  time.units <- attr(time, which = "units")
  time <- list(value = as.numeric(time), units = time.units)

  # Class Object
  object <- structure(
    .Data = list(
      processing = processing.layer,
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

#' Predic method for Inductive Confidence Machine for Regression
#' @author David Senhora Navega
#' @export
#'
#' @param object an icmr object
#' @param newdata a numeric vector or matrix with the same variables used to
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
predict.icmr <- function(object, newdata, alpha = NULL, conformal = T, ...) {

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

  if (missing(newdata)) {

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

    # Processing Layer
    if (!is.null(object$processing))
      x <- compute(object = object$processing, x = newdata)

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
print.icmr <- function(object) {
  cat("\n Inductive Confidence Machine using Ridge Regression")
}
