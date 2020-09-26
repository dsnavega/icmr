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
is.estimate.layer <- function(x) inherits(x = x, what = "estimate.layer")

#' @author David Senhora Navega
#' @noRd
#'
initialise.estimate.layer <- function(object, x, y, control) {

  if(!is.estimate.layer(object))
    stop("\n(-) object is not an 'estimate.layer'.")

  control.names <- c("interval", "truncate")
  for(name in control.names) {
    assign(name, control[[name]])
  }

  if (is.null(interval) & ncol(y) == 1) {
    n <- sum(!is.na(y))
    bw <- sd(x = y, na.rm = T)  * n ^ -0.25
    interval <- range(x = y, na.rm = T) + c(-bw, bw) * 0.2
  }

  # Compute Ridge Regression
  regressor <- ridge(A = x, b = y)

  if (truncate & ncol(y) == 1) {
    .truncate <- function(x, interval) pmax(pmin(x, interval[2]), interval[1])
    regressor$predicted <- .truncate(regressor$predicted, interval)
  }

  layer <- structure(
    .Data = list(
      regressor = regressor,
      truncate = truncate,
      interval = interval
    ),
    class = class(object)
  )

  return(layer)

}

#' @author David Senhora Navega
#' @noRd
#'
compute.estimate.layer <- function(object, x) {

  if(!is.estimate.layer(object))
    stop("\n(-) object is not an 'estimate.layer'.")

  .truncate <- function(x, interval) pmax(pmin(x, interval[2]), interval[1])

  components <- names(object)
  for (name in components) {
    assign(name, object[[name]])
  }

  if (missing(x))
    return(regressor$predicted)

  newdata <- x
  predicted <- predict(object = regressor, newdata = newdata)

  if (truncate)
    predicted <- .truncate(predicted, interval)

  return(predicted)

}
