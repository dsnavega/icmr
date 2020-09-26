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
is.variance.layer <- function(x) {
  inherits(x = x, what = "variance.layer")
}

#' @author David Senhora Navega
#' @noRd
#'
initialise.variance.layer <- function(object, estimate.layer) {

  if(!is.variance.layer(object))
    stop("\n(-) object is not a 'variance.layer'.")

  if (is.estimate.layer(estimate.layer)) {
    estimate <- estimate.layer$regressor$predicted
  } else {
    stop("\n(-) 'estimate.layer' not supplied to 'variance.layer'.")
  }

  variance <- abs(estimate.layer$regressor$diagnostics$residual)
  regressor <- ridge(A = estimate, b = variance)

  variance <- regressor$predicted * 1.2533
  n <- length(variance)
  interval <- c(sd(variance) / sqrt(n - 1), max(variance) + sd(variance))
  variance <- pmax(pmin(variance, interval[2]), interval[1])

  regressor$predicted <- variance

  layer <- structure(
    .Data = list (
      regressor = regressor,
      truncate = T,
      interval = interval,
      scaling = 1.2533
    ),
    class = "variance.layer"
  )

  return(layer)

}

#' @author David Senhora Navega
#' @noRd
#'
compute.variance.layer <- function(object, x) {

  .truncate <- function(x, interval) pmax(pmin(x, interval[2]), interval[1])

  components <- names(object)
  for (name in components) {
    assign(name, object[[name]])
  }

  if (missing(x)) {

    variance <- object$regressor$predicted
    return(variance)

  } else {

    variance <- as.numeric(predict(object = regressor, newdata = x) * scaling)
    if (truncate)
      variance <- .truncate(variance, interval)
    return(variance)

  }

}
