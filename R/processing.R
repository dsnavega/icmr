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
is.processing.layer <- function(x) inherits(x = x, what = "processing.layer")

#' @author David Senhora Navega
#' @noRd
#'
initialise.processing.layer <- function(object, x, control) {

  if (!is.processing.layer(object))
    stop("\n(-) object is not a 'processing.layer'.")

  input.names <- colnames(x)
  scaling <- generate_scaling(x = data.matrix(x))

  layer <- structure(
    .Data = list(
      input.names = input.names,
      scaling = scaling
    ),
    class = class(object)
  )

  return(layer)

}

#' @author David Senhora Navega
#' @noRd
#'
compute.processing.layer <- function(object, x, task = "scale") {

  if(!is.processing.layer(object))
    stop("\n(-) object is not a 'processing.layer'.")

  components <- names(object)
  for (name in components) {
    assign(name, object[[name]])
  }

  # if (all.equal(colnames(x), input.names) != TRUE)
  #   stop("\n(-) Input names do not match processing layer parameterization.")

  z <- switch(task,

    scale = {
      compute_scaling(
        object = scaling, x = data.matrix(rbind(x))
      )
    },

    rescale = {
      compute_rescaling(
        object = scaling, x = data.matrix(rbind(x))
      )
    }

  )

  if (task == "scale")
    z[is.na(z)] <- 0

  return(z)

}

#' @author David Senhora Navega
#' @noRd
#'
generate_scaling <- function(x) {

  m <- NCOL(x)
  center <- apply(X = cbind(x), MARGIN = 2, FUN = mean, na.rm = T)
  scale <-  apply(X = cbind(x), MARGIN = 2, FUN = sd, na.rm = T)
  n <- colSums(!is.na(x))
  se <- scale / sqrt(n)
  scaling <- list(
    center = center, scale = scale, se = se, m = m, names = colnames(x)
  )
  return(scaling)

}

#' @author David Senhora Navega
#' @noRd
#'
compute_scaling <- function(object, x) {

  if (!is.matrix(x))
    x <- rbind(x)

  if (NCOL(x) != object$m)
    stop("\n(-) Number of parameters do not match number of inputs in x.")

  scaled <- t((t(x) - object$center) / object$scale)

  return(scaled)

}

#' @author David Senhora Navega
#' @noRd
#'
compute_rescaling <- function(object, x) {

  if (!is.matrix(x))
    x <- rbind(x)

  if (NCOL(x) != object$m)
    stop("\n(-) Number of parameters do not match number of inputs in x.")

  rescaled <- t(t(x) * object$scale + object$center)

  return(rescaled)

}
