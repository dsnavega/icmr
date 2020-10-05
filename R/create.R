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
create.layer <- function(type, ...) {

  if (missing(type))
    stop("\n(-) No type of layer selected. Type ?create.layer for details.")

  layer <- switch(type,

    "processing" = {
      structure(.Data = list(...), class = c("processing.layer"))
    },

    "estimate" = {
      structure(.Data = list(...), class = c("estimate.layer"))
    },

    "variance" = {
      structure(.Data = list(...), class = c("variance.layer"))
    },

    "conformal" = {
      structure(.Data = list(...), class = c("conformal.layer"))
    },

  )

  return(layer)

}

#' @author David Senhora Navega
#' @noRd
#'
initialise <- function(object, ...) {
  UseMethod(generic = "initialise", object = object)
}

#' @author David Senhora Navega
#' @noRd
#'
compute <- function(object, ...) {
  UseMethod(generic = "compute", object = object)
}
