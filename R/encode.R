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

#' Encode data.frame of factors using One-Hot Encoding
#'
#' @author David Senhora Navega
#' @export
#'
#' @param data a data.frame where all components are factors.
#' @param sep a character defining the separator used for naming the columns of
#' encoded data.
#' @return a matrix with encode factors.
#'
encode <- function(data, sep = ": ") {

  m <- ncol(data)
  vnames <- colnames(data)

  if (!is.data.frame(data))
    stop("\n(-) data must be a data.frame object")

  if (any(!sapply(data,is.factor)))
    stop("\n(-) All columns of data must be factors.")

  if (inherits(data, c("tbl_df", "tbl"))) {
    data <- as.data.frame(data)
    colnames(data) <- vnames
  }

  encoder <- function(x) {

    n <- length(x)
    m <- nlevels(x)

    encoding_matrix <- matrix(0, nrow = n, ncol = m)
    ones <- (1L:n) + n * (unclass(x) - 1L)
    encoding_matrix[ones] <- 1
    colnames(encoding_matrix) <- levels(x)
    mode(encoding_matrix) <- "integer"
    return(encoding_matrix)

  }

  encoded_list <- lapply(data, encoder)
  named_list <- lapply(seq_len(m), function(mth) {

    encoded_matrix <- encoded_list[[mth]]
    n <- ncol(encoded_matrix)
    colnames(encoded_matrix) <- paste0(
      vnames[mth], sep = sep, colnames(encoded_matrix)
    )

    return(encoded_matrix)

  })

  encoded <- do.call(cbind, named_list)

  return(encoded)

}
