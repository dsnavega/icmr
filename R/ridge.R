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

#' Ridge Regression
#'
#' Ridge Regression using efficient Leave-One-Out Cross-Validation based on
#' Singular Value Decomposition
#'
#' @author David Senhora Navega
#' @import stats
#' @noRd
#'
#' @md
#'
#' @param A a matrix of predictor(s)
#' @param b a column-oriented matrix with the response(s)
#' @param center a named list with components 'A' and 'b' storing the parameters
#' for centering A and b. The aforementioned parameters should be supplied as a
#' numeric vector with the length maching the number of columns of A and b.
#' Default is a named list with NULL components, data is centered based on
#'  average value of the column(s) of A and b. See Details.
#' @param W a numerical vector of weights, used for weighted regression. Default
#' is NULL. See Details.
#' @returns a "ridge" object (See Details.)
#' @references
#' Shao, Z., & Er, M. J. (2016). Efficient Leave-One-Out Cross-Validation-based
#' Regularized Extreme Learning Machine. Neurocomputing, 194(6), 260–270.
#' https://doi.org/10.1016/j.neucom.2016.02.058
#' @details
#' No intercept is fitted in this implementation, instead the response
#' (b) is centered on its average value (Default) or a the value(s) supplied in
#' the b component of the named list supplied through the center argument. The
#' equivalent to the model intercept is stored in component 'center$b' of the
#' object created by this function.
#' If W is supplied both A and b are centered on the weighted average value of
#' the column(s) of A and b. The value of A and b are then scaled by the square
#' root of W. This allows for efficient weighted ridge regression.
#'
#' The ridge object (list) has the following components:
#' * "x": Model Coefficients. See See Details
#' * "mse": Mean Square Error, Leave-One-Out.
#' * "nmse": Normalized Mean Squared Error, Leave-One-Out.
#' * "predicted": Fitted values Leave-One-Out.
#' * "diagnostics": a list of model diagnostics information.
#' * "C": Optimal lambda penalty term.
#' * "center": A list of parameters for data centering. See Details.
#'
ridge <- function(A, b, center = list(A = NULL, b = NULL), W = NULL) {

  # Exception Handling
  if(any(is.na(A)))
    stop("\n(-) NA values detected in A.")

  if(any(is.na(b)))
    stop("\n(-) NA values detected in b.")

  if(any(is.na(b)))
    stop("\n(-) NA values detected in b.")

  # if(!(is.vector(A) & is.numeric(A)) & !is.matrix(A))
  #   stop("\n(-) A must be a numeric vector or a matrix.")

  if ((is.vector(A) | is.numeric(A)))
    A <- cbind(A)

  if(!(is.vector(b) & is.numeric(b)) & !is.matrix(b))
    stop("\n(-) b must be a numeric vector or a matrix.")

  if ((is.vector(b) | is.numeric(b)))
    b <- cbind(b)

  if(nrow(A) != nrow(b))
    stop("\n(-) A and b do not have the same number of observations.")

  if (!is.null(W)) {

    if(any(is.na(W)))
      stop("\n(-) NA values detected in W.")

    if(!(is.vector(W) & is.numeric(W)))
      stop("\n(-) W must be a numeric vector.")

    if(nrow(A) != length(W))
      stop("\n(-) Number of weights do not match number of observations.")

    if(any(W < 0))
      stop("\n(-) Weights must be positive.")

  }

  if (!is.list(center))
    stop("\n(-) center must be a list.")

  if (!all(c("A", "b") %in% names(center)))
    stop("\n(-) center must be a list with two named components 'A' and 'b'.")

  # Parameters
  center <- center[c("A", "b")]
  n <- nrow(A)
  p <- ncol(A)
  m <- ncol(b)

  if (is.null(colnames(A)))
    colnames(A) <- paste0("A", seq_len(ncol(A)))

  # Center Data
  if (!is.null(W)) {

    center_A <- as.numeric(t(cbind(W)) %*% A) / sum(W)
    names(center_A) <- colnames(A)
    A <- sweep(A, 2, center_A,  "-") * sqrt(W)

    center_b <- as.numeric((t(cbind(W)) %*% b) / sum(W))
    b <- sweep(b, 2, center_b,  "-") * sqrt(W)

  } else {

    if (is.null(center$A)) {
      center_A <- as.numeric(colMeans(x = A))
      names(center_A) <- colnames(A)
    } else {
      pA <- length(center$A)
      if(length(center$A) != p)
        stop("\n(!) length(center$A) != ncol(A) [", pA, ", ", p, "].")
      center_A <- center$A
      names(center_A) <- colnames(A)
    }

    if (is.null(center$b)) {
      center_b <- as.numeric(colMeans(x = b))
      names(center_b) <- colnames(b)
    } else {
      pb <- length(center$b)
      if(length(center$b) != m)
        stop("\n(!) length(center$A) != ncol(A) [", pb, ", ", m, "].")
      center_b <- center$b
    }

    if (is.null(center$A) & !is.null(center$b))
      center_A <- rep(0, p)

    A <- sweep(A, 2, center_A,  "-")
    b <- sweep(b, 2, center_b,  "-")

  }

  # Algorithm
  if(nrow(A) > ncol(A)) {

    # Singular Value Decomposition
    svd <- svd(A)
    U <- svd$u
    S <- svd$d
    V <- svd$v

    # Helper functions
    compute_estimate <- function(C, U, S, V, b) {

      # Regularization
      theta <- (S ^ 2) / ((S ^ 2) + C)
      gamma <- theta * t(U)

      # Diagonal Values of the Hat Matrix
      H <- rowSums(U * t(gamma))

      # Estimate of b
      Y <- U %*% (gamma %*% b)

      # Leave-One-Out Mean Squared Error & Normalized Mean Squared Error
      if(m > 1) {

        yvar <-  apply(b, 2, var)
        mse <- (1 / n) * colSums((((b - Y) / (1 - H)) ^ 2))
        nmse <- (1 / n) * colSums(sweep(((b - Y) / (1 - H)) ^ 2, 2, yvar, "/"))

      } else {
        yvar <- as.vector(var(b))

        mse <- (1 / n) * colSums(((b - Y) / (1 - H)) ^ 2)
        nmse <- (1 / n) * colSums((((b - Y) / (1 - H)) ^ 2) / yvar)

      }

      # Leave-One-Out Estimate
      Y <- ((Y - (b * H)) / (1 - H))

      # Attribute names
      colnames(Y) <- colnames(b)
      names(mse) <- colnames(b)
      names(nmse) <- colnames(b)

      # return
      object <- list(mse = mse, nmse = nmse, fitted = Y, H = H)
      return(object)

    }

    compute_coefficients <- function(C, U, S, V, b) {

      # Regularization
      theta <- S / ((S ^ 2) + C)

      # Regression Coefficients
      beta <- V %*% (theta * (t(U) %*% b))

      return(beta)
    }

    optimise_press_rgt <- function(x, U, S, V, b) {
      mean(compute_estimate(C = x, U = U, S = S, V = V, b = b)$nmse)
    }

    # Get Optimal Lambda and Model Information
    prange <- pmax(range(c(S, .Machine$double.eps), .Machine$double.eps))
    optimum <- optimise(
      f = optimise_press_rgt, interval = prange,
      U = U, S = S, V = V, b = b
    )

    C <- optimum$minimum
    coefficient <- compute_coefficients(U = U, S = S, V = V, b = b, C = C)
    rownames(coefficient) <- colnames(A)

    estimate_object <- compute_estimate(U = U, S = S, V = V, b = b, C = C)
    fitted <- estimate_object$fitted
    fitted <- sweep(fitted, 2, center_b, "+")
    known <- sweep(b, 2, center_b, "+")
    colnames(fitted) <- colnames(b)

    # Diagnostics
    leverage <- estimate_object$H
    residual <- (known - fitted)
    variance <- sapply(estimate_object$mse, function(mse) {
      sqrt(mse * (1 - leverage))
    })

    studentized <- residual / variance
    dffits <- studentized * sqrt(leverage / (1 - leverage))
    cook <- (1 / p) * (studentized ^ 2 * (leverage / (1 - leverage)))

    diagnostics <- list(
      residual = unname(residual),
      studentized = unname(studentized),
      dffits = unname(dffits),
      cook = unname(cook),
      leverage = unname(leverage)
    )

    # Class Object
    object <- structure(
      .Data = list(
        x = coefficient,
        rmse = sqrt(estimate_object$mse),
        mse = estimate_object$mse,
        nmse = estimate_object$nmse,
        predicted = fitted,
        diagnostics = diagnostics,
        C = C,
        center = list(
          A = center_A,
          b = center_b
        )
      ),
      class = "ridge"
    )

    return(object)

  } else {

    # Singular Value Decomposition
    svd <- svd(t(A))
    U <- svd$u
    S <- svd$d
    V <- svd$v

    # Helper functions
    compute_estimate <- function(C, U, S, V, b) {

      # Regularization
      theta <- (S ^ 2) / ((S ^ 2) + C)
      gamma <- theta * t(V)

      # Diagonal Values of the Hat Matrix
      H <- rowSums(V * t(gamma))

      # Estimate of b
      Y <- V %*% (gamma %*% b)

      # Leave-One-Out Mean Squared Error & Normalized Mean Squared Error
      if(m > 1) {

        yvar <-  apply(b, 2, var)
        mse <- (1 / n) * colSums((((b - Y) / (1 - H)) ^ 2))
        nmse <- (1 / n) * colSums(sweep(((b - Y) / (1 - H)) ^ 2, 2, yvar, "/"))


      } else {

        yvar <- as.vector(var(b))
        mse <- (1 / n) * colSums(((b - Y) / (1 - H)) ^ 2)
        nmse <- (1 / n) * colSums((((b - Y) / (1 - H)) ^ 2) / yvar)

      }

      # Leave-One-Out Estimate
      Y <- (Y - (b * H)) / (1 - H)

      # Attribute names
      colnames(Y) <- colnames(b)
      names(mse) <- colnames(b)
      names(nmse) <- colnames(b)

      # return
      object <- list(mse = mse, nmse = nmse, fitted = Y, H = H)
      return(object)

    }

    compute_coefficients <- function(C, U, S, V, b) {

      # Regularization
      theta <- S / ((S ^ 2) + C)

      # Regression Coefficients
      beta <- U %*% (theta * (t(V) %*% b))

      return(beta)

    }

    optimise_press_cgt <- function(C, U, S, V, b) {
      mean(compute_estimate(C = C, U = U, S = S, V = V, b = b)$nmse)
    }

    # Get Optimal Lambda and Model Information
    prange <- pmax(range(c(S, .Machine$double.eps), .Machine$double.eps))
    optimum <- stats::optimise(
      f = optimise_press_cgt, interval = prange,
      U = U, S = S, V = V, b = b
    )

    C <- optimum$minimum

    coefficient <- compute_coefficients(U = U, S = S, V = V, b = b, C = C)
    rownames(coefficient) <- colnames(A)

    estimate_object <- compute_estimate(U = U, S = S, V = V, b = b, C = C)
    fitted <- estimate_object$fitted
    fitted <- sweep(fitted, 2, center_b, "+")
    known <- sweep(b, 2, center_b, "+")
    colnames(fitted) <- colnames(b)

    # Diagnostics
    leverage <- estimate_object$H
    residual <- (known - fitted)
    variance <- sapply(estimate_object$mse, function(mse) {
      sqrt(mse * (1 - leverage))
    })
    studentized <- residual / variance
    dffits <- studentized * sqrt(leverage / (1 - leverage))
    cook <- (1 / p) * (studentized ^ 2 * (leverage / (1 - leverage)))

    diagnostics <- list(
      residual = residual,
      studentized = studentized,
      dffits = dffits,
      cook = cook,
      leverage = leverage
    )

    # Class Object
    object <- structure(
      .Data = list(
        x = coefficient,
        rmse = sqrt(estimate_object$mse),
        mse = estimate_object$mse,
        nmse = estimate_object$nmse,
        predicted = fitted,
        diagnostics = diagnostics,
        C = C,
        center = list(
          A = center_A,
          b = center_b
        )
      ),
      class = "ridge"
    )

    return(object)

  }

}

#' Predict method for ridge object
#'
#' @author David Senhora Navega
#' @noRd
#'
#' @param object a ridge object
#' @param newdata a matrix of new data. If not given LOOCV predictions are
#' returned.
#' @param ... not implemented
#'
#' @return a matrix or column vector with predicted values.
#'
#'
predict.ridge <- function(object, newdata, ...) {

  if (!is.ridge(object))
    stop("\n(-) object is no a ridge model.")

  if (missing(newdata)) {
    predicted <- object$predicted
    return(predicted)
  }

  center_A <- object$center$A
  center_b <- object$center$b
  beta <- object$x

  newdata <- rbind(newdata)
  if (ncol(newdata) != nrow(beta))
    stop("\n(-) Predictors in newdata do not match learned coefficients.")

  newdata <- sweep(newdata, 2, center_A, "-")
  predicted <- newdata %*% beta
  predicted <- sweep(predicted, 2, center_b, "+")

  return(predicted)

}

#'Print method for Ridge Regression
#'@noRd
print.ridge <- function(object,...) {
  cat("\n Ridge Regression\n")
  cat("\n RMSE (LOOCV):", mean(object$rmse))
}

#' @author David Senhora Navega
#' @noRd
#'
is.ridge <- function(x) inherits(x = x, what = "ridge")

