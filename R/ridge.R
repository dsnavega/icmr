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
#' @param W a numeric vector of weights, used for weighted regression. Default
#' is NULL.
#' @param kernel a logical stating if A is a kernel matrix. Default = F.
#'
#' @returns a "ridge" object (See Details.)
#' @references
#' Shao, Z., & Er, M. J. (2016). Efficient Leave-One-Out Cross-Validation-based
#' Regularized Extreme Learning Machine. Neurocomputing, 194(6), 260–270.
#' https://doi.org/10.1016/j.neucom.2016.02.058
#' @details
#' No intercept is fitted in this implementation, instead the response
#' (b) is centered on its average value (Default). If W is supplied both A and b are centered on the weighted average value of
#' the column(s) of A and b. This allows for efficient weighted ridge regression.
#'
#' The ridge object (list) has the following components:
#' * "x": Model Coefficients. See See Details
#' * "rmse": Root Mean Square Error, Leave-One-Out.
#' * "predicted": Fitted values Leave-One-Out.
#' * "diagnostics": a list of model diagnostics information.
#' * "C": Optimal, lambda, penalty term.
#' * "scaling": A list of parameters for data scaling.
#' * "kernel": a logical stating if the model is a kernel rigde regression model
#' * "K": the kernel matrix supplied as A.
#'
ridge <- function(A, b, W = NULL, kernel = F, label = NULL) {

  # Exception Handling
  if (any(is.na(A)))
    stop("\n(-) NA values detected in A.")

  if (any(is.na(b)))
    stop("\n(-) NA values detected in b.")

  if (any(is.na(b)))
    stop("\n(-) NA values detected in b.")

  # if(!(is.vector(A) & is.numeric(A)) & !is.matrix(A))
  #   stop("\n(-) A must be a numeric vector or a matrix.")

  if ((is.vector(A) | is.numeric(A)))
    A <- cbind(A)

  if (!(is.vector(b) & is.numeric(b)) & !is.matrix(b))
    stop("\n(-) b must be a numeric vector or a matrix.")

  if ((is.vector(b) | is.numeric(b)))
    b <- cbind(b)

  if (nrow(A) != nrow(b))
    stop("\n(-) A and b do not have the same number of observations.")

  if (kernel)
    W <- NULL

  if (!is.null(W)) {

    if (any(is.na(W)))
      stop("\n(-) NA values detected in W.")

    if (!(is.vector(W) & is.numeric(W)))
      stop("\n(-) W must be a numeric vector.")

    if (nrow(A) != length(W))
      stop("\n(-) Number of weights do not match number of observations.")

    if (any(W < 0))
      stop("\n(-) Weights must be positive.")

  }

  # Parameters
  n <- nrow(A)
  p <- ncol(A)
  m <- ncol(b)

  if (is.null(colnames(A)))
    colnames(A) <- paste0("A", seq_len(ncol(A)))

  if (is.null(W))
    W <- rep(x = 1, times = n)

  # Scaling
  scaling <- list(
    A = list(
      mean = apply(A, 2, weighted_mean, weights = W),
      sd = apply(A, 2, weighted_sd, weights = W)
      # sd = rep(x = 1, times = ncol(A))
    ),
    b = list(
      mean = apply(b, 2, weighted_mean, weights = W),
      # sd = apply(b, 2, weighted_sd, weights = W)
      sd = rep(x = 1, times = ncol(b))
    )
  )

  if (all(unique(A) %in% c(0, 1)) | kernel) {
    scaling$A$mean <- rep(x = 0, times = p)
    scaling$A$sd <- rep(x = 1, times = p)
  }

  if (kernel) {
    A <- center_kernel(x = A)
  } else {
    A <- sweep(A, 2, scaling$A$mean, "-")
    A <- sweep(A, 2, scaling$A$sd, "/")
    A <- A * sqrt(W)
  }

  b <- sweep(b, 2, scaling$b$mean, "-")
  b <- sweep(b, 2, scaling$b$sd, "/")
  b <- b * sqrt(W)

  # Fitting
  if (nrow(A) > ncol(A)) {

    # Singular Value Decomposition
    svd <- svd(A)
    U <- svd$u
    S <- svd$d
    V <- svd$v

    # Helpers
    compute_estimate <- function(C, U, S, V, b) {

      # Regularization
      C <- rep(C, times = length(S))
      theta <- (S ^ 2) / ((S ^ 2) + C)
      gamma <- theta * t(U)

      # Diagonal Values of the Hat Matrix
      H <- rowSums(U * t(gamma))

      # Estimate of b
      Y <- U %*% gamma %*% b

      # Leave-One-Out Mean Squared Error & Normalized Mean Squared Error
      if (m > 1) {

        yvar <-  apply(b, 2, var)
        mse <- (1 / n) * colSums((((b - Y) / (1 - H)) ^ 2))
        nmse <- (1 / n) * colSums(sweep(((b - Y) / (1 - H)) ^ 2, 2, yvar, "/"))

      } else {
        yvar <- as.vector(var(b))

        mse <- (1 / n) * colSums(((b - Y) / (1 - H)) ^ 2)
        nmse <- (1 / n) * colSums((((b - Y) / (1 - H)) ^ 2) / yvar)

      }

      # Leave-One-Out Estimate
      Y <- (Y - b * H) / (1 - H)

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
      C <- rep(C, times = length(S))
      theta <- S / ((S ^ 2) + C)
      # Regression Coefficients
      beta <- V %*% (theta * t(U) %*% b)
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

    # Re-scaling
    fitted <- estimate_object$fitted
    fitted <- sweep(fitted, 2, scaling$b$sd, "*")
    fitted <- sweep(fitted, 2, scaling$b$mean, "+")
    colnames(fitted) <- colnames(b)

    known <- b
    known <- sweep(known, 2, scaling$b$sd, "*")
    known <- sweep(known, 2, scaling$b$mean, "+")

    scaler <- matrix(scaling$b$sd / scaling$A$sd, nrow = p, ncol = m, byrow = T)
    coefficient <- rbind(Intercept = scaling$b$mean, coefficient * scaler)

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
        rmse = sqrt(estimate_object$mse) * scaling$b$sd,
        predicted = fitted,
        diagnostics = diagnostics,
        C = C,
        scaling = scaling,
        kernel = kernel,
        K = if (kernel) {A} else {NULL}
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
      if (m > 1) {

        yvar <-  apply(b, 2, var)
        mse <- (1 / n) * colSums((((b - Y) / (1 - H)) ^ 2))
        nmse <- (1 / n) * colSums(sweep(((b - Y) / (1 - H)) ^ 2, 2, yvar, "/"))


      } else {

        yvar <- as.vector(var(b))
        mse <- (1 / n) * colSums(((b - Y) / (1 - H)) ^ 2)
        nmse <- (1 / n) * colSums((((b - Y) / (1 - H)) ^ 2) / yvar)

      }

      # Leave-One-Out Estimate
      Y <- (Y - b * H) / (1 - H)

      # Attribute names
      colnames(Y) <- colnames(b)
      names(mse) <- colnames(b)
      names(nmse) <- colnames(b)

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

    # Re-scaling
    fitted <- estimate_object$fitted
    fitted <- sweep(fitted, 2, scaling$b$sd, "*")
    fitted <- sweep(fitted, 2, scaling$b$mean, "+")
    colnames(fitted) <- colnames(b)

    known <- b
    known <- sweep(known, 2, scaling$b$sd, "*")
    known <- sweep(known, 2, scaling$b$mean, "+")

    scaler <- matrix(scaling$b$sd / scaling$A$sd, nrow = p, ncol = m, byrow = T)
    coefficient <- rbind(Intercept = scaling$b$mean, coefficient * scaler)

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
        rmse = sqrt(estimate_object$mse) * scaling$b$sd,
        predicted = fitted,
        diagnostics = diagnostics,
        C = C,
        scaling = scaling,
        kernel = kernel,
        K = if (kernel) {A} else {NULL}
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
#' @param x a matrix of new data. If not given LOOCV predictions are
#' returned.
#' @param ... not implemented
#'
#' @return a matrix or column vector with predicted values.
#'
#'
predict.ridge <- function(object, x, ...) {

  if (!is.ridge(object))
    stop("\n(-) object is no a ridge model.")

  if (missing(x)) {
    predicted <- object$predicted
    return(predicted)
  }


  if (object$kernel) {

    newdata <- cbind(1, center_kernel(x = object$K, y = rbind(x)))

  } else {

    newdata <- sweep(rbind(x), 2, object$scaling$A$mean, "-")
    newdata <- cbind(1, rbind(newdata))
    colnames(newdata)[1] <- "Intercept"

  }

  rownames(newdata) <- seq_len(nrow(newdata))
  predicted <- newdata %*% object$x

  return(predicted)

}

#'Print method for Ridge Regression
#'@noRd
print.ridge <- function(object,...) {

  if (object$kernel) {
    cat("\n Kernel Ridge Regression\n")
  } else {
     cat("\n Ridge Regression\n")
  }

  cat("\n RMSE (LOOCV):", mean(object$rmse))
  cat("\n Lambda:", mean(object$C))

}

#' @author David Senhora Navega
#' @noRd
#'
is.ridge <- function(x) inherits(x = x, what = "ridge")

#' @author David Senhora Navega
#' @noRd
#'
weighted_mean <- function(x, weights, na.rm = T) {
  value <- sum(x * weights, na.rm = na.rm) / sum(weights, na.rm = na.rm)
  return(value)
}

#' @author David Senhora Navega
#' @noRd
#'
weighted_sd <- function(x, weights, na.rm = T) {
  ss <- ((x - weighted_mean(x = x,weights = weights)) ^ 2)
  wss <- sum(x = ss * weights, na.rm = na.rm) / sum(x = weights, na.rm = na.rm)
  value <- sqrt(wss)
  return(value)
}

#' @author David Senhora Navega
#' @noRd
#'
center_kernel <- function(x, y) {

  if (missing(y)) {

    n <- nrow(x)
    one <- matrix(data = 1 / n, nrow = n, ncol = n)
    x <- x - one %*% x - x %*% one + one %*% x %*% one

    return(invisible(x))

  } else {

    n <- nrow(y)
    m <- ncol(y)

    one <- matrix(1 / m, m, m)
    two <- matrix(1 / m, n, m)
    y <- y - two %*% x - y %*% one + two %*% x %*% one

    return(invisible(y))

  }

}
