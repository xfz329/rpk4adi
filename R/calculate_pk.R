#' @title Compute the pk value to Measure the Performance of Anesthetic Depth Indicators.
#'
#' @param x_in a vector, the indicator.
#' @param y_in a vector, the state.
#'
#' @return a list containing all the matrices and variables during the calculation.
#'     The value list$type is "PK", which indicated the list is return-value of the function calculate_pk().
#'     The type of list$basic is also a list, which contains the most important results of the function.
#'     The type of list$matrices is also a list, which contains all the matrices during the calculation.
#'     The type of list$details is also a list, which contains all the intermediate variables during the calculation.
#'
#' @references Warren D. Smith, Robert C. Dutton, Ty N. Smith; Measuring the Performance of Anesthetic Depth Indicators. Anesthesiology 1996; 84:38–51 doi: https://doi.org/10.1097/00000542-199601000-00005.
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#' x1 <- c(0, 0, 0, 0, 0, 0)
#' y1 <- c(1, 1, 1, 1, 1, 2)
#' ans <- calculate_pk(x1, y1)
#'
#' ## show the most important results.
#' print(ans$basic)
#'
#' x2 <- c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6)
#' y2 <- c(1, 1, 1, 1, 1, 2, 1, 1, 3, 3, 2, 2, 2, 2, 2, 1, 3, 3, 3, 3, 3, 3, 3, 3)
#' ans2 <- calculate_pk(x2, y2)
#'
#' ## show the full results.
#' print(ans)
calculate_pk <- function(x_in, y_in) {
    x <- x_in
    y <- y_in

    # check the input type of x and y.
    stopifnot(is.vector(x))
    stopifnot(is.vector(y))

    stopifnot(!any(is.na(x)))
    stopifnot(!any(is.nan(x)))

    stopifnot(!any(is.na(y)))
    stopifnot(!any(is.nan(y)))

    stopifnot(length(x) == length(y))
    stopifnot(length(x) >= 2)

    # get n_cases.
    n_case <- length(x)

    # construct basic matrix.
    DATA <- data.table(
        x = x,
        y = y,
        k = 1:n_case,
        Ry = numeric(n_case),
        Cx = numeric(n_case),
        PKm = numeric(n_case)
    )

    # check y and set the category.
    DATA <- DATA[order(y)]
    current <- DATA[1, "y"]
    category <- 1
    for (i in 1:n_case) {
        if (current == DATA[i, "y"]) {
            DATA[i, "Ry"] <- category
        } else {
            current <- DATA[i, "y"]
            category <- category + 1
            DATA[i, "Ry"] <- category
        }
    }

    # whether jackknife could be done or not.
    y_category <- DATA$y
    jack_ok <- TRUE
    for (i in unique(DATA$y)) {
        if (2 > sum(y_category == i)) {
            jack_ok <- FALSE
        }
    }

    # get the row num for the matrix.
    rows <- length(unique(DATA$y))
    stopifnot(rows >= 2)
    if (rows > 2) {
        jack_ok <- TRUE
    }

    # check x  and set the category.
    DATA <- DATA[order(x)]
    current <- DATA[1, "x"]
    category <- 1
    for (i in 1:n_case) {
        if (current == DATA[i, "x"]) {
            DATA[i, "Cx"] <- category
        } else {
            current <- DATA[i, "x"]
            category <- category + 1
            DATA[i, "Cx"] <- category
        }
    }

    # get the col num for the matrix
    x_category <- DATA$y
    cols <- length(unique(DATA$x))

    # restore data by the index k.
    DATA <- DATA[order(k)]

    # construct matrix A.
    A <- matrix(numeric(rows * cols), nrow = rows, ncol = cols)
    for (k in 1:n_case) {
        i <- DATA[[k, "Ry"]]
        j <- DATA[[k, "Cx"]]
        A[i, j] <- A[i, j] + 1
    }

    # construct matrix S.
    S <- matrix(numeric(rows * cols), nrow = rows, ncol = cols)
    for (i in 1:rows) {
        S[i, 1] <- A[i, 1]
        if (cols >= 2) {
            for (j in 2:cols) {
                S[i, j] <- S[i, j - 1] + A[i, j]
            }
        }
    }

    # construct matrix C, D and T with the help of the assist matrix SA, CA, DA and TA.
    C <- matrix(numeric(rows * cols), nrow = rows, ncol = cols)
    D <- matrix(numeric(rows * cols), nrow = rows, ncol = cols)
    T <- matrix(numeric(rows * cols), nrow = rows, ncol = cols)
    SA <- matrix(numeric(rows * 2), nrow = rows, ncol = 2)
    CA <- matrix(numeric(rows * 2), nrow = rows, ncol = 2)
    DA <- matrix(numeric(rows * 2), nrow = rows, ncol = 2)
    TA <- matrix(numeric(rows * 2), nrow = rows, ncol = 2)

    for (i in 1:rows) {
        for (j in 1:cols) {
            if (A[i, j] != 0) {
                for (ai in 1:rows) {
                    if (ai < i) {
                        if (j > 1) {
                            C[i, j] <- C[i, j] + S[ai, j - 1]
                        }
                        if (j < cols) {
                            D[i, j] <- D[i, j] + S[ai, cols] - S[ai, j]
                        }
                        T[i, j] <- T[i, j] + A[ai, j]
                    } else if (ai > i) {
                        if (j < cols) {
                            C[i, j] <- C[i, j] + S[ai, cols] - S[ai, j]
                        }
                        if (j > 1) {
                            D[i, j] <- D[i, j] + S[ai, j - 1]
                        }
                        T[i, j] <- T[i, j] + A[ai, j]
                    }
                }
                CA[i, 1] <- CA[i, 1] + A[i, j] * C[i, j]
                DA[i, 1] <- DA[i, 1] + A[i, j] * D[i, j]
                TA[i, 1] <- TA[i, 1] + A[i, j] * T[i, j]
                CA[2, 2] <- CA[2, 2] + A[i, j] * C[i, j] * C[i, j]
                DA[2, 2] <- DA[2, 2] + A[i, j] * D[i, j] * D[i, j]
                TA[2, 2] <- TA[2, 2] + A[i, j] * C[i, j] * D[i, j]
            }
        }
        SA[1, 1] <- SA[1, 1] + S[i, cols]
        CA[1, 2] <- CA[1, 2] + CA[i, 1]
        DA[1, 2] <- DA[1, 2] + DA[i, 1]
        TA[1, 2] <- TA[1, 2] + TA[i, 1]
    }

    # calculate.
    n <- SA[1, 1]
    Qc <- CA[1, 2]
    Qd <- DA[1, 2]
    Qtx <- TA[1, 2]
    Qcdt <- Qc + Qd + Qtx
    dyx <- (Qc - Qd) / Qcdt
    PK <- (dyx + 1) / 2
    Qcc <- CA[2, 2]
    Qdd <- DA[2, 2]
    Qcd <- TA[2, 2]
    Term1 <- Qcc - 2 * Qcd + Qdd
    Term2 <- 0
    Term3 <- 0

    for (i in 1:rows) {
        ni <- S[i, cols]
        Qci <- CA[i, 1]
        Qdi <- DA[i, 1]
        Term2 <- Term2 + (n - ni) * (Qci - Qdi)
        Term3 <- Term3 + ni * (n - ni) * (n - ni)
    }
    Term2 <- -2 * dyx * Term2
    Term3 <- dyx * dyx * Term3
    SE1 <- sqrt(Term1 + Term2 + Term3) / Qcdt
    SE0 <- sqrt(Term1 - (Qc - Qd) * (Qc - Qd) / n) / Qcdt

    SPKm <- NaN
    SSPKm <- NaN
    PKj <- NaN
    SEj <- NaN

    # do jackknife.
    if (jack_ok) {
        SPKm <- 0
        SSPKm <- 0

        for (k in 1:n_case) {
            i <- DATA[[k, "Ry"]]
            j <- DATA[[k, "Cx"]]
            Crc <- C[i, j]
            Drc <- D[i, j]
            Trc <- T[i, j]
            Qcm <- Qc - 2 * Crc
            Qdm <- Qd - 2 * Drc
            Qtxm <- Qtx - 2 * Trc
            Qcdtm <- Qcm + Qdm + Qtxm
            PKm <- (Qcm + Qtxm / 2) / Qcdtm
            DATA[[k, "PKm"]] <- PKm
            SPKm <- SPKm + PKm
            SSPKm <- SSPKm + PKm * PKm
        }

        PKj <- n_case * PK - (n_case - 1) * SPKm / n_case
        SEj <- sqrt((n_case - 1) * (SSPKm - SPKm * SPKm / n_case) / n_case)
    }

    # save the matrices.
    matrices <- list(
        A = A,
        S = S,
        C = C,
        D = D,
        T = T,
        SA = SA,
        CA = CA,
        DA = DA,
        TA = TA
    )

    # save the variables.
    basic <- list(
        PK = PK,
        SE0 = SE0,
        SE1 = SE1,
        jack_ok = jack_ok,
        PKj = PKj,
        SEj = SEj
    )

    details <- list(
        PK = PK,
        SE0 = SE0,
        SE1 = SE1,
        jack_ok = jack_ok,
        PKj = PKj,
        SEj = SEj,
        n_case = n_case,
        n = n,
        Qc = Qc,
        Qd = Qd,
        Qtx = Qtx,
        Qcdt = Qcdt,
        dyx = dyx,
        Qcc = Qcc,
        Qdd = Qdd,
        Qcd = Qcd,
        Term1 = Term1,
        Term2 = Term2,
        Term3 = Term3,
        PKm = DATA$PKm,
        SPKm = SPKm,
        SSPKm = SSPKm
    )

    # return the ans.
    ans <- list(
        type = "PK",
        basic = basic,
        matrices = matrices,
        details = details
    )
    return(ans)
}
