#' @title Compare two answers of the PK values.
#'
#' @description Both of the two input have to be the output of the function calculate_pk().
#'
#'
#' @param pk1 a list, the output of the function calculate_pk().
#' @param pk2 a list, the output of the function calculate_pk().
#'
#' @return a list containing all the variables during the calculation.
#'     The value list$type is "PKC", which indicated the list is the return-value of the function compare_pk().
#'     The type of list$group is also a list, which contains the normal distribution test results for the group variables.
#'     The type of list$pair is also a list, which contains the t distribution test results for the pair variables.
#'     The type of list$details is also a list, which contains all the intermediate variables during the calculation.
#'
#' @references Warren D. Smith, Robert C. Dutton, Ty N. Smith; Measuring the Performance of Anesthetic Depth Indicators. Anesthesiology 1996; 84:38–51 doi: https://doi.org/10.1097/00000542-199601000-00005.
#'
#' @importFrom stats pnorm
#' @importFrom stats pt
#'
#' @export
#'
#' @examples
#' x1 <- c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6)
#' y1 <- c(1, 1, 1, 1, 1, 2, 1, 1, 3, 3, 2, 2, 2, 2, 2, 1, 3, 3, 3, 3, 3, 3, 3, 3)
#'
#' pk1 <- calculate_pk(x_in = x1, y_in = y1)
#' print(pk1$basic)
#'
#' x2 <- c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6)
#' y2 <- c(1, 1, 2, 1, 1, 2, 1, 2, 3, 3, 2, 2, 1, 2, 2, 2, 3, 3, 3, 3, 2, 3, 3, 2)
#'
#' pk2 <- calculate_pk(x_in = x2, y_in = y2)
#' print(pk2$basic)
#'
#' ans <- compare_pks(pk1, pk2)
#' print(ans$group)
#' print(ans$pair)
compare_pks <- function(pk1, pk2) {
    # check the input type of pk1 and pk2.
    stopifnot(is.list(pk1) & pk1$type == "PK")
    stopifnot(is.list(pk2) & pk2$type == "PK")
    stopifnot(pk1$details$n_case == pk2$details$n_case)
    stopifnot(pk1$details$n_case > 1)

    n_case <- pk1$details$n_case

    # calculate the p value using pnorm().
    PKD <- pk1$details$PKj - pk2$details$PKj
    SED <- sqrt(pk1$details$SEj * pk1$details$SEj + pk2$details$SEj * pk2$details$SEj)
    ZD <- PKD / SED
    auc <- pnorm(ZD) - 0.5
    ZP <- 1 - 2 * auc
    ZJ <- judgeP(ZP)

    # calculate the p value using pt().
    PKmD <- pk1$details$PKm - pk2$details$PKm
    SumD <- 0
    SSD <- 0
    for (i in 1:n_case) {
        current <- PKmD[i]
        SumD <- SumD + current
        SSD <- SSD + current * current
    }

    DF <- n_case - 1
    PKDJ <- n_case * PKD - DF / n_case * SumD
    SEDJ <- sqrt(DF / n_case * (SSD - 1 / n_case * SumD * SumD))
    TD <- PKDJ / SEDJ
    TP <- pt(q = abs(TD), df = DF, lower.tail = FALSE)
    TJ <- judgeP(TP)

    # save the variables.
    Z <- list(
        PKD = PKD,
        SED = SED,
        ZD = ZD,
        ZP = ZP,
        ZJ = ZJ
    )

    T <- list(
        DF = DF,
        PKDJ = PKDJ,
        SEDJ = SEDJ,
        TD = TD,
        TP = TP,
        TJ = TJ
    )

    details <- list(
        n_case = n_case,
        PKD = PKD,
        SED = SED,
        ZD = ZD,
        ZP = ZP,
        ZJ = ZJ,
        DF = DF,
        PKDJ = PKDJ,
        SEDJ = SEDJ,
        TD = TD,
        TP = TP,
        TJ = TJ,
        PKmD = PKmD,
        SumD = SumD,
        SSD = SSD
    )

    # return the ans.
    ans <- list(
        type = "PKC",
        group = Z,
        pair = T,
        details = details
    )
    return(ans)
}

#' @title Judge the interval of a p value.
#'
#' @param p double, the p value.
#'
#' @return a string indicating the interval of the given p value.
#'
#' @keywords internal
#'
#' @noRd

judgeP <- function(p) {
    if (p > 0.05) {
        return("P > 0.05")
    } else if (p > 0.01) {
        return("P > 0.01")
    } else if (p > 0.001) {
        return("P > 0.001")
    }
    return("P < 0.001")
}
