library(tidyverse)
library(plyr)
library(readxl)
library(stargazer)

factorStats <- read.csv("factorstats.csv")
factorStats <- factorStats[-1]

lmBulk <- function(x, variables, omit = NULL, printStar = FALSE) {
    # Allows for quick rotation of dependent var and checking of dependence of models
    n <- base::length(variables)

    if (n < 2) {
        stop(paste(
            "Expected vector length of 2 or more. Received length of",
            n
        ))
    }

    if (!all(variables %in% names(x))) {
        missing_vars <- dplyr::setdiff(variables, names(x))
        stop(paste(
            "The following variables were not found in the dataframe:",
            paste(missing_vars, collapse = ", ")
        ))
    }

    if (!is.null(omit)) {
        dep_vars <- base::setdiff(variables, omit)
    } else {
        dep_vars <- variables
    }

    n <- length(dep_vars)
    model_list <- list()

    for (a in 1:n) {
        dep <- dep_vars[a]
        indep <- base::setdiff(variables, dep)

        model <- lm(
            stats::as.formula(paste(dep, "~", paste(indep, collapse = " + "))),
            data = x
        )

        model_name <- paste0("ModelDep", dep)
        model_list[[model_name]] <- model
    }

    if (printStar == T) {
        stargazer::stargazer(
            model_list,
            type = "html",
            out = paste0(paste0(variables, collapse = "_"), ".html")
        )
    }

    return(model_list)
}

lmBulk(
    factorStats,
    c("cFactor3", "cFactor4", "cFactor7", "activities.5.eqmore"),
    omit = "activities.5.eqmore",
    printStar = T
)
