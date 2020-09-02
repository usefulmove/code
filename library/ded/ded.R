# (version 0.0.3)


# ded::recode
#
#    description
#    recode observations, similar to dplyr::recode with reversed ("new=old")
#    assignment arguments
#
#    usage
#    ded_recode(.df, .x, ...)
#
#    arguments
#      .df    data frame (or tibble) to modify
#      .x     vector in data frame to modify (must be character or
#             factor vector).
#      ...    replacements. character strings of the form
#             "new_name=old_name", "new_name2=old_name2", "new_name3=old_name3", . . .
#
#    value
#    modified data frame

ded_recode <- function(.df, .x, ...) {
    .x <- deparse(substitute(.x))

    args <- rlang::list2(...)
    args <- stringr::str_split(args,"=")

    if (class(.df[[.x]])=="factor") {
        b_coerce=TRUE
        .df[[.x]] <- as.character(.df[[.x]])
    } else {
        b_coerce=FALSE
    }

    for (i in seq(1,length(args))) {
        .df[[.x]][.df[[.x]]==args[[i]][[2]]] <- args[[i]][[1]]
    }

    if (b_coerce) {
        .df[[.x]] <- as.factor(.df[[.x]])
    }

    return(.df)
}
