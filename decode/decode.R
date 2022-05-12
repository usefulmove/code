# (decode library version 0.8.3)

#    decode_recode
#
#    description
#    recode observations, similar to dplyr::recode with reversed ("new=old")
#    assignment arguments
#
#    usage
#    decode_recode(.df, .x, ...)
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

decode_recode <- function(.df, .x, ...) {
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

#    decode_connect
#
#    description
#
#    usage
#    decode_connect(.x)
#
#    arguments
#    .con_name  name of data connection
#
#    value
#    connection object

decode_connect <- function(.con_name) {

  if (.con_name == "library") {
    .con <-
      DBI::dbConnect( # library database (AWS)
        RMariaDB::MariaDB(),
        dbname = "library",
        host = "coradbinstance.chkmsmjosdxs.us-west-1.rds.amazonaws.com",
        username = safer::decrypt_string(
                     "Us7mTdd0+ArYK3sZlhC9+2sM/Cc=",
                     rawToChar(
                       get(
                         ".dbkey",
                         envir = .GlobalEnv
                       )
                     )
                   ),
        password = safer::decrypt_string(
                     "CKfGcJQcBuxQQ3dVsk0NsmsM/CcsxB8L",
                     rawToChar(
                       get(
                         ".dbkey",
                         envir = .GlobalEnv
                       )
                     )
                   ),
        port = 3306
      )
    assign("global_library_con", .con, envir = .GlobalEnv) # add to global namespace

  } else if (.con_name == "development") {

    .con <-
      DBI::dbConnect( # development database (AWS)
        RMariaDB::MariaDB(),
        dbname = "development",
        host = "coradbinstance.chkmsmjosdxs.us-west-1.rds.amazonaws.com",
        username = safer::decrypt_string(
                     "Us7mTdd0+ArYK3sZlhC9+2sM/Cc=",
                     rawToChar(
                       get(
                         ".dbkey",
                         envir = .GlobalEnv
                       )
                     )
                   ),
        password = safer::decrypt_string(
                     "CKfGcJQcBuxQQ3dVsk0NsmsM/CcsxB8L",
                     rawToChar(
                       get(
                         ".dbkey",
                         envir = .GlobalEnv
                       )
                     )
                   ),
        port = 3306
      )
    assign("global_development_con", .con, envir = .GlobalEnv) # add to global namespace

  } else if (.con_name == "local") {

    .con <-
      DBI::dbConnect( # local MySQL database
        RMariaDB::MariaDB(),
        dbname = "minic",
        host = "localhost",
        username = "root",
        password = rstudioapi::askForPassword(prompt = "Enter password for localhost:"),
        port = 3306
      )
    assign("global_local_con", .con, envir = .GlobalEnv) # add to global namespace

  }

}

decode_disconnect <- function(.con_name) {
  if (.con_name == "library") {
    .con <- get("global_library_con", envir = .GlobalEnv)
    DBI::dbDisconnect(.con) # library database (AWS)
  } else if (.con_name == "development") {
    .con <- get("global_development_con", envir = .GlobalEnv)
    DBI::dbDisconnect(.con) # development database (AWS)
  } else if (.con_name == "local") {
    .con <- get("global_local_con", envir = .GlobalEnv)
    DBI::dbDisconnect(.con) # local database
  }
}

decode_append <- function(.data, .con_name, .dob_name) {

  if (.con_name == "library") {
    decode_connect("library")

    .con <- get("global_library_con", envir = .GlobalEnv)
    DBI::dbWriteTable(
      .con,
      .dob_name,
      .data,
      append = TRUE
    )

    decode_disconnect("library")

  } else if (.con_name == "development") {

    decode_connect("development")

    .con <- get("global_development_con", envir = .GlobalEnv)
    DBI::dbWriteTable(
      .con,
      .dob_name,
      .data,
      append = TRUE
    )

    decode_disconnect("development")

  } else if (.con_name == "local") {

    decode_connect("local")

    .con <- get("global_local_con", envir = .GlobalEnv)
    DBI::dbWriteTable(
      .con,
      .dob_name,
      .data,
      append = TRUE
    )

    decode_disconnect("local")

  } else {

  }

}

decode_create <- function(.data, .con_name, .dob_name) {

  if (.con_name == "library") {
    decode_connect("library")

    .con <- get("global_library_con", envir = .GlobalEnv)
    DBI::dbWriteTable(
      .con,
      .dob_name,
      .data,
      overwrite = TRUE
    )

    decode_disconnect("library")
  } else if (.con_name == "development") {
    decode_connect("development")

    .con <- get("global_development_con", envir = .GlobalEnv)
    DBI::dbWriteTable(
      .con,
      .dob_name,
      .data,
      overwrite = TRUE
    )

    decode_disconnect("development")
  } else if (.con_name == "local") {
    decode_connect("local")

    .con <- get("global_local_con", envir = .GlobalEnv)
    DBI::dbWriteTable(
      .con,
      .dob_name,
      .data,
      overwrite = TRUE
    )

    decode_disconnect("local")
  } else if (.con_name == "code") {
    .data |>
      jsonlite::write_json(
        stringr::str_glue("~/repos/code/library/library.{.dob_name}.json")
      )
  }
}

decode_read <- function(.con_name, .dob_name) {

  if (.con_name == "library") {
    decode_connect("library")

    .con <- get("global_library_con", envir = .GlobalEnv)
    .data <-
      DBI::dbGetQuery(
        .con,
        stringr::str_glue(
          "select * from {.dob_name}"
        )
      )

    decode_disconnect("library")
    return(.data)
  }

}

decode_readSQL <- function(.con_name, .query) {

  if (.con_name == "library") {

    decode_connect("library")

    .con <- get("global_library_con", envir = .GlobalEnv)
    .data <-
      DBI::dbGetQuery(
        .con,
        .query
      )

    decode_disconnect("library")
    return(.data)
  }

}
