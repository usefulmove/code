# (decode library version 0.1.0)

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
    .dob <-
      DBI::dbConnect( # library database (AWS)
        RMariaDB::MariaDB(),
        dbname = "library",
        host = "coradbinstance.chkmsmjosdxs.us-west-1.rds.amazonaws.com",
        username = "root",
        password = "rootroot",
        port = 3306
      )
  }

  return(.dob)
}

decode_disconnect <- function(.con_name) {
  if (.con_name == "library") {
    DBI::dbDisconnect(global_library_con) # library database (AWS)
  }
}

decode_overwrite <- function(.con_name, .dob_name, .data) {

  if (.con_name == "library") {
    .con <- decode_connect("library")

    DBI::dbWriteTable(
      .con,
      .dob_name,
      .data,
      overwrite = TRUE
    )

    global_library_con <<- .con # global connection variable

    decode_disconnect("library")
  }

  #if (.con_name == "code.library.us_demographics_census2020") {
  #  .data %>%
  #    jsonlite::write_json(
  #      "~/repos/code/library/.ibrary.us_demographics_census2020.json"
  #    )
  #}

  if (.con_name == "code") {
    .data %>%
      jsonlite::write_json(
        str_glue("~/repos/code/library/library.{.dob_name}.json")
      )
  }

}

decode_read <- function(.con_name, .dob_name) {

  if (.con_name == "library") {

    .db <- decode_connect("library")

    .data <-
      dbGetQuery(
        .db,
        str_glue(
          "select * from {.dob_name}"
        )
      )

    global_library_con <<- .db # global connection variable

    decode_disconnect("library")

    return(.data)
  }
}

#decode_append <- function(TODO) # get data object
#decode_getdob <- function(TODO) # get data object
#decode_senddob <- function(TODO) # send message to data object

