
# If var_label of this column is null, return col.
# Otherwise, return the var_label of the column
# @param data a dataframe.
# @param col a column name in `data`
#

get_label <- function(data, col){

  # If var_label of this column is null, return col
  # otherwise, return the var_label of the column
  out <- var_label(data[[col]])

  if(is.null(out) | rlang::is_empty(out)) out <- capitalize(col)

  out

}

# If unit of this column is null, return missing value.
# Otherwise, return the unit of the column
# @param data a dataframe.
# @param col a column name in `data`
#

get_units <- function(data, col){

  attr(data[[col]],'units') %||% NA_character_

}

# If abbreviations for this column are null, return missing value.
# Otherwise, return the abbreviations for the column
# @param data a dataframe.
# @param col a column name in `data`
#

get_abbrs <- function(data, col){

  attr(data[[col]],'abbrs') %||% NA_character_

}

# If note of this column is null, return missing value.
# Otherwise, return the notes for the column
# @param data a dataframe.
# @param col a column name in `data`
#

get_notes <- function(data, col){

  attr(data[[col]],'notes') %||% NA_character_

}


# If group of this column is null, return missing value.
# Otherwise, return the group of the column
# @param data a dataframe.
# @param col a column name in `data`

get_groups <- function(data, col){

  attr(data[[col]],'group') %||% "None"

}

# If this column belongs to multiple classes, return the primitive class type.
# Otherwise, return NA_character_ and report warning
# @param data a dataframe.
# @param col a column name in `data`

get_class <- function(data, col){

  class_vec <- class(data[[col]])
  # class_vec <- class(col)
  #
  ret <- intersect(class_vec,  c('factor', 'numeric', 'integer'))

  if(length(ret) == 1) return(ret)
  else if(length(ret) == 0) stop(glue("Variable {col} doesn't contains any of the 'factor', 'numeric', 'integer' classes"))
  else if(length(ret) >1) stop(glue("Variable {col} contains more than one of the 'factor', 'numeric', 'integer' classes"))
  # I don't know why, but this doesn't work. Probably because of the ret
  # case_when(
  #   length(ret) == 1 ~ ret,
  #   length(ret) == 0 ~ stop(glue("Variable {col} doesn't contains any of the 'factor', 'numeric', 'integer' classes")),
  #   length(ret) >1  ~ stop(glue("Variable {col} contains more than one of the 'factor', 'numeric', 'integer' classes"))
  #           )

}
