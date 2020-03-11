
check_blanks <- function(data){

  fctrs <- names(which(map_lgl(data, is.factor)))

  if(!is_empty(fctrs)){

    lvls <- map(set_names(fctrs), ~levels(data[[.x]]))

    blank_lvls <- names(which(map_lgl(lvls, ~ any(.x %in% c('', ' ')))))

    if(!is_empty(blank_lvls)) stop("factors in data contain blank",
      " levels: ", list_things(blank_lvls), ".\nBlank levels can",
      " cause errors in tabulation functions.", call. = FALSE)

  }

}

check_dots <- function(.dots, valid_args){

  bad_args <- setdiff(names(.dots), valid_args)

  if(!vec_is_empty(bad_args)){
    stop(
      paste(
        "The following arguments are unrecognized:",
        list_things(bad_args)
      ),
      call. = FALSE
    )
  }

  .dots

}

check_tibble_one_input <- function(object){

  # check names in object
  correct_names <- c("group","variable","labels")
  missing_names <- !correct_names %in% names(object)

  if(any(missing_names)){
    stop(
      "tibble_one objects should include group, variable, and labels column.",
      " \nThe tibble_one object supplied does not have the following columns: ",
      glue::glue_collapse(
        correct_names[missing_names],sep = ", ",last = ", and"
      ),
      call. = FALSE
    )
  }

}

check_meta <- function(meta){

  # check variable types in meta data
  if( !all(meta$data$type %in% c('factor', 'numeric', 'integer')) ) {

    out_variables <- meta$data %>%
      filter(!type %in% c('factor', 'numeric', 'integer')) %>%
      mutate(variable = paste0(variable, ' (',type,')')) %>%
      pluck('variable') %>%
      paste(collapse = ' -- ')

    out_msg <- paste(
      "tibble_one is compatible with factor, numeric, and integer variables.",
      "Please inspect the following variables in your input data:",
      out_variables,
      sep= '\n'
    )

    stop(out_msg, call. = FALSE)

  }

  meta

}


