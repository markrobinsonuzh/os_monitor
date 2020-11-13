#' Tibble reactive, tibble with extra attributes
#'
#' @param data data.frame
#' @param name attribute, character 
#' @param input_value attribute, logical
#' @param valid_input attribute, logical
#' @param try_to_retrieve attribute, logical
#' @param retrieval_done attribute, logical
#' @param successfully_retrieved attribute, logical
#' @param ... for use with tibble
#'
#' @return tibble_reac class (tibble with extra attributes)
#' @export
#'
#' @examples
new_tibble_reac <- function(data, name="", input_value = "", valid_input = FALSE, 
                            try_to_retrieve = FALSE, retrieval_done = FALSE, 
                            successfully_retrieved = FALSE, try_to_merge=FALSE, ...) {
  stopifnot(is.data.frame(data))
  data <-  tibble::new_tibble(data, ..., nrow = nrow(data), class = "tibble_reac")
  attributes(data) <- c(attributes(data), 
                        name=name,
                        input_value=input_value,
                        valid_input=valid_input, 
                        try_to_retrieve=try_to_retrieve,
                        retrieval_done=retrieval_done,
                        successfully_retrieved=successfully_retrieved,
                        try_to_merge=try_to_merge)
  data
}

#' validate tibble_reac
#'
#' @param x tibble_reac
#'
#' @return
#' @export
#'
#' @examples
validate_tibble_reac <- function(x) {
  name <- attr(x, "name")
  input_value <- attr(x, "input_value")
  valid_input <- attr(x, "valid_input")
  try_to_retrieve <- attr(x, "try_to_retrieve")
  retrieval_done <- attr(x, "retrieval_done")
  successfully_retrieved <- attr(x, "successfully_retrieved")
  try_to_merge <- attr(x, "try_to_merge")
  if (!is(name,"character")) {
    stop(
      "name must be character",
      call. = FALSE
    )
  }
  if (!is(input_value,"character")) {
    stop(
      "input_value must be character",
      call. = FALSE
    )
  }
  
  if (!is(valid_input,"logical")) {
    stop(
      "valid_input must be logical.",
      call. = FALSE
    )
  }
  if (!is(try_to_retrieve,"logical")) {
    stop(
      "try_to_retrieve must be logical",
      call. = FALSE
    )
  }
  if (!is(successfully_retrieved,"logical")) {
    stop(
      "successfully_retrieved must be logical.",
      call. = FALSE
    )
  }
  if (!is(try_to_merge,"logical")) {
    stop(
      "try_to_merge must be logical",
      call. = FALSE
    )
  }
  if (!valid_input && (try_to_retrieve || retrieval_done || successfully_retrieved)) {
    stop(
      "if valid_input is FALSE, try_to_retrieve and successfully_retrieved have to be FALSE",
      call. = FALSE
    )
  }
  if ((try_to_retrieve && retrieval_done)) {
    stop(
      "either try_to_retrieve or retrieval_done has to be FALSE.",
      call. = FALSE
    )
  }

  x
}

#' convert tibble to tibble_reac
#'
#' @param data tibble or data.frame
#' @param ... for tibble
#'
#' @return
#' @export
#'
#' @examples
as_tibble_reac <- function(data, ...) {
  x <- new_tibble_reac(data, ...)
  validate_tibble_reac(x)
}

#' name methods
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
name <- function(x) {
  UseMethod("name")
}
#' @export
name.tibble_reac <- function(x) attr(x, "name")
#' @export
"name<-" <- function(x, value) {
  UseMethod("name<-")
}
#' @export
"name<-.tibble_reac" <- function(x, value) {
  stopifnot(is(value,"character"))
  attr(x, "name") <- value
  x
}


# input_value methods
#' @export
input_value <- function(x) {
  UseMethod("input_value")
}
#' @export
input_value.tibble_reac <- function(x) attr(x, "input_value")
#' @export
input_value.default <- function(x) attr(x, "input_value")

#' @export
"input_value<-" <- function(x, value) {
  UseMethod("input_value<-")
}
#' @export
"input_value<-.default" <- function(x, value) {
  stopifnot(is(value,"character"))
  attr(x, "input_value") <- value
  x
}
#' @export
"input_value<-.tibble_reac" <- function(x, value) {
  stopifnot(is(value,"character"))
  attr(x, "input_value") <- value
  x
}

# valid_input methods
#' @export
valid_input <- function(x) {
  UseMethod("valid_input")
}
#' @export
valid_input.tibble_reac <- function(x) attr(x, "valid_input")
#' @export
valid_input.default <- function(x) attr(x, "valid_input")
#' @export
"valid_input<-" <- function(x, value) {
  UseMethod("valid_input<-")
}
#' @export
"valid_input<-.default" <- function(x, value) {
  stopifnot(is(value,"logical"))
  attr(x, "valid_input") <- value
  x
}
#' @export
"valid_input<-.tibble_reac" <- function(x, value) {
  stopifnot(is(value,"logical"))
  attr(x, "valid_input") <- value
  x
}

# try_to_retrieve methods
#' @export
try_to_retrieve <- function(x) {
  UseMethod("try_to_retrieve")
}
#' @export
try_to_retrieve.tibble_reac <- function(x) attr(x, "try_to_retrieve")
#' @export
try_to_retrieve.default <- function(x) attr(x, "try_to_retrieve")
#' @export
"try_to_retrieve<-" <- function(x, value) {
  UseMethod("try_to_retrieve<-")
}
#' @export
"try_to_retrieve<-.default" <- function(x, value) {
  stopifnot(is(value,"logical"))
  attr(x, "try_to_retrieve") <- value
  if(value && retrieval_done(x)){
    retrieval_done(x) <- FALSE
  }
  x
}
#' @export
"try_to_retrieve<-.tibble_reac" <- function(x, value) {
  stopifnot(is(value,"logical"))
  attr(x, "try_to_retrieve") <- value
  if(value && retrieval_done(x)){
    retrieval_done(x) <- FALSE
  }
  x
}

# retrieval_done methods
#' @export
retrieval_done <- function(x) {
  UseMethod("retrieval_done")
}
#' @export
retrieval_done.tibble_reac <- function(x) attr(x, "retrieval_done")
#' @export
retrieval_done.default <- function(x) attr(x, "retrieval_done")
#' @export
"retrieval_done<-" <- function(x, value) {
  UseMethod("retrieval_done<-")
}
#' @export
"retrieval_done<-.default" <- function(x, value) {
  stopifnot(is(value,"logical"))
  attr(x, "retrieval_done") <- value
  if(value && try_to_retrieve(x)){
    try_to_retrieve(x) <- FALSE
  }
  x
}
#' @export
"retrieval_done<-.tibble_reac" <- function(x, value) {
  stopifnot(is(value,"logical"))
  attr(x, "retrieval_done") <- value
  if(value && try_to_retrieve(x)){
    try_to_retrieve(x) <- FALSE
  }
  x
}


# successfully_retrieved methods
#' @export
successfully_retrieved <- function(x) {
  UseMethod("successfully_retrieved")
}
#' @export
successfully_retrieved.tibble_reac <- function(x) attr(x, "successfully_retrieved")
#' @export
successfully_retrieved.default <- function(x) attr(x, "successfully_retrieved")
#' @export
"successfully_retrieved<-" <- function(x, value) {
  UseMethod("successfully_retrieved<-")
}
#' @export
"successfully_retrieved<-.default" <- function(x, value) {
  stopifnot(is(value,"logical"))
  attr(x, "successfully_retrieved") <- value
  x
}
#' @export
"successfully_retrieved<-.tibble_reac" <- function(x, value) {
  stopifnot(is(value,"logical"))
  attr(x, "successfully_retrieved") <- value
  x
}

# try_to_merge methods
#' @export
try_to_merge <- function(x) {
  UseMethod("try_to_merge")
}
#' @export
try_to_merge.tibble_reac <- function(x) attr(x, "try_to_merge")
#' @export
try_to_merge.default <- function(x) attr(x, "try_to_merge")
#' @export
"try_to_merge<-" <- function(x, value) {
  UseMethod("try_to_merge<-")
}
#' @export
"try_to_merge<-.default" <- function(x, value) {
  stopifnot(is(value,"logical"))
  attr(x, "try_to_merge") <- value
  x
}
#' @export
"try_to_merge<-.tibble_reac" <- function(x, value) {
  stopifnot(is(value,"logical"))
  attr(x, "try_to_merge") <- value
  x
}

#' change attribute of reactive value 
#'
#' @param x reactiveVal tibble_reac
#' @param method character, which attribute to change
#' @param value character, value to give attribute
#'
#' @return
#' @export
#'
#' @examples
assign_to_reactiveVal <- function(x, method, value){
  do.call(paste0(method,"<-"), list(x=x(),value=value)) %>% 
    x()
}

#' change class of x to tibble_reac with attributes of tbl_reac
#'
#' @param x reactiveVal tibble_reac
#' @param tbl_reac tibble_reac
#'
#' @return
#' @export
#'
#' @examples
to_tibble_reac_template <- function(x, tbl_reac){
  attr_ls <- attributes(tbl_reac)
  attr_ls <- attr_ls[!(names(attr_ls) %in% c("names","row.names","class"))]
  attr_ls[["data"]] <- x
  do.call("as_tibble_reac", attr_ls)
}

#' check if data retrieval was successfull and set attribute successfully_retrieved
#'
#' @param x reactiveVal tibble_reac
#'
#' @return
#' @export
#'
#' @examples
check_and_set_successfull_retrieval <- function(x){
  if(dim(x())[1] != 0){
    assign_to_reactiveVal(x, "successfully_retrieved", TRUE)
  } else {
    assign_to_reactiveVal(x, "successfully_retrieved", FALSE)
  }
}


# sloop::s3_dispatch(name(tblreac) <- "lkjl")
# valid_input(tblreac) <- TRUE
# attributes(tblreac)
# 
# 
# tibble(a=letters[1:20],b=1:20) %>% 
# as_tibble_reac(name="lkj", input_value="lkjlkj") %>% 
#   attributes()
# install.packages("sloop")
# input_value(tibble(a=letters[1:20],b=1:20))

# class(tblreac)
# tbl <- as_tibble(tblreac)
# class(tbl)
# tblreac <- as_tibble_reac(tibble(a=letters[1:20],b=1:20))
# try_to_retrieve(tblreac) <- TRUE
# valid_input(tblreac) <- TRUE
# to_tibble_reac_template(tibble(a=letters[1:20],b=1:20),tblreac) %>% 
#   valid_input()