## ----knitr setup, include=FALSE------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = "#>")
knitr::opts_knit$set(root.dir = tempdir())

## ----setup---------------------------------------------------------------
library(exampletestr)
library(testthat)
devtools::create("tempkg")
devtools::use_testthat("tempkg")
file.copy(system.file("extdata", "utils.R", package = "exampletestr"), 
          "tempkg/R")
devtools::document("tempkg")

## ----Look at utils file, eval=FALSE--------------------------------------
#  #' Does parsing of text give an error?
#  #'
#  #' Can a character vector (where each line is treated as a line of R code) be
#  #' parsed as an R expression (or several R expressions) without giving an
#  #' error?
#  #'
#  #' @param text_expr The expression to be evaluated, as a character vector.
#  #'
#  #' @return `TRUE` if the code gives an error and `FALSE` otherwise.
#  #' @examples
#  #' text_parse_error("a <- 1")
#  #' text_parse_error("a <- ")
#  #' @export
#  text_parse_error <- function(text_expr) {
#    try_res <- try(parse(text = text_expr), silent = TRUE)
#    error <- inherits(try_res, "try-error")
#    if (error) attr(error, "message") <- attr(try_res, "message")
#    error
#  }
#  
#  #' Text expression groups.
#  #'
#  #' Given a character vector of R expressions, break the vector up into groups of
#  #' lines, where each group of lines is a valid R expression.
#  #'
#  #' @param text_expr A character vector.
#  #' @param remove_comments Should comments be removed?
#  #'
#  #' @return A list of character vectors, each of which can be evaluated as a
#  #'   valid R expression.
#  #' @examples
#  #' text_expr <- c("a <- 1",
#  #' "fx <- function(x) {",
#  #' "  x + 1",
#  #' "}  # this comment should disappear")
#  #' extract_expressions(text_expr)
#  #' @export
#  extract_expressions <- function(text_expr, remove_comments = TRUE) {
#    stopifnot(length(text_expr) > 0)
#    expr_groups <- list()
#    i <- 1
#    while (i <= length(text_expr)) {
#      j <- 0
#      expr <- text_expr[i]
#      while(text_parse_error(expr)) {
#        j <- j + 1
#        expr <- text_expr[i:(i + j)]
#      }
#      expr_groups <- append(expr_groups, list(expr))
#      i <- i + j + 1
#    }
#    if (remove_comments) {
#      expr_groups <- purrr::map(expr_groups, ~ formatR::tidy_source(text = .,
#          comment = !remove_comments, arrow = TRUE, indent = 2, output = FALSE,
#          width.cutoff = 50)) %>%
#        purrr::map(getElement, "text.tidy") %>%
#        purrr::map(paste0, "\n") %>%
#        purrr::map(readr::read_lines)
#      for (i in seq_along(expr_groups)) {
#        if (filesstrings::all_equal(expr_groups[[i]], character(0))) {
#          expr_groups[[i]] <- ""
#        }
#      }
#    }
#    empties <- purrr::map_lgl(expr_groups, ~ isTRUE(all.equal(., "")))
#    expr_groups <- expr_groups[!empties]
#    lapply(expr_groups, stringr::str_trim, side = "right")
#    # str_trim because sometimes formatR leaves unnecessary trailing whitespace
#  }
#  
#  #' Construct an `expect_equal` expression
#  #'
#  #' Construct an `expect_equal` expression from a character vector
#  #' containing an expression to be evaluated.
#  #'
#  #' @param text_expr A character vector of lines that, when executed produce a
#  #'   single output.
#  #'
#  #' @return A character vector. The lines of text containing the
#  #'   `expect_equal` code corresponding to the input, which will help to
#  #'   write the test file based on documentation examples. Remember that
#  #'   this is something that you're intended to fill the gaps in later.
#  #'
#  #' @examples
#  #' text_expr <- c("sum(1, ", "2)")
#  #' cat(paste(text_expr, collapse = "\n"))
#  #' construct_expect_equal(text_expr)
#  #' cat(paste(construct_expect_equal(text_expr), collapse = "\n"))
#  #' @export
#  construct_expect_equal <- function(text_expr) {
#    text_expr[1] <- paste0("expect_equal(", text_expr[1])
#    l <- length(text_expr)
#    text_expr[l] <- paste0(text_expr[l], ", )")
#    text_expr
#  }
#  
#  #' Extract examples from a `.Rd` file as a character vector.
#  #'
#  #' This is a convenient wrapper to [tools::Rd2ex] which actually returns a character vector of the examples in the `.Rd` file.
#  #'
#  #' @param rd_file_path The path to the `.Rd` file.
#  #'
#  #' @return A character vector.
#  #'
#  #' @examples
#  #' this_function_rd <- system.file("extdata", "extract_examples_rd.Rd",
#  #'                                 package = "exampletestr")
#  #' extract_examples_rd(this_function_rd)
#  #' @export
#  extract_examples_rd <- function(rd_file_path) {
#    tc <- textConnection(" ", "w")
#    tools::Rd2ex(rd_file_path, tc)
#    examples_lines <- textConnectionValue(tc)
#    close(tc)
#    examples_lines
#  }
#  

## ----Demonstrate extract_examples----------------------------------------
extract_examples("utils", pkg_dir = "tempkg")

## ----Demonstrate make_test_shell-----------------------------------------
lapply(extract_examples("utils", pkg_dir = "tempkg"), make_test_shell, "whatevs")

## ----make make_test_shell look nice, eval=FALSE--------------------------
#  test_that("whatevs", {
#    expect_equal(text_eval_error("a <- 1"), )
#    expect_equal(text_eval_error("a <- "), )
#  })

## ----make_tests_shells_file----------------------------------------------
make_tests_shells_file("utils", pkg_dir = "tempkg")

## ----test_utils.R contents, eval=FALSE-----------------------------------
#  context("Utils")
#  
#  test_that("construct_expect_equal works", {
#    text_expr <- c("sum(1, ", "2)")
#    expect_equal(cat(paste(text_expr, collapse = "\n")), )
#    expect_equal(construct_expect_equal(text_expr), )
#    expect_equal(cat(paste(construct_expect_equal(text_expr), collapse = "\n")), )
#  })
#  
#  test_that("extract_examples_rd works", {
#    this_function_rd <- system.file("extdata", "extract_examples_rd.Rd",
#      package = "exampletestr")
#    expect_equal(extract_examples_rd(this_function_rd), )
#  })
#  
#  test_that("extract_expressions works", {
#    text_expr <- c("a <- 1", "fx <- function(x) {", "  x + 1",
#      "}  # this comment should disappear")
#    expect_equal(extract_expressions(text_expr), )
#  })
#  
#  test_that("text_parse_error works", {
#    expect_equal(text_parse_error("a <- 1"), )
#    expect_equal(text_parse_error("a <- "), )
#  })

## ----fill in test shell--------------------------------------------------
context("Utils")

test_that("text_parse_error works", {
  expect_false(text_parse_error("a <- 1"))
  expect_true(text_parse_error("a <- "))
})

test_that("extract_expressions works", {
  text_expr <- c("a <- 1",
                 "fx <- function(x) {",
                 "  x + 1",
                 "}")
  expect_equal(extract_expressions(text_expr), list(
    "a <- 1",
    c("fx <- function(x) {",
      "  x + 1",
      "}")
  ))
})

test_that("construct_expect_equal works", {
  text_expr <- c("sum(1, ", "2)")
  expect_equal(construct_expect_equal(text_expr), c("expect_equal(sum(1, ",
                                                    "2), )"))
})

## ----setdown, include=FALSE, echo=FALSE----------------------------------
filesstrings::dir.remove("tempkg")

