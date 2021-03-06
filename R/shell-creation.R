#' Make the inside of a [testthat::test_that()] test.
#'
#' Given a character vector of the examples from a function, create the shell of
#' the inside of a [testthat::test_that()] code block (to be filled in by the
#' user) based upon those examples.
#'
#' Assignment lines (lines with `<-` or `=` assignment) and lines starting with
#' `print(`, `stop(`, `warning(`, `message(`, `setwd(`, `plot(`, `ggplot(`,
#' `set.seed(` or `library(` are left alone, others are put in the shell of an
#' `expect_equal()` statement. To prevent anything from being put in the shell
#' of an `expect_equal()` statement, set `e_e = FALSE`. Anything found within a
#' `\\dontrun\{...\}` block is ignored.
#'
#' @param example_block A character vector of the lines in the examples of a
#'   function's documentation.
#' @param e_e Set this to `FALSE` to prevent anything from being put in the
#'   shell of an `expect_equal()` statement.
#'
#' @return A character vector giving the shell of the inside of a
#'   [testthat::test_that()] function call testing all of the calls in the
#'   example block.
#'
#' @examples
#' pkg_dir <- paste0(tempdir(check = TRUE), "/tmpkg")
#' usethis::create_package(pkg_dir, rstudio = FALSE, open = FALSE)
#' fs::file_copy(
#'   system.file("extdata", "detect.R", package = "exampletestr"),
#'   paste0(pkg_dir, "/R")
#' )
#' example_blocks <- exampletestr:::extract_examples("detect", pkg_dir)
#' exampletestr:::make_test_that_innards_shell(example_blocks[[1]])
#' exampletestr:::make_test_that_innards_shell(example_blocks[[1]],
#'   desc = "xyz", e_e = FALSE
#' )
#' fs::dir_delete(pkg_dir)
#' @noRd
make_test_that_innards_shell <- function(example_block, e_e = TRUE,
                                         roxytest = FALSE) {
  checkmate::assert_character(example_block)
  checkmate::assert_flag(e_e)
  checkmate::assert_flag(roxytest)
  if (!length(example_block)) {
    return(character(0))
  }
  expressions <- extract_expressions(example_block)
  if (e_e) {
    for_checking <- expressions %>%
      purrr::map(strex::str_remove_quoted) %>%
      purrr::map(stringr::str_replace_all, " ", "")
    leave_alone <- purrr::map_lgl(
      for_checking,
      ~ any(stringr::str_detect(., paste0(
        "(?:<-|^stop\\(|^warning\\(|^message\\(|",
        "^#|^setwd\\(|^library\\(|",
        "^plot\\(|^ggplot\\(|^print",
        "\\(|^set\\.seed\\()"
      )))
    )
    inside_test_that <- purrr::map2(
      leave_alone, expressions,
      ~ if (.x) {
        .y
      } else {
        construct_expect_equal(.y)
      }
    ) %>%
      unlist()
  } else {
    inside_test_that <- unlist(expressions)
  }
  if (roxytest) {
    inside_test_that_expressions <- inside_test_that %>%
      extract_expressions() %>%
      purrr::map_chr(stringr::str_c, collapse = " ") %>%
      stringr::str_trim()
    ee_expressions <- stringr::str_subset(
      inside_test_that_expressions,
      "^expect_equal\\s*\\("
    ) %>%
      purrr::map(styler::style_text)
    return(c("@testexamples", paste("#'", unlist(ee_expressions))))
  }
  inside_test_that
}

#' Make the shell of a [testthat::test_that()] test.
#'
#' Given a character vector of the examples from a function, create the shell of
#' a [testthat::test_that()] code block (to be filled in by the user) based upon
#' those examples.
#'
#' Assignment lines (lines with `<-` or `=` assignment) and lines starting with
#' `print(`, `stop(`, `warning(`, `message(`, `setwd(`, `plot(`, `ggplot(`,
#' `set.seed(` or `library(` are left alone, others are put in the shell of an
#' `expect_equal()` statement. To prevent anything from being put in the shell
#' of an `expect_equal()` statement, set `e_e = FALSE`. Anything found within a
#' `\\dontrun\{...\}` block is ignored.
#'
#' @param example_block A character vector of the lines in the examples of a
#'   function's documentation.
#' @param desc To be the `desc` argument of the [testthat::test_that()] call.
#' @param e_e Set this to `FALSE` to prevent anything from being put in the
#'   shell of an `expect_equal()` statement.
#'
#' @return A character vector giving the shell of a `test_that` function call
#'   testing all of the calls in the example block.
#'
#' @examples
#' pkg_dir <- paste0(tempdir(check = TRUE), "/tmpkg")
#' usethis::create_package(pkg_dir, rstudio = FALSE, open = FALSE)
#' fs::file_copy(
#'   system.file("extdata", "detect.R", package = "exampletestr"),
#'   paste0(pkg_dir, "/R")
#' )
#' example_blocks <- exampletestr:::extract_examples("detect", pkg_dir)
#' exampletestr:::make_test_shell(example_blocks[[1]])
#' exampletestr:::make_test_shell(example_blocks[[1]],
#'   desc = "xyz", e_e = FALSE
#' )
#' fs::dir_delete(pkg_dir)
#' @noRd
make_test_shell <- function(example_block, desc = "", e_e = TRUE) {
  checkmate::assert_character(example_block)
  checkmate::assert_string(desc)
  checkmate::assert_flag(e_e)
  if (!length(example_block)) {
    return(character(0))
  }
  inside_test_that <- make_test_that_innards_shell(example_block, e_e = e_e)
  c(
    paste0("test_that(\"", desc, "\", {"),
    paste(" ", inside_test_that), # this will prepend two spaces
    "})"
  )
}
