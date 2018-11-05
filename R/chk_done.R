#' check last batchim is niun
#'
#' @param textko text in korean utf8
#' @export
chk_niun_done <- function(textko){
  return(chk_done(textko, 4, 28))
}

#' check last batchim is liul
#'
#' @param textko text in korean utf8
#' @export
chk_liul_done <- function(textko){
  return(chk_done(textko, 8, 28))
}

#' check last moum is eu
#'
#' @param textko text in korean utf8
#' @export
chk_eu_done <- function(textko){
  return(chk_done(textko, 504, 588))
}

#' check last moum is eu
#'
#' @param textko text in korean utf8
#' @export
chk_deu_done <- function(textko){
  return(chk_done_one(textko, 46300))
}

#' @importFrom purrr map_lgl
chk_done <- function(textko, n, by){
  stopifnot(chk_utf8(textko))
  utf8ToInt_vec(textko) %>%
    purrr::map_lgl(~ (.x[length(.x)]  %in% c(44032 + seq(n, 11150, by = by)))) -> res
  return(res)
}

#' @importFrom purrr map_lgl
chk_done_one <- function(textko, n){
  stopifnot(chk_utf8(textko))
  utf8ToInt_vec(textko) %>%
    purrr::map_lgl(~ (.x[length(.x)]  %in% n)) -> res
  return(res)
}

#' @importFrom purrr map_lgl
#' @export
chk_hangul <- function(textko) {
  utf8ToInt_vec(textko) %>%
    purrr::map_lgl(~ all(.x  %in% c(12593:12686,44032:55175))) -> res
  return(res)
}

#' @importFrom utf8 utf8_valid
chk_utf8 <- function(textko) {
  utf8::utf8_valid(textko)
}


utf8ToInt_vec <- function(textko) {
  lapply(textko, utf8ToInt)
}
