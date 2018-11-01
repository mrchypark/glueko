#' check last batchim is niun
#'
#' @param textko text in korean utf8
#' @export
chk_niun_done <- function(textko){
  return(chk_done(textko, 4))
}

#' check last batchim is liul
#'
#' @param textko text in korean utf8
#' @export
chk_liul_done <- function(textko){
  return(chk_done(textko, 8))
}

chk_done <- function(textko, n){
  stopifnot(chk_utf8(textko))
  res <- textko
  if(chk_hangle(textko)){
    res <- utf8ToInt(textko) %in% c(44032 + seq(n, 11150, by = 28))
    res <- res[length(res)]
  }
  return(res)
}

chk_hangle <- function(textko) {
  res <- all(utf8ToInt(textko) %in% c(12593:12686,44032:55175))
  return(res)
}

#' @importFrom utf8 utf8_valid
chk_utf8 <- function(textko) {
  utf8::utf8_valid(textko)
}
