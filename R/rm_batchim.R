#' remove batchim with pos tag etm
#'
#' @param textko text in korean utf8
#' @export
rm_etm <- function(textko){
  tar <- utf8ToInt(textko)
  done <- tar[length(tar)]
  if (chk_niun_done(textko)) {
    done <- done - 4
  }
  if (chk_liul_done(textko)) {
    done <- done - 8
  }
  res <- done
  if(length(tar) != 1){
    str <- tar[1:(length(tar)-1)]
    res <- c(str, done)
  }
  return(intToUtf8(res))
}


#' remove niun batchim
#'
#' @param textko text in korean utf8
#' @export
rm_niun_done <- function(textko){
  res <- bathim_done(textko, -4)
  return(res)
}

#' remove liul batchim
#'
#' @param textko text in korean utf8
#' @export
rm_liul_done <- function(textko){
  res <- bathim_done(textko, -8)
  return(res)
}

#' @importFrom purrr map_if map_chr
bathim_done <- function(textko, n) {
  utf8ToInt_vec(textko) %>%
    purrr::map_if(chk_length_one, ~ .x + n) %>%
    purrr::map_if(chk_length_not_one, ~ c(.x[1:(length(.x)-1)],(.x[length(.x)] + n))) %>%
    purrr::map_chr(intToUtf8) -> res
  return(res)
}

chk_length_not_one <- function(x) {
  length(x) != 1
}

chk_length_one <- function(x) {
  length(x) == 1
}
