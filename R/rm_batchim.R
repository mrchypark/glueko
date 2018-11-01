#' remove batchim
#'
#' @param textko text in korean utf8
#' @export
rm_batchim_done <- function(textko){
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
