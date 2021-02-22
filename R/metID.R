#' @title Show the base information of metID pacakge
#' @description Show the base information of metID pacakge.
#' \lifecycle{maturing}
#' @author Xiaotao Shen
#' \email{shenxt@@stanford.edu}
#' @return A ASCII log of metID
#' @export
#' @examples 
#' metID()

metID <- function() {
  cat(crayon::yellow(
    "`metID()` is deprecated, please use `metID_logo()`."
  ))  
  cat(crayon::green(
    c(
      "                _    _____  ___ ",
      " _ __ ___   ___| |_  \\_   \\/   \\",
      "| '_ ` _ \\ / _ \\ __|  / /\\/ /\\ /",
      "| | | | | |  __/ |_/\\/ /_/ /_// ",
      "|_| |_| |_|\\___|\\__\\____/___,'  ",
      "                                "
    )
    
  ), sep = "\n")
}






# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# metid_logo <-
#   c("                _    _____  ___ ", " _ __ ___   ___| |_  \\_   \\/   \\",
#     "| '_ ` _ \\ / _ \\ __|  / /\\/ /\\ /", "| | | | | |  __/ |_/\\/ /_/ /_// ",
#     "|_| |_| |_|\\___|\\__\\____/___,'  ", "                                "
#   )
# cat(metid_logo, sep = "\n")
