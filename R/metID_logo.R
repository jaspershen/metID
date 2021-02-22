#' @title Show the base information of metID pacakge
#' @description Show the base information of metID pacakge.
#' \lifecycle{maturing}
#' @author Xiaotao Shen
#' \email{shenxt@@stanford.edu}
#' @return A ASCII log of metID
#' @export
#' @examples 
#' metID_logo()

metID_logo <- function() {
  cat(crayon::green("Thank you for using metID!\n"))
  cat(crayon::green("Version 0.4.11 (20210221)\n"))
  cat(
    crayon::green(
      "More information can be found at https://jaspershen.github.io/metID/\n"
    )
  )
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
