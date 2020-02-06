#' @title Show the base information of metID pacakge
#' @description Show the base information of metID pacakge.
#' \lifecycle{maturing}
#' @author Xiaotao Shen
#' \email{shenxt@@stanford.edu}
#' @export

metID <- function(){
  cat(crayon::green("Thank you for using metID!\n"))
  cat(crayon::green("Version 0.3.0 (20200205)\n"))
  cat(crayon::green("More information can be found at https://jaspershen.github.io/metID/\n"))
  cat(crayon::green(
    c("                _    _____  ___ ", " _ __ ___   ___| |_  \\_   \\/   \\", 
      "| '_ ` _ \\ / _ \\ __|  / /\\/ /\\ /", "| | | | | |  __/ |_/\\/ /_/ /_// ", 
      "|_| |_| |_|\\___|\\__\\____/___,'  ", "                                "
    )
    
  ), sep = "\n")
}


.onAttach <- function(libname, pkgname){
  packageStartupMessage(crayon::green(
    "metID,
More information can be found at https://jaspershen.github.io/metID/
If you use metID in you publication, please cite this publication:
Metabolic reaction network-based recursive metabolite annotation for untargeted metabolomics.
Authors: Xiaotao Shen (shenxt1990@163.com)
Maintainer: Xiaotao Shen.
Version 0.3.0 (20200205)"
  ),
  cat(crayon::green(
    c("                _    _____  ___ ", " _ __ ___   ___| |_  \\_   \\/   \\", 
      "| '_ ` _ \\ / _ \\ __|  / /\\/ /\\ /", "| | | | | |  __/ |_/\\/ /_/ /_// ", 
      "|_| |_| |_|\\___|\\__\\____/___,'  ", "                                "
    )
    
  ), sep = "\n")
  )
}




# packageStartupMessage(
#   "metIdentify,
# More information can be found at https://jaspershen.github.io/metID/
# If you use metID in you publication, please cite this publication:
# Metabolic reaction network-based recursive metabolite annotation for untargeted metabolomics.
# Authors: Xiaotao Shen (shenxt1990@163.com)
# Maintainer: Xiaotao Shen.
# Version 0.2.2 (20200125)"
# )





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
