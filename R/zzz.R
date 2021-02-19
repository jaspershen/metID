.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    crayon::green(
      "metID,
More information can be found at https://jaspershen.github.io/metID/
If you use metID in you publication, please cite this publication:
Metabolic reaction network-based recursive metabolite annotation for untargeted metabolomics.
Authors: Xiaotao Shen (shenxt1990@163.com)
Maintainer: Xiaotao Shen.
Version 0.4.1 (20210202)"
    )
# cat(crayon::green(
#   c(
#     "                _    _____  ___ ",
#     " _ __ ___   ___| |_  \\_   \\/   \\",
#     "| '_ ` _ \\ / _ \\ __|  / /\\/ /\\ /",
#     "| | | | | |  __/ |_/\\/ /_/ /_// ",
#     "|_| |_| |_|\\___|\\__\\____/___,'  ",
#     "                                "
#   )
# ), sep = "\n")
  )
}