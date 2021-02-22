.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    crayon::green(
      "metID,
More information can be found at https://jaspershen.github.io/metID/
If you use metID in you publication, please cite this publication:
Metabolic reaction network-based recursive metabolite annotation for untargeted metabolomics.
Authors: Xiaotao Shen (shenxt1990@163.com)
Maintainer: Xiaotao Shen."
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

globalVariables(
  names = c(
    "MS1.peak.name",
    "MS2.spectra.name",
    "Candidate.number",
    "Compound.name",
    "CAS.ID",
    "HMDB.ID",
    "KEGG.ID",
    "Lab.ID",
    "Adduct",
    "mz.error",
    "mz.match.score",
    "RT.error",
    "RT.match.score",
    "CE",
    "SS",
    "Total.score",
    "Database",
    "Peak.name",
    "name",
    "."
  )
)