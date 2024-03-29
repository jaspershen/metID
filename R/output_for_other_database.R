################################################################################
############################    MONA   ########################################
################################################################################

##------------------------------------------------------------------------------
#' @title Export metID database to msp (mona format)
#' @description Export metID database to msp (mona format).
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param databasae metID database.
#' @param path Work directory.
#' @importFrom magrittr %>%
#' @importFrom crayon red yellow green bgRed
#' @importFrom stringr str_detect str_extract
#' @importFrom readr cols
#' @importFrom pbapply pblapply
#' @return msp format files in local folder.
#' @export

# sxtTools::setwd_project()
# load("other_files/all_ms2_database/mike_in_house/msDatabase_hilic0.0.2")
# database = msDatabase_hilic0.0.2
# setwd("other_files/all_ms2_database/mike_in_house")
# write_msp_mona(databasae = databasae)
# x = read_msp_mona(file = "spectra_pos.msp")

write_msp_mona = function(databasae,
                          path = ".") {
  options(warn = -1)
  spectra.info = database@spectra.info
  spectra_pos = database@spectra.data$Spectra.positive
  spectra_neg = database@spectra.data$Spectra.negative
  
  if (length(spectra_pos) == 0 &
      length(spectra_neg) == 0) {
    cat("No MS2 spectra.")
  }
  
  # temp = read_lines("2021_6_10/MoNA-export-LC-MS-MS_Spectra.msp")
  
  ####positive mode
  if (length(spectra_pos) > 0) {
    cat(crayon::yellow("Write positive mode...\n"))
    unlink(
      x = file.path(path, "spectra_pos.msp"),
      recursive = TRUE,
      force = TRUE
    )
    sink(file = file.path(path, "spectra_pos.msp"),
         append = TRUE)
    purrr::walk2(
      .x = names(spectra_pos),
      .y = spectra_pos,
      .f = function(compound_id, spectra) {
        message(compound_id)
        purrr::walk2(
          .x = names(spectra),
          .y = spectra,
          .f = function(ce, single_spectra) {
            temp_spectra_info =
              spectra.info %>%
              dplyr::filter(Lab.ID == compound_id)
            result =
              c(
                paste("Name:", temp_spectra_info$Compound.name),
                paste("Synon:", temp_spectra_info$Synon),
                paste("DB#:", temp_spectra_info$mona.ID),
                paste("InChIKey:", temp_spectra_info$InChIKey),
                paste("InChI:", temp_spectra_info$InChI),
                paste("SMILES:", temp_spectra_info$SMILES),
                paste("Precursor_type:", temp_spectra_info$Precursor_type),
                paste("Spectrum_type:", "MS2"),
                paste("PrecursorMZ:", temp_spectra_info$PrecursorMZ),
                paste(
                  "Instrument_type:",
                  temp_spectra_info$Instrument.type
                ),
                paste("Instrument:", temp_spectra_info$Instrument),
                paste("Ion_mode:", "P"),
                paste("Collision_energy:", ce),
                paste("Formula:", temp_spectra_info$Formula),
                paste("MW:", temp_spectra_info$MW),
                paste("ExactMass:", temp_spectra_info$mz),
                paste("Comments:", temp_spectra_info$Comments),
                paste("Num Peaks:", nrow(single_spectra))
              )
            single_spectra2 =
              single_spectra %>%
              apply(1, function(x) {
                paste(x, collapse = " ")
              })
            result =
              c(result, single_spectra2, "")
            cat(
              result,
              file = file.path(path, "spectra_pos.msp"),
              append = TRUE,
              sep = "\n"
            )
            # writeLines(text = result, con = fileConn)
          }
        )
      }
    )
    sink()
    sink(NULL)
    cat(crayon::green("Done.\n"))
  }
  
  ###negative mode
  if (length(spectra_neg) > 0) {
    cat(crayon::yellow("Write negative mode...\n"))
    unlink(
      x = file.path(path, "spectra_neg.msp"),
      recursive = TRUE,
      force = TRUE
    )
    sink(file = file.path(path, "spectra_neg.msp"),
         append = TRUE)
    purrr::walk2(
      .x = names(spectra_neg),
      .y = spectra_neg,
      .f = function(compound_id, spectra) {
        message(compound_id)
        purrr::walk2(
          .x = names(spectra),
          .y = spectra,
          .f = function(ce, single_spectra) {
            temp_spectra_info =
              spectra.info %>%
              dplyr::filter(Lab.ID == compound_id)
            result =
              c(
                paste("Name:", temp_spectra_info$Compound.name),
                paste("Synon:", temp_spectra_info$Synon),
                paste("DB#:", temp_spectra_info$mona.ID),
                paste("InChIKey:", temp_spectra_info$InChIKey),
                paste("InChI:", temp_spectra_info$InChI),
                paste("SMILES:", temp_spectra_info$SMILES),
                paste("Precursor_type:", temp_spectra_info$Precursor_type),
                paste("Spectrum_type:", "MS2"),
                paste("PrecursorMZ:", temp_spectra_info$PrecursorMZ),
                paste(
                  "Instrument_type:",
                  temp_spectra_info$Instrument.type
                ),
                paste("Instrument:", temp_spectra_info$Instrument),
                paste("Ion_mode:", "P"),
                paste("Collision_energy:", ce),
                paste("Formula:", temp_spectra_info$Formula),
                paste("MW:", temp_spectra_info$MW),
                paste("ExactMass:", temp_spectra_info$mz),
                paste("Comments:", temp_spectra_info$Comments),
                paste("Num Peaks:", nrow(single_spectra))
              )
            single_spectra2 =
              single_spectra %>%
              apply(1, function(x) {
                paste(x, collapse = " ")
              })
            result =
              c(result, single_spectra2, "")
            cat(
              result,
              file = file.path(path, "spectra_neg.msp"),
              append = TRUE,
              sep = "\n"
            )
            # writeLines(text = result, con = fileConn)
          }
        )
      }
    )
    sink()
    sink(NULL)
    cat(crayon::green("Done.\n"))
  }
}


##------------------------------------------------------------------------------
#' @title Export metID database to mgf (mona format)
#' @description Export metID database to mgf (mona format).
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param databasae metID database.
#' @param path Work directory.
#' @importFrom magrittr %>%
#' @importFrom crayon red yellow green bgRed
#' @importFrom stringr str_detect str_extract
#' @importFrom readr cols
#' @importFrom pbapply pblapply
#' @return mgf format files in local folder.
#' @export

# sxtTools::setwd_project()
# load("other_files/all_ms2_database/mike_in_house/msDatabase_hilic0.0.2")
# database = msDatabase_hilic0.0.2
# setwd("other_files/all_ms2_database/mike_in_house")
# write_mgf_mona(databasae = databasae)
# x = read_mgf_mona(file = "spectra_pos.mgf")

write_mgf_mona = function(databasae,
                          path = ".") {
  options(warn = -1)
  spectra.info = database@spectra.info
  spectra_pos = database@spectra.data$Spectra.positive
  spectra_neg = database@spectra.data$Spectra.negative
  
  if (length(spectra_pos) == 0 &
      length(spectra_neg) == 0) {
    cat("No MS2 spectra.")
  }
  
  
  ####positive mode
  if (length(spectra_pos) > 0) {
    cat(crayon::yellow("Write positive mode...\n"))
    unlink(
      x = file.path(path, "spectra_pos.mgf"),
      recursive = TRUE,
      force = TRUE
    )
    sink(file = file.path(path, "spectra_pos.mgf"),
         append = TRUE)
    purrr::walk2(
      .x = names(spectra_pos),
      .y = spectra_pos,
      .f = function(compound_id, spectra) {
        message(compound_id)
        purrr::walk2(
          .x = names(spectra),
          .y = spectra,
          .f = function(ce, single_spectra) {
            temp_spectra_info =
              spectra.info %>%
              dplyr::filter(Lab.ID == compound_id)
            result =
              c(
                paste("Name:", temp_spectra_info$Compound.name),
                paste("Synon:", temp_spectra_info$Synon),
                paste("DB#:", temp_spectra_info$mona.ID),
                paste("InChIKey:", temp_spectra_info$InChIKey),
                paste("InChI:", temp_spectra_info$InChI),
                paste("SMILES:", temp_spectra_info$SMILES),
                paste("Precursor_type:", temp_spectra_info$Precursor_type),
                paste("Spectrum_type:", "MS2"),
                paste("PrecursorMZ:", temp_spectra_info$PrecursorMZ),
                paste(
                  "Instrument_type:",
                  temp_spectra_info$Instrument.type
                ),
                paste("Instrument:", temp_spectra_info$Instrument),
                paste("Ion_mode:", "P"),
                paste("Collision_energy:", ce),
                paste("Formula:", temp_spectra_info$Formula),
                paste("MW:", temp_spectra_info$MW),
                paste("ExactMass:", temp_spectra_info$mz),
                paste("Comments:", temp_spectra_info$Comments),
                paste("Num Peaks:", nrow(single_spectra))
              )
            single_spectra2 =
              single_spectra %>%
              apply(1, function(x) {
                paste(x, collapse = " ")
              })
            result =
              c("BEGIN IONS", result, single_spectra2, "END IONS")
            cat(
              result,
              file = file.path(path, "spectra_pos.mgf"),
              append = TRUE,
              sep = "\n"
            )
            # writeLines(text = result, con = fileConn)
          }
        )
      }
    )
    sink()
    sink(NULL)
    cat(crayon::green("Done.\n"))
  }
  
  ###negative mode
  if (length(spectra_neg) > 0) {
    cat(crayon::yellow("Write negative mode...\n"))
    unlink(
      x = file.path(path, "spectra_neg.mgf"),
      recursive = TRUE,
      force = TRUE
    )
    sink(file = file.path(path, "spectra_neg.mgf"),
         append = TRUE)
    purrr::walk2(
      .x = names(spectra_neg),
      .y = spectra_neg,
      .f = function(compound_id, spectra) {
        message(compound_id)
        purrr::walk2(
          .x = names(spectra),
          .y = spectra,
          .f = function(ce, single_spectra) {
            temp_spectra_info =
              spectra.info %>%
              dplyr::filter(Lab.ID == compound_id)
            result =
              c(
                paste("Name:", temp_spectra_info$Compound.name),
                paste("Synon:", temp_spectra_info$Synon),
                paste("DB#:", temp_spectra_info$mona.ID),
                paste("InChIKey:", temp_spectra_info$InChIKey),
                paste("InChI:", temp_spectra_info$InChI),
                paste("SMILES:", temp_spectra_info$SMILES),
                paste("Precursor_type:", temp_spectra_info$Precursor_type),
                paste("Spectrum_type:", "MS2"),
                paste("PrecursorMZ:", temp_spectra_info$PrecursorMZ),
                paste(
                  "Instrument_type:",
                  temp_spectra_info$Instrument.type
                ),
                paste("Instrument:", temp_spectra_info$Instrument),
                paste("Ion_mode:", "P"),
                paste("Collision_energy:", ce),
                paste("Formula:", temp_spectra_info$Formula),
                paste("MW:", temp_spectra_info$MW),
                paste("ExactMass:", temp_spectra_info$mz),
                paste("Comments:", temp_spectra_info$Comments),
                paste("Num Peaks:", nrow(single_spectra))
              )
            single_spectra2 =
              single_spectra %>%
              apply(1, function(x) {
                paste(x, collapse = " ")
              })
            result =
              c("BEGIN IONS", result, single_spectra2, "END IONS")
            cat(
              result,
              file = file.path(path, "spectra_neg.mgf"),
              append = TRUE,
              sep = "\n"
            )
            # writeLines(text = result, con = fileConn)
          }
        )
      }
    )
    sink()
    sink(NULL)
    cat(crayon::green("Done.\n"))
  }
}



################################################################################
############################    MassBank   #####################################
################################################################################


##------------------------------------------------------------------------------
#' @title Export metID database to msp (MassBank format)
#' @description Export metID database to msp (MassBank format).
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param databasae metID database.
#' @param path Work directory.
#' @importFrom magrittr %>%
#' @importFrom crayon red yellow green bgRed
#' @importFrom stringr str_detect str_extract
#' @importFrom readr cols
#' @importFrom pbapply pblapply
#' @return msp format files in local folder.
#' @export

# sxtTools::setwd_project()
# load("other_files/all_ms2_database/mike_in_house/msDatabase_hilic0.0.2")
# database = msDatabase_hilic0.0.2
# setwd("other_files/all_ms2_database/mike_in_house")
# write_msp_massbank(databasae = databasae)
# x = read_msp_mona(file = "spectra_pos.msp")

write_msp_massbank = function(databasae,
                              path = ".") {
  options(warn = -1)
  spectra.info = database@spectra.info
  spectra_pos = database@spectra.data$Spectra.positive
  spectra_neg = database@spectra.data$Spectra.negative
  
  if (length(spectra_pos) == 0 &
      length(spectra_neg) == 0) {
    cat("No MS2 spectra.")
  }
  
  
  ####positive mode
  if (length(spectra_pos) > 0) {
    cat(crayon::yellow("Write positive mode...\n"))
    unlink(
      x = file.path(path, "spectra_pos.msp"),
      recursive = TRUE,
      force = TRUE
    )
    sink(file = file.path(path, "spectra_pos.msp"),
         append = TRUE)
    purrr::walk2(
      .x = names(spectra_pos),
      .y = spectra_pos,
      .f = function(compound_id, spectra) {
        message(compound_id)
        purrr::walk2(
          .x = names(spectra),
          .y = spectra,
          .f = function(ce, single_spectra) {
            temp_spectra_info =
              spectra.info %>%
              dplyr::filter(Lab.ID == compound_id)
            result =
              c(
                paste("Name:", temp_spectra_info$Compound.name),
                paste("Synon:", temp_spectra_info$Synon),
                paste("DB#:", temp_spectra_info$massbank.ID),
                paste("InChIKey:", temp_spectra_info$InChIKey),
                paste("InChI:", temp_spectra_info$InChI),
                paste("SMILES:", temp_spectra_info$SMILES),
                paste("Precursor_type:", temp_spectra_info$Precursor_type),
                paste("Spectrum_type:", "MS2"),
                paste("PrecursorMZ:", temp_spectra_info$PrecursorMZ),
                paste(
                  "Instrument_type:",
                  temp_spectra_info$Instrument.type
                ),
                paste("Instrument:", temp_spectra_info$Instrument),
                paste("Ion_mode:", "POSITIVE"),
                paste("Collision_energy:", ce),
                paste("Formula:", temp_spectra_info$Formula),
                paste("MW:", temp_spectra_info$MW),
                paste("ExactMass:", temp_spectra_info$mz),
                paste("Comments:", temp_spectra_info$Comments),
                paste("Splash:", temp_spectra_info$Splash),
                paste("Num Peaks:", nrow(single_spectra))
              )
            single_spectra2 =
              single_spectra %>%
              apply(1, function(x) {
                paste(x, collapse = " ")
              })
            result =
              c(result, single_spectra2, "")
            cat(
              result,
              file = file.path(path, "spectra_pos.msp"),
              append = TRUE,
              sep = "\n"
            )
            # writeLines(text = result, con = fileConn)
          }
        )
      }
    )
    sink()
    sink(NULL)
    cat(crayon::green("Done.\n"))
  }
  
  ###negative mode
  if (length(spectra_neg) > 0) {
    cat(crayon::yellow("Write negative mode...\n"))
    unlink(
      x = file.path(path, "spectra_neg.msp"),
      recursive = TRUE,
      force = TRUE
    )
    sink(file = file.path(path, "spectra_neg.msp"),
         append = TRUE)
    purrr::walk2(
      .x = names(spectra_neg),
      .y = spectra_neg,
      .f = function(compound_id, spectra) {
        message(compound_id)
        purrr::walk2(
          .x = names(spectra),
          .y = spectra,
          .f = function(ce, single_spectra) {
            temp_spectra_info =
              spectra.info %>%
              dplyr::filter(Lab.ID == compound_id)
            result =
              c(
                paste("Name:", temp_spectra_info$Compound.name),
                paste("Synon:", temp_spectra_info$Synon),
                paste("DB#:", temp_spectra_info$massbank.ID),
                paste("InChIKey:", temp_spectra_info$InChIKey),
                paste("InChI:", temp_spectra_info$InChI),
                paste("SMILES:", temp_spectra_info$SMILES),
                paste("Precursor_type:", temp_spectra_info$Precursor_type),
                paste("Spectrum_type:", "MS2"),
                paste("PrecursorMZ:", temp_spectra_info$PrecursorMZ),
                paste(
                  "Instrument_type:",
                  temp_spectra_info$Instrument.type
                ),
                paste("Instrument:", temp_spectra_info$Instrument),
                paste("Ion_mode:", "NEGATIVE"),
                paste("Collision_energy:", ce),
                paste("Formula:", temp_spectra_info$Formula),
                paste("MW:", temp_spectra_info$MW),
                paste("ExactMass:", temp_spectra_info$mz),
                paste("Comments:", temp_spectra_info$Comments),
                paste("Splash:", temp_spectra_info$Splash),
                paste("Num Peaks:", nrow(single_spectra))
              )
            single_spectra2 =
              single_spectra %>%
              apply(1, function(x) {
                paste(x, collapse = " ")
              })
            result =
              c(result, single_spectra2, "")
            cat(
              result,
              file = file.path(path, "spectra_neg.msp"),
              append = TRUE,
              sep = "\n"
            )
            # writeLines(text = result, con = fileConn)
          }
        )
      }
    )
    sink()
    sink(NULL)
    cat(crayon::green("Done.\n"))
  }
}


##------------------------------------------------------------------------------
#' @title Export metID database to mgf (MassBank format)
#' @description Export metID database to mgf (MassBank format).
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param databasae metID database.
#' @param path Work directory.
#' @importFrom magrittr %>%
#' @importFrom crayon red yellow green bgRed
#' @importFrom stringr str_detect str_extract
#' @importFrom readr cols
#' @importFrom pbapply pblapply
#' @return mgf format files in local folder.
#' @export


# sxtTools::setwd_project()
# load("other_files/all_ms2_database/mike_in_house/msDatabase_hilic0.0.2")
# database = msDatabase_hilic0.0.2
# setwd("other_files/all_ms2_database/mike_in_house")
# write_mgf_massbank(databasae = databasae)
# x = read_mgf_mona(file = "spectra_neg.mgf")

write_mgf_massbank = function(databasae,
                              path = ".") {
  options(warn = -1)
  spectra.info = database@spectra.info
  spectra_pos = database@spectra.data$Spectra.positive
  spectra_neg = database@spectra.data$Spectra.negative
  
  if (length(spectra_pos) == 0 &
      length(spectra_neg) == 0) {
    cat("No MS2 spectra.")
  }
  
  
  ####positive mode
  if (length(spectra_pos) > 0) {
    cat(crayon::yellow("Write positive mode...\n"))
    unlink(
      x = file.path(path, "spectra_pos.mgf"),
      recursive = TRUE,
      force = TRUE
    )
    sink(file = file.path(path, "spectra_pos.mgf"),
         append = TRUE)
    purrr::walk2(
      .x = names(spectra_pos),
      .y = spectra_pos,
      .f = function(compound_id, spectra) {
        message(compound_id)
        purrr::walk2(
          .x = names(spectra),
          .y = spectra,
          .f = function(ce, single_spectra) {
            temp_spectra_info =
              spectra.info %>%
              dplyr::filter(Lab.ID == compound_id)
            result =
              c(
                paste("Name:", temp_spectra_info$Compound.name),
                paste("Synon:", temp_spectra_info$Synon),
                paste("DB#:", temp_spectra_info$massbank.ID),
                paste("InChIKey:", temp_spectra_info$InChIKey),
                paste("InChI:", temp_spectra_info$InChI),
                paste("SMILES:", temp_spectra_info$SMILES),
                paste("Precursor_type:", temp_spectra_info$Precursor_type),
                paste("Spectrum_type:", "MS2"),
                paste("PrecursorMZ:", temp_spectra_info$PrecursorMZ),
                paste(
                  "Instrument_type:",
                  temp_spectra_info$Instrument.type
                ),
                paste("Instrument:", temp_spectra_info$Instrument),
                paste("Ion_mode:", "POSITIVE"),
                paste("Collision_energy:", ce),
                paste("Formula:", temp_spectra_info$Formula),
                paste("MW:", temp_spectra_info$MW),
                paste("ExactMass:", temp_spectra_info$mz),
                paste("Comments:", temp_spectra_info$Comments),
                paste("Splash:", temp_spectra_info$Splash),
                paste("Num Peaks:", nrow(single_spectra))
              )
            single_spectra2 =
              single_spectra %>%
              apply(1, function(x) {
                paste(x, collapse = " ")
              })
            result =
              c("BEGIN IONS", result, single_spectra2, "END IONS")
            cat(
              result,
              file = file.path(path, "spectra_pos.mgf"),
              append = TRUE,
              sep = "\n"
            )
            # writeLines(text = result, con = fileConn)
          }
        )
      }
    )
    sink()
    sink(NULL)
    cat(crayon::green("Done.\n"))
  }
  
  ###negative mode
  if (length(spectra_neg) > 0) {
    cat(crayon::yellow("Write negative mode...\n"))
    unlink(
      x = file.path(path, "spectra_neg.mgf"),
      recursive = TRUE,
      force = TRUE
    )
    sink(file = file.path(path, "spectra_neg.mgf"),
         append = TRUE)
    purrr::walk2(
      .x = names(spectra_neg),
      .y = spectra_neg,
      .f = function(compound_id, spectra) {
        message(compound_id)
        purrr::walk2(
          .x = names(spectra),
          .y = spectra,
          .f = function(ce, single_spectra) {
            temp_spectra_info =
              spectra.info %>%
              dplyr::filter(Lab.ID == compound_id)
            result =
              c(
                paste("Name:", temp_spectra_info$Compound.name),
                paste("Synon:", temp_spectra_info$Synon),
                paste("DB#:", temp_spectra_info$massbank.ID),
                paste("InChIKey:", temp_spectra_info$InChIKey),
                paste("InChI:", temp_spectra_info$InChI),
                paste("SMILES:", temp_spectra_info$SMILES),
                paste("Precursor_type:", temp_spectra_info$Precursor_type),
                paste("Spectrum_type:", "MS2"),
                paste("PrecursorMZ:", temp_spectra_info$PrecursorMZ),
                paste(
                  "Instrument_type:",
                  temp_spectra_info$Instrument.type
                ),
                paste("Instrument:", temp_spectra_info$Instrument),
                paste("Ion_mode:", "NEGATIVE"),
                paste("Collision_energy:", ce),
                paste("Formula:", temp_spectra_info$Formula),
                paste("MW:", temp_spectra_info$MW),
                paste("ExactMass:", temp_spectra_info$mz),
                paste("Comments:", temp_spectra_info$Comments),
                paste("Splash:", temp_spectra_info$Splash),
                paste("Num Peaks:", nrow(single_spectra))
              )
            single_spectra2 =
              single_spectra %>%
              apply(1, function(x) {
                paste(x, collapse = " ")
              })
            result =
              c("BEGIN IONS", result, single_spectra2, "END IONS")
            cat(
              result,
              file = file.path(path, "spectra_neg.mgf"),
              append = TRUE,
              sep = "\n"
            )
            # writeLines(text = result, con = fileConn)
          }
        )
      }
    )
    sink()
    sink(NULL)
    cat(crayon::green("Done.\n"))
  }
}





################################################################################
############################    GNPS      #####################################
################################################################################


##------------------------------------------------------------------------------
#' @title Export metID database to msp (gnps format)
#' @description Export metID database to msp (gnps format).
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param databasae metID database.
#' @param path Work directory.
#' @importFrom magrittr %>%
#' @importFrom crayon red yellow green bgRed
#' @importFrom stringr str_detect str_extract
#' @importFrom readr cols
#' @importFrom pbapply pblapply
#' @return msp format files in local folder.
#' @export

# sxtTools::setwd_project()
# load("other_files/all_ms2_database/mike_in_house/msDatabase_hilic0.0.2")
# database = msDatabase_hilic0.0.2
# setwd("other_files/all_ms2_database/mike_in_house")
# write_msp_gnps(databasae = databasae)
# x = read_msp_gnps(file = "spectra_neg.msp")

write_msp_gnps = function(databasae,
                          path = ".") {
  options(warn = -1)
  spectra.info = database@spectra.info
  spectra_pos = database@spectra.data$Spectra.positive
  spectra_neg = database@spectra.data$Spectra.negative
  
  if (length(spectra_pos) == 0 &
      length(spectra_neg) == 0) {
    cat("No MS2 spectra.")
  }
  
  ####positive mode
  if (length(spectra_pos) > 0) {
    cat(crayon::yellow("Write positive mode...\n"))
    unlink(
      x = file.path(path, "spectra_pos.msp"),
      recursive = TRUE,
      force = TRUE
    )
    sink(file = file.path(path, "spectra_pos.msp"),
         append = TRUE)
    purrr::walk2(
      .x = names(spectra_pos),
      .y = spectra_pos,
      .f = function(compound_id, spectra) {
        message(compound_id)
        purrr::walk2(
          .x = names(spectra),
          .y = spectra,
          .f = function(ce, single_spectra) {
            temp_spectra_info =
              spectra.info %>%
              dplyr::filter(Lab.ID == compound_id)
            result =
              c(
                paste("NAME:", temp_spectra_info$Compound.name),
                paste("PRECURSORMZ:", temp_spectra_info$PRECURSORMZ),
                paste("PRECURSORTYPE:", temp_spectra_info$PRECURSORTYPE),
                paste("FORMULA:", temp_spectra_info$Formula),
                paste("Ontology:", temp_spectra_info$Ontology),
                paste("INCHIKEY:", temp_spectra_info$InChIKey),
                paste("SMILES:", temp_spectra_info$SMILES),
                paste("RETENTIONTIME: CCS:", temp_spectra_info$RETENTIONTIME),
                paste("IONMODE:", "Positive"),
                paste(
                  "INSTRUMENTTYPE:",
                  temp_spectra_info$Instrument.type
                ),
                paste("INSTRUMENT:", temp_spectra_info$Instrument),
                paste("COLLISIONENERGY:", ce),
                paste("Comment:", temp_spectra_info$Comment),
                paste("Num Peaks:", nrow(single_spectra))
              )
            single_spectra2 =
              single_spectra %>%
              apply(1, function(x) {
                paste(x, collapse = "\t")
              })
            result =
              c(result, single_spectra2, "", "")
            cat(
              result,
              file = file.path(path, "spectra_pos.msp"),
              append = TRUE,
              sep = "\n"
            )
            # writeLines(text = result, con = fileConn)
          }
        )
      }
    )
    sink()
    sink(NULL)
    cat(crayon::green("Done.\n"))
  }
  
  ###negative mode
  if (length(spectra_neg) > 0) {
    cat(crayon::yellow("Write negative mode...\n"))
    unlink(
      x = file.path(path, "spectra_neg.msp"),
      recursive = TRUE,
      force = TRUE
    )
    sink(file = file.path(path, "spectra_neg.msp"),
         append = TRUE)
    purrr::walk2(
      .x = names(spectra_neg),
      .y = spectra_neg,
      .f = function(compound_id, spectra) {
        message(compound_id)
        purrr::walk2(
          .x = names(spectra),
          .y = spectra,
          .f = function(ce, single_spectra) {
            temp_spectra_info =
              spectra.info %>%
              dplyr::filter(Lab.ID == compound_id)
            result =
              c(
                paste("NAME:", temp_spectra_info$Compound.name),
                paste("PRECURSORMZ:", temp_spectra_info$PRECURSORMZ),
                paste("PRECURSORTYPE:", temp_spectra_info$PRECURSORTYPE),
                paste("FORMULA:", temp_spectra_info$Formula),
                paste("Ontology:", temp_spectra_info$Ontology),
                paste("INCHIKEY:", temp_spectra_info$InChIKey),
                paste("SMILES:", temp_spectra_info$SMILES),
                paste("RETENTIONTIME: CCS:", temp_spectra_info$RETENTIONTIME),
                paste("IONMODE:", "Positive"),
                paste(
                  "INSTRUMENTTYPE:",
                  temp_spectra_info$Instrument.type
                ),
                paste("INSTRUMENT:", temp_spectra_info$Instrument),
                paste("COLLISIONENERGY:", ce),
                paste("Comment:", temp_spectra_info$Comment),
                paste("Num Peaks:", nrow(single_spectra))
              )
            single_spectra2 =
              single_spectra %>%
              apply(1, function(x) {
                paste(x, collapse = "\t")
              })
            result =
              c(result, single_spectra2, "", "")
            cat(
              result,
              file = file.path(path, "spectra_neg.msp"),
              append = TRUE,
              sep = "\n"
            )
            # writeLines(text = result, con = fileConn)
          }
        )
      }
    )
    sink()
    sink(NULL)
    cat(crayon::green("Done.\n"))
  }
}


##------------------------------------------------------------------------------
#' @title Export metID database to mgf (gnps format)
#' @description Export metID database to mgf (gnps format).
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param databasae metID database.
#' @param path Work directory.
#' @importFrom magrittr %>%
#' @importFrom crayon red yellow green bgRed
#' @importFrom stringr str_detect str_extract
#' @importFrom readr cols
#' @importFrom pbapply pblapply
#' @return mgf format files in local folder.
#' @export

# sxtTools::setwd_project()
# load("other_files/all_ms2_database/mike_in_house/msDatabase_hilic0.0.2")
# database = msDatabase_hilic0.0.2
# setwd("other_files/all_ms2_database/mike_in_house")
# write_mgf_gnps(databasae = databasae)
# x = read_mgf_gnps(file = "spectra_pos.mgf")

write_mgf_gnps = function(databasae,
                          path = ".") {
  options(warn = -1)
  spectra.info = database@spectra.info
  spectra_pos = database@spectra.data$Spectra.positive
  spectra_neg = database@spectra.data$Spectra.negative
  
  if (length(spectra_pos) == 0 &
      length(spectra_neg) == 0) {
    cat("No MS2 spectra.")
  }
  
  
  ####positive mode
  if (length(spectra_pos) > 0) {
    cat(crayon::yellow("Write positive mode...\n"))
    unlink(
      x = file.path(path, "spectra_pos.mgf"),
      recursive = TRUE,
      force = TRUE
    )
    sink(file = file.path(path, "spectra_pos.mgf"),
         append = TRUE)
    purrr::walk2(
      .x = names(spectra_pos),
      .y = spectra_pos,
      .f = function(compound_id, spectra) {
        message(compound_id)
        purrr::walk2(
          .x = names(spectra),
          .y = spectra,
          .f = function(ce, single_spectra) {
            temp_spectra_info =
              spectra.info %>%
              dplyr::filter(Lab.ID == compound_id)
            result =
              result =
              c(
                paste("NAME:", temp_spectra_info$Compound.name),
                paste("PRECURSORMZ:", temp_spectra_info$PRECURSORMZ),
                paste("PRECURSORTYPE:", temp_spectra_info$PRECURSORTYPE),
                paste("FORMULA:", temp_spectra_info$Formula),
                paste("Ontology:", temp_spectra_info$Ontology),
                paste("INCHIKEY:", temp_spectra_info$InChIKey),
                paste("SMILES:", temp_spectra_info$SMILES),
                paste("RETENTIONTIME: CCS:", temp_spectra_info$RETENTIONTIME),
                paste("IONMODE:", "Positive"),
                paste(
                  "INSTRUMENTTYPE:",
                  temp_spectra_info$Instrument.type
                ),
                paste("INSTRUMENT:", temp_spectra_info$Instrument),
                paste("COLLISIONENERGY:", ce),
                paste("Comment:", temp_spectra_info$Comment),
                paste("Num Peaks:", nrow(single_spectra))
              )
            single_spectra2 =
              single_spectra %>%
              apply(1, function(x) {
                paste(x, collapse = "\t")
              })
            
            result =
              c("BEGIN IONS", result, single_spectra2, "END IONS", "", "")
            cat(
              result,
              file = file.path(path, "spectra_pos.mgf"),
              append = TRUE,
              sep = "\n"
            )
            # writeLines(text = result, con = fileConn)
          }
        )
      }
    )
    sink()
    sink(NULL)
    cat(crayon::green("Done.\n"))
  }
  
  ###negative mode
  if (length(spectra_neg) > 0) {
    cat(crayon::yellow("Write negative mode...\n"))
    unlink(
      x = file.path(path, "spectra_neg.mgf"),
      recursive = TRUE,
      force = TRUE
    )
    sink(file = file.path(path, "spectra_neg.mgf"),
         append = TRUE)
    purrr::walk2(
      .x = names(spectra_neg),
      .y = spectra_neg,
      .f = function(compound_id, spectra) {
        message(compound_id)
        purrr::walk2(
          .x = names(spectra),
          .y = spectra,
          .f = function(ce, single_spectra) {
            temp_spectra_info =
              spectra.info %>%
              dplyr::filter(Lab.ID == compound_id)
            result =
              result =
              c(
                paste("NAME:", temp_spectra_info$Compound.name),
                paste("PRECURSORMZ:", temp_spectra_info$PRECURSORMZ),
                paste("PRECURSORTYPE:", temp_spectra_info$PRECURSORTYPE),
                paste("FORMULA:", temp_spectra_info$Formula),
                paste("Ontology:", temp_spectra_info$Ontology),
                paste("INCHIKEY:", temp_spectra_info$InChIKey),
                paste("SMILES:", temp_spectra_info$SMILES),
                paste("RETENTIONTIME: CCS:", temp_spectra_info$RETENTIONTIME),
                paste("IONMODE:", "Positive"),
                paste(
                  "INSTRUMENTTYPE:",
                  temp_spectra_info$Instrument.type
                ),
                paste("INSTRUMENT:", temp_spectra_info$Instrument),
                paste("COLLISIONENERGY:", ce),
                paste("Comment:", temp_spectra_info$Comment),
                paste("Num Peaks:", nrow(single_spectra))
              )
            single_spectra2 =
              single_spectra %>%
              apply(1, function(x) {
                paste(x, collapse = "\t")
              })
            
            result =
              c("BEGIN IONS", result, single_spectra2, "END IONS", "", "")
            cat(
              result,
              file = file.path(path, "spectra_neg.mgf"),
              append = TRUE,
              sep = "\n"
            )
            # writeLines(text = result, con = fileConn)
          }
        )
      }
    )
    sink()
    sink(NULL)
    cat(crayon::green("Done.\n"))
  }
}









