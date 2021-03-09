# tinyTools::setwd_project()
# setwd('all_ms2_database/Fiehn/')
# library(dplyr)
# library(ggplot2)
# 
# # rm(list = ls())
# 
# load("../mike_in_house/msDatabase_rplc0.0.2")
# 
# pos = read_msp_database("MSMS-Pos-FiehnHILIC.msp", threads = 5)
# neg = read_msp_database("MSMS-Neg-FiehnHILIC.msp", threads = 5)
# 
# pos_compound_info =
#   purrr::map(pos, function(x){
#     x[[1]]
#   }) %>%
#   do.call(rbind, .) %>%
#   as.data.frame()
# 
# neg_compound_info =
#   purrr::map(neg, function(x){
#     x[[1]]
#   }) %>%
#   do.call(rbind, .) %>%
#   as.data.frame()
# 
# pos_compound_info$Lab.ID =
#   paste("Fiehn_HILIC_POS", 1:nrow(pos_compound_info), sep = "_")
# 
# neg_compound_info$Lab.ID =
#   paste("Fiehn_HILIC_NEG", 1:nrow(neg_compound_info), sep = "_")
# 
# 
# Spectra.positive = purrr::map2(.x = pos, .y = pos_compound_info$Lab.ID,
#                                function(x, y) {
#                                  spec = x[[2]]
#                                  spec = list(spec)
#                                  names(spec) = x$info["COLLISIONENERGY"]
#                                  spec
#                                })
# 
# names(Spectra.positive) = pos_compound_info$Lab.ID
# 
# Spectra.negative = purrr::map2(.x = neg, .y = neg_compound_info$Lab.ID,
#                                function(x, y) {
#                                  spec = x[[2]]
#                                  spec = list(spec)
#                                  names(spec) =x$info["COLLISIONENERGY"]
#                                  spec
#                                })
# 
# names(Spectra.negative) = neg_compound_info$Lab.ID
# 
# 
# spectra.data = list(Spectra.positive,
#                     Spectra.negative)
# 
# names(spectra.data) = c("Spectra.positive", "Spectra.negative")
# 
# spectra.info = rbind(pos_compound_info, neg_compound_info)
# 
# spectra.info =
#   spectra.info %>%
#   dplyr::mutate(
#     CAS.ID = NA,
#     HMDB.ID = NA,
#     KEGG.ID = NA,
#     mz.pos = NA,
#     mz.neg = NA,
#     Submitter = "Fiehn_lab"
#   ) %>%
#   dplyr::rename(Compound.name = NAME,
#                 RT = rt,
#                 Formula = FORMULA) %>%
#   dplyr::select(
#     Lab.ID,
#     Compound.name,
#     mz,
#     RT,
#     CAS.ID,
#     HMDB.ID,
#     KEGG.ID,
#     Formula,
#     mz.pos,
#     mz.neg,
#     Submitter,
#     dplyr::everything()
#   )
# 
# spectra.info$RT = NA
# 
# database.info = msDatabase_rplc0.0.2@database.info
# 
# database.info$Version = "0.0.1"
# database.info$Source = "Fiehn Lab"
# database.info$Link = "https://fiehnlab.ucdavis.edu/projects/fiehnlib"
# database.info$Creater = "Xiaotao Shen"
# database.info$RT = FALSE
# 
# fiehn_hilic_database0.0.1 = msDatabase_rplc0.0.2
# fiehn_hilic_database0.0.1@database.info = database.info
# fiehn_hilic_database0.0.1@spectra.info = spectra.info
# fiehn_hilic_database0.0.1@spectra.data = spectra.data
# 
# fiehn_hilic_database0.0.1
# save(fiehn_hilic_database0.0.1, file = "fiehn_hilic_database0.0.1")
# # 
# # 
