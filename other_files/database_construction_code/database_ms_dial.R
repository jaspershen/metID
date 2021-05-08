# tinyTools::setwd_project()
# setwd('all_ms2_database/ms_dial/')
# library(dplyr)
# library(ggplot2)
# 
# rm(list = ls())
# 
# load("../mike_in_house/msDatabase_rplc0.0.2")
# 
# pos = read_msp_database(file = "MSMS-Public-Pos-VS15.msp", threads = 5)
# neg = read_msp_database("MSMS-Neg-FiehnHILIC.msp")
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
# library(plyr)
# pos_compound_info2 =
# pos_compound_info %>%
#   dlply(.variables = .(mz, NAME, PRECURSORTYPE, FORMULA, Ontology, INCHIKEY, SMILES,
#                        IONMODE, INSTRUMENTTYPE, INSTRUMENT, Comment, `Num Peaks`))
# 
# which(unlist(lapply(pos_compound_info2, nrow)) > 1)
# 
# neg_compound_info2 =
#   neg_compound_info %>%
#   dlply(.variables = .(mz, NAME, PRECURSORTYPE, FORMULA, Ontology, INCHIKEY, SMILES,
#                        IONMODE, INSTRUMENTTYPE, INSTRUMENT, Comment, `Num Peaks`))
# 
# which(unlist(lapply(neg_compound_info2, nrow)) > 1)
# 
# unique_name = intersect(unique(pos_compound_info$NAME),
#                         unique(neg_compound_info$NAME))
# 
# 
# 
# # Lab.ID =
# # data.frame(NAME = unique_name,
# #            Lab.ID = paste("Fiehn_HILIC", 1:length(unique_name), sep = "_"))
# #
# # Lab.ID %>%
# #   dplyr::left_join(pos_compound_info, by = 'NAME')
# 
# 
# 
# length(pos_compound_info$NAME)
# length(unique(pos_compound_info$NAME))
# length(neg_compound_info$NAME)
# length(unique(neg_compound_info$NAME))
# 
# 
# 
# 
# # BloodExpsomeDatabase_version_1.0 =
# #   readxl::read_xlsx("BloodExpsomeDatabase_version_1.0.xlsx")
# #
# # BloodExpsomeDatabase_version_1.0 =
# # BloodExpsomeDatabase_version_1.0 %>%
# #   dplyr::select(1:13) %>%
# #   dplyr::rename(
# #     PubChem.CID = `PubChem CID`,
# #     Compound.name = `Compound Name`,
# #     KEGG.ID = `KEGG ID`,
# #     HMDB.ID = `HMDB ID`,
# #     Formula = `Molecular Formula`,
# #     SMILES = `CanonicalSMILES`,
# #     Multi.component = `Multi component`,
# #     mz = ExactMass,
# #     DataBase.count = `DataBase Count`
# #   ) %>%
# #   dplyr::mutate(
# #     RT = NA,
# #     CAS.ID = NA,
# #     mz.pos = NA,
# #     mz.neg = NA,
# #     Submitter = "Blood exposome"
# #   ) %>%
# #   dplyr::select(mz,
# #                 RT,
# #                 CAS.ID,
# #                 HMDB.ID,
# #                 KEGG.ID,
# #                 Formula,
# #                 mz.pos,
# #                 mz.neg,
# #                 Submitter,
# #                 everything())
# #
# # BloodExpsomeDatabase_version_1.0$Lab.ID =
# #   paste("Blood_exposome", 1:nrow(BloodExpsomeDatabase_version_1.0), sep = "_")
# #
# # BloodExpsomeDatabase_version_1.0 =
# # BloodExpsomeDatabase_version_1.0 %>%
# #   dplyr::select(Lab.ID, everything())
# #
# # openxlsx::write.xlsx(BloodExpsomeDatabase_version_1.0, file = "BloodExpsomeDatabase_version_1.0.xlsx", asTable = TRUE)
# 
# bloodExposomeMS1Database_1.0 =
#   construct_database(
#     path = ".",
#     version = "1.0",
#     metabolite.info.name = "BloodExpsomeDatabase_version_1.0.xlsx",
#     source = "BloodExpsome",
#     link = "https://bloodexposome.org/#/dashboard",
#     creater = "Xiaotao Shen",
#     email = "shenxt@stanford.edu",
#     rt = FALSE,
#     threads = 3
#   )
# 
# 
# save(bloodExposomeMS1Database_1.0, file = "bloodExposomeMS1Database_1.0")
# 
# 
