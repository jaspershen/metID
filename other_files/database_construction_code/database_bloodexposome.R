# tinyTools::setwd_project()
# setwd('all_ms2_database/blood_exposome/')
# library(dplyr)
# library(ggplot2)
# library(XML)
# library(MetaDBparse)
# rm(list = ls())
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
