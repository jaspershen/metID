# tinyTools::setwd_project()
# setwd('all_ms2_database/t3db/')
# library(dplyr)
# library(ggplot2)
# library(XML)
# library(MetaDBparse)
# rm(list = ls())
# 
# t3db =
#   readr::read_csv("toxins.csv")
# 
# t3db =
#   t3db %>%
#   dplyr::filter(Class == "SmallMolecule") %>% 
#   dplyr::rename(
#     Lab.ID = `T3DB ID`,
#     PubChem.CID = `PubChem Compound ID`,
#     Compound.name = Name,
#     HMDB.ID = `HMDB ID`,
#     CAS.ID = `CAS Number`,
#     Formula = `Chemical Formula`,
#     mz = `Monoisotopic Mass`,
#   ) %>%
#   dplyr::mutate(
#     T3DB.ID = Lab.ID,
#     KEGG.ID = NA,
#     RT = NA,
#     mz.pos = NA,
#     mz.neg = NA,
#     Submitter = "T3DB"
#   ) %>%
#   dplyr::select(Lab.ID,
#                 Compound.name,
#                 mz,
#                 RT,
#                 CAS.ID,
#                 HMDB.ID,
#                 KEGG.ID,
#                 Formula,
#                 mz.pos,
#                 mz.neg,
#                 Submitter,
#                 everything())
# 
# 
# openxlsx::write.xlsx(t3db, file = "t3db.xlsx", asTable = TRUE)
# 
# t3dbMS1Database_1.0 =
#   construct_database(
#     path = ".",
#     version = "1.0",
#     metabolite.info.name = "t3db.xlsx",
#     source = "T3DB",
#     link = "http://www.t3db.ca/",
#     creater = "Xiaotao Shen",
#     email = "shenxt@stanford.edu",
#     rt = FALSE,
#     threads = 3
#   )
# 
# save(t3dbMS1Database_1.0, file = "t3dbMS1Database_1.0")
# 
# 
