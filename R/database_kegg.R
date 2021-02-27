# tinyTools::setwd_project()
# setwd('all_ms2_database/kegg/')
# library(dplyr)
# library(ggplot2)
# library(XML)
# library(MetaDBparse)
# rm(list = ls())
# 
# ###to get KEGG database
# library(KEGGgraph)
# library(KEGGREST)
# library(KEGGlincs)
# library(tidyverse)
# 
# # 
# # compound_ID <- 
# #   keggList(database = "compound") %>% 
# #   names() %>% 
# #   unique() %>% 
# #   stringr::str_replace_all(., "cpd:", "")
# # 
# # kegg_compound_database <- 
# #   pbapply::pblapply(compound_ID, function(x){
# #     KEGGREST::keggGet(dbentries = x)[[1]]    
# #   })
# # 
# # save(kegg_compound_database, file = "kegg_compound_database")
# 
# load("kegg_compound_database")
# 
# kegg = 
# kegg_compound_database %>%
#   purrr::map(function(x) {
#     cat(x$ENTRY)
#     KEGG.ID = x$ENTRY
#     Compound.name = paste(x$NAME, collapse = "{}")
#     Formula = x$FORMULA
#     if(is.null(x$FORMULA)){
#       Formula = NA  
#     }
#     mz = as.numeric(x$EXACT_MASS)
#     if(is.null(x$EXACT_MASS)){
#       mz = NA
#     }
#     CAS.ID = stringr::str_replace(grep("CAS", x$DBLINKS, value = TRUE), "CAS: ", "") %>%
#       stringr::str_trim(side = "both")
#     PubChem.ID = stringr::str_replace(grep("PubChem", x$DBLINKS, value = TRUE), "PubChem: ", "") %>%
#       stringr::str_trim(side = "both")
#     
#     if(length(CAS.ID) == 0){
#       CAS.ID = NA
#     }
#     
#     if(length(PubChem.ID) == 0){
#       PubChem.ID = NA
#     }
#     
#     data.frame(Lab.ID = KEGG.ID, 
#                Compound.name,
#                Formula, 
#                mz, 
#                CAS.ID,
#                HMDB.ID = NA,
#                KEGG.ID, 
#                PubChem.ID)
#   }) %>% 
#   do.call(rbind, .) %>% 
#   as.data.frame()
# 
# kegg = 
# kegg %>% 
#   dplyr::filter(!is.na(mz)) %>% 
#   dplyr::mutate(synonym = Compound.name) %>% 
#   dplyr::mutate(
#     RT = NA,
#     mz.pos = NA,
#     mz.neg = NA,
#     Submitter = "KEGG"
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
# kegg$Compound.name = 
#   kegg$Compound.name %>% 
#   stringr::str_split(pattern = "\\{\\}") %>% 
#   purrr::map(function(x){
#     x[1]
#   }) %>% 
#   unlist() %>% 
#   stringr::str_replace(pattern = ";", "")
# 
# openxlsx::write.xlsx(kegg, file = "kegg.xlsx", asTable = TRUE)
# 
# keggMS1Database_1.0 =
#   construct_database(
#     path = ".",
#     version = "1.0",
#     metabolite.info.name = "kegg.xlsx",
#     source = "KEGG",
#     link = "https://www.genome.jp/kegg/compound/",
#     creater = "Xiaotao Shen",
#     email = "shenxt@stanford.edu",
#     rt = FALSE,
#     threads = 3
#   )
# 
# save(keggMS1Database_1.0, file = "keggMS1Database_1.0")
# 
# 
