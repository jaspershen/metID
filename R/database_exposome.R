# tinyTools::setwd_project()
# setwd('all_ms2_database/SAMPLE-zhaojiahui/')
# library(dplyr)
# library(ggplot2)
# library(XML)
# library(MetaDBparse)
# rm(list = ls())
# 
# load("msDatabase6.6.6")
# 
# 
# msDatabase6.6.6@spectra.data
# 
# exposureDatabase = 
#   msDatabase6.6.6
# 
# 
# exposureDatabase@database.info$Source = "South China Agricultural University"
# 
# save(exposureDatabase, file = "exposureDatabase")
# 
# 
# tinyTools::ms2_plot(exposureDatabase@spectra.data$Spectra.positive$CMP_6$NCE10,
#                     exposureDatabase@spectra.data$Spectra.positive$CMP_6$NCE20)
# 
# result =
#   metID::identify_metabolites(
#     ms1.data = "ms1.peak.table.csv",
#     ms2.data = dir(".", "mgf"),
#     database = "exposureDatabase"
#   )
# 
# 
# 
# 
# 
# 
# 
# 
