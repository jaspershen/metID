no_source()

###HILIC
###positive mode
sxtTools::setwd_project()
setwd("manuscript/exercise_study/pHILIC/")
library(tidyverse)
library(data.table)
library(metID)

# peak_table = readr::read_csv("PQI_HILIC_pos.csv")
#
# peak_table =
#   peak_table %>%
#   dplyr::select(name = Compound, mz = `m/z`, rt = `Retention time (min)`) %>%
#   dplyr::mutate(rt = rt * 60)
#
# write.csv(peak_table, "peak_table.csv", row.names = FALSE)
#
# ###level 1
# param1 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 30,
#     polarity = "positive",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "msDatabase_hilic0.0.2"
#   )
#
# ###level 2
# param2 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "hmdbDatabase0.0.2"
#   )
#
# param3 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "massbankDatabase0.0.2"
#   )
#
# param4 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "metlinDatabase0.0.2"
#   )
#
# param5 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "monaDatabase0.0.2"
#   )
#
# param6 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "nistDatabase0.0.2"
#   )
#
# param7 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "orbitrapDatabase0.0.1"
#   )
#
# ##level 3
# param8 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "hmdbMS1Database0.0.1"
#   )
#
#
# result_hilic_pos25 <- identify_metabolite_all(
#   ms1.data = "peak_table.csv",
#   ms2.data = dir("NCE25/", pattern = "mgf"),
#   parameter.list = c(param1, param2, param3, param4, param5, param6, param7, param8),
#   path = "NCE25/"
# )
#
# save(result_hilic_pos25, file = "NCE25/result_hilic_pos25")
#
# result_hilic_pos25 <- identify_metabolite_all(
#   ms1.data = "peak_table.csv",
#   ms2.data = dir(path = "NCE35/", pattern = "mgf"),
#   parameter.list = c(param1, param2, param3, param4, param5, param6, param7),
#   path = "NCE35/"
# )
#
# save(result_hilic_pos35, file = "NCE35/result_hilic_pos35")

load("NCE25/result_hilic_pos25")
load("NCE35/result_hilic_pos35")

annotation_table =
  metID::get_identification_table_all(result_hilic_pos25,
                                      result_hilic_pos35,
                                      candidate.num = 1)

write.csv(annotation_table, file = "annotation_table_hilic_pos.csv", row.names = FALSE)


###HILIC
###negative mode
sxtTools::setwd_project()
setwd("manuscript/exercise_study/nHILIC/")
library(tidyverse)
library(data.table)
library(metID)

# peak_table = readr::read_csv("PQI_HILIC_neg.csv")
#
# peak_table =
#   peak_table %>%
#   dplyr::select(name = Compound, mz = `m/z`, rt = `Retention time (min)`) %>%
#   dplyr::mutate(rt = rt * 60)
#
# write.csv(peak_table, "peak_table.csv", row.names = FALSE)
#
# ###level 1
# param1 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 30,
#     polarity = "negative",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "msDatabase_hilic0.0.2"
#   )
#
# ###level 2
# param2 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "hmdbDatabase0.0.2"
#   )
#
# param3 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "massbankDatabase0.0.2"
#   )
#
# param4 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "metlinDatabase0.0.2"
#   )
#
# param5 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "monaDatabase0.0.2"
#   )
#
# param6 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "nistDatabase0.0.2"
#   )
#
# param7 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "orbitrapDatabase0.0.1"
#   )
#
# ##level 3
# param8 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "hilic",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "hmdbMS1Database0.0.1"
#   )
#
#
# result_hilic_neg25 <- identify_metabolite_all(
#   ms1.data = "peak_table.csv",
#   ms2.data = dir("NCE25/", "mgf"),
#   parameter.list = c(param1, param2, param3, param4, param5, param6, param7, param8),
#   path = "NCE25/"
# )
#
#
# result_hilic_neg35 <- identify_metabolite_all(
#   ms1.data = "peak_table.csv",
#   ms2.data = dir("NCE35/", "mgf"),
#   parameter.list = c(param1, param2, param3, param4, param5, param6, param7),
#   path = "NCE35/"
# )
#
# save(result_hilic_neg25, file = "NCE25/result_hilic_neg25")
# save(result_hilic_neg35, file = "NCE35/result_hilic_neg35")
#
#
load("NCE25/result_hilic_neg25")
load("NCE35/result_hilic_neg35")

annotation_table =
  metID::get_identification_table_all(result_hilic_neg25,
                                      result_hilic_neg35,
                                      candidate.num = 1)

write.csv(annotation_table, file = "annotation_table_hilic_neg.csv", row.names = FALSE)



###RP
###positive mode
sxtTools::setwd_project()
setwd("manuscript/exercise_study/pRPLC/")
library(tidyverse)
library(data.table)
library(metID)
rm(list=ls())

# peak_table = readr::read_csv("PQI_RPLC_pos.csv")
#
# peak_table =
#   peak_table %>%
#   dplyr::select(name = Compound, mz = `m/z`, rt = `Retention time (min)`) %>%
#   dplyr::mutate(rt = rt * 60)
#
# write.csv(peak_table, "peak_table.csv", row.names = FALSE)
#
# ###level 1
# param1 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 30,
#     polarity = "positive",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "msDatabase_rplc0.0.2"
#   )
#
# ###level 2
# param2 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "hmdbDatabase0.0.2"
#   )
#
# param3 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "massbankDatabase0.0.2"
#   )
#
# param4 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "metlinDatabase0.0.2"
#   )
#
# param5 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "monaDatabase0.0.2"
#   )
#
# param6 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "nistDatabase0.0.2"
#   )
#
# param7 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "orbitrapDatabase0.0.1"
#   )
#
# ##level 3
# param8 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "positive",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "hmdbMS1Database0.0.1"
#   )
#
#
# result_rplc_pos25 <- identify_metabolite_all(
#   ms1.data = "peak_table.csv",
#   ms2.data = dir("NCE25/", pattern = "mgf"),
#   parameter.list = c(param1, param2, param3, param4, param5, param6, param7, param8),
#   path = "NCE25/"
# )
#
#
# result_rplc_pos50 <- identify_metabolite_all(
#   ms1.data = "peak_table.csv",
#   ms2.data = dir(path = "NCE50/", pattern = "mgf"),
#   parameter.list = c(param1, param2, param3, param4, param5, param6, param7),
#   path = "NCE50/"
# )
#
# save(result_rplc_pos25, file = "NCE25/result_rplc_pos25")
# save(result_rplc_pos50, file = "NCE50/result_rplc_pos50")

load("NCE25/result_rplc_pos25")
load("NCE50/result_rplc_pos50")

annotation_table =
  metID::get_identification_table_all(result_rplc_pos25,
                                      result_rplc_pos50,
                                      candidate.num = 1)

write.csv(annotation_table, file = "annotation_table_rplc_pos.csv", row.names = FALSE)


###RPLC
###negative mode
sxtTools::setwd_project()
setwd("manuscript/exercise_study/nRPLC/")
library(tidyverse)
library(data.table)
library(metID)
rm(list = ls())

#
# peak_table = readr::read_csv("PQI_RPLC_neg.csv")
#
# peak_table =
#   peak_table %>%
#   dplyr::select(name = Compound, mz = `m/z`, rt = `Retention time (min)`) %>%
#   dplyr::mutate(rt = rt * 60)
#
# write.csv(peak_table, "peak_table.csv", row.names = FALSE)
#
# ###level 1
# param1 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 30,
#     polarity = "negative",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "msDatabase_rplc0.0.2"
#   )
#
# ###level 2
# param2 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "hmdbDatabase0.0.2"
#   )
#
# param3 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "massbankDatabase0.0.2"
#   )
#
# param4 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "metlinDatabase0.0.2"
#   )
#
# param5 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "monaDatabase0.0.2"
#   )
#
# param6 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "nistDatabase0.0.2"
#   )
#
# param7 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "orbitrapDatabase0.0.1"
#   )
#
# ##level 3
# param8 <-
#   identify_metabolites_params(
#     ms1.match.ppm = 25,
#     rt.match.tol = 1000000,
#     polarity = "negative",
#     ce = "all",
#     column = "rp",
#     total.score.tol = 0.5,
#     candidate.num = 3,
#     threads = 8,
#     database = "hmdbMS1Database0.0.1"
#   )
#
#
#
# result_rplc_neg25 <- identify_metabolite_all(
#   ms1.data = "peak_table.csv",
#   ms2.data = dir("NCE25/", "mgf"),
#   parameter.list = c(param1, param2, param3, param4, param5, param6, param7, param8),
#   path = "NCE25/"
# )
#
# result_rplc_neg50 <- identify_metabolite_all(
#   ms1.data = "peak_table.csv",
#   ms2.data = dir("NCE50/", "mgf"),
#   parameter.list = c(param1, param2, param3, param4, param5, param6, param7),
#   path = "NCE50/"
# )
#
# save(result_rplc_neg25, file = "NCE25/result_rplc_neg25")
# save(result_rplc_neg50, file = "NCE50/result_rplc_neg50")


load("NCE25/result_rplc_neg25")
load("NCE50/result_rplc_neg50")

annotation_table =
  metID::get_identification_table_all(result_rplc_neg25,
                                      result_rplc_neg50,
                                      candidate.num = 1)

write.csv(annotation_table, file = "annotation_table_rplc_neg.csv", row.names = FALSE)




