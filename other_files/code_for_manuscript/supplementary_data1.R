###no_function()

tinyTools::setwd_project()
setwd("other_files/manuscript/")
rm(list=ls())
dir()

sheet_name =
  readxl::excel_sheets("Compare_all.xlsx")

sheet_name2 =
  readxl::excel_sheets("Compare_all_important.xlsx")

####different annotations using metID
load("../all_ms2_database/mike_in_house/msDatabase_rplc0.0.2")
load("../all_ms2_database/mike_in_house/msDatabase_hilic0.0.2")
load("../all_ms2_database/nist/nistDatabase0.0.2")
load("../all_ms2_database/hmdb/hmdbDatabase0.0.2")

load("exercise_study/pRPLC/NCE25/result_rplc_pos25")
load("exercise_study/pRPLC/NCE50/result_rplc_pos50")
load("exercise_study/nRPLC/NCE25/result_rplc_neg25")
load("exercise_study/nRPLC/NCE50/result_rplc_neg50")

load("exercise_study/pHILIC/NCE25/result_hilic_pos25")
load("exercise_study/pHILIC/NCE35/result_hilic_pos35")
load("exercise_study/nHILIC/NCE25/result_hilic_neg25")
load("exercise_study/nHILIC/NCE35/result_hilic_neg35")

###this is the peaks with annotations from both metID and paper
common_retrived =
  readxl::read_xlsx("Compare_all.xlsx", sheet = sheet_name[1])

##this is a table that have 6 different annotations from metID and paper
temp_data =
  common_retrived %>%
  dplyr::filter(!`Same or not`)

library(metID)
i=6

# for(i in 1:nrow(temp_data)) {
#   cat(i, " ")
#
#   ###column polarity
#   mode = temp_data$Mode...19[i]
#   mode
#
#   if(mode == "pRPLC"){
#     object = result_rplc_pos25[[1]]
#     lab_database = msDatabase_rplc0.0.2
#     polarity = "positive"
#   }
#
#   if(mode == "nRPLC"){
#     object = result_rplc_neg50[[1]]
#     lab_database = msDatabase_rplc0.0.2
#     polarity = "negative"
#   }
#
#   if(mode == "pHILIC"){
#     object = result_hilic_pos25[[1]]
#     lab_database = msDatabase_hilic0.0.2
#     polarity = "positive"
#   }
#
#   if(mode == "nHILIC"){
#     object = result_hilic_neg25[[1]]
#     lab_database = msDatabase_hilic0.0.2
#     polarity = "negative"
#   }
#
#   mode
#
#   ##compound name for kevin and metID
#   compound.name1 = temp_data$Metabolite_val[i]
#   compound.name2 = temp_data$Annotation[i]
#
#   ###peak name
#   peak.name = temp_data$Cmp.ID[i]
#
#   peak_ms2 =
#     metID::get_ms2_spectrum_from_object(object = object,
#                                         peak.name = peak.name)
#
#   ####database2 is the database for metID annotation result
#   database2 =
#     get(temp_data$Database[i])
#
#
#   ###annotation result from metID
#   temp_data$Annotation[i]
#
#
#  # plot =
#   metID::ms2plot(object = result_hilic_neg25[[temp_data$Database[i]]],
#                  database = database2,
#                  which.peak = peak.name)
#
#  # ggsave(plot = plot, filename = "1.00_137.0243mz_4-Hydroxybenzoate.pdf", height = 7, width = 7)
#
#  stanard_ms2_2 =
#     metID::get_ms2_spectrum(
#       lab.id = "S0280",
#       database = database2,
#       polarity = polarity,
#       ce = "15"
#     )
#
#   score2 =
#     tinyTools::getSpectraMatchScore(exp.spectrum = peak_ms2,
#                                     lib.spectrum = stanard_ms2_2)
#
#   plot2 =
#     tinyTools::ms2_plot(spectrum1 = peak_ms2,
#                         spectrum2 = stanard_ms2_2,
#                         spectrum1_name = "Experimental MS2 plot",
#                         spectrum2_name = compound.name2)
#
#   plot2 = plot2 +
#     labs(title = paste("MS2 match score: ", round(score2, 2), sep = ""))
#
#  plot2
#
#
#
#   lab.id1 =
#     lab_database@spectra.info$Lab.ID[which(lab_database@spectra.info$Compound.name == compound.name1)][1]
#   lab.id1
#   # grep("Glucose",lab_database@spectra.info$Compound.name)
#   # grep("Glucose",lab_database@spectra.info$Compound.name, value = TRUE)
#   # lab.id1 = lab_database@spectra.info$Lab.ID[794]
#
#   stanard_ms2 =
#     metID::get_ms2_spectrum(
#       lab.id = lab.id1,
#       database = lab_database,
#       polarity = polarity,
#       ce = "NCE25"
#     )
#
#   plot1 =
#     tinyTools::ms2_plot(spectrum1 = peak_ms2,
#                         spectrum2 = stanard_ms2,
#                         spectrum1_name = "Experimental MS2 plot",
#                         spectrum2_name = compound.name1)
#
#   score1 =
#   tinyTools::getSpectraMatchScore(exp.spectrum = peak_ms2,
#                                   lib.spectrum = stanard_ms2)
#   plot1 = plot1 +
#     labs(title = paste("MS2 match score: ", round(score1, 2), sep = ""))
#
#   plot1
#
#   library(patchwork)
#
#   plot =
#     plot1 + plot2
# plot
#   ggsave(plot, filename = paste(peak.name, ".pdf", sep = "") %>% stringr::str_replace("/", ""),
#          width = 14, height = 3)
#
# }



####exercise_only this is a table that peaks that only have annotatiosn from paper
exercise_only =
  readxl::read_xlsx("Compare_all.xlsx", sheet = sheet_name[2])

dim(exercise_only)

temp_data = exercise_only

annotation_rplc_pos = readr::read_csv("exercise_study/pRPLC/annotation_table_rplc_pos.csv")
annotation_rplc_neg = readr::read_csv("exercise_study/nRPLC/annotation_table_rplc_neg.csv")

annotation_hilic_pos = readr::read_csv("exercise_study/pHILIC/annotation_table_hilic_pos.csv")
annotation_hilic_neg = readr::read_csv("exercise_study/nHILIC/annotation_table_hilic_neg.csv")

###check if the annotations are from metID
all_result = vector(mode = "list", length = nrow(exercise_only))

for(i in 1:nrow(exercise_only)){
  cat(i, " ")
  temp_mode = exercise_only$Mode...19[i]
  peak_name = exercise_only$Compound[i]
  if(temp_mode == "pRPLC"){
    annotation_table = annotation_rplc_pos
    result1 = result_rplc_pos25[[1]]
    result2 = result_rplc_pos50[[1]]

  }

  if(temp_mode == "nRPLC"){
    annotation_table = annotation_rplc_neg
    result1 = result_rplc_neg25[[1]]
    result2 = result_rplc_neg50[[1]]
  }

  if(temp_mode == "pHILIC"){
    annotation_table = annotation_hilic_pos
    result1 = result_hilic_pos25[[1]]
    result2 = result_hilic_pos35[[1]]
  }

  if(temp_mode == "nHILIC"){
    annotation_table = annotation_hilic_neg
    result1 = result_hilic_neg25[[1]]
    result2 = result_hilic_neg35[[1]]
  }

  idx1 = which(result1@match.result$MS1.peak.name == peak_name)
  idx2 = which(result2@match.result$MS1.peak.name == peak_name)

  if(length(idx1) == 0 & length(idx2) == 0){
    all_result[[i]] = "no_ms2"
    next()
  }

  if(length(idx1) > 0){
  ms2_name_1 = result1@match.result$MS2.spectra.name[idx1]
  anno1 = result1@identification.result[[ms2_name_1]]
  }else{
    anno1 = NULL
  }

  if(length(idx2) > 0){
    ms2_name_2 = result1@match.result$MS2.spectra.name[idx2]
    anno2 = result2@identification.result[[ms2_name_2]]
  }else{
    anno2 = NULL
  }

  anno3 =
    annotation_table %>%
    dplyr::filter(name == peak_name)

  if(is.null(nrow(anno1)) & is.null(nrow(anno2))){
    all_result[[i]] = "no_anno"
  }

  if(!is.null(anno1)){
    all_result[[i]] =  anno1$Compound.name[1]
  }

  if(!is.null(anno2)){
    all_result[[i]] =  anno2$Compound.name[1]
  }

}

library(tidyverse)

all_result %>%
  unlist() %>%
  `!=`("no_ms2") %>%
  which()

###so it means that 63, 64, 70 and 72 also have annotations from metID, they should be remove 
###from the table and go to common retrived
all_result[[63]]

exercise_only$Mode...19[c(63,64,70,72)]
exercise_only$Compound[c(63,64,70,72)]
exercise_only$Metabolite_val[c(63,64,70,72)]

####first peak pHILIC 2.89_114.0661m/z
##metID
get_iden_info(object = result_hilic_pos25[[1]],
              which.peak = "2.89_114.0661m/z",
              database = msDatabase_hilic0.0.2) %>% 
  pull(Compound.name)

get_iden_info(object = result_hilic_pos35[[1]],
              which.peak = "2.89_114.0661m/z",
              database = msDatabase_hilic0.0.2) %>% 
  pull(Compound.name)

exercise_only$Metabolite_val[c(63)]

####second peak pHILIC 11.90_212.0428m/z
##metID
get_iden_info(object = result_hilic_pos25[[1]],
              which.peak = "11.90_212.0428m/z",
              database = msDatabase_hilic0.0.2) %>% 
  pull(Compound.name)

get_iden_info(object = result_hilic_pos35[[1]],
              which.peak = "11.90_212.0428m/z",
              database = msDatabase_hilic0.0.2) %>% 
  pull(Compound.name)

exercise_only$Metabolite_val[c(64)]





####third peak nHILIC 5.16_224.0618m/z
##metID
get_iden_info(object = result_hilic_neg25[[1]],
              which.peak = "5.16_224.0618m/z",
              database = msDatabase_hilic0.0.2) %>% 
  pull(Compound.name)

get_iden_info(object = result_hilic_neg35[[1]],
              which.peak = "5.16_224.0618m/z",
              database = msDatabase_hilic0.0.2) %>% 
  pull(Compound.name)

exercise_only$Metabolite_val[c(70)]


####fourth peak nHILIC 5.53_256.6380n
##metID
get_iden_info(object = result_hilic_neg25[[1]],
              which.peak = "5.53_256.6380n",
              database = msDatabase_hilic0.0.2) %>% 
  pull(Compound.name)

get_iden_info(object = result_hilic_neg35[[1]],
              which.peak = "5.53_256.6380n",
              database = msDatabase_hilic0.0.2) %>% 
  pull(Compound.name)

exercise_only$Metabolite_val[c(72)]


###63, 64 and 70, 72 are same in metID and paper
##so they should remove from the paper only and add to 
##common table as same
peak_id = 
  exercise_only$Compound[c(63,64,70,72)]

###add them to common table
new_common = 
  exercise_only %>% 
  dplyr::filter(Compound %in% peak_id)

new_common$Annotation = 
  c(get_iden_info(object = result_hilic_pos25[[1]],
                  which.peak = "2.89_114.0661m/z",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(Compound.name) %>% 
      `[`(1),
    get_iden_info(object = result_hilic_pos25[[1]],
                  which.peak = "11.90_212.0428m/z",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(Compound.name) %>% 
      `[`(1),
    get_iden_info(object = result_hilic_neg25[[1]],
                  which.peak = "5.16_224.0618m/z",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(Compound.name) %>% 
      `[`(1),
    get_iden_info(object = result_hilic_neg25[[1]],
                  which.peak = "5.53_256.6380n",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(Compound.name) %>% 
      `[`(1)
    )
  
new_common$HMDB = 
  c(get_iden_info(object = result_hilic_pos25[[1]],
                  which.peak = "2.89_114.0661m/z",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(HMDB.ID) %>% 
      `[`(1),
    get_iden_info(object = result_hilic_pos25[[1]],
                  which.peak = "11.90_212.0428m/z",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(HMDB.ID) %>% 
      `[`(1),
    get_iden_info(object = result_hilic_neg25[[1]],
                  which.peak = "5.16_224.0618m/z",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(HMDB.ID) %>% 
      `[`(1),
    get_iden_info(object = result_hilic_neg25[[1]],
                  which.peak = "5.53_256.6380n",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(HMDB.ID) %>% 
      `[`(1)
  )



new_common$KEGG = 
  c(get_iden_info(object = result_hilic_pos25[[1]],
                  which.peak = "2.89_114.0661m/z",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(KEGG.ID) %>% 
      `[`(1),
    get_iden_info(object = result_hilic_pos25[[1]],
                  which.peak = "11.90_212.0428m/z",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(KEGG.ID) %>% 
      `[`(1),
    get_iden_info(object = result_hilic_neg25[[1]],
                  which.peak = "5.16_224.0618m/z",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(KEGG.ID) %>% 
      `[`(1),
    get_iden_info(object = result_hilic_neg25[[1]],
                  which.peak = "5.53_256.6380n",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(KEGG.ID) %>% 
      `[`(1)
  )



new_common$Total.score = 
  c(get_iden_info(object = result_hilic_pos25[[1]],
                  which.peak = "2.89_114.0661m/z",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(Total.score) %>% 
      `[`(1),
    get_iden_info(object = result_hilic_pos25[[1]],
                  which.peak = "11.90_212.0428m/z",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(Total.score) %>% 
      `[`(1),
    get_iden_info(object = result_hilic_neg25[[1]],
                  which.peak = "5.16_224.0618m/z",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(Total.score) %>% 
      `[`(1),
    get_iden_info(object = result_hilic_neg25[[1]],
                  which.peak = "5.53_256.6380n",
                  database = msDatabase_hilic0.0.2) %>% 
      pull(Total.score) %>% 
      `[`(1)
  )



new_common$Database = "msDatabase_hilic0.0.2"
new_common$Level = 1
new_common$Mode...35 = exercise_only$Mode...19[c(63,64,70,72)]
new_common$mz.round = exercise_only$mz.round[c(63,64,70,72)]
new_common$`Same or not` = TRUE

colnames(new_common) == 
colnames(common_retrived)

common_retrived = 
  rbind(common_retrived,
        new_common)

exercise_only = 
  exercise_only %>% 
  dplyr::filter(!Compound %in% peak_id)




##only 68 are different
common_retrived %>% colnames()
exercise_only %>% colnames()

plot =
metID::ms2plot(object = result_hilic_neg25[[1]],
               database = msDatabase_hilic0.0.2,
               which.peak = "5.53_256.6380n")

plot
# ggsave(plot, filename = "exercise_study/all_ms2_plot/5.53_256.6380n.pdf", width = 10, height = 7)


####68 peaks have no MS2 spectra
peaks_68_for_check =
  data.frame(name = temp_data$Compound[-c(63,64,70,72)],
             mode = temp_data$Mode...19[-c(63,64,70,72)])

# write.csv(peaks_68_for_check, file = "peaks_68_for_check.csv", row.names = FALSE)


###output supplementrary Data1
colnames(common_retrived)[1] = "NO1"
colnames(common_retrived)[2] = "ID1"
colnames(common_retrived)[5] = "RT.min1"
colnames(common_retrived)[19] = "Mode1"
colnames(common_retrived)[22] = "NO2"
colnames(common_retrived)[23] = "ID2"
colnames(common_retrived)[27] = "RT.min2"
colnames(common_retrived)[35] = "Mode2"
colnames(exercise_only) = colnames(common_retrived)

library(openxlsx)
wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Times New Roma")
addWorksheet(wb, sheetName = "Common_retrieved", gridLines = FALSE)
addWorksheet(wb, sheetName = "Exercise_only", gridLines = FALSE)
freezePane(wb, sheet = 1, firstRow = TRUE, firstCol = TRUE) 
freezePane(wb, sheet = 2, firstRow = TRUE, firstCol = TRUE) 
writeDataTable(wb, sheet = 1, x = common_retrived,
               colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = 2, x = exercise_only,
               colNames = TRUE, rowNames = FALSE)
saveWorkbook(wb, "Supplementary Data1.xlsx", overwrite = TRUE)


####output MS2 results
metid_only = readxl::read_xlsx("Compare_all_important.xlsx", sheet = sheet_name2[5])

unique(metid_only$Database)

load("../all_ms2_database/mike_in_house/msDatabase_rplc0.0.2")
load("../all_ms2_database/mike_in_house/msDatabase_hilic0.0.2")
load("../all_ms2_database/nist/nistDatabase0.0.2")
load("../all_ms2_database/hmdb/hmdbDatabase0.0.2")
load("../all_ms2_database/orbitrap/orbitrapDatabase0.0.1")
load("../all_ms2_database/massbank/massbankDatabase0.0.2")
load("../all_ms2_database/mona/monaDatabase0.0.2")
load("../all_ms2_database/metlin/metlinDatabase0.0.2")

library(tidyverse)

dim(metid_only)

# for(i in 1:nrow(metid_only)){
#   cat(i, " ")
#   peak_name = metid_only$Cmp.ID[i]
#   mode = metid_only$Mode[i]
#   database = metid_only$Database[i]
# 
#   if(mode == "rplc_pos"){
#     result1 = result_rplc_pos25[[database]]
#     result2 = result_rplc_pos50[[database]]
#     result1@identification.result =
#       result1@identification.result %>%
#       lapply(function(x){
#         if(nrow(x) == 1){
#         return(x)
#       }else{
#         x[1,,drop = FALSE]
#       }
#       })
#     result2@identification.result =
#       result2@identification.result %>%
#       lapply(function(x){
#         if(nrow(x) == 1){
#           return(x)
#         }else{
#           x[1,,drop = FALSE]
#         }
#       })
#     database = get(database)
#   }
# 
#   if(mode == "rplc_neg"){
#     result1 = result_rplc_neg25[[database]]
#     result2 = result_rplc_neg50[[database]]
#     result1@identification.result =
#       result1@identification.result %>%
#       lapply(function(x){
#         if(nrow(x) == 1){
#           return(x)
#         }else{
#           x[1,,drop = FALSE]
#         }
#       })
#     result2@identification.result =
#       result2@identification.result %>%
#       lapply(function(x){
#         if(nrow(x) == 1){
#           return(x)
#         }else{
#           x[1,,drop = FALSE]
#         }
#       })
#     database = get(database)
#   }
# 
#   if(mode == "hilic_pos"){
#     result1 = result_hilic_pos25[[database]]
#     result2 = result_hilic_pos35[[database]]
#     result1@identification.result =
#       result1@identification.result %>%
#       lapply(function(x){
#         if(nrow(x) == 1){
#           return(x)
#         }else{
#           x[1,,drop = FALSE]
#         }
#       })
#     result2@identification.result =
#       result2@identification.result %>%
#       lapply(function(x){
#         if(nrow(x) == 1){
#           return(x)
#         }else{
#           x[1,,drop = FALSE]
#         }
#       })
#     database = get(database)
#   }
# 
#   if(mode == "hilic_neg"){
#     result1 = result_hilic_neg25[[database]]
#     result2 = result_hilic_neg35[[database]]
#     result1@identification.result =
#       result1@identification.result %>%
#       lapply(function(x){
#         if(nrow(x) == 1){
#           return(x)
#         }else{
#           x[1,,drop = FALSE]
#         }
#       })
#     result2@identification.result =
#       result2@identification.result %>%
#       lapply(function(x){
#         if(nrow(x) == 1){
#           return(x)
#         }else{
#           x[1,,drop = FALSE]
#         }
#       })
#     database = get(database)
#   }
# 
#   plot1 =
#   metID::ms2plot(object = result1, database = database, which.peak = peak_name)
# 
#   plot2 =
#     metID::ms2plot(object = result2, database = database, which.peak = peak_name)
# 
#   score1 = metID::get_iden_info(object = result1, which.peak = peak_name, database = database)
#   score2 = metID::get_iden_info(object = result2, which.peak = peak_name, database = database)
#   if(is.null(score1)){
#     score1 = 0
#   }else{
#     score1 =
#       score1 %>%
#       dplyr::arrange(desc(SS)) %>%
#       dplyr::pull(SS)
#     score1 = score1[1]
#   }
# 
#   if(is.null(score2)){
#     score2 = 0
#   }else{
#     score2 =
#       score2 %>%
#       dplyr::arrange(desc(SS)) %>%
#       dplyr::pull(SS)
#     score2 = score2[1]
#   }
# 
#   peak_name = stringr::str_replace(peak_name, "/", "")
# 
#   if(score2 > score1){
#     plot = plot2
#   }else{
#     plot = plot1
#   }
# 
#   ggsave(
#     plot,
#     filename = file.path(
#       "exercise_study/all_ms2_plot/",
#       paste(peak_name, ".pdf", sep = "")
#     ),
#     width = 10,
#     height = 7
#   )
# }



