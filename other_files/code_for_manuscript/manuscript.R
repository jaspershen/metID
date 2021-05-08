tinyTools::setwd_project()
setwd("other_files/manuscript/")
rm(list=ls())
dir()

sheet_name =
  readxl::excel_sheets("Compare_all.xlsx")


####different annotations
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

common_retrived =
  readxl::read_xlsx("Compare_all.xlsx", sheet = sheet_name[1])

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

####exercise_only
exercise_only =
  readxl::read_xlsx("Compare_all.xlsx", sheet = sheet_name[2])

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

all_result[[63]]

exercise_only$Mode...19[c(63,64,70,72)]
exercise_only$Compound[c(63,64,70,72)]
exercise_only$Metabolite_val[c(63,64,70,72)]



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


####output MS2 results
metid_only = readxl::read_xlsx("Compare_all.xlsx", sheet = sheet_name[4])

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



####get the chemical class of all the metID only metabolites
library(tinyTools)
library(metflow2)

hmdb_id1 =
  metid_only[,c("Annotation", "HMDB")] %>%
  dplyr::distinct(Annotation,.keep_all = TRUE)

kegg_id1 =
  metid_only[,c("Annotation", "KEGG")] %>%
  dplyr::distinct(Annotation,.keep_all = TRUE)

# hmdb_id2 <-
#   t(hmdb_id1) %>%
#   as.data.frame() %>%
#   purrr::map(function(x){
#     x[1] = stringr::str_replace(x[1], "\\([0-9]{1,2}\\)$", "")
#     if(x[2] == 'NA'){
#       metflow2::transID(query = x[1], from = "Chemical name", to = "Human Metabolome Database", top = 1)$`Human Metabolome Database`
#     }else{
#       if(nchar(x[2]) == 9){
#         x[2] = stringr::str_replace(x[2], "HMDB", "HMDB00")
#       }
#       x[2]
#     }
#   })
#
# save(hmdb_id2, file = "hmdb_id2")
load("hmdb_id2")

hmdb_id2 =
hmdb_id2 %>%
  unlist %>%
  unname()

# kegg_id2 <-
#   t(kegg_id1) %>%
#   as.data.frame() %>%
#   purrr::map(function(x){
#     cat(x[1], " ")
#     x[is.na(x)] = "NA"
#     x[1] = stringr::str_replace(x[1], "\\([0-9]{1,2}\\)$", "")
#     if(x[2] == 'NA'){
#       metflow2::transID(query = x[1], from = "Chemical name", to = "KEGG", top = 1)$KEGG
#     }else{
#       x[2]
#     }
#   })
#
# save(kegg_id2, file = "kegg_id2")
load("kegg_id2")

kegg_id2 =
  kegg_id2 %>%
  unlist %>%
  unname()

name1 = hmdb_id2[grep("C", hmdb_id2)]
name2 = kegg_id2[grep("HMDB", kegg_id2)]

hmdb_id2[grep("C", hmdb_id2)] = name2
kegg_id2[grep("HMDB", kegg_id2)] = name1

inchikey1 <-
  hmdb_id2 %>%
  pbapply::pblapply(function(x){
    if(is.na(x)){
      return(NA)
    }
    metflow2::transID(query = x, from = "Human Metabolome Database", to = "InChIKey", top = 1)
  }) %>%
  do.call(rbind, .) %>%
  as.data.frame()

inchikey2 <-
  kegg_id2 %>%
  pbapply::pblapply(function(x){
    if(is.na(x)){
      return(NA)
    }
    metflow2::transID(query = x, from = "KEGG", to = "InChIKey", top = 1)
  }) %>%
  do.call(rbind, .) %>%
  as.data.frame()

dim(inchikey1)
dim(inchikey2)

inchikey =
data.frame(inchikey1$InChIKey, inchikey2$InChIKey) %>%
  apply(1, function(x){
  if(all(is.na(x))){
    return(NA)
  }
    x = x[!is.na(x)]
    x[1]
  })

# metabolite_class <- vector(mode = "list", length = length(inchikey))
#
# metabolite_class <-
#   pbapply::pblapply(inchikey, function(x){
#     if(is.na(x)){
#       return(NA)
#     }
#     Sys.sleep(time = 5)
#     result <- metflow2::get_metclass(inchikey = x, sleep = 5)
#   })
# save(metabolite_class, file = "metabolite_class")

load("metabolite_class")

idx <-
  lapply(metabolite_class, class) %>% unlist() %>%
  `==`("logical") %>%
  which()

inchikey[idx]


super_class <- lapply(metabolite_class, function(x){
  if(is.na(x)) return(NA)
  x@classification_info %>%
    dplyr::filter(name == "Superclass") %>%
    pull(value)
}) %>%
  unlist()


class <- lapply(metabolite_class, function(x){
  if(is.na(x)) return(NA)
  x@classification_info %>%
    dplyr::filter(name == "Class") %>%
    pull(value)
}) %>%
  unlist()

sub_class <- lapply(metabolite_class, function(x){
  if(is.na(x)) return(NA)
  x@classification_info %>%
    dplyr::filter(name == "Subclass") %>%
    pull(value)
}) %>%
  unlist()

sub_class[which(sub_class == "Not available")] <- NA

metabolite_tags <- data.frame(unique_compound_name,
                              super_class,
                              class,
                              sub_class,
                              stringsAsFactors = FALSE)


library(openxlsx)

openxlsx::write.xlsx(metabolite_tags, file = "metabolite_class.xlsx", asTable = TRUE)

###pie chart
library(ggpol)

#draw a parliament diagram
metabolite_tags$super_class[is.na(metabolite_tags$super_class)] = "Unknown"
temp_data =
  metabolite_tags %>%
  dplyr::filter(!is.na(super_class)) %>%
  dplyr::group_by(super_class) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(n) %>%
  dplyr::mutate(super_class = factor(super_class, levels = super_class))


class_color = c(ggsci::pal_aaas()(n=10),
                gplots::col2hex(cname = "skyblue"))
# names(class_color) = temp_data$super_class

plot <-
  temp_data %>%
  ggplot() +
  geom_parliament(aes(seats = n, fill =  super_class),
                  color = "white") +
  scale_fill_manual(values = class_color,
                    labels = temp_data$super_class) +
  coord_fixed() +
  theme_void() +
  labs(title  = "645 metabolites",
       subtitle="The number and distribution of class of 645 metabolites")+
  theme(title = element_text(size = 13),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(vjust = -3,hjust = 0.9),
        legend.position = 'bottom',
        legend.direction = "horizontal",
        legend.spacing.y = unit(0.1,"cm"),
        legend.spacing.x = unit(0.1,"cm"),
        legend.key.size = unit(0.8, 'lines'),
        legend.text = element_text(margin = margin(r = 1, unit = 'cm')),
        legend.text.align = 0)+
  annotate("text", x = 0, y = 0.4,
           label = "Metabolite super lass :\n 302 (46.8%) unknown \n 343 (53.2%) known",
           colour = "black",size = 6)+
  guides(fill=guide_legend(nrow = 4,
                           byrow=TRUE,
                           reverse = TRUE,
                           title=NULL))

plot

ggsave(plot,
       filename = "metid_only_class1.pdf", width = 10, height = 7)

names(class_color) = temp_data$super_class

plot =
metabolite_tags %>%
  dplyr::filter(super_class != "Unknown") %>%
  dplyr::group_by(super_class) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(n) %>%
  dplyr::mutate(super_class = factor(super_class, levels = super_class)) %>%
  ggplot() +
  geom_bar(aes(x = 1, y = n, fill = super_class),
           color = "white",
           stat = "identity", show.legend = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = class_color)
plot
ggsave(plot, filename = "metid_only_class2.pdf", width = 7, height = 7)


