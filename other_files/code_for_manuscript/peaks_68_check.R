no_source()

tinyTools::setwd_project()
setwd("other_files/manuscript/")

exercise_only =
readxl::read_xlsx("Supplementary Data1.xlsx", sheet = 2)

# peak_68 = readr::read_csv("exercise_study/iPOP/peaks_68_for_check.csv")

peak_68 = exercise_only

# exercise_only =
# exercise_only %>%
#   dplyr::filter(MSMS != "NO" & Compound %in% peak_68$name)

###HILIC
###positive mode
sxtTools::setwd_project()
setwd("other_files/manuscript/exercise_study/iPOP/pHILIC/")

peak_table = 
peak_68 %>% 
  dplyr::filter(Mode1 == "pHILIC") %>% 
  dplyr::select(Compound, m.z, RT.min) %>% 
  dplyr::rename(name = Compound, mz = m.z, rt = RT.min) %>% 
  dplyr::mutate(rt = rt*60) %>% 
  as.data.frame()

write.csv(peak_table, "peak_table.csv", row.names = FALSE)

library(tidyverse)
library(data.table)
library(metID)

###identifiy peaks with MS2
test1 =
  identify_metabolites(
    ms1.data = "peak_table.csv",
    ms2.data = dir(".", "mgf"),
    polarity = "positive",
    ce = "all",
    column = "hilic",
    candidate.num = 3,
    threads = 8,
    database = "msDatabase_hilic0.0.2",
    path = "."
  )

test2 =
  identify_metabolites(
    ms1.data = "peak_table.csv",
    ms2.data = NULL,
    polarity = "positive",
    ce = "all",
    column = "hilic",
    candidate.num = 3,
    threads = 8,
    database = "msDatabase_hilic0.0.2",
    path = "."
  )

test1
test2

identification_table1 =
  get_identification_table(test1,
                               candidate.num = 1,
                               type = "new")

identification_table1[,c("name", "Compound.name", "mz.error", "RT.error", "SS")] %>%
  dplyr::left_join(exercise_only[,c("Compound", "Metabolite_val")],
                   by = c("name" = "Compound"))

identification_table2 =
  get_identification_table(test2,
                           candidate.num = 1,
                           type = "new")

identification_table2[,c("name", "Compound.name", "mz.error", "RT.error", "SS")] %>%
  dplyr::left_join(exercise_only[,c("Compound", "Metabolite_val")],
                   by = c("name" = "Compound"))

identification_table_hilic_pos = 
  get_identification_table(test2, candidate.num = 1, type = "new")


###HILIC
###negative mode
sxtTools::setwd_project()
setwd("other_files/manuscript/exercise_study/iPOP/nHILIC/")

library(tidyverse)
library(data.table)
library(metID)

peak_table = 
  peak_68 %>% 
  dplyr::filter(Mode1 == "nHILIC") %>% 
  dplyr::select(Compound, m.z, RT.min) %>% 
  dplyr::rename(name = Compound, mz = m.z, rt = RT.min) %>% 
  dplyr::mutate(rt = rt*60) %>% 
  as.data.frame()

write.csv(peak_table, "peak_table.csv", row.names = FALSE)

peak_table

test1 =
  identify_metabolites(
    ms1.data = "peak_table.csv",
    ms2.data = dir(".", "mgf"),
    polarity = "negative",
    ce = "all",
    column = "hilic",
    candidate.num = 3,
    threads = 8,
    database = "msDatabase_hilic0.0.2",
    path = "."
  )

test2 =
  identify_metabolites(
    ms1.data = "peak_table.csv",
    ms2.data = NULL,
    polarity = "negative",
    ce = "all",
    column = "hilic",
    candidate.num = 3,
    threads = 8,
    database = "msDatabase_hilic0.0.2",
    path = "."
  )

test1
test2

identification_table1 =
  get_identification_table(test1,
                           candidate.num = 1,
                           type = "new")

identification_table1[,c("name", "Compound.name", "mz.error", "RT.error", "SS")] %>%
  dplyr::left_join(exercise_only[,c("Compound", "Metabolite_val")],
                   by = c("name" = "Compound")) %>% 
  as.data.frame()

identification_table2 =
  get_identification_table(test2,
                           candidate.num = 1,
                           type = "new")

identification_table2[,c("name", "Compound.name", "mz.error", "RT.error", "SS")] %>%
  dplyr::left_join(exercise_only[,c("Compound", "Metabolite_val")],
                   by = c("name" = "Compound"))


identification_table_hilic_neg = 
  get_identification_table(test2, candidate.num = 1, type = "new")



###RPLC
###positive mode
sxtTools::setwd_project()
setwd("other_files/manuscript/exercise_study/iPOP/pRPLC/")

library(tidyverse)
library(data.table)
library(metID)

peak_table = 
  peak_68 %>% 
  dplyr::filter(Mode1 == "pRPLC") %>% 
  dplyr::select(Compound, m.z, RT.min) %>% 
  dplyr::rename(name = Compound, mz = m.z, rt = RT.min) %>% 
  dplyr::mutate(rt = rt*60) %>% 
  as.data.frame()

write.csv(peak_table, "peak_table.csv", row.names = FALSE)

peak_table

test1 =
  identify_metabolites(
    ms1.data = "peak_table.csv",
    ms2.data = dir(".", "mgf"),
    polarity = "positive",
    ce = "all",
    column = "rp",
    candidate.num = 3,
    threads = 8,
    database = "msDatabase_rplc0.0.2",
    path = "."
  )

test2 =
  identify_metabolites(
    ms1.data = "peak_table.csv",
    ms2.data = NULL,
    polarity = "positive",
    ce = "all",
    column = "rp",
    candidate.num = 3,
    threads = 8,
    database = "msDatabase_rplc0.0.2",
    path = "."
  )

test1
test2

identification_table1 =
  get_identification_table(test1,
                           candidate.num = 1,
                           type = "new")

identification_table1[,c("name", "Compound.name", "mz.error", "RT.error", "SS")] %>%
  dplyr::left_join(exercise_only[,c("Compound", "Metabolite_val")],
                   by = c("name" = "Compound")) %>%
  as.data.frame()

identification_table2 =
  get_identification_table(test2,
                           candidate.num = 1,
                           type = "new")

identification_table2[,c("name", "Compound.name", "mz.error", "RT.error", "SS")] %>%
  dplyr::left_join(exercise_only[,c("Compound", "Metabolite_val")],
                   by = c("name" = "Compound"))  %>%
  as.data.frame()


test = list(msDatabase_rplc0.0.2 = test1, 
            msDatabase_rplc0.0.2_2 = test2)

identification_table_rplc_pos = 
  get_identification_table_all(test, 
                               candidate.num = 1)

###RPLC
###negative mode
sxtTools::setwd_project()
setwd("other_files/manuscript/exercise_study/iPOP/nRPLC/")

library(tidyverse)
library(data.table)
library(metID)

peak_table = 
  peak_68 %>% 
  dplyr::filter(Mode1 == "nRPLC") %>% 
  dplyr::select(Compound, m.z, RT.min) %>% 
  dplyr::rename(name = Compound, mz = m.z, rt = RT.min) %>% 
  dplyr::mutate(rt = rt*60) %>% 
  as.data.frame()

write.csv(peak_table, "peak_table.csv", row.names = FALSE)

peak_table

test1 =
  identify_metabolites(
    ms1.data = "peak_table.csv",
    ms2.data = dir(".", "mgf"),
    polarity = "negative",
    ce = "all",
    column = "rp",
    candidate.num = 3,
    threads = 8,
    database = "msDatabase_rplc0.0.2",
    path = "."
  )

test2 =
  identify_metabolites(
    ms1.data = "peak_table.csv",
    ms2.data = NULL,
    polarity = "negative",
    ce = "all",
    column = "rp",
    candidate.num = 3,
    threads = 8,
    database = "msDatabase_rplc0.0.2",
    path = "."
  )

test1
test2

identification_table1 =
  get_identification_table(test1,
                           candidate.num = 1,
                           type = "new")

identification_table1[,c("name", "Compound.name", "mz.error", "RT.error", "SS")] %>%
  dplyr::left_join(exercise_only[,c("Compound", "Metabolite_val")],
                   by = c("name" = "Compound"))  %>%
  as.data.frame()

identification_table2 =
  get_identification_table(test2,
                           candidate.num = 1,
                           type = "new")

identification_table2[,c("name", "Compound.name", "mz.error", "RT.error", "SS")] %>%
  dplyr::left_join(exercise_only[,c("Compound", "Metabolite_val")],
                   by = c("name" = "Compound"))  %>%
  as.data.frame()


test = list(msDatabase_rplc0.0.2 = test1, 
            msDatabase_rplc0.0.2_2 = test2)

identification_table_rplc_neg = 
  get_identification_table_all(test, 
                               candidate.num = 1)


dim(identification_table_hilic_pos)
dim(identification_table_hilic_neg)
dim(identification_table_rplc_pos)
dim(identification_table_rplc_neg)

colnames(identification_table_hilic_pos)
colnames(identification_table_hilic_neg)
colnames(identification_table_rplc_pos)
colnames(identification_table_rplc_neg)

identification_table_hilic_pos$Level = 2
identification_table_hilic_neg$Level = 2
identification_table_rplc_pos$Database = "msDatabase_rplc0.0.2"
identification_table_rplc_neg$Database = "msDatabase_rplc0.0.2"

identification_table = 
  rbind(identification_table_hilic_pos,
        identification_table_hilic_neg,
        identification_table_rplc_pos,
        identification_table_rplc_neg)

dim(identification_table)
dim(peak_68)

identification_table =
  identification_table[match(peak_68$Compound, identification_table$name), ]

peak_68$Annotation = identification_table$Compound.name
peak_68$HMDB = identification_table$HMDB.ID
peak_68$KEGG = identification_table$KEGG.ID
peak_68$Total.score = identification_table$Total.score
peak_68$Database = identification_table$Database
peak_68$Level = identification_table$Level
peak_68$Mode2 = peak_68$Mode1

peak_68[c(22,33),]


peak_68$`Same or not` = TRUE


write.csv(peak_68, "peak_68_annotation.csv", row.names = FALSE)
