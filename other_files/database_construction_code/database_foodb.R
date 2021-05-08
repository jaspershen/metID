##
no_source()

tinyTools::setwd_project()
setwd('all_ms2_database/foodb/')

library(dplyr)
library(ggplot2)
library(XML)
library(MetaDBparse)
rm(list = ls())

library(readr)

Compound =
    read_csv("Compound.csv")

Content =
  read_csv("Content.csv")

Food =
  read_csv("Food.csv")

FoodTaxonomy =
  read_csv("FoodTaxonomy.csv")

# utils::unzip(normalizePath(zip.file), exdir = normalizePath(base.loc))
# input <- file.path(base.loc, "full database.xml")
# header <- readLines(input, n = 10)
# hasInfo <- grep(x = header, pattern = "version", value = TRUE, perl = TRUE)[2]
# version <- stringr::str_match(string = hasInfo, pattern = "version=\"(.*)\" exported")[, 2]
# theurl <- RCurl::getURL("https://go.drugbank.com/stats", .opts = list(ssl.verifypeer = FALSE))
# tables <- XML::readHTMLTable(theurl, header = FALSE)
# stats <- data.table::as.data.table(tables[[1]])
# colnames(stats) <- c("Description", "Count")
# n <- as.numeric(as.character(gsub(x = stats[Description == "Total Number of Drugs"]$Count, pattern = ",", replacement = "")))
# envir <- environment()
# envir$db.formatted <- data.frame(compoundname = rep(NA, n), baseformula = rep(NA, n), identifier = rep(NA, n), structure = rep(NA, n), charge = rep(NA, n), description = rep(NA, n))
# envir$pb <- pbapply::startpb(min = 0, max = n)
# envir$idx <- 0
# metabolite <- function(currNode, currEnvir = envir) {
#   if (currEnvir$idx %% 10 == 0) {
#     pbapply::setpb(currEnvir$pb, currEnvir$idx)
#   }
#   currEnvir$idx <- currEnvir$idx + 1
#   properties <- currNode[["calculated-properties"]]
#   if (is.null(properties)) {
#     properties <- currNode[["experimental-properties"]]
#   }
#   proplist <- XML::xmlToList(properties)
#   if (length(proplist) == 0) {
#     return(NULL)
#   }
#   which.form <- which(sapply(proplist, function(x) {
#     if ("kind" %in% names(x)) {
#       res <- x[["kind"]] == "Molecular Formula"
#     }
#     else {
#       res <- FALSE
#     }
#     res
#   }))
#   which.struc <- which(sapply(proplist, function(x) {
#     if ("kind" %in% names(x)) {
#       res <- x[["kind"]] == "SMILES"
#     }
#     else {
#       res <- FALSE
#     }
#     res
#   }))
#   which.charge <- which(sapply(proplist, function(x) {
#     if ("kind" %in% names(x)) {
#       res <- x[["kind"]] == "Physiological Charge"
#     }
#     else {
#       res <- FALSE
#     }
#     res
#   }))
#   if (length(which.form) == 0 & length(which.struc) == 0) {
#     return(NULL)
#   }
#   currEnvir$db.formatted[currEnvir$idx, "compoundname"] <- XML::xmlValue(currNode[["name"]])
#   currEnvir$db.formatted[currEnvir$idx, "identifier"] <- XML::xmlValue(currNode[["drugbank-id"]])
#   currEnvir$db.formatted[currEnvir$idx, "baseformula"] <- proplist[[which.form]][["value"]]
#   currEnvir$db.formatted[currEnvir$idx, "structure"] <- if (length(which.struc) > 0) {
#     proplist[[which.struc]][["value"]]
#   }
#   else {
#     ""
#   }
#   currEnvir$db.formatted[currEnvir$idx, "description"] <- XML::xmlValue(currNode[["description"]])
#   currEnvir$db.formatted[currEnvir$idx, "charge"] <- if (length(which.charge) > 0) {
#     proplist[[which.charge]][["value"]]
#   }
#   else {
#     0
#   }
# }
# res <- XML::xmlEventParse(file = input, branches = list(drug = metabolite, `drugbank-metabolite-id-value` = print))
# envir$db.formatted <- envir$db.formatted[-1, ]
# drugbank2 = envir$db.formatted
# save(drugbank2, file = "drugbank2")

load("drugbank2")


# ## parse data from XML and save it to memory
# read_drugbank_xml_db("full database.xml")
#
# ## load drugs data
# drugs <- drugs()
#
# ## load drug groups data
# drug_groups <- drug_groups()
#
# ## load drug targets actions data
# drug_targets_actions <- targets_actions()
# save(drugs, file = "drugs")
#
# load("drugs")
#
# ###only remain small molecules
# general_information =
# drugs$general_information %>%
#   dplyr::filter(type == "small molecule") %>%
#   dplyr::rename(Lab.ID = primary_key) %>%
#   dplyr::select(Lab.ID, everything()) %>%
#   dplyr::mutate(drugbank.ID = Lab.ID) %>%
#   dplyr::rename(Compound.name = name,
#                 CAS.ID = cas_number,
#                 mz = monoisotopic_mass,) %>%
#   dplyr::select(-c(description, synthesis_reference, fda_label, msds))
#
# drug_classification =
# drugs$drug_classification %>%
#   dplyr::filter(drugbank_id %in% general_information$Lab.ID) %>%
#   dplyr::rename(Lab.ID = drugbank_id) %>%
#   dplyr::select(Lab.ID, everything()) %>%
#   dplyr::select(-c(description, direct_parent, alternative_parents, substituents))
#
# library(plyr)
# synonyms =
#   drugs$synonyms %>%
#   dplyr::filter(`drugbank-id` %in% general_information$Lab.ID) %>%
#   plyr::dlply(.variables = .(`drugbank-id`)) %>%
#   purrr::map(function(x) {
#     if (nrow(x) == 1) {
#       return(x)
#     } else{
#       x$synonym = paste(x$synonym, collapse = "{}")
#       x$language = paste(x$language, collapse = "{}")
#       x$coder = paste(x$coder, collapse = "{}")
#       x[1,,drop = FALSE]
#     }
#   }) %>%
#   do.call(rbind, .) %>%
#   as.data.frame() %>%
#   dplyr::rename(Lab.ID = `drugbank-id`) %>%
#   dplyr::select(Lab.ID, everything()) %>%
#   dplyr::select(-coder)
#
# drugbank =
#   general_information %>%
#   dplyr::left_join(drug_classification, by = c("Lab.ID")) %>%
#   dplyr::left_join(synonyms, by = c("Lab.ID"))
#
# drugbank$RT = NA
# drugbank$HMDB.ID = NA
# drugbank$KEGG.ID = NA
# drugbank$mz.pos = NA
# drugbank$mz.neg = NA
# drugbank$Submitter = "drugbnak"
#
# save(drugbank, file = "drugbank")
load("drugbank")


drugbank2 =
drugbank2 %>%
  dplyr::select(baseformula, identifier) %>%
  dplyr::filter(!is.na(identifier)) %>%
  dplyr::rename(Lab.ID = identifier, Formula = baseformula)

drugbank =
drugbank %>%
  dplyr::left_join(drugbank2, by = "Lab.ID")

drugbank =
drugbank %>%
  dplyr::select(Lab.ID, mz, RT, CAS.ID, HMDB.ID, KEGG.ID, Formula, mz.pos, mz.neg, Submitter, everything()) %>%
  dplyr::filter(!is.na(Formula))

drugbank$mz = as.numeric(drugbank$mz)

openxlsx::write.xlsx(drugbank, file = "drugbank.xlsx", asTable = TRUE)

drugbankMS1Database5.1.8 =
  construct_database(
    path = ".",
    version = "5.1.8",
    metabolite.info.name = "drugbank.xlsx",
    source = "drugbank",
    link = "https://go.drugbank.com/",
    creater = "Xiaotao Shen",
    email = "shenxt@stanford.edu",
    rt = FALSE,
    threads = 3
  )


save(drugbankMS1Database5.1.8, file = "drugbankMS1Database5.1.8")


