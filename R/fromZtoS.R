# setwd("D:/study/database and library/zhu_nist")
# load("zhuMetlib.rda")
# 
# info <- zhuMetlib$meta$compound
# 
# colnames(info) <- c("Lab.ID", "Compound.name", "mz", "Formula", "CAS.ID", "HMDB.ID")
# 
# RT <- rep(NA, nrow(info))
# 
# info <- data.frame(info, RT, stringsAsFactors = FALSE)
# 
# info$mz <- as.numeric(info$mz)
# 
# KEGG.ID <- rep(NA, nrow(info))
# 
# info <- data.frame(info, KEGG.ID, stringsAsFactors = FALSE)
# 
# info <- info[,c("Lab.ID", "Compound.name", "mz", "RT", "CAS.ID", "HMDB.ID", "KEGG.ID", "Formula")]
# 
# 
# spectra.pos <- zhuMetlib$compound$pos
# spectra.neg <- zhuMetlib$compound$neg
# 
# Spectra <- list("Spectra.positive" = spectra.pos,
#                 "Spectra.negative" = spectra.neg)
# 
# 
# database.info <- list("Version" = "0.0.1",
#                      "Source" = "NIST",
#                      "Link" = "https://www.nist.gov/srd/national-standard-reference-data-series",
#                      "Creater" = "Xiaotao Shen",
#                      "Email" = "shenxt1990@163.com",
#                      "RT" = FALSE)
# 
# 
# 
# nistDatabase0.0.1 <- new(Class = "databaseClass", 
#                         database.info = database.info,
#                         spectra.info = info,
#                         spectra.data = Spectra)
# 
# save(nistDatabase0.0.1, file = 'nistDatabase0.0.1', compress = "xz")
# 
# 
# 
# 
# 
# 
# 
# setwd("D:/study/database and library/HMDB")
# load("hmdbLib.rda")
# 
# info <- hmdbLib$meta$compound
# 
# colnames(info) <- c("Lab.ID", "Compound.name", "mz", "Formula", "CAS.ID", "HMDB.ID", 
#                     "SMILES", "ChemSpider.ID", "METLIN.ID", "KEGG.ID", "SMPDB.ID", "Instrument", "CE", "InChikey")
# info <- info[,-c(11)]
# RT <- rep(NA, nrow(info))
# 
# info <- data.frame(info, RT, stringsAsFactors = FALSE)
# info <- info[,c("Lab.ID", "Compound.name", "mz", "RT", "CAS.ID", "HMDB.ID", "KEGG.ID", "Formula", "SMILES", 
#                 "ChemSpider.ID", "METLIN.ID", "Instrument", "CE", "InChikey")]
# info$mz <- as.numeric(info$mz)
# 
# metabolite.name <- apply(info[,-c(1, 13)], 1, function(x){
#   x <- as.character(x)
#   x <- stringr::str_trim(x)
#   paste(x, collapse = ";")
# })
# 
# unique.metabolite.name <- unique(metabolite.name)
# 
# ce <- info$CE
# ce2 <- lapply(unique.metabolite.name, function(x){
#   temp.idx <- which(metabolite.name == x)
#   temp.ce <- ce[temp.idx]
#   if(any(is.na(temp.ce))){
#     temp.ce[is.na(temp.ce)] <- paste("Unknown", 1:length(temp.ce[is.na(temp.ce)]), sep = "_")  
#     # ce[temp.idx] <- temp.ce
#   }
#   temp.ce
# })
# 
# 
# temp.idx <- lapply(unique.metabolite.name, function(x){
#   temp.idx <- which(metabolite.name == x)
#   unname(temp.idx)
# })
# 
# for(i in 1:length(temp.idx)){
#   ce[temp.idx[[i]]] <- ce2[[i]]
# }
# 
#  
# cbind(ce, info$CE)
# 
# 
# info$CE <- ce
# 
# 
# # info <- info[!duplicated(info[,-1]),]
# 
# spectra.pos <- hmdbLib$compound$pos
# spectra.neg <- hmdbLib$compound$neg
# 
# 
# lab.id <- lapply(unique.metabolite.name, function(x){
#   info$Lab.ID[which(x == metabolite.name)]
# })
# 
# 
# spectra.pos2 <- lapply(lab.id, function(x){
#   idx <- match(x, names(spectra.pos))
#   if(all(is.na(idx))) return(NA)
#   # idx <- idx[!is.na(idx)]
#   temp.spec <- spectra.pos[idx]
#   temp.spec <- lapply(temp.spec, function(x)x[[1]])
#   names(temp.spec) <- info$CE[match(x, info$Lab.ID)]
#   temp.spec <- temp.spec[unlist(lapply(temp.spec, function(x) !is.null(x)))]
#   temp.spec
# })
# 
# spectra.neg2 <- lapply(lab.id, function(x){
#   idx <- match(x, names(spectra.neg))
#   if(all(is.na(idx))) return(NA)
#   # idx <- idx[!is.na(idx)]
#   temp.spec <- spectra.neg[idx]
#   temp.spec <- lapply(temp.spec, function(x)x[[1]])
#   names(temp.spec) <- info$CE[match(x, info$Lab.ID)]
#   temp.spec <- temp.spec[unlist(lapply(temp.spec, function(x) !is.null(x)))]
#   temp.spec
# })
# 
# 
# info <- info[!duplicated(info[,-c(1,13)]),]
# info <- info[,-c(13)]
# 
# 
# metabolite.name2 <- apply(info[,-c(1)], 1, function(x){
#   x <- as.character(x)
#   x <- stringr::str_trim(x)
#   paste(x, collapse = ";")
# })
# 
# sum(metabolite.name2 == unique.metabolite.name)
# 
# # info$Lab.ID <- paste("shen", paste(info$HMDB.ID, info$Instrument, sep = "_"), sep = "_")
# info$Lab.ID <- paste(paste("shen", 1:nrow(info), sep = "_"), info$HMDB.ID, sep = "_")
# 
# names(spectra.pos2) <- names(spectra.neg2) <- info$Lab.ID
# 
# 
# 
# spectra.pos2 <- spectra.pos2[which(!unlist(lapply(spectra.pos2, function(x)all(is.na(x)))))]
# spectra.neg2 <- spectra.neg2[which(!unlist(lapply(spectra.neg2, function(x)all(is.na(x)))))]
# 
# 
# spectra.pos3 <- lapply(spectra.pos2, function(x){
#   x <- lapply(x, function(y){
#     y$mz <- as.numeric(y$mz)
#     y$intensity <- as.numeric(y$intensity)
#     y
#   })
#   x
# })
# 
# spectra.neg3 <- lapply(spectra.neg2, function(x){
#   x <- lapply(x, function(y){
#     y$mz <- as.numeric(y$mz)
#     y$intensity <- as.numeric(y$intensity)
#     y
#   })
#   x
# })
# 
# 
# Spectra <- list("Spectra.positive" = spectra.pos3,
#                 "Spectra.negative" = spectra.neg3)
# 
# 
# database.info <- list("Version" = "0.0.1",
#                       "Source" = "HMDB",
#                       "Link" = "http://www.hmdb.ca/",
#                       "Creater" = "Xiaotao Shen",
#                       "Email" = "shenxt1990@163.com",
#                       "RT" = FALSE)
# 
# 
# 
# hmdbDatabase0.0.1 <- new(Class = "databaseClass", 
#                          database.info = database.info,
#                          spectra.info = info,
#                          spectra.data = Spectra)
# 
# save(hmdbDatabase0.0.1, file = 'hmdbDatabase0.0.1', compress = "xz")
# 
# 
# 
# 
# ####METLIN database
# setwd("D:/study/database and library/metlin")
# load("metlinlib.rdata")
# metlinlib <- metlinlib$v1.0.1710
# 
# info <- metlinlib$meta$compound
# 
# colnames(info) <- c("Lab.ID", "Compound.name", "mz", "Formula", "HMDB.ID", 
#                    "KEGG.ID", "METLIN.ID", "CAS.ID")
# 
# RT <- rep(NA, nrow(info))
# 
# info <- data.frame(info, RT, stringsAsFactors = FALSE)
# info <- info[,c("Lab.ID", "Compound.name", "mz", "RT", "CAS.ID", "HMDB.ID", "KEGG.ID", "Formula",
#                 "METLIN.ID")]
# info$mz <- as.numeric(info$mz)
# 
# 
# 
# 
# 
# 
# spectra.pos <- metlinlib$compound$pos
# spectra.neg <- metlinlib$compound$neg
# 
# Spectra <- list("Spectra.positive" = spectra.pos,
#                 "Spectra.negative" = spectra.neg)
# 
# 
# database.info <- list("Version" = "0.0.1",
#                       "Source" = "METLIN",
#                       "Link" = "https://metlin.scripps.edu/landing_page.php?pgcontent=mainPage",
#                       "Creater" = "Xiaotao Shen",
#                       "Email" = "shenxt1990@163.com",
#                       "RT" = FALSE)
# 
# 
# 
# metlinDatabase0.0.1 <- new(Class = "databaseClass", 
#                          database.info = database.info,
#                          spectra.info = info,
#                          spectra.data = Spectra)
# 
# save(metlinDatabase0.0.1, file = 'metlinDatabase0.0.1', compress = "xz")
# 
# 
# 
# ###orbitrap
# setwd("D:/study/database and library/nist_orbitrap")
# load("orbitrapMetlib.rda")
# 
# 
# 
# 
# 
# info <- orbitrapMetlib$meta$compound
# 
# hmdbDatabase0.0.1@spectra.info$HMDB.ID[match(info$cas, hmdbDatabase0.0.1@spectra.info$CAS.ID)]
# 
# colnames(info) <- c("Lab.ID", "Compound.name", "mz", "Formula", "CAS.ID", "HMDB.ID")
# 
# load("D:/study/database and library/HMDB/hmdbAllinf.rda")
# 
# hmdb.id <- cbind(as.character(hmdbAllinf$HMDBID[match(info$CAS.ID, hmdbAllinf$CAS_Registry_Number)]),
#               info$HMDB.ID)
# 
# 
# hmdb.id <- apply(hmdb.id, 1, function(x){
#   x <- as.character(x)
#   if(all(is.na(x))) return(NA)
#   x <- x[!is.na(x)]
#   return(x[1])
# })
# 
# info$hmdb <- hmdb.id
# 
# KEGG.ID <- as.character(hmdbAllinf$KEGGID[match(info$CAS.ID, hmdbAllinf$CAS_Registry_Number)])
# KEGG.ID[is.na(KEGG.ID)] <- ""
# KEGG.ID[KEGG.ID == "\t C01586"] <- "C01586"
# KEGG.ID[KEGG.ID == ""] <- NA
# 
# RT <- rep(NA, nrow(info))
# 
# info <- data.frame(info, RT, KEGG.ID, stringsAsFactors = FALSE)
# info <- info[,c("Lab.ID", "Compound.name", "mz", "RT", "CAS.ID", "HMDB.ID", "KEGG.ID", "Formula")]
# info$mz <- as.numeric(info$mz)
# 
# 
# 
# spectra.pos <- orbitrapMetlib$compound$pos
# spectra.neg <- orbitrapMetlib$compound$neg
# 
# Spectra <- list("Spectra.positive" = spectra.pos,
#                 "Spectra.negative" = spectra.neg)
# 
# 
# database.info <- list("Version" = "0.0.1",
#                       "Source" = "NIST",
#                       "Link" = "https://www.nist.gov/",
#                       "Creater" = "Xiaotao Shen",
#                       "Email" = "shenxt1990@163.com",
#                       "RT" = FALSE)
# 
# 
# 
# orbitrapDatabase0.0.1 <- new(Class = "databaseClass", 
#                            database.info = database.info,
#                            spectra.info = info,
#                            spectra.data = Spectra)
# 
# save(orbitrapDatabase0.0.1, file = 'orbitrapDatabase0.0.1', compress = "xz")
# 
# 
# 
# ###massbank
# setwd("D:/study/database and library/MassBank")
# # load("massbank.rdata")
# load("D:/my github/metTools/data/massBankLib.rda")
# 
# info <- massBankLib$meta$compound
# 
# colnames(info) <- c("Lab.ID", "Compound.name", "mz", "Formula", "CAS.ID", "HMDB.ID", 
#                     "SMILES","IUPAC", "Pubchem.ID","ChemSpider.ID", "Instrument.type", 
#                     "Instrument", "CE", "Adduct", "Precursor.mz")
# info <- info[,-c(14,15)]
# 
# RT <- rep(NA, nrow(info))
# KEGG.ID <- as.character(hmdbAllinf$KEGGID[match(info$CAS.ID, hmdbAllinf$CAS_Registry_Number)])
# info <- data.frame(info, RT, KEGG.ID, stringsAsFactors = FALSE)
# info <- info[,c("Lab.ID", "Compound.name", "mz", "RT", "CAS.ID", "HMDB.ID", "KEGG.ID", "Formula", "SMILES", 
#                 "ChemSpider.ID", "Instrument", "Instrument.type", "CE", "IUPAC", "Pubchem.ID")]
# 
# 
# 
# info$mz <- as.numeric(info$mz)
# 
# info$CE <- stringr::str_replace(string = info$CE, pattern = "eV", replacement = "")
# info$CE <- stringr::str_replace(string = info$CE, pattern = "V", replacement = "")
# info$CE <- stringr::str_trim(info$CE)
# 
# 
# 
# metabolite.name <- apply(info[,-c(1, 13)], 1, function(x){
#   x <- as.character(x)
#   x <- stringr::str_trim(x)
#   paste(x, collapse = ";")
# })
# 
# unique.metabolite.name <- unique(metabolite.name)
# 
# ce <- info$CE
# ce2 <- lapply(unique.metabolite.name, function(x){
#   temp.idx <- which(metabolite.name == x)
#   temp.ce <- ce[temp.idx]
#   if(any(is.na(temp.ce))){
#     temp.ce[is.na(temp.ce)] <- paste("Unknown", 1:length(temp.ce[is.na(temp.ce)]), sep = "_")  
#     # ce[temp.idx] <- temp.ce
#   }
#   temp.ce
# })
# 
# 
# temp.idx <- lapply(unique.metabolite.name, function(x){
#   temp.idx <- which(metabolite.name == x)
#   unname(temp.idx)
# })
# 
# for(i in 1:length(temp.idx)){
#   ce[temp.idx[[i]]] <- ce2[[i]]
# }
# 
# 
# cbind(ce, info$CE)
# 
# 
# info$CE <- ce
# 
# 
# # info <- info[!duplicated(info[,-1]),]
# 
# spectra.pos <- massBankLib$compound$pos
# spectra.neg <- massBankLib$compound$neg
# 
# 
# lab.id <- lapply(unique.metabolite.name, function(x){
#   info$Lab.ID[which(x == metabolite.name)]
# })
# 
# 
# spectra.pos2 <- lapply(lab.id, function(x){
#   idx <- match(x, names(spectra.pos))
#   if(all(is.na(idx))) return(NA)
#   # idx <- idx[!is.na(idx)]
#   temp.spec <- spectra.pos[idx]
#   temp.spec <- lapply(temp.spec, function(x)x[[1]])
#   names(temp.spec) <- info$CE[match(x, info$Lab.ID)]
#   temp.spec <- temp.spec[unlist(lapply(temp.spec, function(x) !is.null(x)))]
#   temp.spec
# })
# 
# spectra.neg2 <- lapply(lab.id, function(x){
#   idx <- match(x, names(spectra.neg))
#   if(all(is.na(idx))) return(NA)
#   # idx <- idx[!is.na(idx)]
#   temp.spec <- spectra.neg[idx]
#   temp.spec <- lapply(temp.spec, function(x)x[[1]])
#   names(temp.spec) <- info$CE[match(x, info$Lab.ID)]
#   temp.spec <- temp.spec[unlist(lapply(temp.spec, function(x) !is.null(x)))]
#   temp.spec
# })
# 
# 
# info2 <- info[!duplicated(info[,-c(1,13)]),]
# info2 <- info2[,-c(13)]
# 
# 
# metabolite.name2 <- apply(info2[,-c(1)], 1, function(x){
#   x <- as.character(x)
#   x <- stringr::str_trim(x)
#   paste(x, collapse = ";")
# })
# 
# info2 <- info2[-54,]
# metabolite.name2 <- metabolite.name2[-54]
# 
# match(metabolite.name2, unique.metabolite.name)
# setdiff(metabolite.name2, unique.metabolite.name)
# setdiff(unique.metabolite.name,metabolite.name2)
# # intersect(metabolite.name2, unique.metabolite.name)
# 
# sum(metabolite.name2 == unique.metabolite.name)
# 
# # info$Lab.ID <- paste("shen", paste(info$HMDB.ID, info$Instrument, sep = "_"), sep = "_")
# info <- info2
# info$Lab.ID <- paste("MassBank", 1:nrow(info), sep = "_")
# 
# names(spectra.pos2) <- names(spectra.neg2) <- info$Lab.ID
# 
# 
# 
# spectra.pos2 <- spectra.pos2[which(!unlist(lapply(spectra.pos2, function(x)all(is.na(x)))))]
# spectra.neg2 <- spectra.neg2[which(!unlist(lapply(spectra.neg2, function(x)all(is.na(x)))))]
# 
# 
# spectra.pos3 <- lapply(spectra.pos2, function(x){
#   x <- lapply(x, function(y){
#     y$mz <- as.numeric(y$mz)
#     y$intensity <- as.numeric(y$intensity)
#     y
#   })
#   x
# })
# 
# spectra.neg3 <- lapply(spectra.neg2, function(x){
#   x <- lapply(x, function(y){
#     y$mz <- as.numeric(y$mz)
#     y$intensity <- as.numeric(y$intensity)
#     y
#   })
#   x
# })
# 
# 
# Spectra <- list("Spectra.positive" = spectra.pos3,
#                 "Spectra.negative" = spectra.neg3)
# 
# 
# database.info <- list("Version" = "0.0.1",
#                       "Source" = "MassBank",
#                       "Link" = "http://www.massbank.jp/",
#                       "Creater" = "Xiaotao Shen",
#                       "Email" = "shenxt1990@163.com",
#                       "RT" = FALSE)
# 
# 
# 
# massbankDatabase0.0.1 <- new(Class = "databaseClass", 
#                          database.info = database.info,
#                          spectra.info = info,
#                          spectra.data = Spectra)
# 
# save(massbankDatabase0.0.1, file = 'massbankDatabase0.0.1', compress = "xz")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###massbank
# setwd("D:/study/database and library/MoNA")
# # load("massbank.rdata")
# load("D:/my github/metTools/data/monaLib.rda")
# load("mona_spec")
# info <- lapply(mona_spec, function(x) x[[1]])
# 
# comment <- unlist(pbapply::pblapply(info, function(x){
#   as.character(x["Comments", 1])
# }))
# 
# 
# which(unlist(pbapply::pblapply(comment, function(x){
#   length(grep(pattern = "\\[c16h15n7-h\\]\\+", x = x)) > 0
# })))
# 
# 
# comment <- pbapply::pblapply(comment, function(x){
#   x <- stringr::str_split(string = x, pattern = "\\\"")[[1]]
#   x <- x[which(x != "" & x != " ")]
#   temp.idx <- grep("theoretical m/z", x)
#   if(length(temp.idx) > 0){
#     x <- x[-temp.idx]
#   }
#   x
# })
# 
# comment2 <- pbapply::pblapply(comment, function(x){
#   x <- stringr::str_split(string = x, pattern = "=", n = 2)
#   x <- do.call(rbind, x)
#   colnames(x) <- c("Item", "Value")
#   x
# })
# 
# load('comment2')
# item.list <- unique(unlist(pbapply::pblapply(comment2, function(x){
# x[,1]  
# })))
# 
# item.list <- item.list[-grep("\\+", item.list)]
# 
# item.list <- item.list[item.list %in% c("SMILES", "cas", "pubchem cid", "chemspider", "InChI", "accession", "exact mass", "submitter",
#                          "kegg", "chebi", "pubchem", "lipidmaps", "compound class", "lipidbank", "scientific name",
#                          "hmdb", "pubchem sid", "ionization energy", "ion type", "ms type", "ion mode", "fragment voltage",
#                          "cas number", "pubmed id", "adduct", "exactmass", "synonym", "PubChem", "collision energy spread",
#                          "formula", "accurate mass", "adduct ion name", "species", "collision energy voltage", "collision energy level",
#                          "smiles", "compound id", "instrumenttype", "nomralized collision energy")]
# 
# 
# rm(list = "comment2")
# # for(i in 123788:length(comment2)){
# #   cat(i, " ")
# #   x <- comment2[[i]]
# #   temp <- data.frame("Item" = item.list, "Value" = NA, stringsAsFactors = FALSE)
# #   x <- x[x[,1] %in% temp$Item,, drop = FALSE]
# #   temp[match(x[,1], temp[,1]),] <- x
# #   temp <- as.data.frame(t(temp))
# #   temp <- temp[2,,drop = FALSE]
# #   # temp
# # }
# 
# # for(i in 1:nrow())
# 
# 
# comment3 <- pbapply::pblapply(comment2, function(x){
#   temp <- data.frame("Item" = item.list, "Value" = NA, stringsAsFactors = FALSE)
#   x <- x[x[,1] %in% temp$Item,, drop = FALSE]
#   temp[match(x[,1], temp[,1]),] <- x
#   temp <- as.data.frame(t(temp))
#   temp <- temp[2,,drop = FALSE]
#   temp
# })
# 
# 
# save(comment3, file = "comment3")
# rm(list = "comment2")
# 
# 
# comment4 <- pbapply::pblapply(comment3, function(x){
#   x <- apply(x, 1, as.character)[,1,drop = TRUE]
#   x
# })
# 
# rm(list = "comment3")
# 
# 
# comment5 <- do.call(rbind, comment4)
# 
# colnames(comment5) <- item.list
# 
# save(comment5, file = "comment5")
# rm(list = "comment4")
# load("mona_spec")
# 
# spectra.data <- pbapply::pblapply(mona_spec, function(x){
#   x[[2]]
# })
# 
# rm(list = "mona_spec")
# 
# info <- pbapply::pblapply(mona_spec, function(x){
#   x <- x[[1]]
#   x <- x[which(!rownames(x) %in% c("Comments", "Num Peaks")),]
# })
# 
# item.list2 <- unique(unlist(pbapply::pblapply(info, function(x){
#   rownames(x)
# })))
# 
# item.list2 <- item.list2[item.list2 %in% c("Name", "Synon", "InChIKey", "Precursor_type", "Spectrum_type", 
#                                            "Instrument_type", "Instrument", "Ion_mode",
#                                         "Collision_energy", "Formula", "ExactMass")]
# 
# 
# 
# 
# 
# info <- pbapply::pblapply(info, function(x){
#   temp <- data.frame("Item" = item.list2, "Value" = NA, stringsAsFactors = FALSE)
#   x <- x[rownames(x) %in% temp$Item,, drop = FALSE]
#   temp[match(rownames(x), temp[,1]),2] <- x[,1]
#   temp <- as.data.frame(t(temp), stringsAsFactors = FALSE)
#   temp <- temp[2,,drop = FALSE]
#   temp
# })
# 
# info <- do.call(rbind, info)
# 
# colnames(info) <- item.list2
# 
# 
# info <- cbind(info, comment5)
# 
# rm(list = "comment5")
# save(info, file = "info")
# load("info")
# info2 <- t(apply(info, 1, as.character))
# colnames(info2) <- colnames(info)
# 
# rm(list = "info")
# 
# info2 <- as.data.frame(info2, stringsAsFactors = FALSE)
# 
# ###remove some spectra
# ##Not MS2 spectrum
# spectrum.type <- info2$Spectrum_type
# spectrum.type[is.na(spectrum.type)] <- ""
# remove.idx <- which(info2$Spectrum_type %in% c("MS", "MS1", "MS3", 'MS4'))
# info2 <- info2[-remove.idx,]
# spectra.data <- spectra.data[-remove.idx]
# 
# ##No polarity
# polarity <- info2[,c("Ion_mode", "ion mode")]
# polarity <- apply(polarity, 1, function(x){
#   x <- as.character(x)
#   if(all(is.na(x))) return(NA)
#   x <- x[!is.na(x)]
#   return(x[1])
# })
# 
# remove.idx <- which(is.na(polarity))
# info2 <- info2[-remove.idx,]
# spectra.data <- spectra.data[-remove.idx]
# 
# 
# ###nature product
# class <- info2$`compound class`
# class[is.na(class)] <- ""
# 
# remove.idx <- grep("Natural Product", class, value = FALSE)
# info2 <- info2[-remove.idx,]
# spectra.data <- spectra.data[-remove.idx]
# 
# 
# Compound.name <- info2$Name
# sum(is.na(Compound.name))
# 
# Compound.name <- info2$Name
# sum(is.na(Compound.name))
# 
# Synonym <- info2$Synon
# sum(is.na(Synonym))
# 
# Spectra.adduct <- info2[,c("Precursor_type", "adduct", "ion type")]
# 
# Spectra.adduct <- apply(Spectra.adduct, 1, function(x){
#   x <- as.character(x)
#   if(all(is.na(x))) return(NA)
#   x <- x[!is.na(x)]
#   return(x[1])
# })
# 
# Spectra.type <- info2$Spectrum_type
# 
# Instrument.type <- info2[,c("Instrument_type", "instrumenttype")]
# Instrument.type <- apply(Instrument.type, 1, function(x){
#   x <- as.character(x)
#   if(all(is.na(x))) return(NA)
#   x <- x[!is.na(x)]
#   return(x[1])
# })
# 
# Instrument <- info2$Instrument
# 
# Polarity <- info2[, c("Ion_mode", "ion mode")]
# Polarity <- apply(Polarity, 1, function(x){
#   x <- as.character(x)
#   if(all(is.na(x))) return(NA)
#   x <- x[!is.na(x)]
#   return(x[1])
# })
# 
# Polarity[Polarity == "P"] <- "Positive"
# Polarity[Polarity == "N"] <- "Negative"
# 
# CE <- info2[,c("Collision_energy","collision energy voltage")]
# 
# CE <- apply(CE, 1, function(x){
#   x <- as.character(x)
#   if(all(is.na(x))) return(NA)
#   x <- x[!is.na(x)]
#   return(x[1])
# })
# 
# CE <- stringr::str_replace_all(string = CE, pattern = "eV|V", replacement = "")
# CE <- stringr::str_trim(CE)
# 
# 
# Formula <- info2[,c("Formula", "formula")]
# 
# Formula <- apply(Formula, 1, function(x){
#   x <- as.character(x)
#   if(all(is.na(x))) return(NA)
#   x <- x[!is.na(x)]
#   return(x[1])
# })
# 
# 
# mz <- info2[,c("ExactMass", "exactmass", "accurate mass")]
# mz <- apply(mz, 1, function(x){
#   x <- as.character(x)
#   if(all(is.na(x))) return(NA)
#   x <- x[!is.na(x)]
#   return(x[1])
# })
# 
# mz <- as.numeric(mz)
# 
# SMILES <- info2[,c("SMILES", "smiles")]
# SMILES <- apply(SMILES, 1, function(x){
#   x <- as.character(x)
#   if(all(is.na(x))) return(NA)
#   x <- x[!is.na(x)]
#   return(x[1])
# })
# 
# 
# CAS.ID <- info2[,c("cas", "cas number")]
# 
# CAS.ID <- apply(CAS.ID, 1, function(x){
#   x <- as.character(x)
#   if(all(is.na(x))) return(NA)
#   x <- x[!is.na(x)]
#   return(x[1])
# })
# 
# PubChem.CID <- info2$`pubchem cid`
# 
# Chemspider.ID <- info2$chemspider
# 
# InChI <- info2$InChI
# 
# MoNA.ID <- info2$accession
# 
# Submitter <- info2$submitter
# 
# KEGG.ID <- info2$kegg
# 
# Chebi.ID <- info2$chebi
# 
# PubChem.ID <- info2[,c("pubchem", "pubmed id", "PubChem")]
# 
# PubChem.ID <- apply(PubChem.ID, 1, function(x){
#   x <- as.character(x)
#   if(all(is.na(x))) return(NA)
#   x <- x[!is.na(x)]
#   return(x[1])
# })
# 
# 
# Lipidmaps.ID <- info2$lipidmaps
# 
# 
# Compound.class <- info2$`compound class`
# 
# Lipidbank.ID <- info2$lipidbank
# 
# HMDB.ID <- info2$hmdb
# 
# PubChem.SID <- info2$`pubchem sid`
# 
# 
# InChIKey <- info2$InChIKey
# InChI <- info2$InChI
# Spectra.adduct <- info2$Precursor_type
# 
# 
# spectra.info <- data.frame(Lab.ID = NA, Compound.name, mz, RT = NA, CAS.ID, HMDB.ID, KEGG.ID, Formula, 
#                            CE, Chebi.ID, Chemspider.ID, Compound.class, InChI, InChIKey, Instrument,
#                            Instrument.type, Polarity, PubChem.CID, PubChem.ID, PubChem.SID, SMILES,
#                            Spectra.adduct, Spectra.type, Submitter, 
#                            Synonym, 
#                            stringsAsFactors = FALSE
#                            )
# 
# save(spectra.info, file = "spectra.info")
# save(spectra.data, file = "spectra.data")
# load("spectra.data")
# ##remove some spectrum
# remove.idx <- which(is.na(spectra.info$mz))
# 
# spectra.info <- spectra.info[-remove.idx,]
# spectra.data <- spectra.data[-remove.idx]
# 
# length(spectra.data)
# dim(spectra.info)
# 
# remove.idx <- which(is.na(spectra.info$Formula))
# 
# spectra.info <- spectra.info[-remove.idx,]
# spectra.data <- spectra.data[-remove.idx]
# length(spectra.data)
# dim(spectra.info)
# 
# remove.idx <- which(is.na(spectra.info$Compound.name))
# 
# 
# table(spectra.info$Submitter)
# 
# remove.idx <- c(grep("(Markus Kohlhoff)|(GNPS Collaboration)|(Arpana Vaniya)", spectra.info$Submitter))
# 
# table(spectra.info$Submitter[remove.idx])
# 
# spectra.info[grep("Arpana Vaniya", spectra.info$Submitter), "Compound.name"]
# 
# spectra.info <- spectra.info[-remove.idx,]
# spectra.data <- spectra.data[-remove.idx]
# 
# length(spectra.data)
# dim(spectra.info)
# 
# ##remove some duplicated spectra
# temp.id <- apply(spectra.info[,c(2, 8, 15, 17, 22, 24, 9)], 1, function(x){
#   paste(as.character(x), collapse = ";")
# })
# 
# unique.temp.id <- unique(temp.id)
# 
# temp.idx <- which(unlist(pbapply::pblapply(unique.temp.id, function(x){
#   sum(x == temp.id)
# })) > 1)
# 
# 
# remove.idx <- which(duplicated(temp.id))
# 
# spectra.info <- spectra.info[-remove.idx,]
# spectra.data <- spectra.data[-remove.idx]
# 
# length(spectra.data)
# dim(spectra.info)
# 
# 
# # spectra.info$CE <- stringr::str_repla
# 
# 
# 
# Lab.ID1 <- apply(spectra.info[,c(2, 8, 15, 17, 22, 24, 9)], 1, function(x){
#   paste(as.character(x), collapse = ";")
# })
# 
# Lab.ID2 <- apply(spectra.info[,c(2, 8, 15, 17, 22, 24)], 1, function(x){
#   paste(as.character(x), collapse = ";")
# })
# 
# 
# spectra.info$Lab.ID <- Lab.ID2
# 
# names(spectra.data) <- Lab.ID1
# 
# 
# spectra.pos <- spectra.data[grep("Positive", Lab.ID1)]
# spectra.neg <- spectra.data[grep("Negative", Lab.ID1)]
# 
# unique.lab.id.pos <- unique(grep("Positive", Lab.ID2, value = TRUE))
# unique.lab.id.neg <- unique(grep("Negative", Lab.ID2, value = TRUE))
# 
# 
# spectra.pos.name2 <- grep("Positive", Lab.ID2, value = TRUE)
# spectra.neg.name2 <- grep("Negative", Lab.ID2, value = TRUE)
# 
# 
# spectra.pos2 <- pbapply::pblapply(unique.lab.id.pos, function(x){
#   temp.idx <- which(x == spectra.pos.name2)
#   temp.spectra.pos <- spectra.pos[temp.idx]
#   temp.ce <- unlist(lapply(stringr::str_split(string = names(temp.spectra.pos), pattern = ";"), function(x){
#     tail(x, 1)
#   }))
#   
#   temp.ce[is.na(temp.ce)] <- paste("Unknown", 1:length(temp.ce[is.na(temp.ce)]), sep = "_")
#   names(temp.spectra.pos) <- temp.ce
#   temp.spectra.pos
# })
# 
# 
# 
# names(spectra.pos2) <- unique.lab.id.pos
# 
# 
# spectra.neg2 <- pbapply::pblapply(unique.lab.id.neg, function(x){
#   temp.idx <- which(x == spectra.neg.name2)
#   temp.spectra.neg <- spectra.neg[temp.idx]
#   temp.ce <- unlist(lapply(stringr::str_split(string = names(temp.spectra.neg), pattern = ";"), function(x){
#     tail(x, 1)
#   }))
#   
#   temp.ce[is.na(temp.ce)] <- paste("Unknown", 1:length(temp.ce[is.na(temp.ce)]), sep = "_")
#   names(temp.spectra.neg) <- temp.ce
#   temp.spectra.neg
# })
# 
# 
# 
# names(spectra.neg2) <- unique.lab.id.neg
# 
# 
# ###change ID
# colnames(spectra.info)
# Lab.ID <- spectra.info$Lab.ID
# 
# Lab.ID3 <- apply(spectra.info[,c(2, 8, 15, 22, 24)], 1, function(x){
#   paste(as.character(x), collapse = ";")
# })
# 
# 
# remove.idx <- which(duplicated(Lab.ID3))
# 
# Lab.ID <- Lab.ID[-remove.idx]
# Lab.ID3 <- Lab.ID3[-remove.idx]
# 
# spectra.info2 <- spectra.info[-remove.idx,]
# 
# spectra.info2$Lab.ID <- paste("MONA", 1:nrow(spectra.info2), sep = "_")
# 
# 
# sum(is.na(match(stringr::str_replace(string = names(spectra.pos2), pattern = "Positive;", replacement = ""),
#       Lab.ID3)))
# 
# 
# names(spectra.pos2) <- spectra.info2$Lab.ID[match(stringr::str_replace(string = names(spectra.pos2), pattern = "Positive;", replacement = ""),
#       Lab.ID3)]
# 
# 
# sum(is.na(match(stringr::str_replace(string = names(spectra.neg2), pattern = "Negative;", replacement = ""),
#                 Lab.ID3)))
# 
# 
# names(spectra.neg2) <- spectra.info2$Lab.ID[match(stringr::str_replace(string = names(spectra.neg2), pattern = "Negative;", replacement = ""),
#                                                   Lab.ID3)]
# 
# 
# intersect(names(spectra.pos2), names(spectra.neg2))
# 
# 
# Spectra <- list("Spectra.positive" = spectra.pos2,
#                 "Spectra.negative" = spectra.neg2)
# 
# 
# database.info <- list("Version" = "0.0.1",
#                       "Source" = "MoNA",
#                       "Link" = "http://mona.fiehnlab.ucdavis.edu/",
#                       "Creater" = "Xiaotao Shen",
#                       "Email" = "shenxt1990@163.com",
#                       "RT" = FALSE)
# 
# 
# 
# monaDatabase0.0.1 <- new(Class = "databaseClass", 
#                              database.info = database.info,
#                              spectra.info = spectra.info2,
#                              spectra.data = Spectra)
# 
# save(monaDatabase0.0.1, file = 'monaDatabase0.0.1', compress = "xz")
# 
