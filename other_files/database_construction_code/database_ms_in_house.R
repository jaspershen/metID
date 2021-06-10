# # # ###this code is ued for curation for MS2 library
# # # ##MoNA
# # # all_speatra_list <- readr::read_csv("D:/study/database and library/MoNA/MoNA-export-All_Spectra-identifier-table-ids/MoNA-export-All_Spectra-identifier-table.csv")
# # # all_spectra_image <- readr::read_csv("D:/study/database and library/MoNA/MoNA-export-All_Spectra-spectrum-images-png/MoNA-export-All_Spectra-spectrum-images.csv")
# # #
# # # mona_spec <- readMSP_MoNA(file = "D:/study/database and library/MoNA/MoNA-export-All_Spectra-msp/MoNA-export-All_Spectra.msp")
# # # save(mona_spec, file = "mona_spec", compress = "xz")
# # #
# # # #instrument_type
# # #
# # # instrument_type <- lapply(mona_spec, function(x){
# # #   as.character(x[[1]]["Instrument_type", 1])
# # # })
# # #
# # # instrument_type2 <- unlist(instrument_type)
# # #
# # # instrument_type3 <- instrument_type2[!is.na(instrument_type2)]
# # # table(instrument_type3)
# # #
# # # unique(instrument_type3)
# # #
# # #
# # # ####NIST
# # #
# # #
# # # ####METLIN
# # #
# # #
# # # ####zhulab in-house
# # #
# # #
# # #
# # #
# # # ####MassBank library
# # # setwd("D:/study/database and library/MassBank")
# # # file <- dir()
# # # massbank <- list()
# # #
# # # for(i in 1:length(file)){
# # #   cat(i, " ")
# # #   load(file[i])
# # #   massbank <- c(massbank, get(gsub(pattern = ".rdata", replacement = "", x = file[i])))
# # # }
# # #
# # # save(massbank, file = "massbank.rdata")
# # #
# # # massbank[[1]]
# # #
# # #
# # # instrument <- lapply(massbank, function(x){
# # #   x[[1]][9, 2]
# # # })
# # #
# # # instrument <- unlist(instrument)
# # # table(instrument)
# # #
# # # polarity <- lapply(massbank, function(x){
# # #   x[[1]][11, 2]
# # # })
# # #
# # # polarity <- unlist(polarity)
# # #
# # # table(polarity)
# # #
# # # ce <- lapply(massbank, function(x){
# # #   x[[1]][12, 2]
# # # })
# # #
# # # ce <- unlist(ce)
# # # table(ce)
# # #
# # #
# # #
# # #
# # #
# # #
# # # names(zhuMetlib)
# # #
# # # zhuMetlib.compound <- zhuMetlib[[1]]
# # # zhuMetlib.compound.pos <- zhuMetlib.compound[[1]]
# # # zhuMetlib.compound.neg <- zhuMetlib.compound[[2]]
# # #
# # # zhuMetlib.meta <- zhuMetlib[[2]]
# # # zhuMetlib.meta.compound <- zhuMetlib.meta[[1]]
# # # zhuMetlib.meta.pos <- zhuMetlib.meta[[2]]
# # # zhuMetlib.meta.neg <- zhuMetlib.meta[[3]]
# # #
# # #
# # #
# # #
# # # instrument <- lapply(massbank, function(x){
# # #   x[[1]][9, 2]
# # # })
# # #
# # # instrument <- unlist(instrument)
# # #
# # # ##only LC-ESI-QTOF, MALDI-TOFTOF, GC-EI-TOF, LC-ESI-ITTOF, ESI-ITTOF,APCI-ITTOF,LC-ESI-TOF
# # # #MALDI-TOF, CE-ESI-TOF, ESI-QTOF are remained
# # #
# # # idx <- which(instrument %in% c("LC-ESI-QTOF",
# # #                                "MALDI-TOFTOF", "GC-EI-TOF",
# # #                                "LC-ESI-ITTOF", "ESI-ITTOF",
# # #                                "APCI-ITTOF", "LC-ESI-TOF",
# # #                                "MALDI-TOF", "CE-ESI-TOF", "ESI-QTOF"
# # # ))
# # #
# # # massbank1 <- massbank[idx]
# # #
# # #
# # #
# # # polarity <- lapply(massbank1, function(x){
# # #   x[[1]][11, 2]
# # # })
# # #
# # # polarity <- unlist(polarity)
# # # massbank.pos <- massbank1[which(polarity == "POSITIVE")]
# # # massbank.neg <- massbank1[which(polarity == "NEGATIVE")]
# # #
# # #
# # # ce <- lapply(massbank1, function(x){
# # #   x[[1]][12, 2]
# # # })
# # #
# # # ce <- unlist(ce)
# # # table(ce)
# # #
# # # colnames(zhuMetlib.meta.compound)
# # #
# # # name <- unlist(lapply(massbank1, function(x){
# # #   x[[1]][1,2]
# # # }))
# # #
# # # name <- unlist(lapply(strsplit(name, split = ';'), function(x) x[1]))
# # #
# # # formula <- unlist(lapply(massbank1, function(x){
# # #   x[[1]][2,2]
# # # }))
# # #
# # # cas <- unlist(lapply(massbank1, function(x){
# # #   x[[1]][6,2]
# # # }))
# # #
# # # pubchem <- unlist(lapply(massbank1, function(x){
# # #   x[[1]][7,2]
# # # }))
# # #
# # # mz <- unlist(lapply(massbank1, function(x){
# # #   x[[1]][3,2]
# # # }))
# # #
# # # mz <- as.numeric(mz)
# # #
# # # labid <- paste("MB", 1:length(massbank1), sep = "")
# # #
# # # hmdb <- rep(NA, length(mz))
# # #
# # # colnames(zhuMetlib.meta.compound)
# # # meta.compound <- data.frame(labid, name, mz, formula, cas, hmdb, stringsAsFactors = FALSE)
# # #
# # # meta.pos <- data.frame(labid, mz, "10ppm", "qc1", stringsAsFactors = FALSE)[which(polarity == "POSITIVE"),]
# # # colnames(meta.pos)[3:4] <- c("ppm", "qc")
# # # meta.pos <- list(meta.pos)
# # # names(meta.pos) <- "30"
# # #
# # # meta.neg <- data.frame(labid, mz, "10ppm", "qc1", stringsAsFactors = FALSE)[which(polarity == "NEGATIVE"),]
# # # colnames(meta.neg)[3:4] <- c("ppm", "qc")
# # # meta.neg <- list(meta.neg)
# # # names(meta.neg) <- "30"
# # #
# # #
# # #
# # #
# # # compound.pos <- lapply(massbank.pos, function(x) x[[2]])
# # # compound.pos <- lapply(compound.pos, function(x){
# # #   colnames(x)[2] <- "intensity"
# # #   x <- list(x)
# # #   names(x) <- "30"
# # #   x
# # # })
# # #
# # # names(compound.pos) <- labid[which(polarity == "POSITIVE")]
# # #
# # # names(zhuMetlib)
# # #
# # # compound.neg <- lapply(massbank.neg, function(x) x[[2]])
# # # compound.neg <- lapply(compound.neg, function(x){
# # #   colnames(x)[2] <- "intensity"
# # #   x <- list(x)
# # #   names(x) <- "30"
# # #   x
# # # })
# # #
# # # names(compound.neg) <- labid[which(polarity == "NEGATIVE")]
# # # #
# # # # zhuMetlib.compound <- zhuMetlib[[1]]
# # # # zhuMetlib.compound.pos <- zhuMetlib.compound[[1]]
# # # # zhuMetlib.compound.neg <- zhuMetlib.compound[[2]]
# # # #
# # # # zhuMetlib.meta <- zhuMetlib[[2]]
# # # # zhuMetlib.meta.compound <- zhuMetlib.meta[[1]]
# # # # zhuMetlib.meta.pos <- zhuMetlib.meta[[2]]
# # # # zhuMetlib.meta.neg <- zhuMetlib.meta[[3]]
# # #
# # # compound <- list(compound.pos, compound.neg)
# # # names(compound) <- c("pos", "neg")
# # #
# # # meta <- list(meta.compound, meta.pos, meta.neg)
# # # names(meta) <- names(zhuMetlib.meta)
# # #
# # # massBankLib <- list(compound, meta)
# # # names(massBankLib) <- names(zhuMetlib)
# # #
# # #
# # # save(massBankLib, file = "massBankLib")
# # #
# # #
# # # names(massBankLib)
# # # names(zhuMetlib)
# # #
# # #
# # # names(massBankLib[[1]])
# # # names(zhuMetlib[[1]])
# # #
# # #
# # # names(massBankLib[[1]][[1]])
# # # names(zhuMetlib[[1]][[1]])
# # #
# # # names(massBankLib[[2]])
# # # names(zhuMetlib[[2]])
# # #
# # # names(massBankLib[[2]][[1]])
# # # names(zhuMetlib[[2]][[1]])
# # #
# # #
# # # names(massBankLib[[2]][[2]])
# # # names(zhuMetlib[[2]][[2]])
# # #
# # #
# # # load("mz")
# # #
# # # cbind(massBankLib$meta$compound$mz, mz)
# # #
# # # temp.idx <- match(massBankLib$meta$compound$name[which(is.na(mz))],
# # #                   unlist(lapply(massbank, function(x) strsplit(x$ms2.info[1,2], split = ";")[[1]][1])))
# # #
# # #
# # #
# # # mz[which(is.na(mz))] <-
# # #   as.numeric(unlist(lapply(massbank[temp.idx], function(x) x$ms2.info[3,2])))
# # #
# # #
# # #
# # # temp.idx.pos <- match(massBankLib$meta$pos$`30`$labid, massBankLib$meta$compound$labid)
# # # temp.idx.neg <- match(massBankLib$meta$neg$`30`$labid, massBankLib$meta$compound$labid)
# # #
# # #
# # # massBankLib$meta$pos$`30`$mz <-  massBankLib$meta$compound$mz[temp.idx.pos]
# # #
# # # massBankLib$meta$neg$`30`$mz <- massBankLib$meta$compound$mz[temp.idx.neg]
# # #
# # # massBankLib$meta$compound$mz <- mz
# # # save(massBankLib, file = "massBankLib")
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # 
# # ###Massbank data
# # setwd("D:/my github/metTools/data")
# # load("massbank.rdata")
# # instrument <- lapply(massbank, function(x){
# #   x[[1]][9, 2]
# # })
# # 
# # instrument <- unlist(instrument)
# # table(instrument)
# # 
# # ce <- unlist(lapply(massbank, function(x){
# #   x[[1]][12, 2]
# # }))
# # 
# # ##polarity
# # polarity <- unlist(lapply(massbank, function(x){
# #   x[[1]][11, 2]
# # }))
# # 
# # table(polarity)
# # 
# # massbank.pos <- massbank[which(polarity == "POSITIVE")]
# # massbank.neg <- massbank[which(polarity == "NEGATIVE")]
# # 
# # ##creat met.compound information
# # names(zhuMetlib)
# # zhuMetlib.compound <- zhuMetlib[[1]]
# # zhuMetlib.compound.pos <- zhuMetlib.compound[[1]]
# # zhuMetlib.compound.neg <- zhuMetlib.compound[[2]]
# # 
# # zhuMetlib.meta <- zhuMetlib[[2]]
# # zhuMetlib.meta.compound <- zhuMetlib.meta[[1]]
# # zhuMetlib.meta.pos <- zhuMetlib.meta[[2]]
# # zhuMetlib.meta.neg <- zhuMetlib.meta[[3]]
# # 
# # colnames(zhuMetlib.meta.compound)
# # 
# # name <- unlist(lapply(massbank, function(x){
# #   x[[1]][1,2]
# # }))
# # 
# # name <- unlist(lapply(strsplit(name, split = ';'), function(x) x[1]))
# # 
# # formula <- unlist(lapply(massbank, function(x){
# #   x[[1]][2,2]
# # }))
# # 
# # cas <- unlist(lapply(massbank, function(x){
# #   x[[1]][6,2]
# # }))
# # 
# # pubchem <- unlist(lapply(massbank, function(x){
# #   x[[1]][7,2]
# # }))
# # 
# # mz <- unlist(lapply(massbank, function(x){
# #   x[[1]][3,2]
# # }))
# # 
# # mz <- as.numeric(mz)
# # 
# # labid <- paste("MB", 1:length(massbank), sep = "")
# # 
# # hmdb <- rep(NA, length(mz))
# # 
# # massbank[[1]]$ms2.info$name
# # 
# # colnames(zhuMetlib.meta.compound)
# # smile <- unlist(lapply(massbank, function(x){
# #   x[[1]][4,2]
# # }))
# # 
# # iupac <- unlist(lapply(massbank, function(x){
# #   x[[1]][5,2]
# # }))
# # 
# # pubchem <- unlist(lapply(massbank, function(x){
# #   x[[1]][7,2]
# # }))
# # 
# # 
# # chemspider <- unlist(lapply(massbank, function(x){
# #   x[[1]][8,2]
# # }))
# # 
# # instrument.type <- unlist(lapply(massbank, function(x){
# #   x[[1]][9,2]
# # }))
# # 
# # 
# # instrument <- unlist(lapply(massbank, function(x){
# #   x[[1]][10,2]
# # }))
# # 
# # 
# # ce <- unlist(lapply(massbank, function(x){
# #   x[[1]][12,2]
# # }))
# # 
# # 
# # adduct <- unlist(lapply(massbank, function(x){
# #   x[[1]][14,2]
# # }))
# # 
# # precursor.mz <- adduct <- unlist(lapply(massbank, function(x){
# #   x[[1]][13,2]
# # }))
# # 
# # 
# # meta.compound <- data.frame(labid, name, mz, formula, cas, hmdb,
# #                             smile, iupac, pubchem, chemspider,
# #                             instrument.type, instrument, ce,
# #                             adduct, precursor.mz,
# #                             stringsAsFactors = FALSE)
# # 
# # meta.pos <- data.frame(labid, mz, "10ppm", "qc1",
# #                        stringsAsFactors = FALSE)[which(polarity == "POSITIVE"),]
# # colnames(meta.pos)[3:4] <- c("ppm", "qc")
# # meta.pos <- list(meta.pos)
# # names(meta.pos) <- "30"
# # 
# # meta.neg <- data.frame(labid, mz, "10ppm", "qc1", stringsAsFactors = FALSE)[which(polarity == "NEGATIVE"),]
# # colnames(meta.neg)[3:4] <- c("ppm", "qc")
# # meta.neg <- list(meta.neg)
# # names(meta.neg) <- "30"
# # 
# # compound.pos <- lapply(massbank.pos, function(x) x[[2]])
# # compound.pos <- lapply(compound.pos, function(x){
# #   colnames(x)[2] <- "intensity"
# #   x <- list(x)
# #   names(x) <- "30"
# #   x
# # })
# # 
# # names(compound.pos) <- labid[which(polarity == "POSITIVE")]
# # 
# # compound.neg <- lapply(massbank.neg, function(x) x[[2]])
# # compound.neg <- lapply(compound.neg, function(x){
# #   colnames(x)[2] <- "intensity"
# #   x <- list(x)
# #   names(x) <- "30"
# #   x
# # })
# # 
# # names(compound.neg) <- labid[which(polarity == "NEGATIVE")]
# # 
# # 
# # compound <- list(compound.pos, compound.neg)
# # names(compound) <- c("pos", "neg")
# # 
# # meta <- list(meta.compound, meta.pos, meta.neg)
# # names(meta) <- names(zhuMetlib.meta)
# # 
# # massBankLib <- list(compound, meta)
# # names(massBankLib) <- names(zhuMetlib)
# # 
# # 
# # save(massBankLib, file = "massBankLib.rda", compress = "xz")
# # 
# # 
# # ####MoNA
# # setwd("D:/study/database and library/MoNA/")
# # mona_spec <- readMSP_MoNA(file = "D:/study/database and library/MoNA/MoNA-export-All_Spectra-msp/MoNA-export-All_Spectra.msp")
# # 
# # save(mona_spec, file = "mona_spec", compress = "xz")
# # # load("mona_spec")
# # ##polarity
# # polarity <- unlist(lapply(mona_spec, function(x){
# #   as.character(x[[1]]["Ion_mode", 1])
# # }))
# # 
# # table(polarity)
# # ###peak with no polarity are removed
# # mona_spec <- mona_spec[which(!is.na(polarity))]
# # mona.pos <- mona_spec[which(polarity == "P")]
# # mona.neg <- mona_spec[which(polarity == "N")]
# # 
# # ##creat met.compound information
# # names(zhuMetlib)
# # zhuMetlib.compound <- zhuMetlib[[1]]
# # zhuMetlib.compound.pos <- zhuMetlib.compound[[1]]
# # zhuMetlib.compound.neg <- zhuMetlib.compound[[2]]
# # 
# # zhuMetlib.meta <- zhuMetlib[[2]]
# # zhuMetlib.meta.compound <- zhuMetlib.meta[[1]]
# # zhuMetlib.meta.pos <- zhuMetlib.meta[[2]]
# # zhuMetlib.meta.neg <- zhuMetlib.meta[[3]]
# # 
# # colnames(zhuMetlib.meta.compound)
# # 
# # name <- unlist(lapply(mona_spec, function(x){
# #   x[[1]]["Name",1]
# # }))
# # sum(is.na(name))
# # temp.idx1 <- which(!is.na(name))
# # # name <- unlist(lapply(strsplit(name, split = ';'), function(x) x[1]))
# # 
# # formula <- unlist(lapply(mona_spec, function(x){
# #   x[[1]]["Formula",1]
# # }))
# # 
# # sum(is.na(formula))
# # temp.idx2 <- which(!is.na(formula))
# # 
# # 
# # 
# # cas <- rep(NA, length(mona_spec))
# # 
# # pubchem <- rep(NA, length(mona_spec))
# # 
# # mz <- unlist(lapply(mona_spec, function(x){
# #   x[[1]]["ExactMass",1]
# # }))
# # 
# # mz <- as.numeric(mz)
# # temp.idx3 <- which(!is.na(mz))
# # 
# # hmdb <- rep(NA, length(mona_spec))
# # 
# # temp.idx <- intersect(intersect(temp.idx1, temp.idx2), temp.idx3)
# # 
# # mona_spec <- mona_spec[temp.idx]
# # labid <- paste("MONA", 1:length(mona_spec), sep = "")
# # 
# # ##polarity
# # polarity <- unlist(lapply(mona_spec, function(x){
# #   as.character(x[[1]]["Ion_mode", 1])
# # }))
# # 
# # table(polarity)
# # 
# # mona.pos <- mona_spec[which(polarity == "P")]
# # mona.neg <- mona_spec[which(polarity == "N")]
# # 
# # 
# # name <- unlist(lapply(mona_spec, function(x){
# #   x[[1]]["Name",1]
# # }))
# # sum(is.na(name))
# # 
# # formula <- unlist(lapply(mona_spec, function(x){
# #   x[[1]]["Formula",1]
# # }))
# # 
# # sum(is.na(formula))
# # 
# # cas <- rep(NA, length(mona_spec))
# # 
# # smile <- rep(NA, length(mona_spec))
# # 
# # pubchem <- rep(NA, length(mona_spec))
# # 
# # mz <- unlist(lapply(mona_spec, function(x){
# #   x[[1]]["ExactMass",1]
# # }))
# # 
# # mz <- as.numeric(mz)
# # 
# # hmdb <- rep(NA, length(mona_spec))
# # 
# # rownames(mona_spec[[1]]$info)
# # 
# # colnames(zhuMetlib.meta.compound)
# # 
# # InChIKey <- unlist(lapply(mona_spec, function(x){
# #   as.character(x[[1]]["InChIKey",1])
# # }))
# # 
# # 
# # instrument.type <- unlist(lapply(mona_spec, function(x){
# #   x[[1]]["Instrument_type",1]
# # }))
# # 
# # 
# # instrument <- unlist(lapply(mona_spec, function(x){
# #   x[[1]]["Instrument",1]
# # }))
# # 
# # 
# # ce <- unlist(lapply(mona_spec, function(x){
# #   x[[1]]["Collision_energy",1]
# # }))
# # 
# # 
# # adduct <- unlist(lapply(mona_spec, function(x){
# #   x[[1]]["Precursor_type",1]
# # }))
# # 
# # 
# # comment <- unlist(lapply(mona_spec, function(x){
# #   x[[1]]["Comments",1]
# # }))
# # 
# # precursor.mz <- unlist(lapply(mona_spec, function(x){
# #   x[[1]]["PrecursorMZ",1]
# # }))
# # 
# # 
# # meta.compound <- data.frame(labid, name, mz, formula, cas, hmdb,
# #                             smile, adduct,
# #                             instrument.type, instrument, ce,
# #                             adduct, precursor.mz, comment,
# #                             stringsAsFactors = FALSE)
# # 
# # meta.pos <- data.frame(labid, mz, "10ppm", "qc1",
# #                        stringsAsFactors = FALSE)[which(polarity == "P"),]
# # colnames(meta.pos)[3:4] <- c("ppm", "qc")
# # meta.pos <- list(meta.pos)
# # names(meta.pos) <- "30"
# # 
# # meta.neg <- data.frame(labid, mz, "10ppm", "qc1",
# #                        stringsAsFactors = FALSE)[which(polarity == "N"),]
# # colnames(meta.neg)[3:4] <- c("ppm", "qc")
# # meta.neg <- list(meta.neg)
# # names(meta.neg) <- "30"
# # 
# # compound.pos <- lapply(mona.pos, function(x) x[[2]])
# # compound.pos <- lapply(compound.pos, function(x){
# #   colnames(x)[2] <- "intensity"
# #   x <- list(x)
# #   names(x) <- "30"
# #   x
# # })
# # 
# # names(compound.pos) <- labid[which(polarity == "P")]
# # 
# # compound.neg <- lapply(mona.neg, function(x) x[[2]])
# # compound.neg <- lapply(compound.neg, function(x){
# #   colnames(x)[2] <- "intensity"
# #   x <- list(x)
# #   names(x) <- "30"
# #   x
# # })
# # 
# # names(compound.neg) <- labid[which(polarity == "N")]
# # 
# # 
# # compound <- list(compound.pos, compound.neg)
# # names(compound) <- c("pos", "neg")
# # 
# # meta <- list(meta.compound, meta.pos, meta.neg)
# # names(meta) <- names(zhuMetlib.meta)
# # 
# # monaLib <- list(compound, meta)
# # names(monaLib) <- names(zhuMetlib)
# # 
# # 
# # save(monaLib, file = "monaLib.rda", compress = "xz")
# # 
# # 
# # 
# # 
# # ###metlin
# # setwd("D:/study/database and library/metlin")
# # load("metlinlib.rdata")
# # names(metlinlib)
# # metlinLib <- metlinlib[[1]]
# # names(metlinLib)
# # 
# # head(metlinLib$meta$compound)
# # 
# # temp.pos <- mapply(function(x,y){
# #   names(x) <- stringr::str_c(y, names(x), sep = "_")
# #   x
# # },
# # x = metlinLib$compound$pos,
# # y = names(metlinLib$compound$pos))
# # 
# # temp.pos <- do.call(what = c, args = temp.pos)
# # 
# # names(temp.pos) <- unlist(lapply(stringr::str_split(string = names(temp.pos), pattern = "\\."), function(x){
# #   x[2]
# # }))
# # 
# # 
# # 
# # 
# # temp.neg <- mapply(function(x,y){
# #   names(x) <- stringr::str_c(y, names(x), sep = "_")
# #   x
# # },
# # x = metlinLib$compound$neg,
# # y = names(metlinLib$compound$neg))
# # 
# # temp.neg <- do.call(what = c, args = temp.neg)
# # 
# # names(temp.neg) <- unlist(lapply(stringr::str_split(string = names(temp.neg), pattern = "\\."), function(x){
# #   x[2]
# # }))
# # 
# # 
# # temp.name <- unique(c(names(temp.pos), names(temp.neg)))
# # 
# # 
# # temp.compound <- metlinLib$meta$compound
# # 
# # temp.compound <- apply(temp.compound, 1, function(x){
# #   x <- t(data.frame(x))
# #   x <- as.data.frame(x)
# #   new.id <- grep(pattern = x[1,1], x = temp.name, value = TRUE)
# #   x <- x[rep(1, length(new.id)),]
# #   x$labid <- new.id
# #   rownames(x) <- NULL
# #   x
# # })
# # 
# # temp.compound <- do.call(rbind, temp.compound)
# # 
# # rownames(temp.compound) <- NULL
# # 
# # names(metlinLib$meta$pos)
# # 
# # 
# # temp.meta.pos <- metlinLib$meta$pos
# # temp.meta.neg <- metlinLib$meta$neg
# # 
# # temp.meta.pos <- mapply(function(x,y){
# #   x$labid <- stringr::str_c(x$labid, y, sep = "_")
# #   list(x)
# # },
# # x = temp.meta.pos,
# # y = names(temp.meta.pos))
# # 
# # temp.meta.neg <- mapply(function(x,y){
# #   x$labid <- stringr::str_c(x$labid, y, sep = "_")
# #   list(x)
# # },
# # x = temp.meta.neg,
# # y = names(temp.meta.neg))
# # 
# # 
# # temp.meta.pos <- do.call(rbind, args = temp.meta.pos)
# # temp.meta.neg <- do.call(rbind, args = temp.meta.neg)
# # 
# # rownames(temp.meta.pos) <- NULL
# # rownames(temp.meta.neg) <- NULL
# # 
# # 
# # length(temp.pos)
# # dim(temp.meta.pos)
# # 
# # length(temp.neg)
# # dim(temp.meta.neg)
# # 
# # temp.pos <- lapply(temp.pos, function(x){
# #   x <- list(x)
# #   names(x) <- "30"
# #   x
# # })
# # 
# # 
# # temp.neg <- lapply(temp.neg, function(x){
# #   x <- list(x)
# #   names(x) <- "30"
# #   x
# # })
# # 
# # 
# # temp.meta.pos <- list(temp.meta.pos)
# # names(temp.meta.pos) <- "30"
# # 
# # temp.meta.neg <- list(temp.meta.neg)
# # names(temp.meta.neg) <- "30"
# # 
# # metlinLib$compound$pos <- temp.pos
# # metlinLib$compound$neg <- temp.neg
# # 
# # metlinLib$meta$compound <- temp.compound
# # metlinLib$meta$pos <- temp.meta.pos
# # metlinLib$meta$neg <- temp.meta.neg
# # 
# # save(metlinLib, file = "metlibLib.rda", compress = "xz")
# # 
# # 
# # ##zhu_nist
# # setwd("D:/study/database and library/zhu_nist")
# # load("zhuMetlib.rda")
# # names(zhuMetlib)
# # nistLib <- zhuMetlib
# # names(nistLib)
# # 
# # head(nistLib$meta$compound)
# # 
# # 
# # 
# # temp.pos <- mapply(function(x,y){
# #   names(x) <- stringr::str_c(y, names(x), sep = "_")
# #   x
# # },
# # x = nistLib$compound$pos,
# # y = names(nistLib$compound$pos))
# # 
# # temp.pos <- do.call(what = c, args = temp.pos)
# # 
# # names(temp.pos) <- unlist(lapply(stringr::str_split(string = names(temp.pos), pattern = "\\."), function(x){
# #   x[2]
# # }))
# # 
# # 
# # # for(i in 1:length(nistLib$compound$neg)){
# # #   cat(i, " ")
# # #   x <- nistLib$compound$neg[[i]]
# # #   y <- names(nistLib$compound$neg)[i]
# # #   if(length(x) != 0){
# # #     names(x) <- stringr::str_c(y, names(x), sep = "_")
# # # }
# # #
# # # }
# # 
# # temp.neg <- mapply(function(x,y){
# #   if(length(x) != 0){
# #     names(x) <- stringr::str_c(y, names(x), sep = "_")
# #     x
# #   }
# # },
# # x = nistLib$compound$neg,
# # y = names(nistLib$compound$neg))
# # 
# # temp.neg <- do.call(what = c, args = temp.neg)
# # 
# # names(temp.neg) <- unlist(lapply(stringr::str_split(string = names(temp.neg), pattern = "\\."), function(x){
# #   x[2]
# # }))
# # 
# # 
# # temp.name <- unique(c(names(temp.pos), names(temp.neg)))
# # 
# # 
# # temp.compound <- nistLib$meta$compound
# # 
# # temp.compound <- apply(temp.compound, 1, function(x){
# #   x <- t(data.frame(x))
# #   x <- as.data.frame(x)
# #   new.id <- grep(pattern = x[1,1], x = temp.name, value = TRUE)
# #   x <- x[rep(1, length(new.id)),]
# #   x$labid <- new.id
# #   rownames(x) <- NULL
# #   x
# # })
# # 
# # temp.compound <- do.call(rbind, temp.compound)
# # 
# # rownames(temp.compound) <- NULL
# # 
# # names(nistLib$meta$pos)
# # 
# # 
# # temp.meta.pos <- nistLib$meta$pos
# # temp.meta.neg <- nistLib$meta$neg
# # 
# # temp.meta.pos <- mapply(function(x,y){
# #   x$labid <- stringr::str_c(x$labid, y, sep = "_")
# #   list(x)
# # },
# # x = temp.meta.pos,
# # y = names(temp.meta.pos))
# # 
# # temp.meta.neg <- mapply(function(x,y){
# #   x$labid <- stringr::str_c(x$labid, y, sep = "_")
# #   list(x)
# # },
# # x = temp.meta.neg,
# # y = names(temp.meta.neg))
# # 
# # 
# # temp.meta.pos <- do.call(rbind, args = temp.meta.pos)
# # temp.meta.neg <- do.call(rbind, args = temp.meta.neg)
# # 
# # rownames(temp.meta.pos) <- NULL
# # rownames(temp.meta.neg) <- NULL
# # 
# # 
# # length(temp.pos)
# # dim(temp.meta.pos)
# # 
# # length(temp.neg)
# # dim(temp.meta.neg)
# # 
# # temp.pos <- lapply(temp.pos, function(x){
# #   x <- list(x)
# #   names(x) <- "30"
# #   x
# # })
# # 
# # 
# # temp.neg <- lapply(temp.neg, function(x){
# #   x <- list(x)
# #   names(x) <- "30"
# #   x
# # })
# # 
# # 
# # temp.meta.pos <- list(temp.meta.pos)
# # names(temp.meta.pos) <- "30"
# # 
# # temp.meta.neg <- list(temp.meta.neg)
# # names(temp.meta.neg) <- "30"
# # 
# # nistLib$compound$pos <- temp.pos
# # nistLib$compound$neg <- temp.neg
# # 
# # nistLib$meta$compound <- temp.compound
# # nistLib$meta$pos <- temp.meta.pos
# # nistLib$meta$neg <- temp.meta.neg
# # 
# # save(nistLib, file = "nistLib.rda", compress = "xz")
# # 
# # 
# # 
# # ###orbitrap
# # setwd("D:/study/database and library/nist_orbitrap")
# # load("orbitrapMetlib.rda")
# # names(orbitrapMetlib)
# # orbitrapLib <- orbitrapMetlib
# # names(orbitrapLib)
# # 
# # head(orbitrapLib$meta$compound)
# # 
# # 
# # 
# # temp.pos <- mapply(function(x,y){
# #   names(x) <- stringr::str_c(y, names(x), sep = "_")
# #   x
# # },
# # x = orbitrapLib$compound$pos,
# # y = names(orbitrapLib$compound$pos))
# # 
# # temp.pos <- do.call(what = c, args = temp.pos)
# # 
# # names(temp.pos) <- unlist(lapply(stringr::str_split(string = names(temp.pos), pattern = "\\."), function(x){
# #   x[2]
# # }))
# # 
# # 
# # # for(i in 1:length(orbitrapLib$compound$neg)){
# # #   cat(i, " ")
# # #   x <- orbitrapLib$compound$neg[[i]]
# # #   y <- names(orbitrapLib$compound$neg)[i]
# # #   if(length(x) != 0){
# # #     names(x) <- stringr::str_c(y, names(x), sep = "_")
# # # }
# # #
# # # }
# # 
# # temp.neg <- mapply(function(x,y){
# #   if(length(x) != 0){
# #     names(x) <- stringr::str_c(y, names(x), sep = "_")
# #     x
# #   }
# # },
# # x = orbitrapLib$compound$neg,
# # y = names(orbitrapLib$compound$neg))
# # 
# # temp.neg <- do.call(what = c, args = temp.neg)
# # 
# # names(temp.neg) <- unlist(lapply(stringr::str_split(string = names(temp.neg), pattern = "\\."), function(x){
# #   x[2]
# # }))
# # 
# # 
# # temp.name <- unique(c(names(temp.pos), names(temp.neg)))
# # 
# # 
# # temp.compound <- orbitrapLib$meta$compound
# # 
# # temp.compound <- apply(temp.compound, 1, function(x){
# #   x <- t(data.frame(x))
# #   x <- as.data.frame(x)
# #   new.id <- grep(pattern = x[1,1], x = temp.name, value = TRUE)
# #   x <- x[rep(1, length(new.id)),]
# #   x$labid <- new.id
# #   rownames(x) <- NULL
# #   x
# # })
# # 
# # temp.compound <- do.call(rbind, temp.compound)
# # 
# # rownames(temp.compound) <- NULL
# # 
# # names(orbitrapLib$meta$pos)
# # 
# # 
# # temp.meta.pos <- orbitrapLib$meta$pos
# # temp.meta.neg <- orbitrapLib$meta$neg
# # 
# # temp.meta.pos <- mapply(function(x,y){
# #   x$labid <- stringr::str_c(x$labid, y, sep = "_")
# #   list(x)
# # },
# # x = temp.meta.pos,
# # y = names(temp.meta.pos))
# # 
# # temp.meta.neg <- mapply(function(x,y){
# #   x$labid <- stringr::str_c(x$labid, y, sep = "_")
# #   list(x)
# # },
# # x = temp.meta.neg,
# # y = names(temp.meta.neg))
# # 
# # 
# # temp.meta.pos <- do.call(rbind, args = temp.meta.pos)
# # temp.meta.neg <- do.call(rbind, args = temp.meta.neg)
# # 
# # rownames(temp.meta.pos) <- NULL
# # rownames(temp.meta.neg) <- NULL
# # 
# # 
# # length(temp.pos)
# # dim(temp.meta.pos)
# # 
# # length(temp.neg)
# # dim(temp.meta.neg)
# # 
# # temp.pos <- lapply(temp.pos, function(x){
# #   x <- list(x)
# #   names(x) <- "30"
# #   x
# # })
# # 
# # 
# # temp.neg <- lapply(temp.neg, function(x){
# #   x <- list(x)
# #   names(x) <- "30"
# #   x
# # })
# # 
# # 
# # temp.meta.pos <- list(temp.meta.pos)
# # names(temp.meta.pos) <- "30"
# # 
# # temp.meta.neg <- list(temp.meta.neg)
# # names(temp.meta.neg) <- "30"
# # 
# # orbitrapLib$compound$pos <- temp.pos
# # orbitrapLib$compound$neg <- temp.neg
# # 
# # orbitrapLib$meta$compound <- temp.compound
# # orbitrapLib$meta$pos <- temp.meta.pos
# # orbitrapLib$meta$neg <- temp.meta.neg
# # 
# # save(orbitrapLib, file = "orbitrapLib.rda", compress = "xz")
# # 
# # 
# # ###HMDB
# # setwd("D:/study/database and library/HMDB/hmdb_experimental_msms_spectra")
# # library(XML)
# # library("methods")
# # 
# # file <- dir()
# # hmdbLib <- vector(mode = "list", length = length(file))
# # for(i in 1:length(file)){
# #   cat(i, " ")
# #   temp.file <- file[i]
# #   result <- xmlParse(file = temp.file)
# #   rootnode <- xmlRoot(result)
# #   info <- xmlSApply(rootnode, function(x) xmlSApply(x, xmlValue))
# #   names(info) <- unname(names(rootnode))
# #   info <- lapply(info, unname)
# #   ms2.mz <- sapply(getNodeSet(doc = rootnode[[26]], path = "//mass-charge"), xmlValue)
# #   ms2.int <- sapply(getNodeSet(doc = rootnode[[26]], path = "//intensity"), xmlValue)
# #   ms2 <- data.frame("mz" = ms2.mz, "intensity" = ms2.int,
# #                     stringsAsFactors = FALSE)
# #   info[[26]] <- ms2
# # 
# #   ms2.info <- info[c(10, 17, 18, 24, 25)]
# #   ms2.info <- lapply(ms2.info, function(x){
# #     if(length(x) == 0){
# #       x <- NA
# #     }
# #     x
# #   })
# #   ms2.spec <- info[[26]]
# # 
# #   ms2.info <- do.call(rbind, ms2.info)
# #   ms2.info <- data.frame("name" = rownames(ms2.info), "value" = ms2.info[,1],
# #                          stringsAsFactors = FALSE)
# #   ms2.info <- tibble::as_tibble(ms2.info)
# # 
# #   temp.spec <- list(ms2.info = ms2.info, ms2.spec = ms2.spec)
# #   hmdbLib[[i]] <- temp.spec
# # 
# # }
# # 
# # hmdb <- hmdbLib
# # save(hmdb, file = "hmdb")
# # load("hmdb")
# # ##polarity
# # polarity <- unlist(lapply(hmdb, function(x){
# #   x[[1]][3, 2]
# # }))
# # 
# # temp.idx1 <- which(polarity != "N/A")
# # 
# # table(polarity)
# # 
# # hmdb <- hmdb[temp.idx1]
# # 
# # polarity <- unlist(lapply(hmdb, function(x){
# #   x[[1]][3, 2]
# # }))
# # 
# # polarity <- stringr::str_to_lower(polarity)
# # 
# # instrument <- lapply(hmdb, function(x){
# #   x[[1]][1, 2]
# # })
# # 
# # instrument <- unlist(instrument)
# # table(instrument)
# # 
# # ce <- unlist(lapply(hmdb, function(x){
# #   x[[1]][2, 2]
# # }))
# # 
# # sum(is.na(ce))
# # 
# # 
# # 
# # ##creat met.compound information
# # names(zhuMetlib)
# # zhuMetlib.compound <- zhuMetlib[[1]]
# # zhuMetlib.compound.pos <- zhuMetlib.compound[[1]]
# # zhuMetlib.compound.neg <- zhuMetlib.compound[[2]]
# # 
# # zhuMetlib.meta <- zhuMetlib[[2]]
# # zhuMetlib.meta.compound <- zhuMetlib.meta[[1]]
# # zhuMetlib.meta.pos <- zhuMetlib.meta[[2]]
# # zhuMetlib.meta.neg <- zhuMetlib.meta[[3]]
# # 
# # colnames(zhuMetlib.meta.compound)
# # 
# # hmdb.id <- unlist(lapply(hmdb, function(x){
# #   x[[1]][4,2]
# # }))
# # 
# # hmdb.id <- unname(hmdb.id)
# # hmdb.id <- stringr::str_replace(string = hmdb.id, pattern = "00", replacement = "")
# # 
# # 
# # load("G:/Project/MetDNA/annotation and identification/annotation/HMDB compound database/hmdbAllinf.rda")
# # 
# # name <- as.character(hmdbAllinf$Name[match(hmdb.id, hmdbAllinf$HMDBID)])
# # sum(is.na(name))
# # 
# # 
# # formula <- as.character(hmdbAllinf$Formula[match(hmdb.id, hmdbAllinf$HMDBID)])
# # 
# # cas <- as.character(hmdbAllinf$CAS_Registry_Number[match(hmdb.id, hmdbAllinf$HMDBID)])
# # 
# # pubchem <- rep(NA, length(name))
# # cas <- as.character(hmdbAllinf$CAS_Registry_Number[match(hmdb.id, hmdbAllinf$HMDBID)])
# # 
# # 
# # mz <- as.character(hmdbAllinf$MonoisotopicMass[match(hmdb.id, hmdbAllinf$HMDBID)])
# # 
# # mz <- as.numeric(mz)
# # 
# # labid <- paste("HM", 1:length(hmdb), sep = "")
# # 
# # # hmdb.id <- rep(NA, length(mz))
# # 
# # hmdb[[1]]$ms2.info$name
# # 
# # colnames(zhuMetlib.meta.compound)
# # 
# # metlinid <- as.character(hmdbAllinf$MetlinID[match(hmdb.id, hmdbAllinf$HMDBID)])
# # 
# # 
# # keggid <- as.character(hmdbAllinf$KEGGID[match(hmdb.id, hmdbAllinf$HMDBID)])
# # 
# # smpdbid <- as.character(hmdbAllinf$SMPDB_ID[match(hmdb.id, hmdbAllinf$HMDBID)])
# #   smile <- as.character(hmdbAllinf$Smiles[match(hmdb.id, hmdbAllinf$HMDBID)])
# # 
# #   InChiKey <- as.character(hmdbAllinf$InChiKey[match(hmdb.id, hmdbAllinf$HMDBID)])
# #   ChemSpider_ID <- as.character(hmdbAllinf$ChemSpider_ID[match(hmdb.id, hmdbAllinf$HMDBID)])
# # 
# # 
# # meta.compound <- data.frame(labid, name, mz, formula, cas, hmdb.id,
# #                             smile, ChemSpider_ID, metlinid, keggid, smpdbid,
# #                             instrument, ce,InChiKey,
# #                             stringsAsFactors = FALSE)
# # meta.compound <- meta.compound[which(!is.na(name) & !is.na(mz) & !is.na(formula)),]
# # 
# # polarity <- polarity[which(!is.na(name) & !is.na(mz) & !is.na(formula))]
# # 
# # hmdb.pos <- hmdb[which(polarity == "positive")]
# # hmdb.neg <- hmdb[which(polarity == "negative")]
# # 
# # meta.pos <- data.frame(
# # "labid" = meta.compound$labid,
# # "mz" = meta.compound$mz,
# # "10ppm", "qc1",
# #  stringsAsFactors = FALSE)[which(polarity == "positive"),]
# # colnames(meta.pos)[3:4] <- c("ppm", "qc")
# # meta.pos <- list(meta.pos)
# # names(meta.pos) <- "30"
# # 
# # meta.neg <- data.frame(
# #   "labid" = meta.compound$labid,
# #   "mz" = meta.compound$mz,
# #   "10ppm", "qc1",
# #   stringsAsFactors = FALSE)[which(polarity == "negative"),]
# # 
# # colnames(meta.neg)[3:4] <- c("ppm", "qc")
# # meta.neg <- list(meta.neg)
# # names(meta.neg) <- "30"
# # 
# # compound.pos <- lapply(hmdb.pos, function(x) x[[2]])
# # 
# # 
# # 
# # compound.pos <- lapply(compound.pos, function(x){
# #   if(nrow(x) == 0) return(NA)
# #   colnames(x)[2] <- "intensity"
# #   x <- list(x)
# #   names(x) <- "30"
# #   x
# # })
# # 
# # names(compound.pos) <- labid[which(polarity == "positive")]
# # 
# # temp.idx.pos <- which(!unlist(lapply(compound.pos, is.na)))
# # compound.pos <- compound.pos[temp.idx.pos]
# # 
# # compound.neg <- lapply(hmdb.neg, function(x) x[[2]])
# # compound.neg <- lapply(compound.neg, function(x){
# #   colnames(x)[2] <- "intensity"
# #   x <- list(x)
# #   names(x) <- "30"
# #   x
# # })
# # 
# # names(compound.neg) <- labid[which(polarity == "negative")]
# # 
# # 
# # compound <- list(compound.pos, compound.neg)
# # names(compound) <- c("pos", "neg")
# # 
# # meta <- list(meta.compound, meta.pos, meta.neg)
# # names(meta) <- names(zhuMetlib.meta)
# # 
# # hmdbLib <- list(compound, meta)
# # names(hmdbLib) <- names(zhuMetlib)
# # 
# # 
# # save(hmdbLib, file = "hmdbLib.rda", compress = "xz")
# # 
# # 
# # 
# # 
# 
# 
# # library(magrittr)
# # 
# # names(hmdbDatabase0.0.1@spectra.data$Spectra.positive) %in% hmdbDatabase0.0.1@spectra.info$Lab.ID %>% 
# # which(.) %>%
# #   `[` (hmdbDatabase0.0.1@spectra.info$HMDB.ID, .) %>%
# #   unique(.) %>%
# #   length
# # 
# # 
# # names(hmdbDatabase0.0.1@spectra.data$Spectra.negative) %in% hmdbDatabase0.0.1@spectra.info$Lab.ID %>% 
# #   which(.) %>%
# #   `[` (hmdbDatabase0.0.1@spectra.info$HMDB.ID, .) %>%
# #   unique(.) %>%
# #   length
# 
# 
# 
# 
# ##
# # sxtTools::setwd_project()
# # setwd("ms2_database/")
# # dir()
# # load("hmdbDatabase0.0.1")
# # hmdbDatabase0.0.1
# # 
# # hmdbDatabase0.0.2 <- new(Class = "databaseClass",
# #                          database.info = hmdbDatabase0.0.1@database.info,
# #                          spectra.info = hmdbDatabase0.0.1@spectra.info,
# #                          spectra.data = hmdbDatabase0.0.1@spectra.data
# #                          )
# # 
# # 
# # hmdbDatabase0.0.2@database.info$Version <- "0.0.2"
# # 
# # save(hmdbDatabase0.0.2, file = "hmdbDatabase0.0.2", compress = "xz")
# # 
# # 
# # 
# 
# 
# 
# 
# 
# 
# 
# # ###########---------------------------------------------------------------------
# # #
# # sxtTools::setwd_project()
# # setwd("ms2_database/")
# # rm(list = ls())
# # 
# # #-------------------------------------------------
# # ##msDatabase_hilic0.0.1 to msDatabase_hilic0.0.2
# # load("msDatabase_hilic0.0.1")
# # msDatabase_hilic0.0.1
# # 
# # names(msDatabase_hilic0.0.1)
# # database.info <- 
# #   msDatabase_hilic0.0.1@database.info
# # spectra.info <- 
# #   msDatabase_hilic0.0.1@spectra.info
# # spectra.data <- 
# #   msDatabase_hilic0.0.1@spectra.data
# # 
# # 
# # spectra.info$Compound.name
# # 
# # 
# # remove_idx <- 
# #   which(is.na(spectra.info$Compound.name))
# # 
# # if(length(remove_idx) > 0){
# #   spectra.info <- 
# #     spectra.info[-remove_idx,]
# # }
# # 
# # 
# # ##--------------------------------------------------------------------------
# # ###change the name with like (1) 
# # temp_str <- 
# #   "\\([0-9]\\){1,2}$"
# # 
# #   spectra.info$Compound.name %>% 
# #   stringr::str_subset(temp_str)
# # 
# # 
# #   spectra.info$Compound.name <- 
# #     spectra.info$Compound.name %>% 
# #     stringr::str_replace(temp_str, "")
# # 
# # 
# #   ##--------------------------------------------------------------------------
# #   ###remove another name
# #   spectra.info$Compound.name[spectra.info$Compound.name == "NMMA (Tilarginine)"] <-
# #     "Tilarginine"
# #   
# #   temp_str <- 
# #     " \\(.{3,60}\\)$"
# #   
# #   spectra.info$Compound.name %>% 
# #     stringr::str_subset(temp_str)
# #   
# #   spectra.info$Compound.name <- 
# #     spectra.info$Compound.name %>% 
# #     stringr::str_replace(temp_str, "") %>% 
# #     stringr::str_trim()
# # 
# #   
# #   ##remove the "|" names
# #   temp_str <- 
# #     "\\|"
# #   
# #   spectra.info$Compound.name %>% 
# #     stringr::str_subset(temp_str)
# #   
# #   
# #   spectra.info$Compound.name <- 
# #     spectra.info$Compound.name %>% 
# #     stringr::str_split(temp_str) %>% 
# #     lapply(function(x){
# #       x[1]
# #     }) %>% 
# #     unlist()
# # 
# #   ##change all the names are capital
# #   temp_name <- spectra.info$Compound.name %>% 
# #     str_extract_all("[[:alpha:]]+") %>% 
# #     lapply(function(x) paste(x, collapse = "")) %>% 
# #     unlist() %>% 
# #     str_extract_all("[[:lower:]]+") %>% 
# #     lapply(function(x) paste(x, collapse = "")) %>% 
# #     unlist()
# #   
# #   
# #   temp_idx <- 
# #     which(temp_name == "" &
# #             !stringr::str_detect(spectra.info$Compound.name, "\\:")&
# #             nchar(spectra.info$Compound.name) >= 5
# #           )
# #   spectra.info$Compound.name[temp_idx] <- 
# #   spectra.info$Compound.name[temp_idx] %>% 
# #     stringr::str_to_title()
# #   
# #   spectra.info$Compound.name <- 
# #     spectra.info$Compound.name %>% 
# #     stringr:::str_trim()
# # 
# #   
# #   
# #   spectra.info$Compound.name
# #   
# #   
# #   ###
# #   ##get KEGG and HMDB using metID::tran_ID
# #   kegg.id <- 
# #     pbapply::pblapply(spectra.info$Compound.name, function(x){
# #       metflow2::transID(query = x, from = "Chemical name", to = "KEGG", top = 1)
# #     }) %>% 
# #     do.call(rbind, .)
# #   
# #   
# #   hmdb.id <- 
# #     pbapply::pblapply(spectra.info$Compound.name, function(x){
# #       metflow2::transID(query = x, from = "Chemical name",
# #                         to = "Human Metabolome Database", top = 1)
# #     }) %>% 
# #     do.call(rbind, .)
# #   
# #   ID_trans <-
# #     data.frame(
# #       KEGG = kegg.id$KEGG,
# #       HMDB = hmdb.id$`Human Metabolome Database`,
# #       stringsAsFactors = FALSE
# #     )
# #   
# #   idx1 <- grep("C", spectra.info$HMDB.ID)
# #   kegg1 <- spectra.info$HMDB.ID[idx1]
# #   
# #   idx2 <- grep("HMDB", spectra.info$KEGG.ID)
# #   hmdb2 <- spectra.info$KEGG.ID[idx2]
# #   
# #   spectra.info$KEGG.ID[idx1] <- kegg1
# #   
# #   spectra.info$HMDB.ID[idx2] <- hmdb2
# #   
# #   
# #   
# #   
# #   
# #   hmdb_kegg <- 
# #     spectra.info %>% 
# #     select(KEGG.ID, HMDB.ID) %>% 
# #     data.frame(ID_trans, stringsAsFactors = FALSE)
# #   
# #   hmdb_kegg <-
# #     apply(hmdb_kegg, 1, function(x){
# #       hmdb <- x[c(2,4)] 
# #       hmdb <- hmdb[!is.na(hmdb)]
# #       kegg <- x[c(1,3)]
# #       kegg <- kegg[!is.na(kegg)]
# #       
# #       hmdb <- ifelse(length(hmdb) == 0, NA, hmdb[1])
# #       kegg <- ifelse(length(kegg) == 0, NA, kegg[1])
# #       
# #       c(hmdb, kegg)
# #       
# #     }) %>% 
# #     t() %>% 
# #     as_tibble()
# #   
# #   
# #   colnames(hmdb_kegg) <- 
# #     c("HMDB.ID", "KEGG.ID")
# #   
# #   hmdb_kegg$HMDB.ID <- 
# #     hmdb_kegg$HMDB.ID %>% 
# #     sapply(function(x) {
# #       if(is.na(x)) {
# #         return(NA) 
# #       }
# #       if(nchar(x) == 9){
# #         pre <- stringr::str_extract(x, "[A-Za-z]{1,}")
# #         end <- stringr::str_extract(x, "[0-9]{1,}")
# #         end <- paste("00", end, sep = "")
# #         x <- paste(pre, end, sep = "")
# #       }
# #       x 
# #     }
# #     ) %>% 
# #     unname()
# #   
# #   cbind(variable_info$HMDB.ID, hmdb_kegg[,1])
# #   
# #   variable_info[,c("HMDB.ID", 'KEGG.ID')] <-
# #     hmdb_kegg
# #   
# #   
# #   
# #   
# #   
# #   database.info$Version <- "0.0.2"
# #   
# #   msDatabase_hilic0.0.2 <- new(
# #     Class = "databaseClass",
# #     database.info = database.info,
# #     spectra.info = spectra.info,
# #     spectra.data = spectra.data
# #   )
# #   
# #   save(msDatabase_hilic0.0.2, file = "msDatabase_hilic0.0.2", compress = "xz")
# #   
# #   
# #    
# #    
# 
# # ###########---------------------------------------------------------------------
# # sxtTools::setwd_project()
# # setwd("ms2_database/")
# # 
# # 
# # #-------------------------------------------------
# # ##massBankDatabase0.0.1 to massBankDatabase0.0.2
# # load("massbankDatabase0.0.1")
# # massbankDatabase0.0.1
# # 
# # names(massbankDatabase0.0.1)
# # 
# # database.info <-
# #   massbankDatabase0.0.1@database.info
# # 
# # spectra.info <-
# #   massbankDatabase0.0.1@spectra.info
# # 
# # spectra.data <-
# #   massbankDatabase0.0.1@spectra.data
# # 
# # spectra.info$Compound.name
# # 
# # 
# # remove_idx <-
# #   which(is.na(spectra.info$Compound.name))
# # 
# # if(length(remove_idx) > 0){
# #   spectra.info <-
# #     spectra.info[-remove_idx,]
# # }
# # 
# #   database.info$Version <- "0.0.2"
# # 
# #   massbankDatabase0.0.2 <- new(
# #     Class = "databaseClass",
# #     database.info = database.info,
# #     spectra.info = spectra.info,
# #     spectra.data = spectra.data
# #   )
# # 
# # save(massbankDatabase0.0.2, file = "massbankDatabase0.0.2", compress = "xz")
#   
# 
# ###########---------------------------------------------------------------------
# # sxtTools::setwd_project()
# # setwd("ms2_database/")
# # 
# # #-------------------------------------------------
# # ##massBankDatabase0.0.1 to massBankDatabase0.0.2
# # load("metlinDatabase0.0.1")
# # metlinDatabase0.0.1
# # 
# # names(metlinDatabase0.0.1)
# # 
# # database.info <-
# #   metlinDatabase0.0.1@database.info
# # 
# # spectra.info <-
# #   metlinDatabase0.0.1@spectra.info
# # 
# # spectra.data <-
# #   metlinDatabase0.0.1@spectra.data
# # 
# # spectra.info$Compound.name
# # 
# # 
# # remove_idx <-
# #   which(is.na(spectra.info$Compound.name))
# # 
# # if(length(remove_idx) > 0){
# #   spectra.info <-
# #     spectra.info[-remove_idx,]
# # }
# # 
# # database.info$Version <- "0.0.2"
# # 
# # 
# # ##get KEGG and HMDB using metID::tran_ID
# #   kegg.id <-
# #     pbapply::pblapply(spectra.info$Compound.name, function(x){
# #       metflow2::transID(query = x, from = "Chemical name", to = "KEGG", top = 1)
# #     }) %>%
# #     do.call(rbind, .)
# # 
# #   hmdb.id <-
# #     pbapply::pblapply(spectra.info$Compound.name, function(x){
# #       metflow2::transID(query = x, from = "Chemical name",
# #                         to = "Human Metabolome Database", top = 1)
# #     }) %>%
# #     do.call(rbind, .)
# # 
# #   ID_trans <-
# #     data.frame(
# #       KEGG = kegg.id$KEGG,
# #       HMDB = hmdb.id$`Human Metabolome Database`,
# #       stringsAsFactors = FALSE
# #     )
# # 
# # 
# #   hmdb_kegg <-
# #     spectra.info %>%
# #     select(KEGG.ID, HMDB.ID) %>%
# #     data.frame(ID_trans, stringsAsFactors = FALSE)
# # 
# #   hmdb_kegg <-
# #     apply(hmdb_kegg, 1, function(x){
# #       hmdb <- x[c(2,4)]
# #       hmdb <- hmdb[!is.na(hmdb)]
# #       kegg <- x[c(1,3)]
# #       kegg <- kegg[!is.na(kegg)]
# # 
# #       hmdb <- ifelse(length(hmdb) == 0, NA, hmdb[1])
# #       kegg <- ifelse(length(kegg) == 0, NA, kegg[1])
# # 
# #       c(hmdb, kegg)
# # 
# #     }) %>%
# #     t() %>%
# #     as_tibble()
# # 
# # 
# #   colnames(hmdb_kegg) <-
# #     c("HMDB.ID", "KEGG.ID")
# # 
# #   hmdb_kegg$HMDB.ID <-
# #     hmdb_kegg$HMDB.ID %>%
# #     sapply(function(x) {
# #       if(is.na(x)) {
# #         return(NA)
# #       }
# #       if(nchar(x) == 9){
# #         pre <- stringr::str_extract(x, "[A-Za-z]{1,}")
# #         end <- stringr::str_extract(x, "[0-9]{1,}")
# #         end <- paste("00", end, sep = "")
# #         x <- paste(pre, end, sep = "")
# #       }
# #       x
# #     }
# #     ) %>%
# #     unname()
# # 
# #   cbind(spectra.info$HMDB.ID, hmdb_kegg[,1])
# # 
# #   spectra.info[,c("HMDB.ID", 'KEGG.ID')] <-
# #     hmdb_kegg
# # 
# # 
# # 
# # 
# # metlinDatabase0.0.2 <- new(
# #   Class = "databaseClass",
# #   database.info = database.info,
# #   spectra.info = spectra.info,
# #   spectra.data = spectra.data
# # )
# # 
# # save(metlinDatabase0.0.2, file = "metlinDatabase0.0.2", compress = "xz")
# # 
# # 
# # ###########---------------------------------------------------------------------
# # sxtTools::setwd_project()
# # setwd("ms2_database/")
# # 
# # #-------------------------------------------------
# # ##massBankDatabase0.0.1 to massBankDatabase0.0.2
# # load("nistDatabase0.0.1")
# # nistDatabase0.0.1
# # 
# # database.info <-
# #   nistDatabase0.0.1@database.info
# # 
# # spectra.info <-
# #   nistDatabase0.0.1@spectra.info
# # 
# # spectra.data <-
# #   nistDatabase0.0.1@spectra.data
# # 
# # spectra.info$Compound.name
# # 
# # 
# # nistDatabase0.0.2 <- new(
# #   Class = "databaseClass",
# #   database.info = database.info,
# #   spectra.info = spectra.info,
# #   spectra.data = spectra.data
# # )
# # 
# # save(nistDatabase0.0.2, file = "nistDatabase0.0.2", compress = "xz")
# # 
# # 
# # 
# # 
# # ###########---------------------------------------------------------------------
# # sxtTools::setwd_project()
# # setwd("ms2_database/")
# # 
# # #-------------------------------------------------
# # ##massBankDatabase0.0.1 to massBankDatabase0.0.2
# # load("monaDatabase0.0.1")
# # monaDatabase0.0.1
# # 
# # database.info <-
# #   monaDatabase0.0.1@database.info
# # 
# # spectra.info <-
# #   monaDatabase0.0.1@spectra.info
# # 
# # spectra.data <-
# #   monaDatabase0.0.1@spectra.data
# # 
# # spectra.info$Compound.name
# # 
# # 
# # monaDatabase0.0.2 <- new(
# #   Class = "databaseClass",
# #   database.info = database.info,
# #   spectra.info = spectra.info,
# #   spectra.data = spectra.data
# # )
# # 
# # save(monaDatabase0.0.2, file = "monaDatabase0.0.2", compress = "xz")
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# 
# 
# ######msDatabase_rplc0.0.3
# tinyTools::setwd_project()
# setwd("other_files/all_ms2_database/mike_in_house/")
# load("msDatabase_rplc0.0.2")
# msDatabase_rplc_0.0.3 =
#   msDatabase_rplc0.0.2
# 
# msDatabase_rplc_0.0.3@database.info$Source = "msDatabase_rplc"
# msDatabase_rplc_0.0.3@database.info$Version = "0.0.3"
# 
# msDatabase_rplc_0.0.3@spectra.info$Compound.name
# msDatabase_rplc_0.0.3@spectra.info$HMDB.ID
# msDatabase_rplc_0.0.3@spectra.info$KEGG.ID
# 
# library(tidyverse)
# 
# # hmdb_id2 =
# #   msDatabase_rplc_0.0.3@spectra.info$Compound.name %>%
# #   purrr::map(function(x){
# #     metflow2::transID(
# #       query = x,
# #       from = "Chemical Name",
# #       to = "Human Metabolome Database",
# #       top = 1
# #     )
# #   }) %>%
# #   do.call(rbind, .) %>%
# #   as.data.frame()
# #
# # save(hmdb_id2, file = "hmdb_id2")
# load("hmdb_id2")
# 
# # kegg_id2 =
# #   msDatabase_rplc_0.0.3@spectra.info$Compound.name %>%
# #   purrr::map(function(x){
# #     metflow2::transID(
# #       query = x,
# #       from = "Chemical Name",
# #       to = "KEGG",
# #       top = 1
# #     )
# #   }) %>%
# #   do.call(rbind, .) %>%
# #   as.data.frame()
# #
# # save(kegg_id2, file = "kegg_id2")
# load("kegg_id2")
# 
# ###rename compound name
# spectra.info = msDatabase_rplc_0.0.3@spectra.info
# 
# spectra.info$Compound.name[which(stringr::str_detect(spectra.info$Compound.name, pattern = "\\([0-9]{1,2}\\)"))] =
# spectra.info$Compound.name[which(stringr::str_detect(spectra.info$Compound.name, pattern = "\\([0-9]{1,2}\\)"))] %>%
# stringr::str_replace("\\([0-9]{1}\\)", "")
# 
# View(spectra.info)
# spectra.info$Compound.name
# 
# 
# 
# check_hmdb_kegg_id =
#   data.frame(Lab.ID = msDatabase_rplc_0.0.3@spectra.info$Lab.ID,
#              name = msDatabase_rplc_0.0.3@spectra.info$Compound.name,
#              kegg_id1 = msDatabase_rplc_0.0.3@spectra.info$KEGG.ID,
#              kegg_id2 = kegg_id2$KEGG,
#              hmdb_id1 = msDatabase_rplc_0.0.3@spectra.info$HMDB.ID,
#              hmdb_id2 = hmdb_id2$`Human Metabolome Database`
#              )
# 
# check_hmdb_kegg_id$check = NA
# 
# write.csv(check_hmdb_kegg_id, "check_hmdb_kegg_id.csv", row.names = FALSE)

