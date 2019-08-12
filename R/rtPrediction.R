# # title rtPrediction
# # description Predict RTs of kegg metabolites using MS2 matched metabolites.
# # author Xiaotao Shen
# # \email{shenxt@@sioc.ac.cn}
# # param data data From readAnnotation.
# # param prefer.adduct The reliable adducts in this LC system. Default is all.
# # param threads How many threads do you want to use? Default is the number of
# # your PC threads - 3.
# # param inHouse.compound.md The molecular descriptors of inhouse compounds.
# # param kegg.compound.md The molecular descriptors of kegg compounds.
# # return The predicted in house RT and KEGG RT.
# # export
# 
# 
# 
# setwd("D:/study/database and library/RT prediction/RPLC")
# load("msDatabase_rplc0.0.1")
# load("HMDB.metabolite.data")
# spectra.info <- msDatabase_rplc0.0.1@spectra.info
# spectra.info$HMDB.ID
# 
# library(magrittr)
# 
# temp.idx1 <- spectra.info$Compound.name %>%
#   match(.,HMDB.metabolite.data$Compound.name)
# 
# name1 <- spectra.info$Compound.name
# name2 <- HMDB.metabolite.data$Compound.name
# synonyms <- HMDB.metabolite.data$Synonyms
# synonyms <- pbapply::pblapply(synonyms, function(x){
#   stringr::str_split(string = x, pattern = ";")[[1]]
# })
# 
# temp.idx1 <- name1 %>%
#   match(.,name2)
# 
# temp.idx2 <- pbapply::pblapply(name1, function(x){
# # unlist(lapply(synonyms, function(y){
# #   match(x, y)
# # })) %>%
# #     is.na %>%
# #     `!` %>%
# #     which
#   temp.idx <- which(unlist(lapply(synonyms, function(y){
#     temp.idx <- match(x, y)
#     if(is.na(temp.idx)) return(FALSE)
#     return(TRUE)
#   })))
#   if(length(temp.idx) == 0) return(NA)
#   return(temp.idx[1])
# })
# 
# 
# temp.idx2 <- unlist(temp.idx2)
# 
# 
# temp.idx <- data.frame(temp.idx1, temp.idx2, stringsAsFactors = FALSE)
# 
# 
# temp.idx <- apply(temp.idx, 1,function(x){
#   if(is.na(x[1]) & is.na(x)[2]) return(NA)
#   if(is.na(x[1]) & !is.na(x)[2]) return(x[2])
#   if(!is.na(x[1])) return(x[1])
# })
# 
# 
# cas.id1 <- spectra.info$CAS.ID
# hmdb.id1 <- spectra.info$HMDB.ID
# kegg.id1 <- spectra.info$KEGG.ID
# 
# cas.id <- data.frame(cas.id1, HMDB.metabolite.data$CAS.ID[temp.idx],
#                      stringsAsFactors = FALSE)
# cas.id <- apply(cas.id, 1, function(x){
#   if(is.na(x[1]) & is.na(x)[2]) return(NA)
#   if(is.na(x[1]) & !is.na(x)[2]) return(x[2])
#   if(!is.na(x[1])) return(x[1])
# })
# 
# 
# hmdb.id <- data.frame(hmdb.id1, HMDB.metabolite.data$HMDB.ID[temp.idx],
#                      stringsAsFactors = FALSE)
# hmdb.id <- apply(hmdb.id, 1, function(x){
#   if(is.na(x[1]) & is.na(x)[2]) return(NA)
#   if(is.na(x[1]) & !is.na(x)[2]) return(x[2])
#   if(!is.na(x[1])) return(x[1])
# })
# 
# 
# kegg.id <- data.frame(kegg.id1, HMDB.metabolite.data$KEGG.ID[temp.idx],
#                       stringsAsFactors = FALSE)
# kegg.id <- apply(kegg.id, 1, function(x){
#   if(is.na(x[1]) & is.na(x)[2]) return(NA)
#   if(is.na(x[1]) & !is.na(x)[2]) return(x[2])
#   if(!is.na(x[1])) return(x[1])
# })
# 
# 
# spectra.info$CAS.ID <- cas.id
# spectra.info$HMDB.ID <- hmdb.id
# spectra.info$KEGG.ID <- kegg.id
# 
# 
# # name1
# # 
# # 
# # library(CTSgetR)
# # # cas.id <- CTSgetR(id = name1, from = "Chemical Name", to = "CAS")
# # # hmdb.id <- CTSgetR(id = name1, from = "Chemical Name", to = "Human Metabolome Database")
# # # kegg.id <- CTSgetR(id = name1, from = "Chemical Name", to = "KEGG")
# # load("name1")
# # cas.id <- vector(mode = "list", length = length(name1))
# # hmdb.id <- vector(mode = "list", length = length(name1))
# # kegg.id <- vector(mode = "list", length = length(name1))
# # 
# # for(i in 614:length(name1)){
# #   cat(i, " ")
# #   temp <- try(CTSgetR(id = name1[i], from = "Chemical Name", to = "CAS"), silent = FALSE)
# #   if(class(temp) == "try-error"){
# #     cas.id[[i]] <- matrix(NA, ncol = 2, nrow = 1)
# #   }else{
# #     cas.id[[i]] <- temp
# #   }
# # }
# # 
# # cas.id <- lapply(cas.id, function(x){
# #   colnames(x) <- c("Chemical Name", "CAS")
# #   x
# # })
# # 
# # cas.id <- do.call(rbind, cas.id)
# # cas.id[,2] <- as.character(cas.id[,2])
# # cbind(cas.id[,2], spectra.info$CAS.ID)
# # 
# # 
# # for(i in 773:length(name1)){
# #   cat(i, " ")
# #   temp <- try(CTSgetR(id = name1[i], from = "Chemical Name", to = "KEGG"), silent = FALSE)
# #   if(class(temp) == "try-error"){
# #     kegg.id[[i]] <- matrix(NA, ncol = 2, nrow = 1)
# #   }else{
# #     kegg.id[[i]] <- temp
# #   }
# # }
# # 
# # kegg.id <- lapply(kegg.id, function(x){
# #   colnames(x) <- c("Chemical", "KEGG")
# #   x
# # })
# # kegg.id <- do.call(rbind, kegg.id)
# # 
# # save(kegg.id, file = "kegg.id")
# # save(cas.id, file = "cas.id")
# # 
# # kegg.id$KEGG <- as.character(kegg.id$KEGG)
# # kegg.id$KEGG[is.na(kegg.id$KEGG)] <- ""
# # temp.idx <- which(kegg.id$KEGG == "")
# # for(i in temp.idx){
# #   cat(i, " ")
# #   temp <- try(CTSgetR(id = name1[i], from = "Chemical Name", to = "Human Metabolome Database"), silent = FALSE)
# #   if(class(temp) == "try-error"){
# #     hmdb.id[[i]] <- matrix(NA, ncol = 2, nrow = 1)
# #   }else{
# #     hmdb.id[[i]] <- temp
# #   }
# # }
# # 
# # save(hmdb.id, file = "hmdb.id")
# # 
# # hmdb.id <- lapply(hmdb.id, function(x){
# #   if(class(x) == "NULL"){
# #     x <- matrix(NA, ncol = 2, nrow = 1)
# #   }
# #   x
# # })
# # 
# # 
# # hmdb.id <- lapply(hmdb.id, function(x){
# #   colnames(x) <- c("Chemical", "KEGG")
# #   x
# # })
# # hmdb.id <- do.call(rbind, hmdb.id)
# # save(hmdb.id, file = "hmdb.id")
# # 
# # kegg.id <- kegg.id$KEGG
# # cas.id <- cas.id$CAS
# # hmdb.id <-hmdb.id$KEGG
# # hmdb.id <- as.character(hmdb.id)
# # 
# # cas.id[which(cas.id == "")] <- NA
# # 
# # hmdb.id[which(hmdb.id == "")] <- NA
# # kegg.id[which(kegg.id == "")] <- NA
# # 
# # cas.id1 <- data.frame(spectra.info$CAS.ID, cas.id, stringsAsFactors = FALSE)
# # 
# # cas.id2 <- apply(cas.id1, 1, function(x){
# #   if(!is.na(x[1])) return(x[1])
# #   if(all(is.na(x))) return(NA)
# #   if(is.na(x[1]) & !is.na(x[2])) return(x[2])
# # })
# # 
# # 
# # hmdb.id1 <- data.frame(spectra.info$HMDB.ID, hmdb.id, stringsAsFactors = FALSE)
# # 
# # hmdb.id2 <- apply(hmdb.id1, 1, function(x){
# #   if(!is.na(x[1])) return(x[1])
# #   if(all(is.na(x))) return(NA)
# #   if(is.na(x[1]) & !is.na(x[2])) return(x[2])
# # })
# # 
# # 
# # kegg.id1 <- data.frame(spectra.info$KEGG.ID, kegg.id, stringsAsFactors = FALSE)
# # 
# # kegg.id2 <- apply(kegg.id1, 1, function(x){
# #   if(!is.na(x[1])) return(x[1])
# #   if(all(is.na(x))) return(NA)
# #   if(is.na(x[1]) & !is.na(x[2])) return(x[2])
# # })
# # 
# # 
# # spectra.info$CAS.ID <- cas.id2
# # spectra.info$HMDB.ID <- hmdb.id2
# # spectra.info$KEGG.ID <- kegg.id2
# 
# write.csv(spectra.info, "spectra.info.csv", row.names = FALSE)
# spectra.info <- read.csv("spectra.info.csv", stringsAsFactors = FALSE)
# spectra.info <- spectra.info[which(!is.na(spectra.info$HMDB.ID)), ]
# spectra.info <- spectra.info[which(spectra.info$HMDB.ID != "0"), ]
# 
# library(plyr)
# 
# spectra.info2 <- plyr::dlply(.data = spectra.info, .variables = .(HMDB.ID))
# 
# 
# hmdb.id <- unname(unlist(lapply(spectra.info2, function(x){
#   x$HMDB.ID[1]
# })))
# 
# rt <- unname(unlist(lapply(spectra.info2, function(x){
#   mean(x$RT)
# })))
# 
# 
# hmdb.id <- unlist(lapply(hmdb.id, function(x){
#   x <- stringr::str_split(string = x, pattern = "\\|")[[1]][1]
#   x <- stringr::str_trim(x, side = "both")
#   if(nchar(x) == 9){
#     x <- stringr::str_replace(string = x, pattern = "HMDB", replacement = "HMDB00")
#   }
#   
# }))
# 
# 
# load("HMDb.metabolite.data")
# smiles <- match(hmdb.id, HMDB.metabolite.data$HMDB.ID) %>%
#   `[` (HMDB.metabolite.data$SMILES, .)
# 
# library(rcdk)
# 
# 
# library(rcdk)
# # load("HMDB.metabolite.data")
# hmdb.smiles <- HMDB.metabolite.data$SMILES
# descNames <- unique(unlist(sapply(get.desc.categories(), get.desc.names)))
# 
# 
# ##1: 1- 30000
# ##2: 30001 - 50000
# ##3: 50001 - 70000
# ##4: 70001 - 90000
# ##5:90001- 114004
# 
# # hmdb.mol5 <- vector(mode = "list", length = length(hmdb.smiles))
# load("hmdb.mol5")
# library(rcdk)
# load("hmdb.smiles")
# for(i in 110001:114004){
#   cat(i, " ")
# temp <- try(parse.smiles(hmdb.smiles[i]))
# if(class(temp) == "try-error"){
#   cat('Error\n')
#   temp <- NA
# }
# hmdb.mol5[[i]] <- temp
# }
# 
# save(hmdb.mol5, file = "hmdb.mol5")
# rm(list = c("hmdb.mol5"))
# print(1)
# 
# 
# 
# 
# 
# 
# 
# temp <- try(parse.smiles(hmdb.smiles[1]))
# temp <- parse.smiles(hmdb.smiles[1])
# temp <- eval.desc(temp, descNames)
# 
# 
# 
# setGeneric(name = "rtPrediction",
#            def = function(inhouse.compound.rt,
#                           inHouse.compound.md,
#                           targeted.compound.md,
#                           use.default.md = TRUE,
#                           column = c("hilic", "rp"),
#                           threads = 3
#            ){
#              column <- match.arg(column)
#              tags <- data[[1]]
#              sample <- data[[2]]
#              sample.int <- apply(sample, 1, median)
#              rm(list = "sample")
#              gc()
#              idx <- which(!is.na(tags$labid))
#              if(length(idx) == 0) stop("No metabolites are identified by MS2 library.\n")
#              tags1 <- tags[idx,]
#              sample.int1 <- sample.int[idx]
#              rm(list = "sample.int")
#              gc()
# 
#              labid <- tags1$labid
#              labid <- lapply(labid, function(x) {
#                strsplit(x, split = ";")[[1]][1]
#              })
#              labid <- unlist(labid)
# 
# 
#              adduct <- tags1$adduct
#              adduct <- lapply(adduct, function(x) {
#                strsplit(x, split = ";")[[1]][1]
#              })
#              adduct <- unlist(adduct)
# 
#              ##remove multipe peaks matched one metabolite
#              dup.id <- unique(labid[duplicated(labid)])
#              if(length(dup.id) > 0){
#                for(i in 1:length(dup.id)){
#                  temp.id <- dup.id[i]
#                  temp.idx <- grep(temp.id, labid)
#                  temp.int <- sample.int1[temp.idx]
#                  # rm(list = c("sample.int1"))
#                  # temp.adduct <- adduct[temp.idx]
#                  # temp.rt <- tags1$rt[temp.idx]
#                  labid[temp.idx[-which.max(temp.int)]]  <- NA
#                }
#              }
# 
#              rt <- tags1$rt
#              rm(list = "tags1")
#              gc()
# 
#              data <- data.frame(labid, rt, adduct, stringsAsFactors = FALSE)
#              data <- data[!is.na(data$labid),,drop = FALSE]
# 
#              ##filter the metabolite who have no MD in inHouse.compound.md
#              temp.idx <- which(data$labid %in% rownames(inHouse.compound.md))
# 
#              # if(length(temp.idx) == 0) stop("The metabolites from MS2 match have no MD in inHouse.compound.md.\n")
#              if(length(temp.idx) == 0) stop("No metabolites are identified by MS2 library.\n")
#              if(length(temp.idx) < 70){
#                prefer.adduct <- 'all'
#              }
#              data <- data[temp.idx,]
# 
# 
#              ##filter data using adduct or not
#              adduct.order <- setdiff(names(sort(table(adduct), decreasing = TRUE)), prefer.adduct)
#              if (prefer.adduct != 'all') {
#                temp.idx <- which(data$adduct %in% prefer.adduct)
#                count <- 1
#                while(length(temp.idx) < 50 & count < length(adduct.order)){
#                  prefer.adduct <- c(prefer.adduct, adduct.order[count])
#                  temp.idx <- which(data$adduct %in% prefer.adduct)
#                  count <- count + 1
#                }
#                data <- data[temp.idx,,drop = FALSE]
#              }
# 
#              if(nrow(data) <= 5) stop("No or less 5 metabolites are identified.")
# 
#              cat("There are ", nrow(data),
#                  " metabolites are used for RT prediction.\n", sep = "")
#              # data <- data[data$adduct == "M+H",]
#              #----------------------------------------------------------------
#              # x <- table(adduct[which(!is.na(labid))])
#              # x <- sort(x, decreasing = T)
#              # par(mar = c(5,5,4,2))
#              # y <- barplot(x, borde= NA, xlab = "Adduct", ylab = "Peak Number",
#              #         cex.lab = 1.8, cex.axis = 1.5, names.arg = "",
#              #         col = c("salmon", rep("black", 1),"lightseagreen",
#              #                 rep("black", 5),"orchid4", rep("black", 1),rep("black", 8)))
#              # par(xpd = T)
#              # text(x = y, y = x + 5, labels = names(x), srt = 90)
#              # pie(x, border = "white",
#              #     col = c("salmon", rep("black", 1),"lightseagreen",
#              #             rep("black", 5),"orchid4", rep("black", 1),rep("black", 8)),
#              #     radius = 1.3)
#              # par(new = T)
#              # pie(1, col = "white", radius = 0.9, border = "white", labels = "")
#              #----------------------------------------------------------------
# 
#              idx <- match(data$labid, rownames(inHouse.compound.md))
#              md <- inHouse.compound.md[idx,]
#              # rm(list = "inHouse.compound.md")
# 
#              ###remove  NA which apper in more than 50% metabolites
#              remove.idx1 <-
#                which(apply(md, 2, function(x) {sum(is.na(x)/nrow(md))})>0.5)
#              md1 <- md[,-remove.idx1]
#              rm(list = c("md"))
#              gc()
#              ##impute NA
#              md2 <- t(impute::impute.knn(data = t(md1))[[1]])
#              rm(list = "md1")
#              gc()
# 
#              #remove MD which are same in all metaboites
#              remove.idx2 <- which(apply(md2, 2, sd) == 0)
#              md3 <- md2[,-remove.idx2]
#              rm(list = c("md2"))
#              gc()
# 
#              ##construct RF model
#              train.y <- data$rt
#              train.x <- md3
#              rm(list = c('data', 'md3'))
#              gc()
# 
#              if(use.default.md){
#                switch(column,
#                       "hilic" = {
#                         marker.name <- c('XLogP', "tpsaEfficiency", "WTPT.5", "khs.dsCH", "MLogP", "nAcid", "nBase", "BCUTp.1l")
#                       },
#                       "rp" = {
#                         marker.name <- c('XLogP', "WTPT.4", "WTPT.5", "ALogp2", "BCUTp.1l")})
#              }else{
#                #construct RF 100 times, find the MDs which are always apperar in
#                # top 5 as markers
#                temp.fun <- function(idx, x, y){
#                  suppressMessages(library(randomForest))
#                  temp <- idx
#                  rf.class <- randomForest::randomForest(x = x, y = y,
#                                                         replace = TRUE, importance = TRUE,
#                                                         proximity = TRUE)
#                  imp <- randomForest::importance(rf.class)
#                  rm(list = c("rf.class", "temp"))
#                  gc()
#                  imp
#                }
#                cat("\n")
#                cat("Find the optimal moleculr descriptor\n")
#                imp <- BiocParallel::bplapply(1:100,
#                                              temp.fun,
#                                              BPPARAM = BiocParallel::SnowParam(workers = threads,
#                                                                                progressbar = FALSE),
#                                              x = train.x,
#                                              y = train.y)
# 
#                md.name <- list()
#                for(i in 1:length(imp)){
#                  md.name[[i]] <- names(sort(imp[[i]][,1], decreasing = TRUE)[1:5])
#                }
# 
#                md.name <- unlist(md.name)
#                md.name <- table(md.name)
#                md.name <- sort(md.name)
#                marker.name <- names(md.name[which(md.name >= 50)])
#                rm(list = "imp")
#                gc()
#                #
#                save(marker.name, file = "marker.name", compress = "xz")
#                save(md.name, file = "md.name", compress = "xz")
#              }
# 
# 
# 
# 
#              idx <- match(marker.name, colnames(train.x))
# 
#              idx <- idx[!is.na(idx)]
#              if(length(idx) == 0){
#                stop("Your markers are not in MD data.\n")
#              }
#              train.x1 <- train.x[,idx]
#              rm(list = c("train.x"))
#              gc()
# 
#              para <- NULL
#              ntree1 <- seq(300,1000,by = 200)
#              mtry1 <- seq(1,length(marker.name),by = 1)
#              for(i in 1:length(ntree1)){
#                para <- rbind(para, cbind(ntree1[i],mtry1))
#              }
#              colnames(para) <- c("ntree", "mtry")
#              mse <- NULL
#              rf.reg <- list()
#              cat("\n")
#              cat("Find the optimal parameters\n")
#              for(i in 1:nrow(para)){
#                cat(i);cat(" ")
#                temp.ntree <- para[i,1]
#                temp.mtry <- para[i,2]
#                rf.reg[[i]] <- randomForest::randomForest(x = train.x1, y = train.y,
#                                                          ntree = temp.ntree,mtry = temp.mtry,
#                                                          replace = TRUE, importance = TRUE, proximity = TRUE)
#                mse[i] <- mean(rf.reg[[i]]$mse)
#              }
#              cat("\n")
#              result <- data.frame(para, mse, stringsAsFactors = FALSE)
#              temp.idx <- which.min(result$mse)
#              ntree <- result$ntree[temp.idx]
#              mtry <- result$mtry[temp.idx]
#              ##
#              rf.reg <- randomForest::randomForest(x = train.x1, y = train.y,
#                                                   ntree = ntree,
#                                                   mtry = mtry,
#                                                   replace = TRUE,
#                                                   importance = TRUE, proximity = TRUE)
# 
#              rm(list = c("train.x1"))
#              gc()
#              ##predict RT in inhouse database
#              test.x <- inHouse.compound.md
#              rm(list = "inHouse.compound.md")
#              gc()
#              test.x <- test.x[,match(marker.name, colnames(test.x))]
#              test.x <- as.data.frame(test.x)
# 
#              inHouse.compound.rt <- rep(NA, nrow(test.x))
#              names(inHouse.compound.rt) <- rownames(test.x)
#              ##impute NA in test.x
#              idx1 <-
#                which(apply(test.x, 1, function(x) {sum(is.na(x))/ncol(test.x) < 0.5}))
#              test.x1 <- test.x[idx1,]
#              test.x1 <- t(impute::impute.knn(data = t(test.x1))[[1]])
#              temp.rt <- predict(object = rf.reg, newdata = test.x1)
#              names(temp.rt) <- rownames(test.x1)
#              inHouse.compound.rt[idx1] <- temp.rt
#              ##NA give the median RT of all peaks
#              inHouse.compound.rt[is.na(inHouse.compound.rt)] <- median(tags$rt)
#              attribute <- rep(NA, length(inHouse.compound.rt))
#              attribute[idx1] <- "Predicted"
#              attribute[is.na(attribute)] <- "Median"
#              inHouse.rt <- data.frame(inHouse.compound.rt, attribute, stringsAsFactors = FALSE)
#              colnames(inHouse.rt)[1] <- "RT"
# 
#              ##cross validation
#              ##predict RT in KEGG database
#              test.x <- kegg.compound.md
#              rm(list = "kegg.compound.md")
#              gc()
#              test.x <- test.x[,match(marker.name, colnames(test.x))]
#              test.x <- as.data.frame(test.x)
# 
#              kegg.compound.rt <- rep(NA, nrow(test.x))
#              names(kegg.compound.rt) <- rownames(test.x)
#              ##impute NA in text.x
#              idx1 <-
#                which(apply(test.x, 1, function(x) {sum(is.na(x))/ncol(test.x) < 0.5}))
#              test.x1 <- test.x[idx1,]
#              test.x1 <- t(impute::impute.knn(data = t(test.x1))[[1]])
#              temp.rt <- predict(object = rf.reg, newdata = test.x1)
#              names(temp.rt) <- rownames(test.x1)
#              kegg.compound.rt[idx1] <- temp.rt
#              ##NA give the median RT of all peaks
#              kegg.compound.rt[is.na(kegg.compound.rt)] <- median(tags$rt)
#              rm(list = "tags")
#              gc()
#              attribute <- rep(NA, length(kegg.compound.rt))
#              attribute[idx1] <- "Predicted"
#              attribute[is.na(attribute)] <- "Median"
#              kegg.rt <- data.frame(kegg.compound.rt, attribute, stringsAsFactors = FALSE)
#              colnames(kegg.rt)[1] <- "RT"
# 
#              rt <- list(inHouse.rt, kegg.rt)
#              names(rt) <- c("inHouse.rt", "KEGG.rt")
#              rt <- rt
#            })
# 
# 
# 
# CTSgetR(id = "HMDB0000444", from = "Chemical Name", to = "Human Metabolome Database")
# CTSgetR(id = "HMDB0000444", from = "Human Metabolome Database", to = "KEGG")
# match("29325-35-7", HMDB.metabolite.data$CAS.ID)
# HMDB.metabolite.data[11002,]
# HMDB.metabolite.data$KEGG.ID[1714]
# match("OS(OC=1C=CC(=CC1OC)CC=C)(=O)=O", HMDB.metabolite.data$SMILES)
# match("UISWGTUSVPTGHL-UHFFFAOYSA-N", HMDB.metabolite.data$inchikey)
# 
# spectra.info <- read.csv("spectra.info.csv", stringsAsFactors = FALSE)
# 
# spectra.info[which(spectra.info == "0", arr.ind = TRUE)] <- NA
# 
# 
# msDatabase_rplc0.0.2 <- msDatabase_rplc0.0.1
# msDatabase_rplc0.0.2@database.info$Version <- "0.0.2"
# msDatabase_rplc0.0.2@database.info$Note <- "V0.0.2: More information"
# msDatabase_rplc0.0.2@spectra.info <- spectra.info
# 
# 
# save(msDatabase_rplc0.0.2, file = "msDatabase_rplc0.0.2")
# 
# 
# 
# 
