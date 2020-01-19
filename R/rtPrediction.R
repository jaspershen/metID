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
# sxtTools::setwd_project()
# setwd("test_data/rt_prediction/RPLC/")
# rm(list = ls())
# load("msDatabase_rplc0.0.1")
# load("HMDB.metabolite.data")
# spectra.info <- msDatabase_rplc0.0.1@spectra.info
# spectra.info$HMDB.ID
# 
# library(tidyverse)
# 
# temp.idx1 <- spectra.info$Compound.name %>%
#   match(., HMDB.metabolite.data$Compound.name)
# 
# name1 <- spectra.info$Compound.name
# name2 <- HMDB.metabolite.data$Compound.name
# synonyms <- HMDB.metabolite.data$Synonyms
# synonyms <- pbapply::pblapply(synonyms, function(x) {
#   stringr::str_split(string = x, pattern = ";")[[1]]
# })
# 
# temp.idx1 <- name1 %>%
#   match(., name2)
# 
# temp.idx2 <- pbapply::pblapply(name1, function(x) {
#   # unlist(lapply(synonyms, function(y){
#   #   match(x, y)
#   # })) %>%
#   #     is.na %>%
#   #     `!` %>%
#   #     which
#   temp.idx <- which(unlist(lapply(synonyms, function(y) {
#     temp.idx <- match(x, y)
#     if (is.na(temp.idx))
#       return(FALSE)
#     return(TRUE)
#   })))
#   if (length(temp.idx) == 0)
#     return(NA)
#   return(temp.idx[1])
# })
# 
# 
# temp.idx2 <- unlist(temp.idx2)
# 
# 
# temp.idx <-
#   data.frame(temp.idx1, temp.idx2, stringsAsFactors = FALSE)
# 
# 
# temp.idx <- apply(temp.idx, 1, function(x) {
#   if (is.na(x[1]) & is.na(x)[2])
#     return(NA)
#   if (is.na(x[1]) & !is.na(x)[2])
#     return(x[2])
#   if (!is.na(x[1]))
#     return(x[1])
# })
# 
# 
# cas.id1 <- spectra.info$CAS.ID
# hmdb.id1 <- spectra.info$HMDB.ID
# kegg.id1 <- spectra.info$KEGG.ID
# 
# cas.id <- data.frame(cas.id1, HMDB.metabolite.data$CAS.ID[temp.idx],
#                      stringsAsFactors = FALSE)
# cas.id <- apply(cas.id, 1, function(x) {
#   if (is.na(x[1]) & is.na(x)[2])
#     return(NA)
#   if (is.na(x[1]) & !is.na(x)[2])
#     return(x[2])
#   if (!is.na(x[1]))
#     return(x[1])
# })
# 
# 
# hmdb.id <-
#   data.frame(hmdb.id1, HMDB.metabolite.data$HMDB.ID[temp.idx],
#              stringsAsFactors = FALSE)
# hmdb.id <- apply(hmdb.id, 1, function(x) {
#   if (is.na(x[1]) & is.na(x)[2])
#     return(NA)
#   if (is.na(x[1]) & !is.na(x)[2])
#     return(x[2])
#   if (!is.na(x[1]))
#     return(x[1])
# })
# 
# 
# kegg.id <-
#   data.frame(kegg.id1, HMDB.metabolite.data$KEGG.ID[temp.idx],
#              stringsAsFactors = FALSE)
# kegg.id <- apply(kegg.id, 1, function(x) {
#   if (is.na(x[1]) & is.na(x)[2])
#     return(NA)
#   if (is.na(x[1]) & !is.na(x)[2])
#     return(x[2])
#   if (!is.na(x[1]))
#     return(x[1])
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
# spectra.info <-
#   read.csv("spectra.info.csv", stringsAsFactors = FALSE)
# spectra.info <- spectra.info[which(!is.na(spectra.info$HMDB.ID)),]
# spectra.info <- spectra.info[which(spectra.info$HMDB.ID != "0"),]
# 
# library(plyr)
# 
# spectra.info2 <-
#   plyr::dlply(.data = spectra.info, .variables = .(HMDB.ID))
# 
# hmdb.id <- unname(unlist(lapply(spectra.info2, function(x) {
#   x$HMDB.ID[1]
# })))
# 
# rt <- unname(unlist(lapply(spectra.info2, function(x) {
#   mean(x$RT)
# })))
# 
# 
# hmdb.id <- unlist(lapply(hmdb.id, function(x) {
#   x <- stringr::str_split(string = x, pattern = "\\|")[[1]][1]
#   x <- stringr::str_trim(x, side = "both")
#   if (nchar(x) == 9) {
#     x <-
#       stringr::str_replace(string = x,
#                            pattern = "HMDB",
#                            replacement = "HMDB00")
#   }
#   x
#   
# }))
# 
# load("HMDB.metabolite.data")
# smiles <- match(hmdb.id, HMDB.metabolite.data$HMDB.ID) %>%
#   `[` (HMDB.metabolite.data$SMILES, .)
# 
# library(rcdk)
# 
# library(rcdk)
# # load("HMDB.metabolite.data")
# hmdb.smiles <- HMDB.metabolite.data$SMILES
# descNames <-
#   unique(unlist(sapply(get.desc.categories(), get.desc.names)))
# 
# 
# ##get all the md descriptors for the standards.
# library(rcdk)
# standard.mols = vector(mode = "list", length = length(smiles))
# for (i in 1:length(standard.mols)) {
#   cat(i, " ")
#   temp <- try(parse.smiles(smiles[i]))
#   if (class(temp) == "try-error") {
#     cat('Error\n')
#     temp <- NA
#   }
#   standard.mols[[i]] <- temp[[1]]
# }
# 
# save(standard.mols, file = "standard.mols")
# 
# standard.md <-
#   pbapply::pblapply(standard.mols, function(x) {
#     md <- try(eval.desc(x, descNames))
#     if (class(md) == "try-error") {
#       md <- matrix(rep(NA, 286), nrow = 1)
#     }
#     md
#   })
# 
# col_name = colnames(standard.md[[1]])
# 
# standard.md <-
#   lapply(standard.md, function(x) {
#     colnames(x) = col_name
#     x
#   })
# 
# 
# 
# standard.md <-
#   standard.md %>%
#   do.call(rbind, .)
# 
# 
# standard.data <-
#   data.frame(hmdb.id, rt, standard.md, stringsAsFactors = FALSE)
# 
# ##remove MD are all NA
# remove.idx <-
#   which(apply(standard.data, 2, function(x) {
#     all(is.na(x))
#   })) %>%
#   unname()
# 
# standard.data <-
#   standard.data[, -remove.idx]
# 
# ##use the marker we used from MetDNA
# marker.name <- c('XLogP', "WTPT.4", "WTPT.5", "ALogp2", "BCUTp.1l")
# 
# standard.data <-
#   standard.data[, c("hmdb.id", "rt", marker.name)]
# 
# 
# ##remove the metabolites which have NA md
# 
# standard.data2  <-
#   standard.data[-15, ]
# 
# standard.data2 <-
#   standard.data2 %>%
#   distinct(., hmdb.id, .keep_all = TRUE)
# 
# 
# remove.idx <-
#   which(apply(standard.data2[, -c(1, 2)], 1, function(x) {
#     any(is.na(x))
#   }))
# 
# remove.idx
# 
# standard.data2 <-
#   standard.data2[-remove.idx, ]
# 
# 
# sum(is.na(standard.data2))
# 
# train.y <- standard.data2$rt
# train.x <- standard.data2[, -c(1, 2)]
# 
# rownames(train.x) <- standard.data2$hmdb.id
# names(train.y) <- standard.data2$hmdb.id
# 
# idx <- match(marker.name, colnames(train.x))
# 
# train.x <- train.x[, idx]
# 
# para <- NULL
# ntree1 <- seq(300, 1000, by = 200)
# mtry1 <- seq(1, length(marker.name), by = 1)
# for (i in 1:length(ntree1)) {
#   para <- rbind(para, cbind(ntree1[i], mtry1))
# }
# colnames(para) <- c("ntree", "mtry")
# mse <- NULL
# rf.reg <- list()
# cat("\n")
# cat("Find the optimal parameters\n")
# for (i in 1:nrow(para)) {
#   cat(i)
#   cat(" ")
#   temp.ntree <- para[i, 1]
#   temp.mtry <- para[i, 2]
#   rf.reg[[i]] <-
#     randomForest::randomForest(
#       x = train.x,
#       y = train.y,
#       ntree = temp.ntree,
#       mtry = temp.mtry,
#       replace = TRUE,
#       importance = TRUE,
#       proximity = TRUE
#     )
#   mse[i] <- mean(rf.reg[[i]]$mse)
# }
# cat("\n")
# result <- data.frame(para, mse, stringsAsFactors = FALSE)
# temp.idx <- which.min(result$mse)
# ntree <- result$ntree[temp.idx]
# mtry <- result$mtry[temp.idx]
# ##
# 
# 
# ###remove the metabolites which may have wrong RTs
# ###remove the metabolites which may have wrong RTs
# 
# all_rt <- all_predict_rt <- vector(mode = "list", 200)
# 
# for (i in 1:200) {
#   cat(i, " ")
#   dis_index <-
#     sample(1:nrow(train.x), nrow(train.x), replace = TRUE) %>% unique() %>% sort()
#   val_index <- setdiff(1:nrow(train.x), dis_index)
#   dis_id <- rownames(train.x)[dis_index]
#   val_id <- rownames(train.x)[val_index]
#   temp.rf.reg <-
#     randomForest::randomForest(
#       x = train.x[dis_index, ],
#       y = train.y[dis_index],
#       ntree = ntree,
#       mtry = mtry,
#       replace = TRUE,
#       importance = TRUE,
#       proximity = TRUE
#     )
#   temp.predict.y <-
#     predict(object = temp.rf.reg, newdata = train.x[val_index, ])
#   
#   all_rt[[i]] <- train.y[val_index]
#   all_predict_rt[[i]] <- temp.predict.y
# }
# 
# 
# all_rt2 <- lapply(all_rt, function(x) {
#   data.frame(id = names(x),
#              rt = x,
#              stringsAsFactors = FALSE)
# })
# 
# all_predict_rt2 <- lapply(all_predict_rt, function(x) {
#   data.frame(id = names(x),
#              rt = x,
#              stringsAsFactors = FALSE)
# })
# 
# 
# all_rt3 <-
#   all_rt2 %>%
#   bind_rows()
# 
# all_predict_rt3 <-
#   all_predict_rt2 %>%
#   bind_rows()
# 
# 
# dim(all_rt3)
# dim(all_predict_rt3)
# 
# 
# all_rt4 <-
#   all_rt3 %>%
#   group_by(id) %>%
#   dplyr::summarise(n = dplyr::n(), mean.rt = mean(rt)) %>%
#   ungroup()
# 
# 
# 
# all_predict_rt4 <-
#   all_predict_rt3 %>%
#   group_by(id) %>%
#   dplyr::summarise(n = dplyr::n(), mean.rt = mean(rt)) %>%
#   ungroup()
# 
# 
# dim(all_rt4)
# dim(all_predict_rt4)
# 
# plot(all_rt4$mean.rt, all_predict_rt4$mean.rt)
# abline(0, 1)
# ##reove.idx
# remove.idx <-
#   which(abs(all_rt4$mean.rt - all_predict_rt4$mean.rt) > 90)
# 
# remove.idx
# train.x2 <- train.x[-remove.idx, ]
# train.y2 <- train.y[-remove.idx]
# 
# 
# all_rt <- all_predict_rt <- vector(mode = "list", 200)
# 
# for (i in 1:200) {
#   cat(i, " ")
#   dis_index <-
#     sample(1:nrow(train.x2), nrow(train.x2), replace = TRUE) %>% unique() %>% sort()
#   val_index <- setdiff(1:nrow(train.x2), dis_index)
#   dis_id <- rownames(train.x2)[dis_index]
#   val_id <- rownames(train.x2)[val_index]
#   temp.rf.reg <-
#     randomForest::randomForest(
#       x = train.x2[dis_index, ],
#       y = train.y2[dis_index],
#       ntree = ntree,
#       mtry = mtry,
#       replace = TRUE,
#       importance = TRUE,
#       proximity = TRUE
#     )
#   temp.predict.y <-
#     predict(object = temp.rf.reg, newdata = train.x2[val_index, ])
#   
#   all_rt[[i]] <- train.y2[val_index]
#   all_predict_rt[[i]] <- temp.predict.y
# }
# 
# 
# all_rt2 <- lapply(all_rt, function(x) {
#   data.frame(id = names(x),
#              rt = x,
#              stringsAsFactors = FALSE)
# })
# 
# all_predict_rt2 <- lapply(all_predict_rt, function(x) {
#   data.frame(id = names(x),
#              rt = x,
#              stringsAsFactors = FALSE)
# })
# 
# 
# all_rt3 <-
#   all_rt2 %>%
#   bind_rows()
# 
# all_predict_rt3 <-
#   all_predict_rt2 %>%
#   bind_rows()
# 
# 
# dim(all_rt3)
# dim(all_predict_rt3)
# 
# 
# all_rt4 <-
#   all_rt3 %>%
#   group_by(id) %>%
#   dplyr::summarise(n = dplyr::n(), mean.rt = mean(rt)) %>%
#   ungroup()
# 
# 
# 
# all_predict_rt4 <-
#   all_predict_rt3 %>%
#   group_by(id) %>%
#   dplyr::summarise(n = dplyr::n(), mean.rt = mean(rt)) %>%
#   ungroup()
# 
# 
# dim(all_rt4)
# dim(all_predict_rt4)
# 
# plot(all_rt4$mean.rt, all_predict_rt4$mean.rt)
# abline(0, 1)
# 
# mean(abs(all_rt4$mean.rt - all_predict_rt4$mean.rt))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# rf.reg <- randomForest::randomForest(
#   x = train.x2,
#   y = train.y2,
#   ntree = ntree,
#   mtry = mtry,
#   replace = TRUE,
#   importance = TRUE,
#   proximity = TRUE
# )
# 
# 
# predict(object = rf.reg, newdata = train.x2)
# 
# plot(predict(object = rf.reg, newdata = train.x2), train.y2)
# 
# predict(object = rf.reg, newdata = standard.data[15, -c(1, 2)])
