# setwd("D:/study/database and library/metabolite database")
# load("hmdbAllinf.rda")
# load("kegg.compound.rda")
# 
# ##should control
# #1 CAS.ID, HMDB.ID,
# BiocManager::install("hmdbQuery")
# library(hmdbQuery)
# 
# lk1 = HmdbEntry(prefix = "http://www.hmdb.ca/metabolites/",
#                 id = "HMDB0000001")
# data(hmdb_disease)
# hmdb_disease
# 
# library(XML)
# library("methods")
# file <- dir()[4]
# result <- xmlParse(file = file)
# rootnode <- xmlRoot(result)
# info <- xmlSApply(rootnode, function(x) xmlSApply(x, xmlValue))
# names(info) <- unname(names(rootnode))
# 
# result2 <- xmlParse(file = file2)
# rootnode2 <- xmlRoot(result2)
# info2 <- xmlSApply(rootnode2, function(x) xmlSApply(x, xmlValue))
# names(info2) <- unname(names(rootnode2))
# 
# file <- dir()
# hmdbLib <- vector(mode = "list", length = length(file))
# for(i in 1:length(file)){
#   cat(i, " ")
#   temp.file <- file[i]
#   result <- xmlParse(file = temp.file)
#   rootnode <- xmlRoot(result)
#   info <- xmlSApply(rootnode, function(x) xmlSApply(x, xmlValue))
#   names(info) <- unname(names(rootnode))
#   info <- lapply(info, unname)
#   ms2.mz <- sapply(getNodeSet(doc = rootnode[[26]], path = "//mass-charge"), xmlValue)
#   ms2.int <- sapply(getNodeSet(doc = rootnode[[26]], path = "//intensity"), xmlValue)
#   ms2 <- data.frame("mz" = ms2.mz, "intensity" = ms2.int,
#                     stringsAsFactors = FALSE)
#   info[[26]] <- ms2
# 
#   ms2.info <- info[c(10, 17, 18, 24, 25)]
#   ms2.info <- lapply(ms2.info, function(x){
#     if(length(x) == 0){
#       x <- NA
#     }
#     x
#   })
#   ms2.spec <- info[[26]]
# 
#   ms2.info <- do.call(rbind, ms2.info)
#   ms2.info <- data.frame("name" = rownames(ms2.info), "value" = ms2.info[,1],
#                          stringsAsFactors = FALSE)
#   ms2.info <- tibble::as_tibble(ms2.info)
# 
#   temp.spec <- list(ms2.info = ms2.info, ms2.spec = ms2.spec)
#   hmdbLib[[i]] <- temp.spec
# 
# }
# 
# 
# 
# 
# 
# 
# 
# 
# ###HMDB download
# library("xml2")
# library("rvest")
# library("dplyr")
# library("stringr")
# site1 <- "http://www.hmdb.ca/metabolites"
# site2 <- "?c=hmdb_id&d=up&page="
# 
# hmdb.id <- vector(mode = "list", length = 4561)
# for(page in 1:4561){
#   cat(page, " ")
#   site <- paste(site1,site2,page,sep = "")
#   webpage <- xml2::read_html(site)
# 
#   temp.id <- webpage %>%
#     html_nodes(".metabolite-link, .cas, .metabolite-name") %>%
#     html_text()
#   temp.id <- matrix(temp.id, ncol = 3, byrow = TRUE)
#   temp.id <- as.data.frame(temp.id, stringsAsFactors = FALSE)
#   temp.idx <- which(temp.id[,2] != "")
#   temp.id[temp.idx,1] <- stringr::str_replace(string = temp.id[temp.idx,1],
#                                               pattern = temp.id[temp.idx,2], replacement = "")
#   # colnames(temp.id) <- c("HMDB.ID", "CAS.ID", "Compound.name")
#   hmdb.id[[page]] <- temp.id
# }
# 
# hmdb.info <- do.call(rbind, hmdb.id)
# hmdb.info <- as.data.frame(hmdb.info)
# save(hmdb.info, file = "hmdb.info")
# 
# 
# 
# 
# # getStore <- function(x){
# #   store <- x@store
# #   store.name <- names(store)
# #   store1 <- lapply(store, function(y){
# #     if(class(y) != "list" & class(y) != "NULL"){
# #       return(y)
# #     }else{
# #       return(NA)
# #     }
# #   })
# #
# #
# #   store1[[6]] <- paste(unlist(store[[6]]), collapse = ";")
# #   store1[[9]] <- paste(unlist(store[[9]]), collapse = ";")
# #   taxonomy <- store[[19]]
# #   taxonomy.name <- names(taxonomy)
# #   taxonomy <- as.data.frame(matrix(taxonomy, nrow = 1))
# #   colnames(taxonomy) <- taxonomy.name
# #   store1[[19]] <- taxonomy
# #
# #   ontology <- store[[20]]
# #   ontology <- unlist(ontology)
# #   names(ontology) <- stringr::str_replace_all(string = names(ontology),
# #                                               pattern = "root\\.|descendant[s]{0,1}\\.", replacement = "")
# #   term.idx <- grep(pattern = "term", names(ontology))
# #   ontology <- ontology[term.idx]
# #
# #   lapply(ontology, function(x){
# #
# #   })
# #
# #
# #
# # }
# 
# 
# 
# # setwd("D:/study/database and library/metabolite database")
# load("hmdb.info")
# HMDB.metabolite.database <- vector(mode = "list", length = nrow(hmdb.info))
# library(hmdbQuery)
# 
# for(i in 8858:10000){
#   cat(i, " ")
#   temp.id <- hmdb.info$V1[i]
#   if(is.na(temp.id)){
#     HMDB.metabolite.database[[i]] <- NA
#   }else{
#     temp.met <- HmdbEntry(prefix = "http://www.hmdb.ca/metabolites/",
#                          id = temp.id)
# 
# 
#     compound.name <- temp.met@metabolite
#     # Diseases <- temp.met@diseases
#     HMDB.ID <- temp.met@id
#     Biospecimens <- paste(temp.met@biospecimens, collapse = ";")
#     Tissue <- paste(temp.met@tissues, collapse = ";")
#     store <- temp.met@store
#     store.name <- names(store)
#     store <- matrix(store, nrow = 1)
#     colnames(store) <- store.name
#     # Store[,1] <- unlist(Store[,1])
#     store[,6] <- paste(unname(unlist(store[,6])), collapse = ";")
#     store[,9] <- paste(unname(unlist(store[,9])), collapse = ";")
#     taxonomy <- store[,19][[1]]
# 
#     taxonomy <- unlist(lapply(taxonomy, function(x) paste(x, collapse = ";")))
#     taxonomy.name <- names(taxonomy)
#     taxonomy <- matrix(taxonomy, nrow = 1)
#     colnames(taxonomy) <- taxonomy.name
#     taxonomy <- as.data.frame(taxonomy)
# 
#     store <- data.frame(store, taxonomy, stringsAsFactors = FALSE)
#     store <- store[,-19]
#     temp.info <- data.frame(compound.name, HMDB.ID, Biospecimens, Tissue, store, stringsAsFactors = FALSE)
#     HMDB.metabolite.database[[i]] <- temp.info
#   }
# 
# }
# 
# 
# save(HMDB.metabolite.database, file = "HMDB.metabolite.database")
# 
# 
# #HMDB.metabolite.database   1:10000
# #HMDB.metabolite.database1   10001:30000
# #HMDB.metabolite.database2   30001:50000
# #HMDB.metabolite.database3   50001:70000
# #HMDB.metabolite.database4   70001:90000
# #HMDB.metabolite.database4   90001:114021
# 
# HMDB.metabolite.database[30001:50000] <- HMDB.metabolite.database2[30001:50000]
# HMDB.metabolite.database[[50000]]
# rm(list = c("HMDB.metabolite.database2"))
# 
# load("HMDB.metabolite.database3")
# HMDB.metabolite.database[50001:70000] <- HMDB.metabolite.database3[50001:70000]
# HMDB.metabolite.database[70000]
# rm(list = "HMDB.metabolite.database3")
# 
# 
# load("HMDB.metabolite.database4")
# HMDB.metabolite.database[70001:90000] <- HMDB.metabolite.database4[70001:90000]
# HMDB.metabolite.database[90000]
# rm(list = "HMDB.metabolite.database4")
# 
# 
# load("HMDB.metabolite.database5")
# HMDB.metabolite.database[90001:114021] <- HMDB.metabolite.database5[90001:114021]
# HMDB.metabolite.database[114021]
# rm(list = "HMDB.metabolite.database5")
# 
# 
# 
# ##pending
# load("HMDB.metabolite.database1")
# HMDB.metabolite.database[10001:30000] <- HMDB.metabolite.database1[10001:30000]
# HMDB.metabolite.database[10001]
# rm(list = "HMDB.metabolite.database1")
# 
# 
# save(HMDB.metabolite.database)
# 
# 
# temp.idx <- which(unlist(pbapply::pblapply(HMDB.metabolite.database, is.null)))
# 
# load("hmdb.info")
# 
# 
# 
# 
# 
# HMDB.metabolite.database10 <- vector(mode = "list", length = nrow(hmdb.info))
# library(hmdbQuery)
# 
# for(i in temp.idx){
#   cat(i, " ")
#   temp.id <- hmdb.info$V1[i]
#   if(is.na(temp.id)){
#     HMDB.metabolite.database10[[i]] <- NA
#   }else{
#     temp.error <- try(temp.met <- HmdbEntry(prefix = "http://www.hmdb.ca/metabolites/",
#                                             id = temp.id))
#     if(class(temp.error) == "try-error"){
#       HMDB.metabolite.database10[[i]] <- NA
#     }else{
#       compound.name <- temp.met@metabolite
#       # Diseases <- temp.met@diseases
#       HMDB.ID <- temp.met@id
#       Biospecimens <- paste(temp.met@biospecimens, collapse = ";")
#       Tissue <- paste(temp.met@tissues, collapse = ";")
#       store <- temp.met@store
#       store.name <- names(store)
#       store <- matrix(store, nrow = 1)
#       colnames(store) <- store.name
#       # Store[,1] <- unlist(Store[,1])
#       # store[,6] <- paste(unname(unlist(store[,6])), collapse = ";")
#       # store[,9] <- paste(unname(unlist(store[,9])), collapse = ";")
#       # taxonomy <- store[,19][[1]]
# 
#       # taxonomy <- unlist(lapply(taxonomy, function(x) paste(x, collapse = ";")))
#       # taxonomy.name <- names(taxonomy)
#       # taxonomy <- matrix(taxonomy, nrow = 1)
#       # colnames(taxonomy) <- taxonomy.name
#       # taxonomy <- as.data.frame(taxonomy)
# 
#       store <- data.frame(store, taxonomy, stringsAsFactors = FALSE)
#       store <- store[,-19]
#       temp.info <- data.frame(compound.name, HMDB.ID, Biospecimens, Tissue, store, stringsAsFactors = FALSE)
#       HMDB.metabolite.database10[[i]] <- temp.info
#     }
#   }
# }
# 
# 
# save(HMDB.metabolite.database10, file = "HMDB.metabolite.database10")
# 
# 
# 
# 
# 
# load("HMDB.metabolite.database10")
# HMDB.metabolite.database[temp.idx] <- HMDB.metabolite.database10[temp.idx]
# # HMDB.metabolite.database[10001]
# rm(list = "HMDB.metabolite.database10")
# 
# save(HMDB.metabolite.database, file = "HMDB.metabolite.database")
# 
# temp.idx <- which(unlist(pbapply::pblapply(HMDB.metabolite.database, is.null)))
# temp.idx2 <- which(unlist(pbapply::pblapply(HMDB.metabolite.database, function(x) {
#   all(is.na(x))})))
# 
# HMDB.metabolite.database[temp.idx2] <- NULL
# 
# 
# setwd("D:/study/database and library/metabolite database")
# 
# load("HMDB.metabolite.database")
# 
# temp.idx3 <- which(unlist(pbapply::pblapply(HMDB.metabolite.database, ncol)) != 57)
# HMDB.metabolite.database[[temp.idx3[1]]]$HMDB.ID
# 
# unique.item <- unique(unlist(pbapply::pblapply(HMDB.metabolite.database, function(x){
#   colnames(x)
# })))
# 
# temp.idx4 <- which(unlist(pbapply::pblapply(HMDB.metabolite.database, function(x){
#   length(grep("V1", colnames(x))) > 0
# })))
# 
# 
# 
# unique.item1 <- unique(unlist(pbapply::pblapply(HMDB.metabolite.database1, function(x){
#   colnames(x)
# })))
# save(unique.item1, file = "unique.item1")
# 
# unique.item2 <- unique(unlist(pbapply::pblapply(HMDB.metabolite.database2, function(x){
#   colnames(x)
# })))
# save(unique.item2, file = "unique.item2")
# 
# unique.item3 <- unique(unlist(pbapply::pblapply(HMDB.metabolite.database3, function(x){
#   colnames(x)
# })))
# save(unique.item3, file = "unique.item3")
# 
# unique.item4 <- unique(unlist(pbapply::pblapply(HMDB.metabolite.database4, function(x){
#   colnames(x)
# })))
# save(unique.item4, file = "unique.item4")
# 
# 
# unique.item5 <- unique(unlist(pbapply::pblapply(HMDB.metabolite.database5, function(x){
#   colnames(x)
# })))
# save(unique.item5, file = "unique.item5")
# 
# unique.item6 <- unique(unlist(pbapply::pblapply(HMDB.metabolite.database6, function(x){
#   colnames(x)
# })))
# save(unique.item6, file = "unique.item6")
# 
# unique.item <- unique(c(unique.item1, unique.item2,
#                       unique.item3, unique.item4,
#                       unique.item5, unique.item6))
# 
# 
# load("unique.item")
# 
# 
# # save(HMDB.metabolite.database1, file = "HMDB.metabolite.database1")
# missing.idx1 <- unname(which(unlist(lapply(HMDB.metabolite.database1[1:10000], is.null))))
# save(missing.idx1, file = "missing.idx1")
# 
# # save(HMDB.metabolite.database2, file = "HMDB.metabolite.database2")
# missing.idx2 <- unname(which(unlist(lapply(HMDB.metabolite.database2[10001:30000], is.null))))
# save(missing.idx2, file = "missing.idx2")
# 
# # save(HMDB.metabolite.database3, file = "HMDB.metabolite.database3")
# missing.idx3 <- unname(which(unlist(lapply(HMDB.metabolite.database3[30001:50000], is.null))))
# save(missing.idx3, file = "missing.idx3")
# 
# # save(HMDB.metabolite.database4, file = "HMDB.metabolite.database4")
# missing.idx4 <- unname(which(unlist(lapply(HMDB.metabolite.database4[50001:70000], is.null))))
# save(missing.idx4, file = "missing.idx4")
# 
# # save(HMDB.metabolite.database5, file = "HMDB.metabolite.database5")
# missing.idx5 <- unname(which(unlist(lapply(HMDB.metabolite.database5[70001:90000], is.null))))
# save(missing.idx5, file = "missing.idx5")
# 
# # save(HMDB.metabolite.database6, file = "HMDB.metabolite.database6")
# missing.idx6 <- unname(which(unlist(lapply(HMDB.metabolite.database6[90001:114021], is.null))))
# save(missing.idx6, file = "missing.idx6")
# 
# 
# load("unique.item")
# hmdb.met.database1 <- pbapply::pblapply(HMDB.metabolite.database1, function(x){
#   if(is.null(x)) return(NULL)
#   temp <- as.data.frame(matrix(NA, nrow = 1, ncol = 50), stringsAsFactors = FALSE)
#   colnames(temp) <- unique.item
#   temp.idx1 <- match(colnames(x), unique.item)
#   temp[temp.idx1] <- x
#   temp
# })
# 
# load("unique.item")
# hmdb.met.database2 <- pbapply::pblapply(HMDB.metabolite.database2, function(x){
#   if(is.null(x)) return(NULL)
#   temp <- as.data.frame(matrix(NA, nrow = 1, ncol = 50), stringsAsFactors = FALSE)
#   colnames(temp) <- unique.item
#   temp.idx2 <- match(colnames(x), unique.item)
#   temp[temp.idx2] <- x
#   temp
# })
# 
# load("unique.item")
# hmdb.met.database3 <- pbapply::pblapply(HMDB.metabolite.database3, function(x){
#   if(is.null(x)) return(NULL)
#   temp <- as.data.frame(matrix(NA, nrow = 1, ncol = 50), stringsAsFactors = FALSE)
#   colnames(temp) <- unique.item
#   temp.idx3 <- match(colnames(x), unique.item)
#   temp[temp.idx3] <- x
#   temp
# })
# 
# load("unique.item")
# hmdb.met.database4 <- pbapply::pblapply(HMDB.metabolite.database4, function(x){
#   if(is.null(x)) return(NULL)
#   temp <- as.data.frame(matrix(NA, nrow = 1, ncol = 50), stringsAsFactors = FALSE)
#   colnames(temp) <- unique.item
#   temp.idx4 <- match(colnames(x), unique.item)
#   temp[temp.idx4] <- x
#   temp
# })
# 
# 
# load("unique.item")
# hmdb.met.database5 <- pbapply::pblapply(HMDB.metabolite.database5, function(x){
#   if(is.null(x)) return(NULL)
#   temp <- as.data.frame(matrix(NA, nrow = 1, ncol = 50), stringsAsFactors = FALSE)
#   colnames(temp) <- unique.item
#   temp.idx5 <- match(colnames(x), unique.item)
#   temp[temp.idx5] <- x
#   temp
# })
# 
# load("unique.item")
# hmdb.met.database6 <- pbapply::pblapply(HMDB.metabolite.database6, function(x){
#   if(is.null(x)) return(NULL)
#   temp <- as.data.frame(matrix(NA, nrow = 1, ncol = 50), stringsAsFactors = FALSE)
#   colnames(temp) <- unique.item
#   temp.idx6 <- match(colnames(x), unique.item)
#   temp[temp.idx6] <- x
#   temp
# })
# 
# #
# # hmdb.met.db2 <- hmdb.met.db2[,which(colnames(hmdb.met.db2) %in% colnames(hmdb.met.db1))]
# # hmdb.met.db3 <- hmdb.met.db3[,which(colnames(hmdb.met.db3) %in% colnames(hmdb.met.db1))]
# # hmdb.met.db4 <- hmdb.met.db4[,which(colnames(hmdb.met.db4) %in% colnames(hmdb.met.db1))]
# # hmdb.met.db5 <- hmdb.met.db5[,which(colnames(hmdb.met.db5) %in% colnames(hmdb.met.db1))]
# # hmdb.met.db6 <- hmdb.met.db6[,which(colnames(hmdb.met.db6) %in% colnames(hmdb.met.db1))]
# 
# 
# 
# hmdb.met.database1 <- do.call(rbind, hmdb.met.database1)
# hmdb.met.database2 <- do.call(rbind, hmdb.met.database2)
# hmdb.met.database3 <- do.call(rbind, hmdb.met.database3)
# hmdb.met.database4 <- do.call(rbind, hmdb.met.database4)
# hmdb.met.database5 <- do.call(rbind, hmdb.met.database5)
# hmdb.met.database6 <- do.call(rbind, hmdb.met.database6)
# 
# 
# rm(HMDB.metabolite.database1)
# 
# rm(HMDB.metabolite.database2)
# 
# rm(HMDB.metabolite.database3)
# 
# rm(HMDB.metabolite.database4)
# 
# rm(HMDB.metabolite.database5)
# 
# rm(HMDB.metabolite.database6)
# 
# 
# hmdb.met.db1 <- hmdb.met.database1
# 
# hmdb.met.db1[,3] <- stringr::str_trim(string = hmdb.met.db1[,3], side = "both")
# hmdb.met.db1[,4] <- stringr::str_trim(string = hmdb.met.db1[,4], side = "both")
# hmdb.met.db1[,5] <- unname(unlist(hmdb.met.db1[,5]))
# hmdb.met.db1[,6] <- unname(unlist(hmdb.met.db1[,6]))
# hmdb.met.db1[,7] <- unname(unlist(hmdb.met.db1[,7]))
# hmdb.met.db1[,8] <- unname(unlist(hmdb.met.db1[,8]))
# 
# table(unlist(lapply(hmdb.met.db1[,9], length)))
# hmdb.met.db1[,9] <- unname(unlist(hmdb.met.db1[,9]))
# 
# 
# hmdb.met.db1[,10] <- unname(unlist(lapply(hmdb.met.db1[,10], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# table(unlist(lapply(hmdb.met.db1[,11], length)))
# hmdb.met.db1[,11] <- unname(unlist(hmdb.met.db1[,11]))
# 
# table(unlist(lapply(hmdb.met.db1[,12], length)))
# hmdb.met.db1[,12] <- unname(unlist(hmdb.met.db1[,12]))
# 
# hmdb.met.db1[,13] <- unname(unlist(lapply(hmdb.met.db1[,13], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# table(unlist(lapply(hmdb.met.db1[,14], length)))
# hmdb.met.db1[,14] <- unname(unlist(lapply(hmdb.met.db1[,14], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# table(unlist(lapply(hmdb.met.db1[,15], length)))
# hmdb.met.db1[,15] <- unname(unlist(lapply(hmdb.met.db1[,15], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# table(unlist(lapply(hmdb.met.db1[,16], length)))
# hmdb.met.db1[,16] <- unname(unlist(lapply(hmdb.met.db1[,16], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# hmdb.met.db1[1:10,17]
# table(unlist(lapply(hmdb.met.db1[,17], length)))
# hmdb.met.db1[,17] <- unname(unlist(lapply(hmdb.met.db1[,17], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,18]
# table(unlist(lapply(hmdb.met.db1[,18], length)))
# hmdb.met.db1[,18] <- unname(unlist(lapply(hmdb.met.db1[,18], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# hmdb.met.db1[1:10,19]
# table(unlist(lapply(hmdb.met.db1[,19], length)))
# hmdb.met.db1[,19] <- unname(unlist(lapply(hmdb.met.db1[,19], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# 
# hmdb.met.db1[1:10,20]
# table(unlist(lapply(hmdb.met.db1[,20], length)))
# hmdb.met.db1[,20] <- unname(unlist(lapply(hmdb.met.db1[,20], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,21]
# table(unlist(lapply(hmdb.met.db1[,21], length)))
# hmdb.met.db1[,21] <- unname(unlist(lapply(hmdb.met.db1[,21], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,22]
# table(unlist(lapply(hmdb.met.db1[,22], length)))
# hmdb.met.db1[,22] <- unname(unlist(lapply(hmdb.met.db1[,22], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# # hmdb.met.db1[1:10,23]
# # table(unlist(lapply(hmdb.met.db1[,22], length)))
# # hmdb.met.db1[,22] <- unname(unlist(lapply(hmdb.met.db1[,22], function(x){
# #   paste(unname(unlist(x)), collapse = ";")
# # })))
# 
# hmdb.met.db1[1:10,25]
# table(unlist(lapply(hmdb.met.db1[,25], length)))
# hmdb.met.db1[,25] <- unname(unlist(lapply(hmdb.met.db1[,25], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,33]
# table(unlist(lapply(hmdb.met.db1[,33], length)))
# hmdb.met.db1[,33] <- unname(unlist(lapply(hmdb.met.db1[,33], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# hmdb.met.db1[1:10,34]
# table(unlist(lapply(hmdb.met.db1[,34], length)))
# hmdb.met.db1[,34] <- unname(unlist(lapply(hmdb.met.db1[,34], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,35]
# table(unlist(lapply(hmdb.met.db1[,35], length)))
# hmdb.met.db1[,35] <- unname(unlist(lapply(hmdb.met.db1[,35], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,36]
# table(unlist(lapply(hmdb.met.db1[,36], length)))
# hmdb.met.db1[,36] <- unname(unlist(lapply(hmdb.met.db1[,36], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,37]
# table(unlist(lapply(hmdb.met.db1[,37], length)))
# hmdb.met.db1[,37] <- unname(unlist(lapply(hmdb.met.db1[,37], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,38]
# table(unlist(lapply(hmdb.met.db1[,38], length)))
# hmdb.met.db1[,38] <- unname(unlist(lapply(hmdb.met.db1[,38], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,39]
# table(unlist(lapply(hmdb.met.db1[,39], length)))
# hmdb.met.db1[,39] <- unname(unlist(lapply(hmdb.met.db1[,39], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,40]
# table(unlist(lapply(hmdb.met.db1[,40], length)))
# hmdb.met.db1[,40] <- unname(unlist(lapply(hmdb.met.db1[,40], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,41]
# table(unlist(lapply(hmdb.met.db1[,41], length)))
# hmdb.met.db1[,41] <- unname(unlist(lapply(hmdb.met.db1[,41], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,42]
# table(unlist(lapply(hmdb.met.db1[,42], length)))
# hmdb.met.db1[,42] <- unname(unlist(lapply(hmdb.met.db1[,42], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,43]
# table(unlist(lapply(hmdb.met.db1[,43], length)))
# hmdb.met.db1[,43] <- unname(unlist(lapply(hmdb.met.db1[,43], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# 
# hmdb.met.db1[1:10,44]
# table(unlist(lapply(hmdb.met.db1[,44], length)))
# hmdb.met.db1[,44] <- unname(unlist(lapply(hmdb.met.db1[,44], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
#   hmdb.met.db1[1:10,45]
# table(unlist(lapply(hmdb.met.db1[,45], length)))
# hmdb.met.db1[,45] <- unname(unlist(lapply(hmdb.met.db1[,45], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# hmdb.met.db1[1:10,46]
# table(unlist(lapply(hmdb.met.db1[,46], length)))
# hmdb.met.db1[,46] <- unname(unlist(lapply(hmdb.met.db1[,46], function(x){
#   paste(unname(unlist(x)), collapse = ";")
# })))
# 
# 
# save(hmdb.met.db1, file = "hmdb.met.db1")
# 
# hmdb.met.db1 <- hmdb.met.db1[,-c(26,27,28,30,31,47,49,50)]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# save(hmdb.met.database1, file = "hmdb.met.database1")
# save(hmdb.met.database2, file = "hmdb.met.database2")
# save(hmdb.met.database3, file = "hmdb.met.database3")
# save(hmdb.met.database4, file = "hmdb.met.database4")
# save(hmdb.met.database5, file = "hmdb.met.database5")
# save(hmdb.met.database6, file = "hmdb.met.database6")
# 
# 
# 
# 
# 
# 
# # install.packages("sqldf")
# #
# #
# # HMDB.metabolite.database <- do.call(rbind, HMDB.metabolite.database)
# #
# #
# # library(sqldf)
# #
# # sqldf('SELECT age, circumference FROM Orange WHERE Tree = 1 ORDER BY circumference ASC')
# # library(DBI)
# # con <- dbConnect(RMySQL::MySQL(), group = "my-db")
# #
# # install.packages("RMySQL")
# #
# # library(RMySQL)
# # help(package = "RMySQL")
# #
# # library(DBI)
# # # Connect to my-db as defined in ~/.my.cnf
# # con <- dbConnect(RMySQL::MySQL(), group = "my-db")
# #
# #
# # conn <- dbConnect(RMySQL::MySQL(), dbname = "shendatabase",
# #                   # username = "root",
# #                   password = "shen1990",
# #                   host = "localhost"
# #                   # port=3306
# #                   )
# #
# #
# # library(RODBC)
# # channel <- odbcConnect(dsn = "shendatabase", uid = "root", pwd = "shen1990")
# # temp <- sqlTables(channel)
# #
# #
# # temp <- sqlFetch(channel = channel, sqtable = "iris")
# #
# #
# # head(iris)
# # sqlSave(channel = channel, dat = iris, rownames = "id", addPK = TRUE)
# #
# # sqlDrop(channel = channel, sqtable = "iris")
# # odbcClose(channel)
# #
# #
# # conn <- dbConnect(MySQL(),
# #                   dbname = "mysql",
# #                   username = "root",
# #                   password = "shen1990",
# #                   host = "127.0.0.1",
# #                   port = 3306)
# #
# #
# # library(sqldf)
# #
# #
# #
# # library("RMySQL")
# # # Create a connection Object to MySQL database.
# # # We will connect to the sampel database named "testdb" that comes with MySql installation.
# #
# # conn <- dbConnect(RMySQL::MySQL(), dbname = "shendatabase",
# #                   username = "root",
# #                   password = "shen1990",
# #                   host = "localhost"
# #                   # port=3306
# # )
# #
# # class(conn)
# # dbListTables(conn)
# # summary(conn, verbose = TRUE)
# #
# # t_demo <- data.frame(a = seq(1:10), b = letters[1:10], c = rnorm(10))
# #
# # RMySQL::dbWriteTable(conn = conn, name = "t_demo", value = t_demo, overwrite = TRUE)
# # dbListTables(conn)
# # RMySQL::dbReadTable(conn = conn, name = "t_demo")
# # RMySQL::dbWriteTable(conn = conn, name = "t_demo", value = t_demo, append = TRUE)
# # rs <- dbSendQuery(conn, "SELECT * FROM t_demo where c>0")
# # dbSendQuery(conn = conn, statement = 'drop table if exists t_demo')
# # dbRemoveTable(conn = conn, name = "t_demo", overwrite = TRUE)
# # dbListTables(conn)
# 
# 
# library("xml2")
# library("rvest")
# library("dplyr")
# library("stringr")
# site1 <- "http://www.hmdb.ca/metabolites"
# site2 <- "?c=hmdb_id&d=up&page="
# 
# hmdb.id2 <- vector(mode = "list", length = 4561)
# for(page in 3001:4561){
#   cat(page, " ")
#   site <- paste(site1,site2,page,sep = "")
#   webpage <- xml2::read_html(site)
#   
#   temp.id <- webpage %>%
#     html_nodes(".metabolite-link, .cas, .metabolite-name") %>%
#     html_text()
#   temp.id <- matrix(temp.id, ncol = 3, byrow = TRUE)
#   temp.id <- as.data.frame(temp.id, stringsAsFactors = FALSE)
#   temp.idx <- which(temp.id[,2] != "")
#   temp.id[temp.idx,1] <- stringr::str_replace(string = temp.id[temp.idx,1],
#                                               pattern = temp.id[temp.idx,2], replacement = "")
#   # colnames(temp.id) <- c("hmdb.id2", "CAS.ID", "Compound.name")
#   hmdb.id2[[page]] <- temp.id
# }
# 
# save(hmdb.id2, file = "hmdb.id2")
# 
# 
# 
# 
# 
