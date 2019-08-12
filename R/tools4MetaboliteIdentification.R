# ms1.info = ms1.info
# ms2.info = ms2.info
# polarity = polarity
# ce = ce
# database = database
# ms1.match.ppm = ms1.match.ppm
# ms2.match.ppm = ms2.match.ppm
# mz.ppm.thr = mz.ppm.thr
# ms2.match.tol = ms2.match.tol
# rt.match.tol = rt.match.tol
# column = column
# ms1.match.weight = ms1.match.weight
# rt.match.weight = rt.match.weight
# ms2.match.weight = ms2.match.weight
# total.score.tol = total.score.tol
# candidate.num = candidate.num
# adduct.table = adduct.table
# threads = threads
# fraction.weight = fraction.weight
# dp.forward.weight = dp.forward.weight
# dp.reverse.weight = dp.reverse.weight
#---------------------------------------------------------------------------

setGeneric(name = "metIdentification", 
           def = function(ms1.info, 
                          ms2.info, 
                          polarity = c("positive", "negative"),
                          ce = '30',
                          database,
                          ms1.match.ppm = 25,
                          ms2.match.ppm = 30,
                          mz.ppm.thr = 400,
                          ms2.match.tol = 0.5,
                          rt.match.tol = 30,
                          column = "rp",
                          ms1.match.weight = 0.25,
                          rt.match.weight = 0.25,
                          ms2.match.weight = 0.5,
                          total.score.tol = 0.5,
                          candidate.num = 3,
                          adduct.table,
                          threads = 3,
                          fraction.weight = 0.3,
                          dp.forward.weight = 0.6,
                          dp.reverse.weight = 0.1
                          ){
             polarity <- match.arg(polarity)
             ms1.info$mz <- as.numeric(ms1.info$mz)
             ms1.info$rt <- as.numeric(ms1.info$rt)
             ##filter the database for using
             ##polarity
             if(polarity == "positive"){
               spectra.data <- database@spectra.data$Spectra.positive
             }else{
               spectra.data <- database@spectra.data$Spectra.negative
             }
             
             ##get the MS2 spectra within the CE values
             if(any(ce == "all")){
               cat("You use all CE values.\n")
               ce <- unique(unlist(lapply(spectra.data, function(x){
                 names(x)
               })))
             }else{
               spectra.data <- lapply(spectra.data, function(x){
                 x <- x[which(names(x) %in% ce)]
                 if(length(x) == 0) return(NULL)
                 return(x)
               })  
             }
             
             ##remove some metabolites which have no spectra
             spectra.data <- spectra.data[which(!unlist(lapply(spectra.data, is.null)))]
             if(length(spectra.data) == 0){
               stop("No spectra with CE: ",paste(ce, collapse = ", "), " in you database.\n")
             }
             
             spectra.info <- database@spectra.info
             spectra.info <- spectra.info[which(spectra.info$Lab.ID %in% names(spectra.data)),]

             rm(list = c("database"))
             cat("\n")
             cat('Identifing metabolites with MS/MS database...\n')
             
             
               identification.result <- BiocParallel::bplapply(1:nrow(ms1.info), 
                                                       FUN = identifyPeak, 
                                                       BPPARAM = BiocParallel::SnowParam(workers = threads,
                                                                                         progressbar = TRUE),
                                                       ms1.info = ms1.info,
                                                       ms2.info = ms2.info,
                                                       spectra.info = spectra.info,
                                                       spectra.data = spectra.data,
                                                       ppm.ms1match = ms1.match.ppm,
                                                       ppm.ms2match = ms2.match.ppm,
                                                       mz.ppm.thr = mz.ppm.thr,
                                                       ms2.match.tol = ms2.match.tol,
                                                       rt.match.tol = rt.match.tol,
                                                       ms1.match.weight = ms1.match.weight,
                                                       rt.match.weight = rt.match.weight,
                                                       ms2.match.weight = ms2.match.weight,
                                                       total.score.tol = total.score.tol,
                                                       adduct.table = adduct.table,
                                                       candidate.num = candidate.num,
                                                       fraction.weight = fraction.weight,
                                                       dp.forward.weight = dp.forward.weight,
                                                       dp.reverse.weight = dp.reverse.weight)
               
               names(identification.result) <- ms1.info$name
               identification.result <- identification.result[which(!unlist(lapply(identification.result, function(x)all(is.na(x)))))]
               if(length(identification.result) == 0){
                 return(list(NULL))
               }
               return(identification.result)
           })

#---------------------------------------------------------------------------
# idx <- 262
# ms1.info = ms1.info
# ms2.info = ms2.info
# spectra.info = spectra.info
# spectra.data = spectra.data
# ppm.ms1match = ms1.match.ppm
# ppm.ms2match = ms2.match.ppm
# mz.ppm.thr = mz.ppm.thr
# ms2.match.tol = ms2.match.tol
# rt.match.tol = rt.match.tol
# ms1.match.weight = ms1.match.weight
# rt.match.weight = rt.match.weight
# ms2.match.weight = ms2.match.weight
# total.score.tol = total.score.tol
# adduct.table = adduct.table
# candidate.num = candidate.num
# fraction.weight = fraction.weight
# dp.forward.weight = dp.forward.weight
# dp.reverse.weight = dp.reverse.weight


# identifyPeak(idx = 264,
#              ms1.info = ms1.info,
#              ms2.info = ms2.info,
#              spectra.info = spectra.info,
#              spectra.data = spectra.data,
#              adduct.table = adduct.table)

setGeneric(name = "identifyPeak",
           def = function(idx,
                          ms1.info,
                          ms2.info,
                          spectra.info,
                          spectra.data,
                          ppm.ms1match = 25,
                          ppm.ms2match = 30,
                          mz.ppm.thr = 400,
                          ms2.match.tol = 0.5,
                          rt.match.tol = 30,
                          ms1.match.weight = 0.25,
                          rt.match.weight = 0.25,
                          ms2.match.weight = 0.5,
                          total.score.tol = 0.5,
                          adduct.table,
                          candidate.num = 3,
                          fraction.weight = 0.3,
                          dp.forward.weight = 0.6,
                          dp.reverse.weight = 0.1,
                          # report top 10 satisfactories
                          ...){
             pk.precursor <- ms1.info[idx, ]
             rm(list = c("ms1.info"))
             pk.mz <- pk.precursor$mz
             pk.rt <- pk.precursor$rt
             pk.spec <- ms2.info[[idx]]
             rm(list = c("ms2.info"))
             
             if (length(pk.spec) == 0) {
               return(NA)
             }

             spectra.mz <- apply(adduct.table, 1, function(x){
               temp.n <- stringr::str_extract(string = as.character(x[1]), pattern = "[0-9]{1}M")
               temp.n <- as.numeric(stringr::str_replace(string = temp.n, pattern = "M", replacement = ""))
               temp.n[is.na(temp.n)] <- 1
               as.numeric(x[2]) + temp.n*as.numeric(spectra.info$mz)
             })
             colnames(spectra.mz) <- adduct.table[,1]
             rownames(spectra.mz) <- spectra.info$Lab.ID
             
             ###mz match
             match.idx <- apply(spectra.mz, 1, function(x){
               temp.mz.error <- abs(x - pk.mz)*10^6/ifelse(pk.mz < 400, 400, pk.mz)
               temp.mz.match.score <- exp(-0.5*(temp.mz.error/(ppm.ms1match))^2)
               data.frame(
                 "addcut" = names(temp.mz.error)[which(temp.mz.error < ppm.ms1match)],
                 "mz.error" = temp.mz.error[which(temp.mz.error < ppm.ms1match)],
                    "mz.match.score" = temp.mz.match.score[which(temp.mz.error < ppm.ms1match)],
                    stringsAsFactors = FALSE)
             })
             
             rm(list = c("spectra.mz", "adduct.table"))
             
             ##remove some none matched
             match.idx <- match.idx[which(unlist(lapply(match.idx, function(x){
               nrow(x)
             })) != 0)]
             
             if (length(match.idx) == 0) {
               return(NA)
             }
             
             match.idx <- mapply(function(x, y){
               list(data.frame("Lab.ID" = y, x, stringsAsFactors = FALSE))
             },
             x = match.idx,
             y = names(match.idx))
             
             match.idx <- do.call(rbind, match.idx)
             # match.idx <- data.frame(rownames(match.idx), match.idx, stringsAsFactors = FALSE)
             colnames(match.idx) <- c("Lab.ID", "Adduct", "mz.error", "mz.match.score")
             rownames(match.idx) <- NULL
             
             ###RT match
             RT.error <- t(apply(match.idx, 1, function(x){
               temp.rt.error <- abs(pk.rt - spectra.info$RT[match(x[1], spectra.info$Lab.ID)])
               temp.rt.match.score <- exp(-0.5*(temp.rt.error/(rt.match.tol))^2)
               c(temp.rt.error, temp.rt.match.score)
             }))
             colnames(RT.error) <- c("RT.error", "RT.match.score")
             match.idx <- data.frame(match.idx, RT.error, stringsAsFactors = FALSE)
             rm(list = c("RT.error"))
             
             if(any(!is.na(match.idx$RT.error))){
               RT.error <- match.idx$RT.error
               RT.error[is.na(RT.error)] <- rt.match.tol - 1
               match.idx <- match.idx[RT.error < rt.match.tol, , drop = FALSE]
             }
             
             if(nrow(match.idx) == 0) {
               return(NA)
             }
             
             ###MS2 spectra match
             ms2.score <- apply(match.idx, 1, function(x){
               x <- as.character(x)
               lib.spec <- spectra.data[[x[1]]]
               dp <- lapply(lib.spec, function(y){
                 tinyTools::getSpectraMatchScore(exp.spectrum = pk.spec, 
                                                lib.spectrum = y, 
                                      ppm.tol = ppm.ms2match,
                                      mz.ppm.thr = mz.ppm.thr,
                                      fraction.weight = fraction.weight,
                                      dp.forward.weight = dp.forward.weight,
                                      dp.reverse.weight = dp.reverse.weight
                                      )
               })
               dp <- dp[which.max(unlist(dp))]
               dp <- unlist(dp)
               data.frame("CE" = names(dp), 
                          "SS" = dp, stringsAsFactors = FALSE)
             })
             
             ms2.score <- do.call(rbind, ms2.score)
             rownames(ms2.score) <- NULL
             
             match.idx <- data.frame(match.idx, ms2.score, stringsAsFactors = FALSE)
             match.idx <- match.idx[which(match.idx$SS > ms2.match.tol), , drop = FALSE]
             rm(list = c("ms2.score"))
             if(nrow(match.idx) == 0) {
               return(NA)
             }
             
             ###total score
             total.score <- apply(match.idx, 1, function(x){
               if(is.na(x["RT.match.score"])){
                 as.numeric(x["mz.match.score"]) * (ms1.match.weight + rt.match.weight/2) +
                   as.numeric(x['SS']) * (ms2.match.weight + rt.match.weight/2)
               }else{
                as.numeric(x['mz.match.score']) * ms1.match.weight + 
                   as.numeric(x['RT.match.score']) * rt.match.weight +
                   as.numeric(x["SS"]) * ms2.match.weight
               }
             })
             
             match.idx <- data.frame(match.idx,
                                     "Total.score" = total.score, 
                                     stringsAsFactors = FASLE)
             rm(list = c("total.score"))
             match.idx <- match.idx[match.idx$Total.score > total.score.tol, , drop = FALSE]
             
             if(nrow(match.idx) == 0) {
               return(NA)
             }
             
             match.idx <- match.idx[order(match.idx$Total.score, decreasing = TRUE),]
             if(nrow(match.idx) > candidate.num){
               match.idx <- match.idx[1:candidate.num,]
             }
             ##add other information
             match.idx <- data.frame(spectra.info[match(match.idx$Lab.ID, spectra.info$Lab.ID), 
                          c("Compound.name", "CAS.ID", "HMDB.ID", "KEGG.ID")], match.idx, 
                        stringsAsFactors = FALSE)
             match.idx <- match.idx[order(match.idx$Total.score, decreasing = TRUE), , drop = FALSE]
             
             return(match.idx)
           })


#----------------------------------------------------------------------------
setGeneric(name = "ListMGF",
           def = function(file){
             mgf.data <- readLines(file)
             nl.rec.new <- 1
             idx.rec <- 1
             rec.list <- list()
             for(nl in 1:length(mgf.data))
             {
               if(mgf.data[nl] == "END IONS")
               {
                 rec.list[idx.rec] <- list(Compound = mgf.data[nl.rec.new : nl])
                 nl.rec.new <- nl + 1
                 idx.rec <- idx.rec + 1
               }
             }
             rec.list
           })


#---------------------------------------------------------------------------
#'@title readMSP
#'@description Read MSP data.
#'@author Xiaotao Shen
#'\email{shenxt1990@@163.com}
#'@param file The vector of names of ms2 files. MS2 file must be msp format.
#'@return Return ms2 data. This is a list.
#'@export
setGeneric('readMSP', function(file) {
  msp.data <- readLines(file)
  # n.tot <- length(msp.data)
  n.null <- which(msp.data == '')
  
  temp.idx1 <- c(1, n.null[-length(n.null)])
  temp.idx2 <- n.null - 1
  
  temp.idx <- data.frame(temp.idx1, temp.idx2,
                         stringsAsFactors = FALSE)
  temp.idx <- apply(temp.idx, 1, list)
  
  temp.idx <- lapply(temp.idx, unlist)
  
  # n.spec <- which(grepl('^\\d', msp.data))
  # n.info <- seq(n.tot)[-c(n.spec, n.null)]
  
  pbapply::pboptions(style = 1)
  info.spec <- pbapply::pblapply(temp.idx, function(idx) {
    
    temp.msp.data <- msp.data[idx[1]:idx[2]]
    
    temp.msp.data <- temp.msp.data[temp.msp.data != ""]
    info.idx <- grep("[A-Za-z]", temp.msp.data)
    temp.info <- temp.msp.data[info.idx]
    temp.info <- strsplit(temp.info, split = ":")
    temp.info <- do.call(rbind, temp.info)
    temp.info <- data.frame(temp.info,
                            stringsAsFactors = FALSE)
    temp.info[,2] <- stringr::str_trim(temp.info[,2])
    colnames(temp.info) <- rownames(temp.info) <- NULL
    rownames(temp.info) <- temp.info[,1]
    temp.info <- temp.info[,-1,drop = FALSE]
    
    temp.spec <- temp.msp.data[-info.idx]
    
    if(length(temp.spec) != 0){
      if(length(grep(" ", temp.spec[1])) == 1){
        temp.spec <- strsplit(temp.spec, split = ' ')
      }
      
      if(length(grep("\t", temp.spec[1])) == 1){
        temp.spec <- strsplit(x = temp.spec, split = "\t")
      }
      
      temp.spec <- do.call(rbind, temp.spec)
      temp.spec <- data.frame(temp.spec,
                              stringsAsFactors = FALSE)
      colnames(temp.spec) <- c('mz', 'intensity')
      rownames(temp.spec) <- NULL
      temp.spec$mz <- as.numeric(as.character(temp.spec$mz))
      temp.spec$intensity <- as.numeric(temp.spec$intensity)
      temp.spec <- temp.spec[temp.spec$intensity != 0,]
    }else{
      temp.spec <- NULL
    }
    
    list('info' = temp.info,
         'spec' = temp.spec)
  })
  
  mz.idx <- grep("[Mm][Zz]", rownames(info.spec[[1]][[1]]))
  rt.idx <- grep("Time|TIME|time|RT|rt|Rt", rownames(info.spec[[1]][[1]]))
  
  ##fix bug in msp data from metAnalyzer
  if(length(rt.idx)==0){
    cat("The msp data are from MetAnalyzer software.\n")
    rt.idx <- grep("NAME|Name|name", rownames(info.spec[[1]][[1]]))
    ##rt.idx is the name of peak
    info.spec <- lapply(info.spec, function(x){
      info <- x[[1]]
      mz <- as.numeric(info[mz.idx, 1])
      rt <- as.character(info[rt.idx, 1])
      info <- c(mz, rt)
      names(info) <- c("mz", "rt")
      x[[1]] <- info
      x
    })
  }else{
    info.spec <- lapply(info.spec, function(x){
      info <- x[[1]]
      mz <- as.numeric(info[mz.idx, 1])
      rt <- as.numeric(info[rt.idx, 1])
      info <- c(mz, rt)
      names(info) <- c("mz", "rt")
      x[[1]] <- info
      x
    })
  }
  
  remove.idx <- which(unlist(lapply(info.spec, function(x) is.null(x[[2]]))))
  if(length(remove.idx) > 0){
    info.spec <- info.spec[-remove.idx]
  }
  
  info.spec <- info.spec
})

#---------------------------------------------------------------------------
#'@title readMSP_MoNA
#'@description Read MSP data from MoNA.
#'@author Xiaotao Shen
#'\email{shenxt1990@@163.com}
#'@param file The vector of names of ms2 files. MS2 file must be msp. The msp data must from MoNA.
#'@return Return ms2 data. This is a list.
#'@export
setGeneric('readMSP_MoNA', function(file) {
  cat("Read MSP data.\n")
  msp.data <- readr::read_lines(file)
  n.null <- which(msp.data == '')
  temp.idx1 <- c(1, n.null[-length(n.null)])
  temp.idx2 <- n.null - 1
  
  temp.idx <- data.frame(temp.idx1, temp.idx2,
                         stringsAsFactors = FALSE)
  
  rm(list = c("temp.idx1", "temp.idx2"))
  gc(verbose = FALSE)
  
  temp.idx <- apply(temp.idx, 1, list)
  
  temp.idx <- lapply(temp.idx, unlist)
  temp.idx <- temp.idx[which(unlist(lapply(temp.idx, function(x){
    x[1] != x[2]
  })))]

  pbapply::pboptions(style = 1)
  # fix bug
  # for(idx in 1:length(temp.idx)){
  # cat(idx, " ")
  # idx <- temp.idx[[idx]]
  
  cat("Transforming...\n")
  info.spec <- pbapply::pblapply(temp.idx, function(idx) {
    if(idx[1] == idx[2]) return(NULL)
    temp.msp.data <- msp.data[idx[1]:idx[2]]
    temp.msp.data <- temp.msp.data[temp.msp.data != ""]
    info.idx <- grep("[A-Za-z]", temp.msp.data)
    temp.info <- temp.msp.data[info.idx]
    temp.info <- stringr::str_split(string = temp.info, pattern = ":", n = 2)
    temp.info <- do.call(rbind, temp.info)
    temp.info <- data.frame(temp.info,
                            stringsAsFactors = FALSE)
    temp.info[,2] <- stringr::str_trim(temp.info[,2], side = "both")
    temp.info[,1] <- stringr::str_trim(temp.info[,1], side = "both")
    colnames(temp.info) <- rownames(temp.info) <- NULL
    #combine synons
    if(length(grep("Synon", temp.info[,1])) > 1){
      Synon <- stringr::str_c(temp.info[stringr::str_which(string = temp.info[,1], pattern = "Synon"),2, 
                                        drop = TRUE],collapse = ";")  
      temp.info[which(temp.info[,1] == "Synon")[1], 2] <- Synon
      temp.info <- temp.info[!duplicated(temp.info[,1]),]
      
    }
    
    rownames(temp.info) <- temp.info[,1]
    temp.info <- temp.info[,-1,drop = FALSE]
    temp.spec <- temp.msp.data[-info.idx]
    
    if(length(temp.spec) != 0){
      if(length(grep(" ", temp.spec[1])) == 1){
        temp.spec <- strsplit(temp.spec, split = ' ')
      }
      
      if(length(grep("\t", temp.spec[1])) == 1){
        temp.spec <- strsplit(x = temp.spec, split = "\t")
      }
      
      temp.spec <- do.call(rbind, temp.spec)
      temp.spec <- data.frame(temp.spec,
                              stringsAsFactors = FALSE)
      colnames(temp.spec) <- c('mz', 'intensity')
      rownames(temp.spec) <- NULL
      temp.spec$mz <- as.numeric(as.character(temp.spec$mz))
      temp.spec$intensity <- as.numeric(temp.spec$intensity)
      temp.spec <- temp.spec[temp.spec$intensity != 0,]
    }else{
      temp.spec <- NULL
    }
    
    list('info' = temp.info,
         'spec' = temp.spec)
  }
  )
  
  rm(list = c("msp.data", "temp.idx"))
  gc()
  ##remove NULL
  info.spec <- info.spec[!unlist(lapply(info.spec, is.null))]
  
  remove.idx <- which(unlist(lapply(info.spec, function(x) is.null(x[[2]]))))
  if(length(remove.idx) > 0){
    info.spec <- info.spec[-remove.idx]
  }
  rm(list = c("remove.idx"))
  gc()
  info.spec <- info.spec
})

#------------------------------------------------------------------------------
setGeneric(name = "WriteMSP",
           def = function(info, fn.pre, spec.all){
             fn.save <- paste0(fn.pre, '_spectra.msp')
             #
             sink(fn.save)
             for (idx in seq(nrow(info))) {
               if (!is.null(spec.all[[idx]])) {
                 if (nrow(spec.all[[idx]]) > 0) {
                   mz <- info[idx, 'Mass']
                   spec <- spec.all[[idx]]
                   cat('IDX: ', idx, '\n', sep = '')
                   cat('PRECURSORMZ: ', mz, '\n', sep = '')
                   cat('Num Peaks: ', nrow(spec), '\n', sep = '')
                   for (nr in seq(nrow(spec))) {
                     cat(paste(spec[nr, ], collapse = ' '), '\n', sep = '')
                   }
                   cat('\n')
                 }
               }
             }
             sink()
           })



#'@title readMZXML
#'@description Read mzXML data.
#'@author Xiaotao Shen
#'\email{shenxt1990@@163.com}
#'@param file The vector of names of ms2 files. MS2 file must be mzXML or mzML.
#'@param threads Thread number
#'@return Return ms2 data. This is a list.
#'@export

setGeneric(name = "readMZXML",
           def = function(file,
                          threads = 3){
             # pbapply::pboptions(style = 1)
             cat("Reading MS2 data...\n")
             # mzxml.data.list <- pbapply::pblapply(file, ListMGF)
             ms2 <- MSnbase::readMSData(files = file, msLevel. = 2, mode = "onDisk")
             cat("Processing...\n")
             
             new.ms2 <- ProtGenerics::spectra(object = ms2)
             rm(list = c("ms2"))
             temp.fun <- function(idx, ms2){
               temp.ms2 <- ms2[[idx]]
               rm(list = c("ms2"))
               info <- data.frame(name = paste("mz", temp.ms2@precursorMz,
                                               "rt", temp.ms2@rt, sep = ""),
                                  "mz" = temp.ms2@precursorMz, 
                                  "rt" = temp.ms2@rt, 
                                  "file" = file[temp.ms2@fromFile],
                                  stringsAsFactors = FALSE)
               duplicated.name <- unique(info$name[duplicated(info$name)])
               if(length(duplicated.name) > 0){
                 lapply(duplicated.name, function(x){
                   info$name[which(info$name == x)] <- paste(x, c(1:sum(info$name == x)), sep = "_")
                 })
               }
               
               rownames(info) <- NULL
               spec <- data.frame("mz" = temp.ms2@mz,
                                  "intensity" = temp.ms2@intensity, 
                                  stringsAsFactors = FALSE)
               list(info = info, spec = spec)
             }
             
             new.ms2 <- BiocParallel::bplapply(X = c(1:length(new.ms2)), 
                                               FUN = temp.fun,
                                               ms2 = new.ms2,
                                               BPPARAM = BiocParallel::SnowParam(workers = threads,
                                                                                 progressbar = TRUE))
             new.ms2 <- new.ms2
           })





# plotMS2match(matched.info = temp.matched.info, exp.spectrum = exp.spectrum, 
#              lib.spectrum = lib.spectrum, database = database)
# 
  # matched.info <- temp.matched.info
  # range.mz = range.mz
  # exp.spectrum = exp.spectrum
  # lib.spectrum = lib.spectrum
  # col.lib = col.lib
  # col.exp = col.exp
  # ce = ce
  # polarity = polarity
  # database = database
setGeneric(name = "plotMS2match",
           def = function(matched.info,
                          range.mz,
                          ppm.tol = 30,
                          mz.ppm.thr = 400,
                          exp.spectrum,
                          lib.spectrum,
                          polarity = c("positive", "negative"),
                          xlab = "Mass to charge ratio (m/z)",
                          ylab = "Relative intensity",
                          col.lib = "red",
                          col.exp = "black",
                          ce = "30",
                          title.size = 15,
                          lab.size = 15,
                          axis.text.size =15,
                          legend.title.size = 15,
                          legend.text.size = 15,
                          database
           ){
             polarity <- match.arg(polarity)
             exp.spectrum[,1] <- as.numeric(exp.spectrum[,1])
             exp.spectrum[,2] <- as.numeric(exp.spectrum[,2])
             
             lib.spectrum[,1] <- as.numeric(lib.spectrum[,1])
             lib.spectrum[,2] <- as.numeric(lib.spectrum[,2])
             
             exp.spectrum[,2] <- exp.spectrum[,2]/max(exp.spectrum[,2])
             lib.spectrum[,2] <- lib.spectrum[,2]/max(lib.spectrum[,2])
             
             exp.spectrum <- as.data.frame(exp.spectrum)
             lib.spectrum <- as.data.frame(lib.spectrum)
             if(missing(range.mz)){
               range.mz <- c(min(exp.spectrum[,1], lib.spectrum[,1]),
                             max(exp.spectrum[,1], lib.spectrum[,1]))
               
             }
             
             matched.spec <- tinyTools::ms2Match(exp.spectrum = exp.spectrum,
                                                 lib.spectrum = lib.spectrum, 
                                                 ppm.tol = ppm.tol, 
                                                 mz.ppm.thr = mz.ppm.thr)
             matched.idx <- which(matched.spec[, "Lib.intensity"] > 0 & 
                                    matched.spec[, "Exp.intensity"] > 0)
             
             plot <- ggplot2::ggplot(data = matched.spec) +
               ggplot2::geom_segment(mapping = ggplot2::aes(x = Exp.mz, y = Exp.intensity - Exp.intensity,
                                                            xend = Exp.mz, yend = Exp.intensity),
                                     colour = col.exp) +
               ggplot2::geom_point(data = matched.spec[matched.idx,,drop = FALSE], 
                                   mapping = ggplot2::aes(x = Exp.mz, y = Exp.intensity), colour = col.exp) +
               ggplot2::xlim(range.mz[1], range.mz[2]) +
               ggplot2::ylim(-1, 1) +
               ggplot2::labs(x = xlab, y = ylab, title = as.character(matched.info["Compound.name"])) +
               ggplot2::theme_bw() +
               ggplot2::theme(
                 # axis.line = ggplot2::element_line(arrow = ggplot2::arrow()),
                 plot.title = ggplot2::element_text(color = "black", size = title.size,
                                                    face = "plain",
                                                    hjust = 0.5),
                 axis.title = ggplot2::element_text(color = "black", size = lab.size,
                                                    face = "plain"),
                 axis.text = ggplot2::element_text(color = "black", size = axis.text.size,
                                                   face = "plain"),
                 legend.title = ggplot2::element_text(color = "black", size = legend.title.size,
                                                      face = "plain"),
                 legend.text = ggplot2::element_text(color = "black", size = legend.text.size,
                                                     face = "plain")
               )

               temp.info <- unlist(database@spectra.info[match(matched.info["Lab.ID"], database@spectra.info$Lab.ID),,drop = TRUE])
               temp.info <- temp.info[!is.na(temp.info)]
               temp.info <- temp.info[sapply(temp.info, stringr::str_count) < 50]
               temp.info <- paste(names(temp.info), temp.info, sep = ": ")

               plot <- plot +
                 ggplot2::annotate(geom = "text", x = -Inf, y = Inf,
                                   label = paste(temp.info, collapse = "\n"),
                                   hjust = 0, vjust = 1)

             temp.info2 <- matched.info[c("mz.error", "RT.error", "SS", "Total.score", "Adduct", "CE")]
             temp.info2 <- temp.info2[!is.na(temp.info2)]
             
             temp.info2 <- paste(names(temp.info2), temp.info2, sep = ": ")
             plot <- plot +
               ggplot2::annotate(geom = "text", x = -Inf, y = -Inf,
                                 label = paste(temp.info2, collapse = "\n"),
                                 hjust = 0, vjust = 0)
             
             plot <- plot +
               ggplot2::annotate(geom = "text", x = Inf, y = Inf,
                                 label = "Experiment MS2 spectrum",
                                 color = col.exp,
                                 hjust = 1, vjust = 1) +
               ggplot2::annotate(geom = "text", x = Inf, y = -Inf,
                                 label = "Database MS2 spectrum",
                                 color = col.lib,
                                 hjust = 1, vjust = -1)

             plot <- plot +
               ggplot2::geom_segment(data = matched.spec,
                                     mapping = ggplot2::aes(x = Lib.mz, y = Lib.intensity - Lib.intensity,
                                                            xend = Lib.mz, yend = -Lib.intensity),
                                     colour = col.lib) +
               ggplot2::geom_point(data = matched.spec[matched.idx, , drop = FALSE], 
                                   mapping = ggplot2::aes(x = Lib.mz, y = -Lib.intensity), colour = col.lib)
             plot
           })



#'@title readMGF
#'@description Read MGF data.
#'@author Xiaotao Shen
#'\email{shenxt1990@@163.com}
#'@param file The vector of names of ms2 files. MS2 file must be mgf.
#'@return Return ms2 data. This is a list.
#'@export

setGeneric(name = "readMGF",
           def = function(file){
             pbapply::pboptions(style = 1)
             cat("Reading MS2 data\n")
             # mgf.data.list <- pbapply::pblapply(file, ListMGF)
             ms2 <- pbapply::pblapply(file, function(mgf.data) {
               mgf.data <- ListMGF(mgf.data)
               # nl.spec <- grep('^\\d', mgf.data)
               nl.spec <- lapply(mgf.data, function(x) grep('^\\d', x))
               info.mz <- lapply(mgf.data, function(x) grep('^PEPMASS', x, value = T))
               info.rt <- lapply(mgf.data, function(x) grep('^RTINSECONDS', x, value = T))
               
               info.mz <- unlist(info.mz)
               #for orbitrap data, the intensity of precursor ion should be removed
               info.mz <- unlist(lapply(strsplit(x = info.mz, split = " "), function(x) x[1]))
               info.mz <- as.numeric(gsub(pattern = "\\w+=", "", info.mz))
               info.rt <- unlist(info.rt)
               info.rt <- as.numeric(gsub(pattern = "\\w+=", "", info.rt))
               
               if(length(mgf.data) == 1){
                 spec <- mapply(function(x, y){
                   temp <- do.call(rbind, strsplit(x[y], split = " "))
                   list(temp)
                 },
                 x = mgf.data,
                 y = nl.spec) 
               }else{
                 spec <- mapply(function(x, y){
                   do.call(rbind, strsplit(x[y], split = " "))
                 },
                 x = mgf.data,
                 y = nl.spec) 
               }
               
               spec <- lapply(spec, function(x){
                 temp <- cbind(as.numeric(x[,1]),as.numeric(x[,2]))
                 temp <- matrix(temp, ncol = 2)
                 # if(nrow(temp) > 0) temp <- temp[temp[,2] >= max(temp[,2])*0.01,]
                 temp <- matrix(temp, ncol = 2)
                 colnames(temp) <- c("mz", "intensity")
                 temp
               })
               
               ms2 <- mapply(function(x,y,z){
                 info <- c(y, z)
                 names(info) <- c("mz", "rt")
                 spectrum <- as.matrix(x)
                 temp <- list(info, spectrum)
                 names(temp) <- c("info", "spec")
                 list(temp)
               },
               x = spec,
               y = info.mz,
               z = info.rt)
               
               ms2
               
             })
             
             
             spec.info <- ms2[[1]]
             if(length(ms2) > 1){
               for(i in 2:length(ms2)){
                 spec.info <- c(spec.info, ms2[[i]])
               }
             }
             
             remove.idx <- which(unlist(lapply(spec.info, function(x) nrow(x[[2]]))) == 0)
             if(length(remove.idx) != 0) spec.info <- spec.info[-remove.idx]
             # ##remove noise
             # cat("\n")
             # cat("Remove noise of MS/MS spectra...\n")
             # spec.info <- pbapply::pblapply(spec.info, function(x){
             #   temp.spec <- x[[2]]
             #   temp.spec <- removeNoise(temp.spec)
             #   x[[2]] <- temp.spec
             #   x
             # })
             
             spec.info <- spec.info
           })


#----------------------------------------------------------------------------
setGeneric(name = "ListMGF",
           def = function(file){
             mgf.data <- readLines(file)
             nl.rec.new <- 1
             idx.rec <- 1
             rec.list <- list()
             for(nl in 1:length(mgf.data))
             {
               if(mgf.data[nl]=="END IONS")
               {
                 rec.list[idx.rec] <- list(Compound = mgf.data[nl.rec.new : nl])
                 nl.rec.new <- nl + 1
                 idx.rec <- idx.rec + 1
               }
             }
             rec.list
           })
