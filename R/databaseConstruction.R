# ###In house database
# setwd("D:/study/database and library/inhouse/Metabolite database/RPLC")
# metabolite.table1 <- readxl::read_xlsx("metabolite.information.rplc.xlsx", sheet = 1)
# metabolite.table2 <- readxl::read_xlsx("metabolite.information.rplc.xlsx", sheet = 2)
# metabolite.table3 <- readxl::read_xlsx("metabolite.information.rplc.xlsx", sheet = 3)
#
# metabolite.table <- rbind(metabolite.table1,
#                           metabolite.table2,
#                           metabolite.table3)
# metabolite.table <- metabolite.table[,-c(1)]
# metabolite.table$RPLC_pos <- as.numeric(metabolite.table$RPLC_pos)
# metabolite.table$RPLC_neg <- as.numeric(metabolite.table$RPLC_neg)
#
# remove.idx <-
#   which(apply(metabolite.table[,c(9,10)], 1, function(x){
#     if(all(is.na(x))) return(TRUE)
#     return(FALSE)
#   }))
#
# metabolite.table <- metabolite.table[-remove.idx,]
#
#
# rt.error <- metabolite.table[,10] - metabolite.table[,9]
# rt.error <- as.numeric(rt.error[,1])
#
# rt.error[is.na(rt.error)] <- 0
#
# rt.error <<- abs(rt.error) * 60
#
# which(rt.error > 30)
#
#
# metabolite.table[287,]
#
# metabolite.table[287,10] <- metabolite.table[287,9]
#
# metabolite.table$RPLC_pos <- metabolite.table$RPLC_pos * 60
# metabolite.table$RPLC_neg <- metabolite.table$RPLC_neg * 60
#
#
# Submitter <- as.character(metabolite.table$Library)
#
# Submitter <- as.character(metabolite.table$Library)
# Compound.name <- as.character(metabolite.table$`Chemical name`)
#
# mz <- as.character(metabolite.table$Mass)
# RT <- apply(metabolite.table[,c(9,10)], 1, function(x){
#   x <- as.numeric(x)
#   x <- x[!is.na(x)]
#   mean(x)
# })
#
# Formula <- metabolite.table$Formula
#
# Family <- metabolite.table$Family
#
# mz.pos <- as.numeric(stringr::str_trim(metabolite.table$`[M+H]+`))
# mz.neg <- as.numeric(stringr::str_trim(metabolite.table$`[M-H]-`))
#
#
# metabolite.info.rplc <- data.frame(Lab.ID = NA, Compound.name = Compound.name,
#                                    mz = mz, RT = RT, CAS.ID = NA, HMDB.ID = NA,
#                                    KEGG.ID = NA, Formula = Formula,
#                                    mz.pos = mz.pos,
#                                    mz.neg = mz.neg, Submitter = Submitter,
#                                    Family = Family,
#                                    stringsAsFactors = FALSE
#                                    )
#
# metabolite.info.rplc$Lab.ID <- paste("MS", 1:nrow(metabolite.info.rplc), sep = "_")
#
# write.csv(metabolite.info.rplc, "metabolite.info.rplc.csv", row.names = FALSE)

# databaseConstruction(path = ".")
##------------------------------------------------------------------------------
#' @title Construct in-house or public MS2 database for metID.
#' @description Construct MS2 spectra database according to mzXML data and compound information table (csv format).
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param path Work directory.
#' @param version The version of you database. Default is 0.0.1.
#' @param metabolite.info.name The metabolite information table name, it must be csv format.
#' The demo data can be got from the `demoData` package.
#' Please see https://jaspershen.github.io/metID/articles/metID.html
#' @param source The source of your database.
#' @param link Website link of the source.
#' @param creater Creater name. For example, Xiaotao Shen.
#' @param email email address.
#' @param rt Do the metabolites have RT information or not?. If not, set it as FALSE.
#' @param mz.tol m/z tolerance for the match between metabolites and precursor m/z of MS2 spectra.
#' @param rt.tol RT tolerance for the match between metabolites and precursor m/z of MS2 spectra.
#' @param threads The number of threads
#' @return A databaseClass object.
#' @seealso The example and demo data of this function can be found
#' https://jaspershen.github.io/metID/articles/metID.html
#' @export

# sxtTools::setwd_project()
# setwd("test_data/database/")


# path = "."
# version = "0.0.1"
# metabolite.info.name = "metabolite.info_RPLC.csv"
# source = "Michael Snyder Lab"
# link = "http://snyderlab.stanford.edu/"
# creater = "Xiaotao Shen"
# email = "shenxt1990@163.com"
# rt = TRUE
# mz.tol = 15
# rt.tol = 30
# threads = 3

# database <-
#   databaseConstruction(metabolite.info.name = "metabolite.info_RPLC.csv")

setGeneric(
  name = "databaseConstruction",
  def = function(path = ".",
                 version = "0.0.1",
                 metabolite.info.name = "metabolite.info.csv",
                 source = "Michael Snyder Lab",
                 link = "http://snyderlab.stanford.edu/",
                 creater = "Xiaotao Shen",
                 email = "shenxt1990@163.com",
                 rt = TRUE,
                 mz.tol = 15,
                 rt.tol = 30,
                 threads = 3) {
    
    cat(crayon::yellow(
      "`databaseConstruction()` is deprecated, use `construct_database()`."
    ))  
    
    ##check data first
    file <- dir(path)
    if (all(file != metabolite.info.name)) {
      cat(crayon::red("No", metabolite.info.name, "in your", path, "\n"))
      return(NULL)
    }
    
    if (all(file != "POS")) {
      cat(crayon::red("No POS file in your", path, "\n"))
    } else{
      file_pos <- dir(file.path(path, "POS"))
      if (length(file_pos) == 0) {
        cat(crayon::red("No mzXML files in POS folder\n"))
      } else{
        if (sum(stringr::str_detect(file_pos, "mzXML")) == 0) {
          cat(crayon::red("No mzXML files in POS folder\n"))
        }
      }
    }
    
    if (all(file != "NEG")) {
      cat(crayon::red("No NEG file in your", path, "\n"))
    } else{
      file_neg <- dir(file.path(path, "NEG"))
      if (length(file_neg) == 0) {
        cat(crayon::red("No mzXML files in NEG folder\n"))
      } else{
        if (sum(stringr::str_detect(file_neg, "mzXML")) == 0) {
          cat(crayon::red("No mzXML files in NEG folder\n"))
        }
      }
    }
    
    ##read metabolite information
    cat(crayon::green("Reading metabolite information...\n"))
    metabolite.info <-
      readTable(file = file.path(path, metabolite.info.name),
                col_types = readr::cols())
    
    cat(crayon::green("Reading positive MS2 data...\n"))
    
    file.pos <-
      dir(file.path(path, 'POS'), full.names = TRUE)
    
    ms2.data.pos <-
      readMZXML(file = file.pos, threads = threads)
    
    ms1.info.pos <- lapply(ms2.data.pos, function(x) {
      x[[1]]
    })
    
    ms1.info.pos <- do.call(rbind, ms1.info.pos)
    
    ms1.info.pos$file <- basename(ms1.info.pos$file)
    
    ms2.info.pos <- lapply(ms2.data.pos, function(x) {
      x[[2]]
    })
    
    rm(list = "ms2.data.pos")
    
    cat(crayon::red("OK\n"))
    
    cat(crayon::green("Reading negative MS2 data...\n"))
    
    file.neg <-
      dir(file.path(path, 'NEG'), full.names = TRUE)
    
    ms2.data.neg <-
      readMZXML(file = file.neg, threads = threads)
    
    ms1.info.neg <- lapply(ms2.data.neg, function(x) {
      x[[1]]
    })
    
    ms1.info.neg <- do.call(rbind, ms1.info.neg)
    
    ms1.info.neg$file <- basename(ms1.info.neg$file)
    
    ms2.info.neg <- lapply(ms2.data.neg, function(x) {
      x[[2]]
    })
    
    rm(list = "ms2.data.neg")
    
    cat(crayon::red("OK\n"))
    
    ###---------------------------------------------------------------------------
    cat(crayon::green("Matching metabolites with MS2 spectra (positive)...\n"))
    
    match.result.pos <-
      SXTMTmatch(
        data1 = as.data.frame(metabolite.info[, c("mz.pos", "RT")]),
        data2 = ms1.info.pos[, c(2, 3)],
        mz.tol = mz.tol,
        rt.tol = rt.tol,
        rt.error.type = "abs"
      )
    
    match.result.pos <- data.frame(match.result.pos,
                                   "file" = ms1.info.pos$file[match.result.pos[, 2]],
                                   stringsAsFactors = FALSE)
    
    unique.idx1 <- unique(match.result.pos[, 1])
    
    spectra.pos <-
      pbapply::pblapply(unique.idx1, function(idx) {
        temp.match.result.pos <-
          match.result.pos[which(match.result.pos == idx), , drop = FALSE]
        if (nrow(temp.match.result.pos) == 0)
          return(NULL)
        temp.submitter <- metabolite.info$Submitter[idx]
        temp.match.result.pos <-
          temp.match.result.pos[grep(temp.submitter, temp.match.result.pos[, 9]),]
        if (nrow(temp.match.result.pos) == 0)
          return(NULL)
        
        if (nrow(temp.match.result.pos) == 1) {
          temp.ms2.pos <- ms2.info.pos[temp.match.result.pos[1, 2]]
          names(temp.ms2.pos) <-
            stringr::str_extract(string = temp.match.result.pos[1, 9],
                                 pattern = "NCE[0-9]{1,3}")
          return(temp.ms2.pos)
        }
        
        unique.file.name <-
          unique(temp.match.result.pos$file)
        
        temp.ms2.pos <-
          lapply(unique.file.name, function(temp.name) {
            temp.x <-
              temp.match.result.pos[which(temp.match.result.pos$file == temp.name), , drop = FALSE]
            temp.idx <-
              which.max(unlist(lapply(ms2.info.pos[temp.x[, 2]], function(y) {
                sum(y[, 2])
              })))
            ms2.info.pos[[temp.x[temp.idx, 2]]]
          })
        
        names(temp.ms2.pos) <-
          stringr::str_extract(string = unique.file.name,
                               pattern = "NCE[0-9]{1,3}")
        temp.ms2.pos
        
        
      })
    
    names(spectra.pos) <-
      metabolite.info$Lab.ID[unique.idx1]
    
    spectra.pos <-
      spectra.pos[which(!unlist(lapply(spectra.pos, is.null)))]
    
    cat(crayon::red("OK\n"))
    ###---------------------------------------------------------------------------
    cat(crayon::green("Matching metabolites with MS2 spectra (negative)...\n"))
    
    match.result.neg <-
      SXTMTmatch(
        data1 = as.data.frame(metabolite.info[, c("mz.neg", "RT")]),
        data2 = ms1.info.neg[, c(2, 3)],
        mz.tol = mz.tol,
        rt.tol = rt.tol,
        rt.error.type = "abs"
      )
    
    match.result.neg <- data.frame(match.result.neg,
                                   "file" = ms1.info.neg$file[match.result.neg[, 2]],
                                   stringsAsFactors = FALSE)
    
    unique.idx1 <- unique(match.result.neg[, 1])
    
    spectra.neg <-
      pbapply::pblapply(unique.idx1, function(idx) {
        temp.match.result.neg <-
          match.result.neg[which(match.result.neg == idx), , drop = FALSE]
        if (nrow(temp.match.result.neg) == 0)
          return(NULL)
        temp.submitter <- metabolite.info$Submitter[idx]
        temp.match.result.neg <-
          temp.match.result.neg[grep(temp.submitter, temp.match.result.neg[, 9]),]
        if (nrow(temp.match.result.neg) == 0)
          return(NULL)
        
        if (nrow(temp.match.result.neg) == 1) {
          temp.ms2.neg <- ms2.info.neg[temp.match.result.neg[1, 2]]
          names(temp.ms2.neg) <-
            stringr::str_extract(string = temp.match.result.neg[1, 9],
                                 pattern = "NCE[0-9]{1,3}")
          return(temp.ms2.neg)
        }
        
        unique.file.name <-
          unique(temp.match.result.neg$file)
        
        temp.ms2.neg <-
          lapply(unique.file.name, function(temp.name) {
            temp.x <-
              temp.match.result.neg[which(temp.match.result.neg$file == temp.name), , drop = FALSE]
            temp.idx <-
              which.max(unlist(lapply(ms2.info.neg[temp.x[, 2]], function(y) {
                sum(y[, 2])
              })))
            ms2.info.neg[[temp.x[temp.idx, 2]]]
          })
        
        names(temp.ms2.neg) <-
          stringr::str_extract(string = unique.file.name,
                               pattern = "NCE[0-9]{1,3}")
        temp.ms2.neg
      })
    
    names(spectra.neg) <-
      metabolite.info$Lab.ID[unique.idx1]
    
    spectra.neg <-
      spectra.neg[which(!unlist(lapply(spectra.neg, is.null)))]
    
    cat(crayon::red("OK\n"))
    
    Spectra <- list("Spectra.positive" = spectra.pos,
                    "Spectra.negative" = spectra.neg)
    
    database.info <- list(
      "Version" = version,
      "Source" = source,
      "Link" = link,
      "Creater" = creater,
      "Email" = email,
      "RT" = rt
    )
    
    spectra.info <- as.data.frame(metabolite.info)
    rm(list = "metabolite.info")
    
    msDatabase0.0.1 <- new(
      Class = "databaseClass",
      database.info = database.info,
      spectra.info = spectra.info,
      spectra.data = Spectra
    )
    
    msDatabase0.0.1@database.info$RT <-
      ifelse(all(is.na(msDatabase0.0.1@spectra.info$RT)), FALSE, TRUE)
    cat(crayon::bgRed("All done!\n"))
    return(msDatabase0.0.1)
  }
)

###S4 class for function metIdentification
#' An S4 class to represent MS1 or MS2 database.
#'
#' @slot database.info Database information.
#' @slot spectra.info Metabolites in database.
#' @slot spectra.data MS2 spectra data.

setClass(
  Class = "databaseClass",
  representation(
    database.info = "list",
    spectra.info = "data.frame",
    spectra.data = "list"
  ),
  prototype = list(
    database.info = list(),
    spectra.info = data.frame(matrix(nrow = 0, ncol = 0), stringsAsFactors = FALSE),
    spectra.data = list()
  )
)

setMethod(
  f = "show",
  signature = "databaseClass",
  definition = function(object) {
    cat(crayon::yellow("-----------Base information------------\n"))
    cat(crayon::green("Version:", object@database.info$Version, "\n"))
    cat(crayon::green("Source:", object@database.info$Source, "\n"))
    cat(crayon::green("Link:", object@database.info$Link, "\n"))
    cat(
      crayon::green(
        "Creater:",
        object@database.info$Creater,
        "(",
        object@database.info$Email,
        ")\n"
      )
    )
    cat(crayon::green(
      ifelse(
        object@database.info$RT,
        "With RT information\n",
        "Without RT informtaion\n"
      )
    ))
    cat(crayon::yellow("-----------Spectral information------------\n"))
    cat(crayon::green(
      "There are",
      ncol(object@spectra.info),
      "items of metabolites in database:\n"
    ))
    cat(crayon::green(paste(
      colnames(object@spectra.info), collapse = "; "
    ), "\n"))
    
    cat(crayon::green("There are", length(
      unique(object@spectra.info$Compound.name)
    ), "metabolites in total\n"))
    
    cat(crayon::green(
      "There are",
      length(object@spectra.data$Spectra.positive),
      "metabolites in positive mode with MS2 spectra.\n"
    ))
    
    cat(crayon::green(
      "There are",
      length(object@spectra.data$Spectra.negative),
      "metabolites in negative mode with MS2 spectra.\n"
    ))
    
    ce.pos <-
      unique(unlist(lapply(
        object@spectra.data$Spectra.positive, names
      )))
    
    ce.neg <-
      unique(unlist(lapply(
        object@spectra.data$Spectra.negative, names
      )))
    
    cat(crayon::green("Collision energy in positive mode (number:):\n"))
    cat(crayon::green("Total number:", length(ce.pos), "\n"))
    cat(crayon::green(paste(head(ce.pos, 10), collapse = "; "), "\n"))
    cat(crayon::green("Collision energy in negative mode:\n"))
    cat(crayon::green("Total number:", length(ce.neg), "\n"))
    cat(crayon::green(paste(head(ce.neg, 10), collapse = "; "), "\n"))
  }
)

#' @title Get MS2 spectra of peaks from databaseClass object
#' @description Get MS2 spectra of peaks from databaseClass object.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param lab.id The lab ID of metabolite.
#' @param database Database (databaseClass object).
#' @param polarity positive or negative.
#' @param ce Collision value.
#' @return A MS2 spectrum (data.frame).
#' @export
#' @seealso The example and demo data of this function can be found
#' https://jaspershen.github.io/metID/articles/metID.html

setGeneric(
  name = "getMS2spectrum",
  def = function(lab.id,
                 database,
                 polarity = c("positive", "negative"),
                 ce = "30") {
    # cat(crayon::yellow(
    # "`getMS2spectrum()` is deprecated, use `get_ms2_spectrum()`."
    # ))
    polarity <- match.arg(polarity)
    if (class(database) != "databaseClass") {
      stop("The database must be databaseClass object.\n")
    }
    pol <- ifelse(polarity == "positive", 1, 2)
    temp <-
      database@spectra.data[[pol]][[match(lab.id, names(database@spectra.data[[pol]]))]]
    temp[[match(ce, names(temp))]]
  }
)


#' @title Get MS2 spectra of peaks from databaseClass object
#' @description Get MS2 spectra of peaks from databaseClass object.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param lab.id The lab ID of metabolite.
#' @param database Database (databaseClass object).
#' @param polarity positive or negative.
#' @param ce Collision value.
#' @return A MS2 spectrum (data.frame).
#' @export
#' @seealso The example and demo data of this function can be found
#' https://jaspershen.github.io/metID/articles/metID.html

setGeneric(
  name = "get_ms2_spectrum",
  def = function(lab.id,
                 database,
                 polarity = c("positive", "negative"),
                 ce = "30") {
    polarity <- match.arg(polarity)
    if (class(database) != "databaseClass") {
      stop("The database must be databaseClass object.\n")
    }
    pol <- ifelse(polarity == "positive", 1, 2)
    temp <-
      database@spectra.data[[pol]][[match(lab.id, names(database@spectra.data[[pol]]))]]
    temp[[match(ce, names(temp))]]
  }
)



#'
#' #' setGeneric(
#' #'   name = "get_ms2_spectrum",
#' #'   def = function(object,
#' #'                  lab.id,
#' #'                  polarity = c("positive", "negative"),
#' #'                  ce = "30") standardGeneric(f = "get_ms2_spectrum"),
#' #'   signature = "object"
#' #'
#' #' )
#' #'
#'
#' #' setMethod(
#' #'   f = "get_ms2_spectrum",
#' #'   signature = "databaseClass",
#' #'   definition = function(object,
#' #'                         lab.id,
#' #'                         polarity = c("positive", "negative"),
#' #'                         ce = "30") {
#' #'     polarity <- match.arg(polarity)
#' #'     pol <- ifelse(polarity == "positive", 1, 2)
#' #'     temp <-
#' #'       object@spectra.data[[pol]][[match(lab.id, names(object@spectra.data[[pol]]))]]
#' #'     temp <- temp[[match(ce, names(temp))]]
#' #'     if(is.null(temp)){
#' #'       temp
#' #'     }else{
#' #'       tibble::as_tibble(temp)
#' #'     }
#' #'   }
#' #' )
