#' @title Identify single peak based on database.
#' @description We can use this function to identify single peak, you can just provide m/z or rt, or you can also provide MS2 spectrum for this peak.
#' \lifecycle{experimental}
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param ms1.mz m/z value of the peaks
#' @param ms1.rt rt value of the peaks
#' @param ms2 MS2 spectra of the peaks. It must be a two column data frame, the
#' first column is m/z and the second column is the intensity.
#' @param ms1.match.ppm Precursor match ppm tolerance.
#' @param ms2.match.ppm Fragment ion match ppm tolerance.
#' @param mz.ppm.thr Accurate mass tolerance for m/z error calculation.
#' @param ms2.match.tol MS2 match (MS2 similarity) tolerance.
#' @param fraction.weight The weight for matched fragments.
#' @param dp.forward.weight Forward dot product weight.
#' @param dp.reverse.weight Reverse dot product weight.
#' @param rt.match.tol RT match tolerance.
#' @param polarity The polarity of data, "positive"or "negative".
#' @param ce Collision energy. Please confirm the CE values in your database. Default is "all".
#' @param column "hilic" (HILIC column) or "rp" (reverse phase).
#' @param ms1.match.weight The weight of MS1 match for total score calculation.
#' @param rt.match.weight The weight of RT match for total score calculation.
#' @param ms2.match.weight The weight of MS2 match for total score calculation.
#' @param path Work directory.
#' @param total.score.tol Total score tolerance. The total score are refering to MS-DIAL.
#' @param candidate.num The number of candidate.
#' @param database MS2 database name.
#' @param threads Number of threads
#' @return A metIdentifyClass object.
#' @export
#' @examples 
#' \dontrun{
#' mz <- 472.3032
#' rt <- 772.906
#' ms2 <- data.frame(
#' mz = c(81.38455,82.19755,85.02840,86.5934,86.98958,89.48135,90.70250,
#'          93.03886, 102.09140, 103.03903, 116.01658, 127.98412, 
#'          134.06819, 152.46967, 162.02180, 162.05521, 162.11261),
#'   intensity = c(1396.341,1488.730,15473.604, 1740.842,2158.014,1351.686,
#'                 1547.099,1325.864,22441.047,76217.016,17809.395,
#'                 1439.743, 1729.786, 1543.765, 2228.743, 
#'                 3508.225, 529120.000),
#'   stringsAsFactors = FALSE
#' )
#' 
#' path <- file.path(".", "example")
#' dir.create(path = path, showWarnings = FALSE)
#' database <- system.file("ms2_database", package = "metID")
#'
#' file.copy(from = file.path(database, "msDatabase_rplc0.0.2"), 
#'          to = path, overwrite = TRUE, recursive = TRUE)
#' 
#' annotation_result <- 
#' identify_single_peak(ms1.mz = mz, 
#'                     ms1.rt = rt, 
#'                     ms2 = ms2, 
#'                     ms1.match.ppm = 15, 
#'                     rt.match.tol = 30,
#'                     ms2.match.tol = 0.5, 
#'                     database = "msDatabase_rplc0.0.2",
#'                     path = path)
#'                     
#' annotation_result
#' }


# ms1.mz
# ms1.rt
# ms2
# ms1.match.ppm = 25
# ms2.match.ppm = 30
# mz.ppm.thr = 400
# ms2.match.tol = 0.5
# fraction.weight = 0.3
# dp.forward.weight = 0.6
# dp.reverse.weight = 0.1
# rt.match.tol = 30
# polarity = "positive"
# ce = "all"
# column = "hilic"
# ms1.match.weight = 0.25
# rt.match.weight = 0.25
# ms2.match.weight = 0.5
# path = "."
# total.score.tol = 0.5
# candidate.num = 3
# database
# threads = 3

setGeneric(
  name = "identify_single_peak",
  def = function(ms1.mz,
                 ms1.rt,
                 ms2,
                 ms1.match.ppm = 25,
                 ms2.match.ppm = 30,
                 mz.ppm.thr = 400,
                 ms2.match.tol = 0.5,
                 fraction.weight = 0.3,
                 dp.forward.weight = 0.6,
                 dp.reverse.weight = 0.1,
                 rt.match.tol = 30,
                 polarity = c("positive", "negative"),
                 ce = "all",
                 column = c("hilic", "rp"),
                 ms1.match.weight = 0.25,
                 rt.match.weight = 0.25,
                 ms2.match.weight = 0.5,
                 path = ".",
                 total.score.tol = 0.5,
                 candidate.num = 3,
                 database,
                 threads = 3) {
    ###Check data
    if (missing(database)) {
      stop("No database is provided.\n")
    }
    
    if (missing(ms1.mz) | missing(ms1.rt) | missing(ms2)) {
      stop("ms1.mz, ms1.rt or ms2 is not provided.\n")
    }
    
    ##parameter specification
    polarity <- match.arg(polarity)
    column <- match.arg(column)
    
    #load MS2 database
    database.name <- database
    load(file.path(path, database.name))
    database <- get(database.name)
    if (class(database) != "databaseClass") {
      stop("database must be databaseClass object\n")
    }
    
    ce.list.pos <-
      unique(unlist(lapply(
        database@spectra.data$Spectra.positive, names
      )))
    ce.list.neg <-
      unique(unlist(lapply(
        database@spectra.data$Spectra.negative, names
      )))
    ce.list <-
      ifelse(polarity == "positive", ce.list.pos, ce.list.neg)
    if (all(ce %in% ce.list) & ce != "all") {
      stop("All ce values you set are not in database. Please check it.\n")
      ce <- ce[ce %in% ce.list]
    }
    rm(list = c("ce.list.pos", "ce.list.neg", "ce.list"))
    
    ##ce values
    if (all(ce != "all")) {
      if (polarity == "positive") {
        ce.list <-
          unique(unlist(
            lapply(database@spectra.data$Spectra.positive, function(x) {
              names(x)
            })
          ))
        if (length(grep("Unknown", ce.list)) > 0) {
          ce <-
            unique(c(ce, grep(
              pattern = "Unknown", ce.list, value = TRUE
            )))
        }
      } else{
        ce.list <-
          unique(unlist(
            lapply(database@spectra.data$Spectra.negative, function(x) {
              names(x)
            })
          ))
        if (length(grep("Unknown", ce.list)) > 0) {
          ce <-
            unique(c(ce, grep(
              pattern = "Unknown", ce.list, value = TRUE
            )))
        }
      }
    }
    
    ##RT in database or not
    if (!database@database.info$RT) {
      cat(crayon::yellow("No RT information in database.\nThe weight of RT have been set as 0.\n"))
    }
    #------------------------------------------------------------------
    ##load adduct table
    if (polarity == "positive" & column == "hilic") {
      data("hilic.pos", envir = environment())
      adduct.table <- hilic.pos
    }
    
    if (polarity == "positive" & column == "rp") {
      data("rp.pos", envir = environment())
      adduct.table <- rp.pos
    }
    
    if (polarity == "negative" & column == "hilic") {
      data("hilic.neg", envir = environment())
      adduct.table <- hilic.neg
    }
    
    if (polarity == "negative" & column == "rp") {
      data("rp.neg", envir = environment())
      adduct.table <- rp.neg
    }
    
    
    name <- paste(paste("mz", ms1.mz, sep = ""),
                  paste("rt", ms1.rt, sep = ""), sep = "")
    
    file <- "test"
    
    ms1.info <- data.frame(name,
                           mz = ms1.mz,
                           rt = ms1.rt,
                           file,
                           stringsAsFactors = FALSE)
    
    if (any(class(ms2) == "matrix") |
        any(class(ms2) == "data.frame")) {
      ms2.info <- list(ms2)
    } else{
      ms2.info <- ms2
    }
    
    names(ms2.info) <- ms1.info$name
    
    if (c(length(ms1.mz), length(ms1.rt), length(ms2.info)) %>%
        unique() %>%
        length() != 1) {
      stop("Length of ms1.mz, ms1.rt and ms2 must be same.\n")
    }
    
    ms2Matchresult <-
      metIdentification(
        ms1.info = ms1.info,
        ms2.info = ms2.info,
        polarity = polarity,
        ce = ce,
        database = database,
        ms1.match.ppm = ms1.match.ppm,
        ms2.match.ppm = ms2.match.ppm,
        mz.ppm.thr = mz.ppm.thr,
        ms2.match.tol = ms2.match.tol,
        rt.match.tol = rt.match.tol,
        column = column,
        ms1.match.weight = ms1.match.weight,
        rt.match.weight = rt.match.weight,
        ms2.match.weight = ms2.match.weight,
        total.score.tol = total.score.tol,
        candidate.num = candidate.num,
        adduct.table = adduct.table,
        threads = threads,
        fraction.weight = fraction.weight,
        dp.forward.weight = dp.forward.weight,
        dp.reverse.weight = dp.reverse.weight
      )
    
    
    ms1.data <- ms1.info %>%
      dplyr::select(-file)
    
    match.result <- data.frame(
      Index1.ms1.data = 1:nrow(ms1.info),
      Index.ms2.spectra = 1:nrow(ms1.info),
      MS1.peak.name = ms1.info$name,
      MS2.spectra.name = ms1.info$name,
      stringsAsFactors = FALSE
    )
    
    return.result <- new(
      Class = "metIdentifyClass",
      ms1.data = ms1.data,
      ms1.info = ms1.info,
      ms2.info = ms2.info,
      identification.result = ms2Matchresult,
      match.result = match.result,
      adduct.table = adduct.table,
      ms1.ms2.match.mz.tol = 0,
      ms1.ms2.match.rt.tol = 0,
      ms1.match.ppm = ms1.match.ppm,
      ms2.match.ppm = ms2.match.ppm,
      ms2.match.tol = ms2.match.tol,
      rt.match.tol = rt.match.tol,
      polarity = polarity,
      ce = paste(ce, collapse = ";"),
      column = column,
      ms1.match.weight = ms1.match.weight,
      rt.match.weight = rt.match.weight,
      ms2.match.weight = ms2.match.weight,
      path = path,
      total.score.tol = total.score.tol,
      candidate.num = candidate.num,
      database = database.name,
      threads = threads,
      version = "0.4.0"
    )
    cat(crayon::bgRed("All done.\n"))
    return(return.result)
    
  }
)
