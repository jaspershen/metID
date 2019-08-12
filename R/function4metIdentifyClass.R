
###S4 class for function metIdentification
setClass(Class = "metIdentifyClass", 
         representation(ms1.data = "data.frame",
                        ms1.info = "data.frame",
                        ms2.info = "list",
                        identification.result = "list",
                        match.result = "data.frame",
                        adduct.table = "data.frame",
                        ms1.ms2.match.mz.tol = "numeric",
                        ms1.ms2.match.rt.tol = "numeric",
                        ms1.match.ppm = "numeric",
                        ms2.match.ppm = "numeric",
                        ms2.match.tol = "numeric",
                        rt.match.tol = "numeric",
                        polarity = "character",
                        ce = "character",
                        column = "character",
                        ms1.match.weight = "numeric",
                        rt.match.weight = "numeric",
                        ms2.match.weight = "numeric",
                        path = "character",
                        total.score.tol = "numeric",
                        candidate.num = "numeric",
                        database = "character",
                        threads = "numeric",
                        version = "character")
)


setMethod(f = "show",
          signature = "metIdentifyClass",
          definition = function(object){
            version <- try(object@version)
            if(class(version) != "try-error"){
            cat("--------------metID version-----------\n")
            cat(object@version, "\n")
            }
            cat("-----------Identifications------------\n")
            cat("(Use getIdentificationTable to get identification table)\n")
            cat("There are", nrow(object@ms1.data), "peaks\n")
            cat(nrow(object@match.result), "peaks have MS2 spectra\n")
            cat("There are",
                length(unique(unlist(lapply(object@identification.result, function(x){
                  x$Compound.name
                })))),
                "metabolites are identified\n"
            )
            if(!is.null(object@identification.result[[1]])){
              cat("There are", length(object@identification.result), "peaks with identification\n")
            }
            
            cat("-----------Parameters------------\n")
            cat("(Use getParam to get all the parameters of this processing)\n")
            cat("Polarity:", object@polarity, "\n")
            cat("Collision energy:", object@ce, "\n")
            cat("database:", object@database, "\n")
            cat("Total score cutoff:", object@total.score.tol, "\n")
            cat("Column:", object@column, "\n")
            cat("Adduct table:\n")
            cat(paste(object@adduct.table$adduct, collapse = ";"))
            # print(head(tibble::as_tibble(object@adduct.table, 5)))
          }
)

#------------------------------------------------------------------------------
#' @title getParams
#' @description Get parameters from a metIdentifyClass object.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object A metIdentifyClass object.
#' @return A data frame contains all the parameters of this metIdentifiyClass object.
#' @export
setGeneric(name = "getParams", 
           def = function(object){
             if(class(object) != "metIdentifyClass") stop("Only for metIdentifyClass\n")
             data.frame("Parameter" = c("ms1.ms2.match.mz.tol",
                                        "ms1.ms2.match.rt.tol",
                                        "ms1.match.ppm",
                                        "ms2.match.ppm",
                                        "ms2.match.tol",
                                        "rt.match.tol",
                                        "polarity",
                                        "ce",
                                        "column",
                                        "ms1.match.weight",
                                        "rt.match.weight",
                                        "ms2.match.weight",
                                        "path",
                                        "total.score.tol",
                                        "candidate.num",
                                        "database",
                                        "threads"
             ),
             "Meaning" = c("MS1 features & MS spectra matching mz tolerance (ppm)",
                           "MS1 features & MS spectra matching RT tolerance (s)",
                           "MS1 match tolerance (ppm)",
                           "MS2 fragment match tolerance (ppm)",
                           "MS2 match tolerance",
                           "RT match tolerance (s)", 
                           "Polarity", 
                           "Collision energy",
                           "Column",
                           "MS1 match weight",
                           "RT match weight",
                           "MS2 match weight",
                           "Work directory",
                           "Total score tolerance",
                           "Candidate number",
                           "MS2 database", 
                           "Thread number"),
             "Value" = c(object@ms1.ms2.match.mz.tol,
                         object@ms1.ms2.match.rt.tol,
                         object@ms1.match.ppm,
                         object@ms2.match.ppm,
                         object@ms2.match.tol,
                         object@rt.match.tol,
                         object@polarity,
                         object@ce,
                         object@column,
                         object@ms1.match.weight,
                         object@rt.match.weight,
                         object@ms2.match.weight,
                         object@path,
                         object@total.score.tol,
                         object@candidate.num,
                         object@database,
                         object@threads), stringsAsFactors = FALSE
             )
           })



##------------------------------------------------------------------------------
#' @title getIdenInfo
#' @description Get identification information from a metIdentifyClass object.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object A metIdentifyClass object.
#' @param which.peak A peak name or "all". "all" means all peaks with identifications will be output.
#' @param database Database used.
#' @return A identification table (data.frame).
#' @export
#' @seealso The example and demo data of this function can be found 
#' https://jaspershen.github.io/metID/articles/metID.html

setGeneric(name = "getIdenInfo",
           def = function(object,
                          which.peak,
                          database){
             if(missing(object) | missing(which.peak) | missing(database)){
               stop("Please provide the object, which.peak and database.\n")
             }
             if(class(object) != "metIdentifyClass") stop("Only for metIdentifyClass\n")
             if(class(database) != "databaseClass") stop("Only for databaseClass\n")
             
             identification.result <- object@identification.result
             
             which.peak <- as.character(which.peak)
             
             if(!which.peak %in% object@ms1.data$name){
               stop(which.peak, " is not in peak table, please check it.\n")
             }
             
             if(is.na(match(which.peak, object@match.result$MS1.peak.name))){
               cat("The peak has no MS2 spectrum.\n")
               return()
             }
             
             if(is.na(match(object@match.result$MS2.spectra.name[match(which.peak, object@match.result$MS1.peak.name)],
                            names(identification.result)))){
               cat("The peak hsa no identification result.\n")
               return()
             }
             
             temp <- match(object@match.result$MS2.spectra.name[match(which.peak, object@match.result$MS1.peak.name)],
                           names(identification.result))
             temp <- identification.result[[temp]]
             temp <- data.frame(temp, database@spectra.info[match(temp$Lab.ID, database@spectra.info$Lab.ID),
                                                            setdiff(colnames(database@spectra.info), colnames(temp))
                                                            ], stringsAsFactors = FALSE)
             temp <- tibble::as_tibble(temp)
             temp
           })

##------------------------------------------------------------------------------
#' @title ms2plot
#' @description Get MS2 match plots from a metIdentifyClass object.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object A metIdentifyClass object.
#' @param database Used database (databaseClass).
#' @param which.peak Peak name(s) or "all". You can use whichHasIden 
#' @param ppm.tol MS2 fragment match ppm.
#' @param mz.ppm.thr The threshold for m/z error calculation.
#' @param path Work directory.
#' @param width The width of MS2 spectra match figure (inch).
#' @param height The height of MS2 spectra match figure (inch).
#' @param interaction.plot Output interactive plot or not.
#' @param range.mz m/z range for MS2 spectra match plot.
#' @param range.int Relative intensity range.
#' @param xlab Title of x axis.
#' @param ylab Title of y axis.
#' @param col.lib Colour of database MS2 spectrum.
#' @param col.exp Colour of experimental MS2 spectrum.
#' @param title.size Font size of title.
#' @param lab.size Font size of title of axis.
#' @param axis.text.size Font size of axis text.
#' @param legend.title.size Legend title size.
#' @param legend.text.size Legend text size.
#' @param figure.type "pdf" or "png".
#' @param threads The number of threads
#' @param one.folder Output all figure in one folder or not.
#' @return A or all ms2 match plot(s).
#' @export 
#' @seealso The example and demo data of this function can be found 
#' https://jaspershen.github.io/metID/articles/metID.html

setGeneric(name = "ms2plot", 
           def = function(object,
                          database,
                          which.peak = "all",
                          ppm.tol = 30,
                          mz.ppm.thr = 400,
                          path = ".",
                          width = 20, 
                          height = 8,
                          interaction.plot = FALSE,
                          range.mz,
                          range.int = c(-1, 1),
                          xlab = "Mass to charge ratio (m/z)",
                          ylab = "Relative intensity",
                          col.lib = "red",
                          col.exp = "black",
                          title.size = 15,
                          lab.size = 12,
                          axis.text.size =12,
                          legend.title.size = 12,
                          legend.text.size = 10,
                          figure.type = c("png", "pdf"),
                          threads = 3,
                          one.folder = TRUE
           ){
             # 
             if(class(object) != "metIdentifyClass") stop("Only for metIdentifyClass\n")
             if(which.peak == "all"){
               which.peak <- object@ms1.data$name
             }
             identification.result <- object@identification.result
             polarity <- object@polarity
             figure.type <- match.arg(figure.type)
             ##-------------------------------------------------------------------
             ##only for one peak
             if(all(which.peak != "all") & length(which.peak) == 1){
               which.peak <- as.character(which.peak)
               if(!which.peak %in% object@ms1.data$name)
                 stop(which.peak, " is not in peak table, please check it.\n")
               ms2.spectra.name <- object@match.result$MS2.spectra.name[match(which.peak, 
                                                                              object@match.result$MS1.peak.name)]
               if(is.na(ms2.spectra.name)){
                 cat(which.peak, "hsa no MS2 spectrum.\n")
                 return()
               }
               temp.idx <- which(names(identification.result) == ms2.spectra.name)
               if(length(temp.idx) == 0){
                 cat(which.peak, "hsa no identification.\n")
                 return()
               }
               matched.info <- identification.result[[temp.idx]]
               
               if(nrow(matched.info) > 1){
                 cat("There are", nrow(matched.info), "identifications.\n")
                 cat(paste(paste(c(1:nrow(matched.info)), as.character(matched.info[,1]), sep = ":"), 
                           collapse = "\n"))
                 cat("\n")
                 which.identification <- "test"
                 while(is.na(which.identification) | !which.identification %in% c(1:length(matched.info))){
                   which.identification <- readline(prompt = "Which identification (index: number)?")
                   which.identification <- as.numeric(which.identification)
                 }
                 matched.info <- unlist(matched.info[which.identification, , drop = TRUE])
               }else{
                 matched.info <- unlist(matched.info[1, , drop = TRUE])
               }
               
               lib.spectrum <- getMS2spectrum(lab.id = matched.info["Lab.ID"], 
                                              database = database, 
                                              polarity = polarity, 
                                              ce = matched.info["CE"])
               exp.spectrum <- object@ms2.info[[match(ms2.spectra.name, names(object@ms2.info))]]
               if(missing(range.mz)){
                 range.mz <- range(c(lib.spectrum[, "mz"], exp.spectrum[, "mz"]))  
               }
               
               plot <- plotMS2match(matched.info = matched.info,
                                    ppm.tol = ppm.tol,
                                    mz.ppm.thr = mz.ppm.thr,
                                    exp.spectrum = exp.spectrum,
                                    lib.spectrum = lib.spectrum,
                                    polarity = polarity,
                                    xlab = xlab,
                                    ylab = ylab,
                                    col.lib = col.lib,
                                    col.exp = col.exp,
                                    ce = matched.info["CE"],
                                    title.size = title.size,
                                    lab.size = lab.size,
                                    axis.text.size = axis.text.size,
                                    legend.title.size = legend.title.size,
                                    legend.text.size = legend.text.size,
                                    database = database
               )
               if(interaction.plot){
                 plot <- plotly::ggplotly(p = plot)
               }
               plot
             }else{
               ##output all MS2 match
               dir.create(path)
               path <- file.path(path, "ms2_match_plot")
               dir.create(path)
               
               if(all(which.peak != "all")){
                 if(!all(which.peak %in% object@ms1.data$name)){
                   stop("Some peaks are not in MMS1 peak table, please check them.\n")
                 }
                 ms2.spectra.name <- object@match.result$MS2.spectra.name[match(which.peak, object@match.result$MS1.peak.name)]
                 which.peak <- which.peak[!is.na(ms2.spectra.name)]
                 ms2.spectra.name <- ms2.spectra.name[!is.na(ms2.spectra.name)]
                 if(length(ms2.spectra.name) == 0){
                   stop("All peaks have no MS2 spectra.\n")
                 }
                 anno.idx <- match(ms2.spectra.name, names(object@identification.result))  
                 if(all(is.na(anno.idx))){
                   stop("All peaks have no identifications.\n")
                 }
                 
                 which.peak <- which.peak[!is.na(anno.idx)]
                 ms2.spectra.name <- ms2.spectra.name[!is.na(anno.idx)]    
                 anno.idx <- anno.idx[!is.na(anno.idx)]
               }
               # cat("There are", length(anno.idx), "peaks with identifications.\n")
               
               if(length(anno.idx) == 0) {
                 return(NULL)
               }
               temp.fun <- function(anno.idx, 
                                    identification.result,
                                    ms2.info,
                                    match.result,
                                    database,
                                    ppm.tol = 30,
                                    mz.ppm.thr = 400,
                                    col.lib = "red",
                                    col.exp = "black",
                                    polarity = c("positive", "nagative"),
                                    range.int = c(-1, 1), 
                                    xlab = "Mass to charge ratio (m/z)",
                                    ylab = "Relative intensity", 
                                    title.size = 15, 
                                    lab.size = 12, 
                                    axis.text.size = 12, 
                                    legend.title.size = 12,  
                                    legend.text.size = 10,
                                    plotMS2match,
                                    getMS2spectrum
               ){
                 matched.info <- identification.result[[anno.idx]]  
                 temp.ms2.spectrum.name <- names(identification.result)[anno.idx]
                 temp.peak.name <- match.result$MS1.peak.name[match(temp.ms2.spectrum.name,
                                                                    match.result$MS2.spectra.name)]
                 
                 if(!one.folder){
                   temp.path <- file.path(path, 
                                          stringr::str_replace_all(string = temp.peak.name, 
                                                                   pattern = "/", replacement = "_"))
                   dir.create(temp.path)
                 }
                 
                 matched.info <- apply(matched.info, 1, list)
                 matched.info <- lapply(matched.info, unlist)
                 
                 non.meaning <- lapply(matched.info, function(temp.matched.info){
                   if(one.folder) {
                     temp.file.name <- 
                       file.path(path,
                                 stringr::str_c(
                                   paste(stringr::str_replace_all(string = temp.peak.name, pattern = "/", replacement = "_"),
                                         paste(as.character(temp.matched.info[c("Total.score", "Lab.ID", "Adduct")]), collapse = ";"),
                                         sep = ";"), ".", figure.type ,sep = "")
                       )
                   }else{
                     temp.file.name <- file.path(temp.path,
                                                 stringr::str_c(paste(as.character(temp.matched.info[c("Total.score", "Lab.ID", "Adduct")]), collapse = ";"), 
                                                                ".", figure.type ,sep = "")
                     )
                   }
                   
                   
                   lib.spectrum <- getMS2spectrum(lab.id = temp.matched.info["Lab.ID"], 
                                                  database = database, 
                                                  polarity = polarity, 
                                                  ce = temp.matched.info["CE"])
                   exp.spectrum <- ms2.info[[match(match.result$MS2.spectra.name[match(temp.peak.name,
                                                                                       match.result$MS1.peak.name)], 
                                                   names(ms2.info))]]
                   range.mz <- range(c(lib.spectrum[, "mz"], exp.spectrum[, "mz"])) 
                   
                   temp.plot <- plotMS2match(matched.info = temp.matched.info,
                                             ppm.tol = ppm.tol,
                                             mz.ppm.thr = mz.ppm.thr,
                                             exp.spectrum = exp.spectrum,
                                             lib.spectrum = lib.spectrum,
                                             polarity = polarity,
                                             xlab = xlab,
                                             ylab = ylab,
                                             col.lib = col.lib,
                                             col.exp = col.exp,
                                             ce = temp.matched.info["CE"],
                                             title.size = title.size,
                                             lab.size = lab.size,
                                             axis.text.size = axis.text.size,
                                             legend.title.size = legend.title.size,
                                             legend.text.size = legend.text.size,
                                             database = database
                   )
                   ggplot2::ggsave(filename = temp.file.name,
                                   plot = temp.plot, 
                                   width = width, height = height)
                   
                 })
               }
               
               BiocParallel::bplapply(X = anno.idx, 
                                      FUN = temp.fun, 
                                      BPPARAM = BiocParallel::SnowParam(workers = threads,
                                                                        progressbar = TRUE),
                                      identification.result = identification.result,
                                      ms2.info = object@ms2.info,
                                      match.result = object@match.result,
                                      database = database,
                                      ppm.tol = ppm.tol,
                                      mz.ppm.thr = mz.ppm.thr,
                                      col.lib = col.lib,
                                      col.exp = col.exp,
                                      polarity = polarity,
                                      xlab = xlab,
                                      ylab = ylab, 
                                      title.size = title.size, 
                                      lab.size = lab.size, 
                                      axis.text.size = axis.text.size, 
                                      legend.title.size = legend.title.size,  
                                      legend.text.size = legend.text.size,
                                      plotMS2match = plotMS2match,
                                      getMS2spectrum = getMS2spectrum
               )
               cat("All is done.\n")
             }
           })

#------------------------------------------------------------------------------
#' @title whichHasIden
#' @description Get the peak names which have identifications.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object A metIdentifyClass object.
#' @return Peak names with identifications.
#' @export
#' @seealso The example and demo data of this function can be found 
#' https://jaspershen.github.io/metID/articles/metID.html

setGeneric(name = "whichHasIden", 
           def = function(object){
             if(class(object) != "metIdentifyClass") stop("Only for metIdentifyClass\n")
             temp <- object@match.result[match(names(object@identification.result), object@match.result$MS2.spectra.name),c(3,4)]
             rownames(temp) <- NULL
             temp
           })

#------------------------------------------------------------------------------
#' @title filterIden
#' @description Filter identifications according to m/z error, RT error, MS similarity and total score.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object A metIdentifyClass object.
#' @param ms1.match.ppm MS1 match ppm.
#' @param rt.match.tol RT match tolerance.
#' @param ms2.match.tol MS2 match (MS2 similarity) tolerance.
#' @param total.score.tol Total score tolerance.
#' @return A new metIdentifyClass.
#' @export
#' @seealso The example and demo data of this function can be found 
#' https://jaspershen.github.io/metID/articles/metID.html
#' 
#' 
setGeneric(name = "filterIden",
           def = function(object,
                          ms1.match.ppm = 25,
                          rt.match.tol = 30,
                          ms2.match.tol = 0.5,
                          total.score.tol = 0.5
           ){
             if(class(object) != "metIdentifyClass") {
               stop("Only for metIdentifyClass\n")
             }
             
             object@ms1.match.ppm <- ms1.match.ppm
             object@rt.match.tol <- rt.match.tol
             object@ms2.match.tol <- ms2.match.tol
             object@total.score.tol <- total.score.tol
             
             identification.result <- object@identification.result
             
             
             identification.result <- lapply(identification.result, function(x){
               RT.error <- x$RT.error
               RT.error[is.na(RT.error)] <- rt.match.tol - 1
               x <- x[which(x$mz.error < ms1.match.ppm & RT.error < rt.match.tol & 
                              x$SS > ms2.match.tol & x$Total.score > total.score.tol),,drop = FALSE]
             })
             
             temp.idx <- which(unlist(lapply(identification.result, function(x){
               nrow(x) != 0
             })))
             
             identification.result <- identification.result[temp.idx]
             object@identification.result <- identification.result
             object
           })

#------------------------------------------------------------------------------
#' @title getMS2spectrum2Object
#' @description Get spectra of peaks from metIdentifyClass object.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param peak.name Peak name.
#' @param object metIdentifyClass.
#' @return A MS2 spectrum.
#' @export
#' @seealso The example and demo data of this function can be found 
#' https://jaspershen.github.io/metID/articles/metID.html

setGeneric(name = "getMS2spectrum2Object", 
           def = function(peak.name,
                          object){
             if(class(object) != "metIdentifyClass") stop("Only for metIdentifyClass\n")
             if(missing(peak.name)) stop('Please provide peak name.\n')
             object@ms2.info[[which(object@match.result$MS2.spectra.name[match(peak.name, object@match.result$MS1.peak.name)] == names(object@ms2.info))]]
           })










