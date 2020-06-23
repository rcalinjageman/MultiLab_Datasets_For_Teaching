## MANYLABS 2 ANALYSES (https://osf.io/8cd4r/)
##
## Incidental Disfluency (Alter et al., 2007): Alter_4_study_global_include_all
##
## This is an auto-generated R script (corresponding coder: https://osf.io/ujgs6/)
##
## It assumes:
##
## 1. You have the following libraries installed: devtools, plyr, tidyverse, rio, osfr
## 2. You have an internet connection so you can download the data from the OSF repository
## 3. You have an internet connection to allow sourcing of the manylabRs function library from Github
## 4. You do not mind that additional packages could be installed, because our functions depend on them (see comment on `init()` below)
##
## You can also use the script by downloading the files mentioned under 2 and 3 and changing the code so they import locally, but this is not recommended. One imprtant reason: We are humans, we might have made an error, by downloading from OSF you can be sure to have the most up-to-date data files and scripts!

# INSTRUCTIONS:
#
# Make sure the data and files can be read
# - Set the variables under ANALYSIS INFO (see TIP below)
#
# Run this script:
# - select all >> Run Selected Line(s)
# - Run line by line
# - source(filename)
#
# Output:
# - Raw dataset in a data.frame
# - List object with filtered, possibly also computed data vectors for analysis, based on the Raw dataset.
# - Clean datset in a data.frame (NOTE: The clean dataset is a long format data.frame representation of the list object)
# - Results in a data frame
# - If `saveAll = TRUE` under ANALYSIS INFO, you should see all the files that are in the 'Results' and 'Data' section in the OSF repository for this analysis.
#   The location of the files are set by the fields of `outdir`
#
# TIP:
#
# If you downloaded the entire data structure from OSF, set the variable `OSFdata.root` to the location of the folder where the data structure can be found, e.g.
# Windows: OSFdata.root <- "C:\My Documents\OSFdata"
# MacOS:   OSFdata.root <- "~/Documents/OSFdata"
#
# The data and results will then be written to the 'Data' and 'Results' folders. If you set `overWrite <- FALSE` a date-time ID will be appended to the filename.


# SETUP ENVIRONMENT ----

library(devtools)
library(plyr)
library(rio)
library(tidyverse)

# Package to get files from OSF
if(!"osfr"%in%installed.packages(fields="Package")){devtools::install_github('CenterForOpenScience/osfr')}
library(osfr)
osfr::login()

# manylabRs sourceable function library
devtools::source_url("https://raw.githubusercontent.com/ManyLabsOpenScience/manylabRs/master/R/manylabRs_SOURCE.R")
# Running init() will install packages that are needed by the functions in `manylabRs_SOURCE.R`.
# These packages might not be needed for the present analysis.
init()

# ANALYSIS INFO ----


study.description      <- 'Incidental Disfluency (Alter et al., 2007)'
analysis.unique.id     <- 11
analysis.name          <- 'Alter.4'
analysis.type          <- 1
analysis.type.name     <- 'study_global_include'
analysis.type.groups   <- 'Source.Global'
Nmin.raw               <- 30
Nmin.cond              <- 15
subset                 <- 'all'
onlineTables           <- TRUE
staticData             <- TRUE
saveAll                <- TRUE
overWrite              <- TRUE
OSFdata.root           <- file.path('~','OSFdata')
analysis.root          <- file.path(OSFdata.root,study.description,analysis.name,'Global')
outdir                 <- list(Data = file.path(analysis.root,'Data'), Results = file.path(analysis.root,'Results'))


# This function will be used to change the raw dataset to a dataset ready for analysis

varfun.Alter.2


if(dplyr::between(analysis.type,2,3)){subset <- "all"}

# GET LOOKUP TABLES ----

if(onlineTables){
  # Get the Keytable with analysis information
  ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
  ML2.key <- ML2.key[!is.na(ML2.key$unique.id)&ML2.key$unique.id==analysis.unique.id,]

  # Get info about the sites
  SourceInfoTable    <- get.GoogleSheet(url = "https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/pub?gid=1435507167&single=true&output=csv")$df
} else {
  # Get the Keytable with analysis information
  ML2.key <- rio::import(file.path(OSFdata.root,"!!KeyTables","ML2_KeyTable.csv"))
  ML2.key <- ML2.key[!is.na(ML2.key$unique.id)&ML2.key$unique.id==analysis.unique.id,]

  # Get info about the sites
  SourceInfoTable    <- rio::import(file.path(OSFdata.root,"!!KeyTables","ML2_SourceInfoTable.csv"))
}

# GET DATA ----

if(!staticData){
  # CANNOT TEST UNTIL OSF DATA ARE PUBLIC
  # Get the correct slate according to info in ML2.key['study.slate']
  if(ML2.key[study,'study.slate'] == 1){
    data <- osfr::download_files(id = 'cwjp3', path =  getwd())
  } else {
    data <- osfr::download_files(id = 'jg9hc', path =  getwd())
  }
  ML2.df <- rio::import(data)
  disp(paste("Downloaded data from OSF"), header = FALSE, footer = FALSE)
} else {
  # Get the correct slate according to info in ML2.key['study.slate']
  if(ML2.key$study.slate == 1){
    ML2.df <- rio::import(file.path(OSFdata.root,"!!RawData","ML2_Slate1.csv"))
  } else {
    ML2.df <- rio::import(file.path(OSFdata.root,"!!RawData","ML2_Slate2.csv"))
  }
}

# PREPARE DATA & OUTPUT ----

# Add a unique ID
ML2.df$uID = seq(1, nrow(ML2.df))

# Get info to create a dataset for the current study
# keytable <- ML2.key
ML2.in <- get.info(ML2.key, colnames(ML2.df), subset)

# Generate chain to select variables for the data frame and create a filter chain for the variables to use for analysis
# Info based on KeyTable information in study.vars, cases.include, site.include, params.NA
ML2.id <- get.chain(ML2.in)

# Apply the df chain to select relevant subset of variables

ML2.df <- ML2.df  %>% dplyr::select(1,6,313,314,315,316,317,318,326,327,328,329,330,331,520,521,522,523,524,525,526,527,528,529,530,531,534,535,536) %>% dplyr::filter(Language=="English"&Setting=="In a lab" )



# Decide which analyses to run on which groups
toRun  <- decide.analysis(ML2.key, analysis.unique.id, analysis.type, doAll = TRUE)

if(length(toRun$studiess)>0){

  if(NROW(ML2.df)>0){

    # Create a variable indicating the study order for each case
    ML2.df$study.order <- NA
    stmp <- strsplit(ML2.df$StudyOrderN,"[|]")

    # Correct differences in study names
    Stud <- ML2.key$study.name
    if(Stud%in%"Tversky"){Stud <- "Tversky.Gati"}
    if(Stud%in%"Rottenstreich"){Stud <- "Rottenstrich"}
    if(Stud%in%"Ross"&(ML2.key['study.slate'] == 1)){Stud <- "Ross.Slate1"}
    if(Stud%in%"Ross"&(ML2.key['study.slate'] == 2)){Stud <- "Ross.Slate2"}
    if(Stud%in%"vanLange"){Stud <- "VanLange"}
    if(Stud%in%"Giessner"){Stud <- "Geissner"}

    ML2.df$study.order <- plyr::laply(seq_along(stmp), function(o){which(grepl(Stud,stmp[[o]]))%00%NA})

    ML2.sr       <- list()
    ML2.var      <- list()
    outputSource <- list()
    dataSource   <- list()
    raw.df       <- list()
    clean.df     <- list()
    cleanData    <- list()
    testVarEqual <- ML2.in$stat.params$var.equal

    # Loop over sites in runGroups within a study
    if(analysis.type==1){
      runGroups <- "all"
    } else {
      runGroups <- sort(na.exclude(unique(ML2.df[[toRun$ugroup]])))
    }

    disp(paste(analysis.unique.id, ML2.key$study.analysis,"- START"), header = toupper(ML2.key$study.analysis), footer = FALSE)
    cat("\n")


    # START GROUPS ----

    for(g in seq_along(runGroups)){

      # Include only datasets that have N >= Nmin.raw & n.group >= Nmin.cond
      listIT     <- FALSE
      nMin1      <- FALSE
      nMin2      <- FALSE
      compN <- compN1 <- compN2 <- 0

      if(analysis.type<4){
        if(runGroups[g]=="all"){
          gID <- rep(TRUE, nrow(ML2.df))
        } else {
          gID <- ML2.df$source%in%runGroups[g]
        }
      } else {
        gID <-  ML2.df$study.order%in%runGroups[g]
      }

      # Check nMin
      if(sum(gID, na.rm=TRUE) >= Nmin.raw){
        nMin1 <- TRUE
        # Get a list containing the data frames to be used in the analysis
        ML2.sr[[g]] <- get.sourceData(ML2.id, ML2.df[gID, ], ML2.in)
      }

      # Double-check nMin
      if(nMin1){
        compN  <- ML2.sr[[g]]$N
        compN1 <- sum(ML2.sr[[g]]$RawDataFilter[[1]]$Included, na.rm = TRUE)
        compN2 <- sum(ML2.sr[[g]]$RawDataFilter[[2]]$Included, na.rm = TRUE)
        if(any(compN >= Nmin.raw)&(all(compN1>=Nmin.cond, compN2>=Nmin.cond))){nMin2 <- TRUE}
      }

      # START ANALYSIS ----------------------------------------

      if(all(nMin1,nMin2)){

# To see the function code type:varfun.Alter.2, or lookup in manylabRs_SOURCE.R
ML2.var[[g]] <- varfun.Alter.2(ML2.sr[[g]])


        # Check equal variance assumption
        if(!is.na(testVarEqual)){
          if(testVarEqual){
            logtxt <- paste(analysis.unique.id,ML2.key$study.analysis,'-', runGroups[g])
            ML2.in$stat.params$var.equal <- decide.EqualVar(ML2.var[[g]],ML2.in$study.vars.labels, ML2.key, group = logtxt) # don't pass the cleanData frame
          }}

        # Run the analysis according to ML2.key: 'stat.test'
        stat.params <<- ML2.in$stat.params


stat.test   <- try.CATCH(with(ML2.var[[g]],t.test(x = DisFluent, y = Fluent, conf.level=stat.params$conf.level, var.equal = stat.params$var.equal, alternative = stat.params$alternative)))


        # Check for errors and warnings
        if(all(is.null(stat.test$warning), grepl("simpleWarning",stat.test$warning),
               !grepl("Error", stat.test$value[[1]]),
               !grepl("message", names(unlist(stat.test))[1]))){
          stat.test  <- stat.test$value
          ConsoleOut <- paste(capture.output(print(stat.test)),collapse="\n")
          listIT     <- TRUE
        }

        if(listIT){
          describe <- get.descriptives(stat.test = stat.test,
                                       vars      = ML2.var[[g]],
                                       keytable  = ML2.key)

          if(any(describe$descr.raw$n<Nmin.cond)){
            listIT<- FALSE
            nMin2 <- FALSE
          }
          rm(describe)
        }

        # START RECORD DATA ----

        if(listIT){

          describe <- get.descriptives(stat.test = stat.test,
                                       vars      = ML2.var[[g]],
                                       keytable  = ML2.key)

          var.lor <- ifelse(grepl("OR",describe$test$estype),
                            sum(1/(table(ML2.var[[g]]$Condition,ML2.var[[g]]$Response)), na.rm = TRUE),
                            NA)

          ESCI  <-   generateOutput(describe        = describe,
                                    var.lor         = var.lor,
                                    runningGroup    = runGroups[g],
                                    runningAnalysis = paste(analysis.unique.id,ML2.key$study.analysis),
                                    stat.params = stat.params)

          # Raw and clean datasets
          if(length(ML2.sr[[g]]$RawDataFilter)>1){
            case.include <- ML2.sr[[g]]$RawDataFilter[[1]]$Included|ML2.sr[[g]]$RawDataFilter[[2]]$Included
            df1 <- ML2.sr[[g]]$RawDataFilter[[1]][ ,-which(colnames( ML2.sr[[g]]$RawDataFilter[[1]])=="Included")]
            raw.df[[g]] <-  cbind.data.frame(df1, analysis.type = c("Global","Primary","Secondary","Order")[analysis.type],subset=subset,case.include = case.include)
          } else {
            case.include <- ML2.sr[[g]]$RawDataFilter[[1]]$Included
            df1 <- ML2.sr[[g]]$RawDataFilter[[1]][ ,-which(colnames( ML2.sr[[g]]$RawDataFilter[[1]])=="Included")]
            raw.df[[g]] <-  cbind.data.frame(df1, analysis.type = c("Global","Primary","Secondary","Order")[analysis.type],subset=subset,cases.include = case.include)
          }





SourceInfo <- raw.df[[g]] %>% dplyr::filter(case.include) %>%
dplyr::summarise(
  N.sources.global    = length(unique(Source.Global)),
  N.sources.primary   = length(unique(Source.Primary)),
  N.sources.secondary = length(unique(Source.Secondary)),
  N.countries         = length(unique(Country)),
  N.locations         = length(unique(Location)),
  N.languages         = length(unique(Language)),
  Pct.WEIRD           = mean(Weird, na.rm=TRUE)*100,
  Tbl.Execution       = paste0(capture.output(table(Execution)),collapse='\n'),
  Tbl.subjectpool     = paste0(capture.output(table(SubjectPool)),collapse='\n'),
  Tbl.setting       = paste0(capture.output(table(Setting)),collapse='\n'),
  Tbl.Tablet        = paste0(capture.output(table(Tablet)),collapse='\n'),
  Tbl.Pencil        = paste0(capture.output(table(Pencil)),collapse='\n'),
  N.studyorders1    = length(unique(StudyOrderN)),
  N.IDiffOrderN     = length(unique(IDiffOrderN)),
  N.uIDs            = length(unique(uID)),
  N.studyorders2    = length(unique(study.order)),
  Tbl.analysistype  = paste0(capture.output(table(analysis.type)),collapse='\n'),
  Tbl.subset        = paste0(capture.output(table(subset)),collapse='\n'),
  N.cases.included  = sum(case.include, na.rm=TRUE),
  N.cases.excluded  = sum(case.include==FALSE,na.rm=TRUE))





          rownames(SourceInfo) <- NULL

          test  <- describe$test
          descr <- describe$descr.raw
          outputSource[[g]] <- get.output(key      = ML2.key,
                                          vars     = ML2.var[[g]],
                                          descr    = descr,
                                          group    = runGroups[g],
                                          analysis = c("Global","Primary","Secondary","Order")[analysis.type],
                                          varEqual = stat.params$var.equal,
                                          test     = test,
                                          ESCI     = ESCI,
                                          test.ConsoleOutput = ConsoleOut,
                                          SourceInfo = SourceInfo,
                                          stat.test = stat.test)

          # Data list for output to spreadsheet
          dataSource[[g]] <- list(
            study.id      = ML2.key$study.id,
            study.slate   = ML2.key$study.slate,
            study.name    = ML2.key$study.name,
            study.source  = runGroups[g],
            analysis.type = c("Global","Primary","Secondary","Order")[analysis.type],
            analysis.name = ML2.key$study.analysis,
            subset        = subset,
            stat.info     = ML2.in,
            stat.data.cleanchain = ML2.id,
            stat.data.raw       = raw.df[[g]],
            stat.data.cleaned   = ML2.sr[[g]][1:length(ML2.sr[[g]])-1],
            stat.data.analysed  = ML2.var[[g]][1:length(ML2.var[[g]])-1],
            stat.test = stat.test)

          suppressMessages(clean.df[[g]] <- plyr::ldply(dataSource[[g]]$stat.data.analysed,reshape2::melt))
          colnames(clean.df[[g]])[colnames(clean.df[[g]])==".id"] <- "Condition"


          cleanData[[g]] <- ML2.var[[g]]$cleanDataFilter


          rm(stat.params)

        } else { # LISTIT
          cat("\nListIT = FALSE\n")
          if(!is.null(stat.test$value)){

            if(grepl("observations",as.character(stat.test$value))){
              disp(paste(analysis.unique.id, ML2.key$study.analysis,'-',
                         runGroups[g],'>> Not enough observations'),
                   header = FALSE, footer = FALSE)}
          } else {
            disp(paste(analysis.unique.id,ML2.key$study.analysis,'-', runGroups[g],'>> stat.test failed:'),
                 header = FALSE, footer = FALSE)
            # disp(paste('value: ',stat.test$value),
            #      header = FALSE, footer = FALSE)
            disp(paste('warning:',stat.test$warning),
                 header = FALSE, footer = FALSE)
          }
          ConsoleOut <- paste(gsub("[[:punct:]]", "", stat.test$warning, perl = TRUE), collapse="\n")
          NN <- lengths(ML2.var[[g]])
          NN <- NN[!names(NN)=="N"]
          N  <- rep(ML2.var[[g]]$N,length.out = length(NN))

          opt <- OutputTemplate()

          outputSource[[g]] <- get.output(key      = ML2.key,
                                          vars     = ML2.var[[g]],
                                          descr    = opt,
                                          group    = runGroups[g],
                                          analysis = c("Global","Primary","Secondary","Order")[analysis.type],
                                          varEqual = stat.params$var.equal,
                                          test     = test,
                                          ESCI     = ESCI,
                                          test.ConsoleOutput = ConsoleOut,
                                          SourceInfo = NA,
                                          stat.test = stat.test)

          cleanData[[g]] <- ML2.var[[g]]$cleanDataFilter

          # Data list for output to spreadsheet
          dataSource[[g]] <- NA


          rm(stat.params)

        } #Listit = FALSE
      } # all nMin 1,2


      # HANDLE ERRORS ----
      if(!nMin1){
        disp(paste0(analysis.unique.id,' ',ML2.key$study.analysis,' - ',
                    runGroups[g],' not included in results >> Cases in source file (',
                    sum(gID, na.rm = TRUE),') < Nmin.raw (',Nmin.raw,')'),
             header = FALSE, footer = FALSE)
      } # Check nMin 1}
      if(!nMin2){
        disp(paste0(analysis.unique.id,' ',ML2.key$study.analysis,' - ',
                    runGroups[g],' not included in results >> Valid cases after varfun (n',
                    c(1,2)[compN < Nmin.cond],"=", compN[compN < Nmin.cond],') < Nmin.cond (',Nmin.cond,')'),
             header = FALSE, footer = FALSE)
      } # Double-check nMin

    } # iterate groups

    disp(paste(analysis.unique.id, ML2.key$study.analysis,"- COMPLETED"), header = FALSE)

    ML2.output  <- plyr::ldply(outputSource)
    ML2.rawdata <- plyr::ldply(raw.df)
    ML2.cleandata <- plyr::ldply(cleanData)

    if(saveAll){
      if(!overWrite){
        basename <- paste0(gsub("[.]","_",analysis.name),"_",analysis.type.name,"_",subset, as.Date(now()))
      } else {
        basename <- paste0(gsub("[.]","_",analysis.name),"_",analysis.type.name,"_",subset)
      }
      if(outdir$Results!=""){
        rio::export(ML2.output, file = file.path(normalizePath(outdir$Results),paste0(basename,"_RESULTS.csv")))
        rio::export(ML2.output, file = file.path(normalizePath(outdir$Results),paste0(basename,"_RESULTS.xlsx")))
      }
      if(outdir$Data!=""){
        if(NCOL(ML2.rawdata)>0){
          rio::export(ML2.rawdata, file = file.path(normalizePath(outdir$Data),paste0(basename,"_RAW_CASE.csv")))
          rio::export(ML2.rawdata, file = file.path(normalizePath(outdir$Data),paste0(basename,"_RAW_CASE.xlsx")))
        }
        if(NCOL(ML2.cleandata)>0){
          if(all(!is.null(ML2.cleandata$uID),!is.null(ML2.rawdata$uID))){
            ML2.cleandata <- dplyr::left_join(ML2.cleandata,ML2.rawdata,by="uID")
          }
          rio::export(ML2.cleandata, file = file.path(normalizePath(outdir$Data),paste0(basename,"_CLEAN_CASE.csv")))
          rio::export(ML2.cleandata, file = file.path(normalizePath(outdir$Data),paste0(basename,"_CLEAN_CASE.xlsx")))
        }
        save(dataSource, file = file.path(normalizePath(outdir$Results),paste0(basename,"_R_OBJECTS.RData")))
      }
    }

    #rm(ML2.in, ML2.var, ML2.id, ML2.df, ML2.sr, outputSource, dataSource, raw.df, clean.df, descr, SourceInfo, nMin1, nMin2, listIT)

  } else { # if nrow > 0

    disp(paste(analysis.unique.id, ML2.key$study.analysis,"- SKIPPED"), header = FALSE)

    ML2.output  <- NULL
    ML2.rawdata <- NULL

    #rm(ML2.in, ML2.var, ML2.id, ML2.df, ML2.sr)
  }
}
