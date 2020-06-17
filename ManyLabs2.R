# Many Labs 2

# Klein, R. A., Vianello, M., Hasselman, F., Adams, B. G., Adams, R. B., Alper, S., … Nosek, B. A. (2018). Many Labs 2: Investigating Variation in Replicability Across Samples and Settings. Advances in Methods and Practices in Psychological Science, 1(4), 443–490. https://doi.org/10.1177/2515245918810225


# Packages ---------------------------------------
  if (!is.element("devtools", installed.packages()[,1]))  install.packages("devtools", dep = TRUE)
  if (!is.element("esci", installed.packages()[,1])) devtools::install_github(repo = "rcalinjageman/esci", dependencies = TRUE)
  if (!is.element("ggplot2", installed.packages()[,1])) install.packages("ggplot2", dep = TRUE)
  if (!is.element("metafor", installed.packages()[,1])) install.packages("metafor", dep = TRUE)

  library(devtools)
  library(esci)
  library(ggplot2)
  library(metafor)
  library(foreign)


library(devtools)
devtools::source_url("https://raw.githubusercontent.com/ManyLabsOpenScience/manylabRs/master/R/manylabRs_SOURCE.R")
1devtools::source_url("https://raw.githubusercontent.com/FredHasselman/invctr/master/R/invictor.R")


# Parameters --------------------------
  # Paths info
  path_original <- "./original_data/ManyLabs1"
  path_extract <- "Data/CleanedDataset.sav"
  path_datafile <- "/CleanedDataset.sav"
  path_prepped <- "./prepped_data"
  path_sites <- "/ManyLabs1_sites.csv"

  # Number of resamples for resampling analysis
  resamples <- 500
  
# Study definitions ---------------------
  studies <- list()
  
  # studies$sunk <- list(iv = "sunkgroup",
  #                      dv = "sunkDV",
  #                      iv_levels = c("paid", "free"),
  #                      ylab = "Rated likelihood of attending game (1-9)",
  #                      reference_group = 2,
  #                      exclude = NULL,
  #                      keeps = NULL,
  #                      name = "Sunk costs",
  #                      type = "eimd",
  #                      type_path = "/Estimate Independent Mean Difference",
  #                      path = "/Sunk_Costs")
  # 
  # studies$anchor1 <- list(iv = "anch1group",
  #                         dv = "anchoring1",
  #                         iv_levels = c("lowanchor", "highanchor"),
  #                         reference_group = 1,
  #                         ylab = "Estimated distance from San Fran to NY (miles)",
  #                         exclude = NULL,
  #                         keeps = NULL,
  #                         name = "Anchoring1 - Distance from SF to NY",
  #                         type = "eimd",
  #                         type_path = "/Estimate Independent Mean Difference",
  #                         path = "/Anchoring1_City_Distance")
  # 
  # studies$anchor2 <- list(iv = "anch2group",
  #                         dv = "anchoring2",
  #                         iv_levels = c("lowanchor", "highanchor"),
  #                         reference_group = 1,
  #                         ylab = "Estimated population of Chicago",
  #                         exclude = NULL,
  #                         keeps = NULL,
  #                         name = "Anchoring2 - Chicago population",
  #                         type = "eimd",
  #                         type_path = "/Estimate Independent Mean Difference",
  #                         path = "/Anchoring2_Chicago_Population")
  # 
  # studies$anchor3 <- list(iv = "anch3group",
  #                         dv = "anchoring3",
  #                         iv_levels = c("lowanchor", "highanchor"),
  #                         reference_group = 1,
  #                         ylab = "Estimated height of Mt. Everest (feet)",
  #                         exclude = NULL,
  #                         keeps = NULL,
  #                         name = "Anchoring3 - Mt Everest height",
  #                         type = "eimd",
  #                         type_path = "/Estimate Independent Mean Difference",
  #                         path = "/Anchoring3_Mt_Everest_Height")
  # 
  # studies$anchor4 <- list(iv = "anch4group",
  #                         dv = "anchoring4",
  #                         iv_levels = c("lowanchor", "highanchor"),
  #                         reference_group = 1,
  #                         ylab = "Estimated babies born daily in USA",
  #                         exclude = NULL,
  #                         keeps = NULL,
  #                         name = "Anchoring4 - US Babies Daily",
  #                         type = "eimd",
  #                         type_path = "/Estimate Independent Mean Difference",
  #                         path = "/Anchoring4_US_Babies_Daily")
  # 
  # studies$gambler <- list(iv = "gambfalgroup",
  #                     dv = "gambfalDV",
  #                     iv_levels = c("two6", "three6"),
  #                     reference_group = 1,
  #                     ylab = "Sqrt of estimated previous rolls, +/-3SD outliers removed",
  #                     exclude = NULL,
  #                     keeps = NULL,
  #                     name = "Retrospective fambler's fallacy",
  #                     type = "eimd",
  #                     type_path = "/Estimate Independent Mean Difference",
  #                     path = "/Retrospective_Gamblers_Fallacy")
  # 
  # studies$quote <- list(iv = "quoteGroup",
  #                         dv = "quote",
  #                         iv_levels = c("disliked source", "liked source"),
  #                         reference_group = 1,
  #                         ylab = "Rated liking of quote (1-9)",
  #                         exclude = NULL,
  #                         keeps = NULL,
  #                         name = "Quote attribution",
  #                         type = "eimd",
  #                         type_path = "/Estimate Independent Mean Difference",
  #                         path = "/Quote_Attribution")
  # 
  studies$flag <- list(iv = "flagGroup",
                        dv = "flagdv",
                        iv_levels = c("no prime", "flag prime"),
                        reference_group = 1,
                        ylab = "Average of 8-item scale of conservative attitudes",
                        exclude = "flagfilter",
                        keeps = NULL,
                        name = "Flag Priming",
                        type = "eimd",
                        type_path = "/Estimate Independent Mean Difference",
                        path = "/Flag_Priming")
  
  studies$currency <- list(iv = "MoneyGroup",
                        dv = "Sysjust",
                        iv_levels = c("Control group", "Money priming group"),
                        reference_group = 1,
                        ylab = "Average of system-justification scale",
                        exclude = NULL,
                        keeps = NULL,
                        name = "Currency priming",
                        type = "eimd",
                        type_path = "/Estimate Independent Mean Difference",
                        path = "/Currency_Priming")
  
  studies$contact <- list(iv = "ContactGroup",
                           dv = "Imagineddv",
                           iv_levels = c("Control group", "Contact group"),
                           reference_group = 1,
                           ylab = "intentions to interact with muslims -dv  for imagined contact",
                           exclude = NULL,
                           keeps = NULL,
                           name = "Imagined contact",
                           type = "eimd",
                           type_path = "/Estimate Independent Mean Difference",
                           path = "/Imagined_Contact")
  
  studies$iat <- list(iv = "sex",
                      dv = "d_art",
                      iv_levels = c("m", "f"),
                      reference_group = 2,
                      ylab = "IAT-measured bias to math vs. art",
                      exclude = "iat_exclude",
                      keeps = NULL,
                      name = "Sex differences in implicit math attitudes",
                      type = "eimd",
                      type_path = "/Estimate Independent Mean Difference",
                      path = "/Sex_Differences_Math_IAT")
  
  

  
  
  # studies$framing <- list(iv = "gainlossgroup", 
  #                      dv = "gainlossDV", 
  #                      iv_levels = c("People will die", "People will be saved"),
  #                      exclude = NULL,
  #                      keeps = NULL,
  #                      name = "Gain versus loss framing",
  #                      type = "epd",
  #                      type_path = "/Estimate Proportion Difference",
  #                      path = "/Gain_Loss_Framing")
  # 


  
  
  
# Site definitions -----------------------------
  sites <- read.csv(file = paste(path_original, path_sites, sep=""), header = TRUE)
  colnames(sites)[1] <- "lab"

# Download and unzip the data ----------------------------------
# Many labs 1 has their data file posted to the OSF, zipped and in SPSS format
  # NOTE: Have to download file manually--on my windows machines this often leaves file that R cannot unzip
  # download.file("https://osf.io/nqg97/download", paste(path_original, "/Datasets.zip", sep=""), cacheOK = FALSE)
  unzip(zipfile = paste(path_original, "/Datasets.zip", sep = ""), 
        files = path_extract, 
        exdir = path_original,
        overwrite = TRUE,
        junkpaths = TRUE)


# Process/clean the data ----------------------
  # Read spss data file using foreign package
  data <- foreign::read.spss(file = paste(path_original, path_datafile, sep = ""), to.data.frame = TRUE)

  # Clean up whitespace in factors
  for(c in colnames(data)) {
    if(is.factor(data[[c]])) {
      levels(data[[c]]) <- trimws(levels(data[[c]]))
    }
  }


# Loop through studies -----------------------
  for (cstudy in studies) {
    print(cstudy$name)
  
    # Setup paths for this study
    study_path <- paste(path_prepped, cstudy$type_path, cstudy$path, sep="")
    dir.create(study_path)
    dir.create(paste(study_path, "/data", sep=""))
    dir.create(paste(study_path, "/plots", sep=""))
                      
  
    # Reduce down to just data for this study
    keeps <- c("session_id", "referrer", cstudy$iv, cstudy$dv, cstudy$exclude, cstudy$keeps)
    study_data <- data[keeps]
  
    # Make an IV and DV column to make things easier to address, will drop these later
    study_data$iv <- study_data[[cstudy$iv]]
    study_data$dv <- study_data[[cstudy$dv]]
  
    # Clean up data-------------------
      # Remove NAs for IV and DV
      study_data <- study_data[!is.na(study_data$iv), ]
      study_data <- study_data[!is.na(study_data$dv), ]
      
      # If defined, restict IV levels down to a list
      if(!is.null(cstudy$iv_levels)) {
        study_data <- study_data[study_data$iv %in% cstudy$iv_levels, ]
        study_data$iv <- droplevels(study_data$iv)
        study_data[[cstudy$iv]] <- droplevels(study_data[[cstudy$iv]])
      }
      
      # If defined, remove any data to exclude
      if(!is.null(cstudy$exclude)) {
        study_data <- study_data[study_data[[cstudy$exclude]] %in% c("Include", "include"), ]
      }
      
  
    # Loop through labs --------------------
    meta_table = data.frame(lab=factor(), 
                              m1=double(),
                              s1=double(),
                              n1=double(),
                              m2=double(),
                              s2=double(),
                              n2=double(),
                              mdiff=double(),
                              ci.low=double(),
                              ci.high=double()
    )
      
    for(lab in levels(study_data$referrer)) {
      print(lab)
      if(nrow(study_data[study_data$referrer == lab, ])>2) {
        # Get effect size for this lab    
        estimate <- esci::estimateMeanDifference(study_data[study_data$referrer == lab, ], iv, dv, reference.group = cstudy$reference_group)
        
        # Store lab results for meta-analysis
        meta_table <- rbind(meta_table, data.frame(
          lab=lab,
          m1=estimate$m2,
          s1=estimate$s2,
          n1=estimate$n2,
          m2=estimate$m1,
          s2=estimate$s1,
          n2=estimate$n1,
          mdiff=estimate$mdiff,
          ci.low=estimate$ci.low,
          ci.high=estimate$ci.high
        ))
        
        
        # Make the plot for this lab and then save it
        myplot <- esci::plotEstimatedDifference(estimate, 
                                                ylab = paste(cstudy$dv, cstudy$ylab, sep = " - "), 
                                                xlab = paste(cstudy$iv, estimate$formatted_mdiff, sep="\n")
                                                )
        myplot <- myplot + ggplot2::ggtitle(lab)
        ggplot2::ggsave(plot = myplot, filename = paste(study_path, "/plots/", lab, ".jpg", sep=""))
        
        # Save the raw data for this lab
        just_study_data <- study_data[!colnames(study_data) %in% c("iv", "dv")]
        write.csv(x = just_study_data[just_study_data$referrer == lab, ], file = paste(study_path, "/data/", lab, ".csv", sep=""))
      }      
    }
    
      
    # Do overall meta-analysis in Cohen's d to check against manuscript
    meta <- esci::estimateOverallRaw(meta_table, label = lab, 
                                     m1 = m1, m2 = m2, 
                                     s1 = s1, s2 = s2, 
                                     n1 = n1, n2 = n2, 
                                     report.cohens.d = TRUE, 
                                     conf.level = .99
                                    )
    print(cstudy$name)
    meta$result_table
    write.csv(x = meta$result_table, file = paste(study_path, "/", cstudy$name, "_meta-analysis_result.csv", sep=""))
    meta_plot <- esci::plotMetaEffect(meta)
    meta_plot <- meta_plot + ggplot2::labs(caption = paste(cstudy$name, "\n", 
                                                           estimate$level1, " - ", estimate$level2, "\n",
                                                           meta$effect.size.name, " = ", 
                                                           format(meta$effect.size, digits=2), 
                                                           " 99% CI[", format(meta$ci.low, digits=2), 
                                                           ", ", format(meta$ci.high, digits=2),
                                                           "]", sep = "")
                                            )
    ggplot2::ggsave(plot = meta_plot, file = paste(study_path, "/", cstudy$name, "_forestplot.jpg", sep=""))
      
    # Initialize columns for capture rates for the leave-one-out (LOO) meta-analysis table
    meta_table$other_lab_capture <- 0
    meta_table$meta_effect <- 0
    meta_table$meta_analysis_capture <- FALSE
    meta_table$same_size_replication_capture <- 0
    meta_table$same_size_replication_count <- 0
    
    # Get meta-analysis info for each 
    if(cstudy$reference_group == 2) {
      meta_table <- metafor::escalc(measure = "MD", data = meta_table, m1i = m1, sd1i = s1, n1i = n1, m2i = m2, sd2i = s2, n2i = n2)
    } else {
      meta_table <- metafor::escalc(measure = "MD", data = meta_table, m1i = m2, sd1i = s2, n1i = n2, m2i = m1, sd2i = s1, n2i = n1)
    }
    
    # Now loop through studies again, storing replication capture rate and loo meta-analysis capture
    for(row in 1:nrow(meta_table)) {
      # For this study, store its ci
      ci.low <- meta_table[row, "ci.low"]
      ci.high <- meta_table[row, "ci.high"]
      # Now count how the proportion of other studies were within this CI
      capture <- (nrow(meta_table[meta_table$mdiff > ci.low & meta_table$mdiff < ci.high, ]) - 1) / nrow(meta_table)
      meta_table[row, "other_lab_capture"] <- capture

      # Do a resampling analysis -- draw a sample of same size and see if this study captures the resample effect size
      resamples_valid <- 0
      resamples_captured <- 0
      ntotal <- meta_table[row, "n1"] + meta_table[row, "n2"]
      loo_study_data <- study_data[!study_data$referrer %in% c(meta_table[row, "lab"]), ]
      
      for(y in 1:resamples) {
        tsample <- sample(x = nrow(loo_study_data), size = ntotal, replace = FALSE)
        loo_estimate <- try(esci::estimateMeanDifference(loo_study_data[tsample, ], iv, dv, reference.group = cstudy$reference_group))
        if(!class(loo_estimate) == "try-error") {
          resamples_valid <- resamples_valid + 1
          if (loo_estimate$mdiff > ci.low & loo_estimate$mdiff < ci.high) { resamples_captured <- resamples_captured + 1 }
        }
      }
      meta_table[row, "same_size_replication_capture"] <- resamples_captured / resamples_valid
      meta_table[row, "same_size_replication_count"] <- resamples_valid

      # Do a meta-analysis without that lab's data, store the effect size, and store if this study captured that effect size
      loo_meta <- try(metafor::rma(yi, vi, data=meta_table[meta_table$lab != meta_table[row, "lab"], ], knha = TRUE))
      if(!class(loo_meta) == "try-error") {
        meta_table[row, "meta_effect"] <- loo_meta$b[1]
        meta_table[row, "meta_analysis_capture"] <- (loo_meta$b[1] > ci.low & loo_meta$b[1] < ci.high)
      } else {
        meta_table[row, "meta_effect"] <- NA
        meta_table[row, "meta_analysis_capture"] <- NA
      }
    }
    
    # Last cleanup of meta-table and merge with site info
    meta_table$yi <- NULL
    meta_table$vi <- NULL
    meta_table <- merge(meta_table, sites)
    names(meta_table)[names(meta_table) == "m1"] <- paste(estimate$level2, "_m", sep="")
    names(meta_table)[names(meta_table) == "m2"] <- paste(estimate$level1, "_m", sep="")
    names(meta_table)[names(meta_table) == "s1"] <- paste(estimate$level2, "_s", sep="")
    names(meta_table)[names(meta_table) == "s2"] <- paste(estimate$level1, "_s", sep="")
    names(meta_table)[names(meta_table) == "n1"] <- paste(estimate$level2, "_n", sep="")
    names(meta_table)[names(meta_table) == "n2"] <- paste(estimate$level1, "_n", sep="")
    
    # Now write the lab-by-lab results for this study to be used for meta-analysis
    write.csv(x = meta_table, file = paste(study_path, "/", cstudy$name, "_by-lab.csv", sep=""))
  }
  