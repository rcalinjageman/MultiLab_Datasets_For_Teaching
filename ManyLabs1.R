# Many Labs 1

# Packages ---------------------------------------
  if (!is.element("devtools", installed.packages()[,1]))  install.packages("devtools", dep = TRUE)
  if (!is.element("esci", installed.packages()[,1])) devtools::install_github(repo = "rcalinjageman/esci", dependencies = TRUE)
  if (!is.element("ggplot2", installed.packages()[,1])) install.packages("ggplot2", dep = TRUE)
  if (!is.element("metafor", installed.packages()[,1])) install.packages("metafor", dep = TRUE)
  if (!is.element("foreign", installed.packages()[,1])) install.packages("foreign", dep = TRUE)

  library(esci)
  library(ggplot2)
  library(metafor)
  library(foreign)


# Parameters --------------------------
  path_original <- "./original_data/ManyLabs1"
  path_extract <- "Data/CleanedDataset.sav"
  path_datafile <- "/CleanedDataset.sav"
  path_prepped <- "./prepped_data"
  path_sites <- "/ManyLabs1_sites.csv"

# Study definitions ---------------------
  studies <- list()
  studies$sunk <- list(iv = "sunkgroup", 
                       dv = "sunkDV", 
                       iv_levels = c("paid", "free"),
                       exclude = NULL,
                       keeps = NULL,
                       name = "Sunk costs",
                       type = "eimd",
                       type_path = "/Estimate Independent Mean Difference",
                       path = "/Sunk_Costs")
  
  # studies$anchor1 <- list(iv = "anch1group", 
  #                         dv = "anchoring1", 
  #                         iv_levels = c("lowanchor", "highanchro"),
  #                         exclude = NULL,
  #                         keeps = NULL,
  #                         name = "Anchoring 1",
  #                         type = "eimd",
  #                         type_path = "/Estimate Independent Mean Difference",
  #                         path = "/Anchoring_1")
  # studies$iat <- list(iv = "sex", 
  #                     dv = "d_art", 
  #                     iv_levels = c("m", "f"), 
  #                     exclude = "iat_exclude", 
  #                     keeps = NULL, 
  #                     name = "Implicit math attitudes", 
  #                     type = "eimd", 
  #                     type_path = "/Estimate Independent Mean Difference", 
  #                     path = "/MathIAT")
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
        study_data <- study_data[study_data[[cstudy$exclude]] %in% "Include", ]
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
      if(nrow(study_data[study_data$referrer == lab, ]>2)) {
        # Get effect size for this lab    
        estimate <- esci::estimateMeanDifference(study_data[study_data$referrer == lab, ], iv, dv)
        
        # Store lab results for meta-analysis
        meta_table <- rbind(meta_table, data.frame(
          lab=lab,
          m1=estimate$m1,
          s1=estimate$s1,
          n1=estimate$n1,
          m2=estimate$m2,
          s2=estimate$s2,
          n2=estimate$n2,
          mdiff=estimate$mdiff,
          ci.low=estimate$ci.low,
          ci.high=estimate$ci.high
        ))
        
        
        # Make the plot for this lab and then save it
        myplot <- esci::plotEstimatedDifference(estimate, 
                                                ylab = cstudy$dv, 
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
    ggplot2::ggsave(plot = meta_plot, file = paste(study_path, "/", cstudy$name, "_forestplot.jpg", sep=""))
      
    # Initialize columns for capture rates for the leave-one-out (LOO) meta-analysis table
    meta_table$replication_capture <- 0
    meta_table$meta_effect <- 0
    meta_table$meta_analysis_capture <- FALSE
    
    # Now fix to be regular mean difference
    meta_table <- metafor::escalc(measure = "MD", data = meta_table, m1i = m1, sd1i = s1, n1i = n1, m2i = m2, sd2i = s2, n2i = n2)
    
    # Now loop through studies again, storing replication capture rate and loo meta-analysis capture
    for(row in 1:nrow(meta_table)) {
      # For this study, store its ci
      ci.low <- meta_table[row, "ci.low"]
      ci.high <- meta_table[row, "ci.high"]
      # Now count how the proportion of other studies were within this CI
      capture <- (nrow(meta_table[meta_table$mdiff > ci.low & meta_table$mdiff < ci.high, ]) - 1) / nrow(meta_table)
      meta_table[row, "replication_capture"] <- capture

      # Do a resampling analysis

      # Do a meta-analysis without that lab's data, store the effect size, and store if this study captured that effect size
      loo_meta <- metafor::rma(yi, vi, data=meta_table[meta_table$lab != meta_table[row, "lab"], ], knha = TRUE)
      meta_table[row, "meta_effect"] <- loo_meta$b[1]
      meta_table[row, "meta_analysis_capture"] <- (loo_meta$b[1] > ci.low & loo_meta$b[1] < ci.high)
    }
    
    # Last cleanup of meta-table and merge with site info
    meta_table$yi <- NULL
    meta_table$vi <- NULL
    meta_table <- merge(meta_table, sites)
    names(meta_table)[names(meta_table) == "m1"] <- paste(estimate$level1, "_m", sep="")
    names(meta_table)[names(meta_table) == "m2"] <- paste(estimate$level2, "_m", sep="")
    names(meta_table)[names(meta_table) == "s1"] <- paste(estimate$level1, "_s", sep="")
    names(meta_table)[names(meta_table) == "s2"] <- paste(estimate$level2, "_s", sep="")
    names(meta_table)[names(meta_table) == "n1"] <- paste(estimate$level1, "_n", sep="")
    names(meta_table)[names(meta_table) == "n2"] <- paste(estimate$level2, "_n", sep="")
    
    # Now write the lab-by-lab results for this study to be used for meta-analysis
    write.csv(x = meta_table, file = paste(study_path, "/", cstudy$name, "_by-lab.csv", sep=""))
  }
  