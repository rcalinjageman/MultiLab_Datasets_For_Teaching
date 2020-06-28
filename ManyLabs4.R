# Many Labs 4

# Klein, R. A., Cook, C. L., Ebersole, C. R., Vitiello, C. A., Nosek, B. A., Chartier, C. R., … Ratliff, K. A. (2019, December 11). 
# Many Labs 4: Failure to Replicate Mortality Salience Effect With and Without Original Author Involvement. 
# https://doi.org/10.31234/osf.io/vef2c

# Packages ---------------------------------------
  if (!is.element("devtools", installed.packages()[,1]))  install.packages("devtools", dep = TRUE)
  if (!is.element("esci", installed.packages()[,1])) devtools::install_github(repo = "rcalinjageman/esci", dependencies = TRUE)
  if (!is.element("ggplot2", installed.packages()[,1])) install.packages("ggplot2", dep = TRUE)
  if (!is.element("metafor", installed.packages()[,1])) install.packages("metafor", dep = TRUE)

  library(esci)
  library(ggplot2)
  library(metafor)


# Parameters --------------------------
  # Paths info
  path_original <- "./original_data/ManyLabs4"
  path_datafile <- "/merged_deidentified.csv"
  path_prepped <- "./prepped_data"
  path_sites <- "/ManyLabs4_sites.csv"


  # Number of resamples for resampling analysis
  resamples <- 500
  
# Study definitions ---------------------
  studies <- list()


  studies$mortality_salience <- list(iv = "ms_condition",
                       dv = "pro_minus_anti",
                       iv_levels = c("tv", "ms"),
                       ylab = "Preference for pro-American essay (difference of 1-9 ratings)",
                       reference_group = 2,
                       exclude = NULL,
                       keeps = c("race", "countryofbirth"),
                       name = "Mortality Salience Effect",
                       type = "eimd",
                       type_path = "/Estimate Independent Mean Difference",
                       path = "/ML4_Mortality_Salience_Effect")



# Site definitions -----------------------------
  sites <- read.csv(file = paste(path_original, path_sites, sep=""), header = TRUE)
  colnames(sites)[1] <- "lab"

# Download and unzip the data ----------------------------------
# Many labs 4 has their data file posted to the OSF is csv format
  # NOTE: Have to download file manually--on my windows machines this often leaves file that R cannot unzip
  download.file("https://osf.io/d8zmp/download", paste(path_original, path_datafile, sep=""), cacheOK = FALSE)


# Process/clean the data ----------------------
  # Read spss data file using foreign package
  data <- read.csv(file = paste(path_original, path_datafile, sep = ""), header = TRUE)
  data$referrer <- data$source
  
  # Fix levels for race
  levels(data$race)[levels(data$race) == "1"] <- "White"
  levels(data$race)[levels(data$race) == "2"] <- "Black or African-American"
  levels(data$race)[levels(data$race) == "3"] <- "American Indian or Alaska Native"
  levels(data$race)[levels(data$race) == "4"] <- "Asian"
  levels(data$race)[levels(data$race) == "5"] <- "Native Hawaiian or Pacific Islander"
  levels(data$race)[levels(data$race) == "6"] <- "Other"

  # Fix levels for race
  levels(data$countryofbirth)[levels(data$countryofbirth) == "1"] <- "USA"
  levels(data$countryofbirth)[levels(data$countryofbirth) == "2"] <- "Other"

  
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
    keeps <- c("participantnum", "referrer", cstudy$iv, cstudy$dv, cstudy$exclude, cstudy$keeps)
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
      
      # If defined, restict DV levels down to a list (for compare two proportion designs)
      if(!is.null(cstudy$dv_levels)) {
        study_data <- study_data[study_data$dv %in% cstudy$dv_levels, ]
        study_data$dv <- droplevels(study_data$dv)
        study_data[[cstudy$dv]] <- droplevels(study_data[[cstudy$dv]])
      }
      
      # If defined, remove any data to exclude
      if(!is.null(cstudy$exclude)) {
        study_data <- study_data[study_data[[cstudy$exclude]] %in% c("Include", "include"), ]
      }
      
    # Loop through labs --------------------
    # Initialize meta_table to store lab-by-lab results
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
    if(cstudy$type == "epd") {
      meta_table = data.frame(lab=factor(), 
                              cases1=double(),
                              n1=double(),
                              cases2=double(),
                              n2=double(),
                              pdiff=double(),
                              ci.low=double(),
                              ci.high=double()
      )
    }
    if(cstudy$type == "ec") {
        meta_table = data.frame(lab=factor(), 
                                r=double(),
                                n=double(),
                                ci.low=double(),
                                ci.high=double()
        )
    }
      
    for(lab in levels(study_data$referrer)) {
      print(lab)
      if(nrow(study_data[study_data$referrer == lab, ])>2) {
        # Get effect size for this lab
        if(cstudy$type == "eimd") {
          estimate <- esci::estimateMeanDifference(study_data[study_data$referrer == lab, ], iv, dv, 
                                                   reference.group = cstudy$reference_group)
          
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
          
          # Make the plot
          myplot <- esci::plotEstimatedDifference(estimate, 
                                                  ylab = paste(cstudy$dv, cstudy$ylab, sep = " - "), 
                                                  xlab = paste(cstudy$iv, estimate$formatted_mdiff, sep="\n")
          )
        }
        if(cstudy$type == "epd") {
          estimate <- esci::estimateProportionDifference(study_data[study_data$referrer == lab, ], iv, dv, 
                                                         group.level = cstudy$group.level, case.level = cstudy$case.level)
          # Store for meta-analysis
          meta_table <- rbind(meta_table, data.frame(
            lab=lab,
            cases1=estimate$cases1,
            n1=estimate$n1,
            cases2=estimate$cases2,
            n2=estimate$n2,
            pdiff=estimate$Pdiff,
            ci.low=estimate$ci.low,
            ci.high=estimate$ci.high
          ))
          
          # Make the plot
          myplot <- esci::plotEstimatedProportionDifference(estimate, 
                                                            ylab = paste(cstudy$dv, cstudy$ylab, sep = " - "), 
                                                            xlab = paste(cstudy$iv, estimate$formatted_pdiff, sep="\n")
          )
        }
        if(cstudy$type == "ec") {
          estimate <- esci::estimateCorrelation(study_data[study_data$referrer == lab, ], iv, dv)
          # Store for meta-analysis
          meta_table <- rbind(meta_table, data.frame(
            lab=lab,
            r=estimate$r,
            n=estimate$n,
            ci.low=estimate$ci.low,
            ci.high=estimate$ci.high
          ))
          
          # Make the plot
          myplot <- esci::plotScatterPlot(estimate, 
                                          show.line = TRUE,
                                          show.meanCI = TRUE,
                                          ylab = paste(cstudy$dv, cstudy$ylab, sep = " - "),
                                          xlab = paste(cstudy$iv, estimate$formatted_r, sep="\n")
                                          )
        }
        # Save the plot
        myplot <- myplot + ggplot2::ggtitle(lab)
        ggplot2::ggsave(plot = myplot, filename = paste(study_path, "/plots/", cstudy$name, " - ", lab, ".jpg", sep=""))
        
        # Save the raw data for this lab
        just_study_data <- study_data[!colnames(study_data) %in% c("iv", "dv")]
        write.csv(x = just_study_data[just_study_data$referrer == lab, ], file = paste(study_path, "/data/", cstudy$name, " - ", lab, ".csv", sep=""))
      }      
    }
    
      
    # Do overall meta-analysis in Cohen's d to check against manuscript
      if(cstudy$type == "eimd") {
        meta <- esci::estimateOverallRaw(meta_table, label = lab, 
                                         m1 = m1, m2 = m2, 
                                         s1 = s1, s2 = s2, 
                                         n1 = n1, n2 = n2, 
                                         report.cohens.d = TRUE, 
                                         conf.level = .99
                                        )
        graph_label <- paste(estimate$level1, " - ", estimate$level2, sep = "")
      }
      
      if(cstudy$type == "ec") {
        meta <- esci::estimateOverallCorrelation(meta_table,
                                                 label = lab,
                                                 rvalues = r,
                                                 ns = n,
                                                 conf.level = .99
                                                 )
        graph_label <- paste(cstudy$iv, " corr with ", cstudy$dv, sep = "")
      }
      if(cstudy$type == "epd") {
        meta <- esci::estimateOverallProportionDifference(meta_table,
                                                 label = lab,
                                                 cases1 = cases1,
                                                 n1 = n1,
                                                 cases2 = cases2,
                                                 n2 = n2,
                                                 conf.level = .99
        )
        graph_label <- paste("Group ", estimate$level1, " - Group ", estimate$level2, " for proportion of ", colnames(estimate$summary_data)[2] , sep = "")
      }
      
      print(cstudy$name)
      meta$result_table
      write.csv(x = meta$result_table, file = paste(study_path, "/", cstudy$name, "_meta-analysis_result.csv", sep=""))
      meta_plot <- esci::plotMetaEffect(meta)
      meta_plot <- meta_plot + ggplot2::labs(caption = paste(cstudy$name, "\n", 
                                                             graph_label, "\n",
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
    if(cstudy$type == "eimd") {
      meta_table <- metafor::escalc(measure = "MD", data = meta_table, m1i = m2, sd1i = s2, n1i = n2, m2i = m1, sd2i = s1, n2i = n1)
    }
    
    if(cstudy$type == "epd") {
      meta_table <- metafor::escalc(measure = "RD", data = meta_table, ai = cases1, ci = cases2, n1i = n1, n2i = n2)
    }
    
    if(cstudy$type == "ec") {
      meta_table <- metafor::escalc(measure = "COR", data = meta_table, ri = r, ni = n)
    }
    
    # Now loop through studies again, storing replication capture rate and loo meta-analysis capture
    for(row in 1:nrow(meta_table)) {
      # For this study, store its ci
      ci.low <- meta_table[row, "ci.low"]
      ci.high <- meta_table[row, "ci.high"]
      
      # Now count how the proportion of other studies were within this CI
      if (cstudy$type == "eimd") {
        capture <- (nrow(meta_table[meta_table$mdiff > ci.low & meta_table$mdiff < ci.high, ]) - 1) / nrow(meta_table)
      }
      if (cstudy$type == "epd") {
        capture <- (nrow(meta_table[meta_table$pdiff > ci.low & meta_table$pdiff < ci.high, ]) - 1) / nrow(meta_table)
      }
      if (cstudy$type == "ec") {
        capture <- (nrow(meta_table[meta_table$r > ci.low & meta_table$r < ci.high, ]) - 1) / nrow(meta_table)
      }
      meta_table[row, "other_lab_capture"] <- capture

      
      # Do a resampling analysis -- draw a sample of same size and see if this study captures the resample effect size
      resamples_valid <- 0
      resamples_captured <- 0
      if (cstudy$type == "ec") {
        ntotal <- meta_table[row, "n"]
      } else {
        ntotal <- meta_table[row, "n1"] + meta_table[row, "n2"]
      }
      loo_study_data <- study_data[!study_data$referrer %in% c(meta_table[row, "lab"]), ]
      
      for(y in 1:resamples) {
        tsample <- sample(x = nrow(loo_study_data), size = ntotal, replace = FALSE)
        
        if (cstudy$type == "eimd") {
          loo_estimate <- try(esci::estimateMeanDifference(loo_study_data[tsample, ], iv, dv, reference.group = cstudy$reference_group))
        }
        if (cstudy$type == "epd") {
          loo_estimate <- esci::estimateProportionDifference(loo_study_data[tsample, ], iv, dv, 
                                                             group.level = cstudy$group.level, case.level = cstudy$case.level)
        }
        if (cstudy$type == "ec") {
          loo_estimate <- esci::estimateCorrelation(loo_study_data[tsample, ], iv, dv)
        }
        
        if(!class(loo_estimate) == "try-error") {
            resamples_valid <- resamples_valid + 1
            if (cstudy$type == "eimd") {
              if (loo_estimate$mdiff > ci.low & loo_estimate$mdiff < ci.high) { 
                resamples_captured <- resamples_captured + 1 
              }
            }
            if (cstudy$type == "epd") {
              if (loo_estimate$Pdiff > ci.low & loo_estimate$Pdiff < ci.high) { 
                resamples_captured <- resamples_captured + 1 
              }
            }
            if (cstudy$type == "ec") {
              if (loo_estimate$r > ci.low & loo_estimate$r < ci.high) { 
                resamples_captured <- resamples_captured + 1 
              }
            }
        }
      }
      
      meta_table[row, "same_size_replication_capture"] <- resamples_captured / resamples_valid
      meta_table[row, "same_size_replication_count"] <- resamples_valid

      # Do a meta-analysis without that lab's data, store the effect size, and store if this study captured that effect size
      loo_meta <- try(metafor::rma(yi, vi, data=meta_table[meta_table$lab != meta_table[row, "lab"], ], knha = TRUE))
      if(!class(loo_meta) %in% c("try-error")) {
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
    # back_table <- meta_table
    meta_table <- merge(meta_table, sites)
    if (cstudy$type == "eimd") {
      names(meta_table)[names(meta_table) == "m1"] <- paste(estimate$level2, "_m", sep="")
      names(meta_table)[names(meta_table) == "m2"] <- paste(estimate$level1, "_m", sep="")
      names(meta_table)[names(meta_table) == "s1"] <- paste(estimate$level2, "_s", sep="")
      names(meta_table)[names(meta_table) == "s2"] <- paste(estimate$level1, "_s", sep="")
      names(meta_table)[names(meta_table) == "n1"] <- paste(estimate$level2, "_n", sep="")
      names(meta_table)[names(meta_table) == "n2"] <- paste(estimate$level1, "_n", sep="")
    }
    
    if (cstudy$type == "epd") {
      names(meta_table)[names(meta_table) == "cases1"] <- paste(cstudy$group_labels[2], "_cases", sep="")
      names(meta_table)[names(meta_table) == "n1"] <- paste(cstudy$group_labels[2], "_n", sep="")
      names(meta_table)[names(meta_table) == "cases2"] <- paste(cstudy$group_labels[1], "_cases", sep="")
      names(meta_table)[names(meta_table) == "n2"] <- paste(cstudy$group_labels[1], "_n", sep="")
      
    } 
          
    # Now write the lab-by-lab results for this study to be used for meta-analysis
    write.csv(x = meta_table, file = paste(study_path, "/", cstudy$name, "_by-lab.csv", sep=""))
  }
  