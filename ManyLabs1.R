# Many Labs 1

# Klein, R. A., Ratliff, K. A., Vianello, M., Adams, R. B., Bahník, Š., Bernstein, M. J., … Nosek, B. a. (2014). Investigating Variation in Replicability. Social Psychology, 45(3), 142–152. https://doi.org/10.1027/1864-9335/a000178

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


  studies$sunk <- list(iv = "sunkgroup",
                       dv = "sunkDV",
                       iv_levels = c("paid", "free"),
                       ylab = "Rated likelihood of attending game (1-9)",
                       reference_group = 2,
                       exclude = NULL,
                       keeps = NULL,
                       name = "Sunk costs",
                       type = "eimd",
                       type_path = "/Estimate Independent Mean Difference",
                       path = "/ML1_Sunk_Costs")

  studies$framing <- list(iv = "gainlossgroup",
                          dv = "gainlossDV",
                          iv_levels = c("People will die", "People will be saved"),
                          dv_levels = c("chose program with exact outcome", "chose program with probability outcome"),
                          ylab = "Proportion choosing the certain option",
                          group_labels = c("Loss Framing", "Gain Framing"),
                          group.level = 2,
                          case.level = 1,
                          exclude = NULL,
                          keeps = NULL,
                          name = "Gain versus loss framing",
                          type = "epd",
                          type_path = "/Estimate Proportion Difference",
                          path = "/ML1_Gain_Loss_Framing")

  studies$anchor1 <- list(iv = "anch1group",
                          dv = "anchoring1",
                          iv_levels = c("lowanchor", "highanchor"),
                          reference_group = 1,
                          ylab = "Estimated distance from San Fran to NY (miles)",
                          exclude = NULL,
                          keeps = NULL,
                          name = "Anchoring1 - Distance from SF to NY",
                          type = "eimd",
                          type_path = "/Estimate Independent Mean Difference",
                          path = "/ML1_Anchoring1_City_Distance")

  studies$anchor2 <- list(iv = "anch2group",
                          dv = "anchoring2",
                          iv_levels = c("lowanchor", "highanchor"),
                          reference_group = 1,
                          ylab = "Estimated population of Chicago",
                          exclude = NULL,
                          keeps = NULL,
                          name = "Anchoring2 - Chicago population",
                          type = "eimd",
                          type_path = "/Estimate Independent Mean Difference",
                          path = "/ML1_Anchoring2_Chicago_Population")

  studies$anchor3 <- list(iv = "anch3group",
                          dv = "anchoring3",
                          iv_levels = c("lowanchor", "highanchor"),
                          reference_group = 1,
                          ylab = "Estimated height of Mt. Everest (feet)",
                          exclude = NULL,
                          keeps = NULL,
                          name = "Anchoring3 - Mt Everest height",
                          type = "eimd",
                          type_path = "/Estimate Independent Mean Difference",
                          path = "/ML1_Anchoring3_Mt_Everest_Height")

  studies$anchor4 <- list(iv = "anch4group",
                          dv = "anchoring4",
                          iv_levels = c("lowanchor", "highanchor"),
                          reference_group = 1,
                          ylab = "Estimated babies born daily in USA",
                          exclude = NULL,
                          keeps = NULL,
                          name = "Anchoring4 - US Babies Daily",
                          type = "eimd",
                          type_path = "/Estimate Independent Mean Difference",
                          path = "/ML1_Anchoring4_US_Babies_Daily")

  studies$gambler <- list(iv = "gambfalgroup",
                      dv = "gambfalDV",
                      iv_levels = c("two6", "three6"),
                      reference_group = 1,
                      ylab = "Sqrt of estimated previous rolls, +/-3SD outliers removed",
                      exclude = NULL,
                      keeps = NULL,
                      name = "Retrospective fambler's fallacy",
                      type = "eimd",
                      type_path = "/Estimate Independent Mean Difference",
                      path = "/ML1_Retrospective_Gamblers_Fallacy")

  studies$lowhighcategory <- list(iv = "scalesgroup",
                                  dv = "scales",
                                  iv_levels = c("low category scale", "high category scale"),
                                  dv_levels = c("less than 2 1/2 hrs", "more than 2 1/2 hrs"),
                                  ylab = "Proportion reporting watching more than 2 1/2 hours of TV/day",
                                  group_labels = c("Low-Frequency Scale", "High-Frequency Scale"),
                                  group.level = 2,
                                  case.level = 2,
                                  exclude = NULL,
                                  keeps = NULL,
                                  name = "Low-versus-high category scales",
                                  type = "epd",
                                  type_path = "/Estimate Proportion Difference",
                                  path = "/ML1_Low_vs_High_Scales")

  studies$reciprocity <- list(iv = "reciprocitygroup",
                              dv = "reciprocityus",
                              iv_levels = c("Asked second", "Asked first"),
                              dv_levels = c("no", "yes"),
                              ylab = "Proportion saying yes, the US should allow reporters in from North Korea",
                              group_labels = c("Asked about US behavior second", "Asked about US behavior first"),
                              group.level = 1,
                              case.level = 2,
                              exclude = NULL,
                              keeps = NULL,
                              name = "Norm of reciprocity",
                              type = "epd",
                              type_path = "/Estimate Proportion Difference",
                              path = "/ML1_Norm_of_Reciprocity")
  
  studies$allowforbid <- list(iv = "allowedforbiddenGroup",
                              dv = "allowedforbidden",
                              iv_levels = c("allowed", "forbidden"),
                              dv_levels = c("does not support banning anti-democratic speeches", "support banning anti-democratic speeches"),
                              ylab = "Proportion supporting a ban on anti-democratic speeches",
                              group_labels = c("Should speeches against democracy be allowed", "Should speeches against democracy be forbidden"),
                              group.level = 1,
                              case.level = 2,
                              exclude = NULL,
                              keeps = NULL,
                              name = "Allowed_Forbidden",
                              type = "epd",
                              type_path = "/Estimate Proportion Difference",
                              path = "/ML1_Allowed_Forbidden")
  
  studies$quote <- list(iv = "quoteGroup",
                          dv = "quote",
                          iv_levels = c("disliked source", "liked source"),
                          reference_group = 1,
                          ylab = "Rated liking of quote (1-9)",
                          exclude = NULL,
                          keeps = NULL,
                          name = "Quote attribution",
                          type = "eimd",
                          type_path = "/Estimate Independent Mean Difference",
                          path = "/ML1_Quote_Attribution")

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
                        path = "/ML1_Flag_Priming")

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
                        path = "/ML1_Currency_Priming")

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
                           path = "/ML1_Imagined_Contact")

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
                      path = "/ML1_Sex_Differences_Math_IAT")

  studies$IATcorrelation <- list(iv = "d_art",
                                 dv = "IATexp.overall",
                                 ylab = "Average score across self-reported attitudes about math",
                                 exclude = "IATEXPfilter",
                                 keeps = NULL,
                                 name = "Implicit math attitudes relations with self-reported",
                                 type = "ec",
                                 type_path = "/Estimate Correlation",
                                 path = "/ML1_Implicit_Math_and_Self_Reported")



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

  # Fix levels for IATEXPFilter
  data$IATEXPfilter <- as.factor(data$IATEXPfilter)
  levels(data$IATEXPfilter) <- c("Exclude", "Include")
  
  # Fix coding of allowed/forbid responses --- see note in SPSS syntax file for ManyLabs 1 @ https://osf.io/er9xg/
  data$allowedforbidden_fixed <- "support banning anti-democratic speeches"
  data[data$allowedforbiddenGroup == "forbidden" & !is.na(data$allowedforbidden) & data$allowedforbidden == "NO", ]$allowedforbidden_fixed <- "does not support banning anti-democratic speeches"
  data[data$allowedforbiddenGroup == "allowed" & !is.na(data$allowedforbidden) & data$allowedforbidden == "YES", ]$allowedforbidden_fixed <- "does not support banning anti-democratic speeches"
  data[is.na(data$allowedforbidden), ]$allowedforbidden_fixed <- NA
  data$allowedforbidden <- as.factor(data$allowedforbidden_fixed)
  
    
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
      if(cstudy$reference_group == 2) {
        meta_table <- metafor::escalc(measure = "MD", data = meta_table, m1i = m1, sd1i = s1, n1i = n1, m2i = m2, sd2i = s2, n2i = n2)
      } else {
        meta_table <- metafor::escalc(measure = "MD", data = meta_table, m1i = m2, sd1i = s2, n1i = n2, m2i = m1, sd2i = s1, n2i = n1)
      }
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
  