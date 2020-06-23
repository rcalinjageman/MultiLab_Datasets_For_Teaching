# MultiLab_Datasets_For_Teaching
This project provides data sets for teaching statistics, drawn from published data from multi-lab research projects in psychology.

The goal is to provide instructors with real data sets that students can use to practice statistical skills.  With multi-lab data sets, students or teams of students can be assigned different labs to analyze.  This is useful because
* it's no fun when everyone gets the exact same answer
* analyzing their own data sets lets students interpret and share their interpretations across the class, 
* this can help students grapple with the reality of sampling variation (pretty key for stats education, right?).
* it naturally leads students to be interested in meta-analysis, and allows meta-analyiss to be folded into each stats topic in an organic way.



## Project Structure ##
You'll find instructor resources in the "prepped_data" directory.

This directory is organized by study design:
* Estimate Correlation has data sets that involve correlating two numeric variables (the estimation version of a correlation test)
* Estimate Independent Mean Difference has data sets where you compare the means of two independent groups (the estimation version of an independent-samples t-test)
* Estimate Proportion Difference has data sets where you compare proportions of two independent groups (the estimation version on a 2x2 Chi square)

For each study you will find:
* A data directory - this has data, split by lab, In .csv format.  The name of the file is the name of the lab that collected the data.  The data has been simplified--in general only variables needed for the analysis have been included, though some demographic and/or attention check variables are also sometimes included to enable a bit of exploration
* A plots directory - this has a plot for each lab including labels with the effect size and CI for that lab.  This is kind of the answer key for instructors--it lets you pull up, at a glance, what the students should have obtained for each lab data set.
* A meta_analysis_results file in csv format-- this gives the meta-analysis of the study across all participating labs.  It usually matches well with what was reported in the multi-lab manuscript, though there can be subtle differences due to different processing pipelines, meta-analysis options, etc.
* A by_lab file in csv format -- this has all the data needed, lab-by-lab, for students to meta-analyze the study of interest.  So after analyzing some of the different lab results, they can actually try out the meta-analysis
* A forest plot - shows each lab and the overall meta-analysis effect size


## Analysis Scripts ##
In the root directory of the project you will find R scripts--one for each multi-lab study.  You'll see that each *tries* to download the data from online and then process in accordance with the scripts posted for that project, but this is not always as possible as it might seem.

You'll also see a directory called "original data".  Obviously, this has the original data.  In addition, it often has other files I've found necessary to be able to properly process the mutli-lab data.

## Work in progress ##
This is a work in progress.  Your help, suggestions, feature requests, bug reports, etc. are welcome.
