# pet_adoption
Experimenting with machine learning models that attempt to predict pet adoption speeds

# Introduction

This repository contains the scripts, data sets and report required for the final module of [Professional Certificate in Data Science learning programme by HarvardX](https://www.edx.org/professional-certificate/harvardx-data-science)

The final Course Capstone (PH125.9x) requires the student to apply machine learning techniques that go beyond standard linear regression. The student will have the opportunity to use a publicly available dataset to solve the problem of choice. 

The ability to clearly communicate the process and insights gained from an analysis is an important skill for data scientists.

Data set can be selected from [Kaggle](www.kaggle.com) or [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets.html). 

# Summary of file contents:
* The R script file contains codes used for data exploration, visualisation and building the prediction models
* The Rmd file contains codes to generate the PDF report that describes the process and outcome of the development of models that predict pet adoption speeds
* The R object files (.rds) contain models that will be loaded by the Rmd script to generate the PDF report


# Detail description of files in this repository:

## File sets (required):
1. Report in PDF: cyo_project.PDF
2. Report in Rmd: cyo_project.Rmd
3. Script in R that generates my prediction models and result findings: cyo_project.R
4. There are 4 data files in CSV format (pets.csv, state_labels.csv, color_labels.csv, breed_labels.csv)
5. Prediction models stored as R objects in RDS format that the Rmd file will load to produce the PDF report. Otherwise it can take hours (in my case, days on a standard MacBook Air with 8GB RAM) to completely run all the experimental models.  There are 4 files (model_1.rds, model_2.rds, model_3_ntrees.rds, model_3.rds) that will be used by the Rmd script.

## File sets (optional):
1. The results.rds contain the final results (this file is not loaded by Rmd script, it is included for reference should one be interested)
2. The model_3_modellist_ntrees.rds file that contains the models for all ntree values of model 3 is 280MB - and could not be loaded to GitHub or the edX site.  You may run the codes for generating model_3 in cyo_project.R file, or contact me should you be interested to have a copy.  The file is optional and not required by the Rmd script to generate the PDF file

## Note: 
1. All the R, Rmd, CSV data files and RDS object files are to be saved to the same directory for the Rmd and R scripts to run correctly.
2. The R script will make use of RStudioApi call to set the working directory in order to pick up the CSV data files correctly.
3. The Rmd script will use a locally defined variable 'working_dir' to pick up the CSV data files and R object files that contain the relevant prediction models.  Change the 'working_dir' in the Rmd file according to the local directory where the files are stored on your machine.
