# Welcome to US EPA Data Discovery Tool-PFAS!

This is a quality assurance procedure for cleaning up Per- and polyfluoroalkyl substances (PFAS) surface water concentration data and can be adapted for other variables.

## Purpose

The purpose of this document is to provide an order and general description of the steps taken to access, download, clean, and tag PFAS surface water data supplied by the US EPA’s Water Quality Portal. This document demonstrates the use of the R package ["EPATADA"](https://usepa.github.io/EPATADA/index.html) to assist with coding processes used to generate the results of a nationwide study of ambient surface water PFAS concentrations in the US. The code, data, and supporting documentation are all available free of charge. The document is broken down into sections that outline how each attached .R file can be used sequentially to evaluate surface water data in a reproducible manner. 

## Authors and Affiliations

Authors: Hannah Ferriby1, Kateri Salk1, Matt Dunn1, Christopher Wharton1, Tammy Newcomer-Johnson2

Affiliations: 1. Tetra Tech Inc. 2. United States Environmental Protection Agency 

Corresponding Author: [Tammy Newcomer-Johnson](newcomer-johnson.tammy@epa.gov) 

Preferred citation: H. Ferriby, K. Salk, M. Dunn, C. Wharton, and T. Newcomer-Johnson. 2025. US EPA Data Discovery Tool-PFAS. https://github.com/TammyNewcomerJohnson/EPATADA-PFAS-SW

# How to Use TADA-PFAS-SW

[Tools for Automated Data Analysis](https://usepa.github.io/EPATADA/index.html)

[Function Reference Glossary](https://usepa.github.io/EPATADA/reference/index.html)

## Included .R Files

data_pull.R

data_processing.R

data_visualization.R

NARS_Data_Review.R


## Outline of data_pull.R: 

### 1.	Setup and Package Installation: The following packages are required to perform the analysis presented in this study.
   a.	Check and Install Dependencies:
      i.	Checks for the remotes package and installs it if not already present.
      ii.	Installs the EPATADA package from GitHub.
   b.	Load Required Libraries:
      i.	Required R libraries (EPATADA, tidyverse, scatterpie, ggplot2, sf, scales) loaded.
### 2.	Data Retrieval: EPATADA package is used to query and pull data from US EPA’s Water Quality Portal based on the characteristicName. 
   a.	Using TADA_BigDataRetrieval:
      i.	Pulls data for PFAS-related compounds from a specified source using predefined characteristics:
         1.	characteristicType: Includes broad PFAS categories (e.g., "PFAS, Perfluorinated Alkyl Substance").
         2.	characteristicName: Targets specific PFAS compounds (e.g., “Perfluorooctanoic acid”, “Perfluorooctanesulfonate”).
         3.	Write Results of Data Pull to CSV: The unfiltered, uncleaned dataset is exported as a CSV file (data_pull.csv) for further processing or analysis.
### 3.	Initial Plotting to Create Map of all matrices and data 
   a.	Filter by States with data: Create new dataset containing only states with available surface water and/or tissue data 
   b.	Create scatterpie map with circle radius representing number of samples present per state
   c.	Including all matrices and all unfiltered data for PFOA and PFOS


## Outline of data_processing.R:

### 1.	Setup and Initial Data Import: Loading new packages and the recently exported data for further analysis. 
   a.	Load Packages: Required R libraries (EPATADA, tidyverse, sf, ggplot2, scatterpie, scales) loaded.
   b.	Data Import: PFAS dataset from data pull (data_export.csv) imported.
   c.	Change characteristicName to figure-friendly abbreviations 
### 2.	 Harmonize state name, location, and state codes 
   a.	Load state codes: state_codes.txt 
   b.	Load in shape file (.shp) of United States for later plotting 
### 3.	Filter Data for PFAS Compounds: Pull only samples we can identify as surface water and tissue for the pre-selected PFAS
   a.	Filter by Media and MediaSubdivisionName: Retain only records from surface water samples and tissue samples.
   b.	Filter by Compound: Ensure samples are from a predefined list of PFAS compounds (perfluorooctanoic acid and perfluorooctanesulfonate).
   c.	Harmonize Names: Abbreviate specific PFAS compound names for consistency.
### 4.	 Preparing Initial Summary Maps with Unfiltered Data 
   a.	Filter by States with data: Create new dataset containing only states with available surface water and/or tissue data 
   b.	Create scatterpie map with circle radius representing number of samples present per state
   c.	Including only unfiltered surface water and tissue data 
### 5.	Quality Control (QC) Tagging: Add tags for quality control parameters of specific concern to our analysis such as units and methods used. The following steps includes processes designed by EPATADA’s development team to automate quality control and cleaning processes for data downloaded from the Water Quality Portal.
   a.	Result Unit Validity: Adds a tag (TADA.ResultUnit.Flag) to validate measurement units.
   b.	Sample Fraction Validity: Tags sample fraction (e.g., dissolved, total) issues (TADA.SampleFraction.Flag).
   c.	Method Speciation Validity: Tags issues with method speciation (TADA.MethodSpeciation.Flag).
   d.	Harmonization: Harmonizes synonyms across records for characteristic/fraction/speciation/unit, adding tags like TADA.Harmonized.Flag.
### 6.	Tagging for Outliers and Thresholds: Add tags for evaluation of potentially erroneous data. 
   a.	Unrealistic Values: Tags values above upper or below lower thresholds.
   b.	Continuous Data: Identifies continuous data (commented out in this code).
### 7.	Method Check: Identify which method was applied to measure PFAS in each sample
   a.	Analytical Methods: Tags data based on the validity of analytical methods used (TADA.AnalyticalMethod.Flag).
### 8.	Duplicates Check: Identify any duplicate samples in dataset.
   a.	Tags potential duplicates across multiple organizations (TADA.MultipleOrgDupGroupID).
   b.	Tags single-organization duplicates (TADA.SingleOrgDup.Flag).
### 9.	Quality Control Activity Identification: Check for any quality control flags that were uploaded by the original data source. 
   a.	QC Samples: Tags data from QC-related activities (TADA.ActivityType.Flag).
### 10.	Coordinate Validation: Confirm that coordinates of sampling location are accurate/realistic. 
   a.	Invalid Coordinates: Tags records with problematic coordinates, e.g., locations outside the United States (TADA.InvalidCoordinates.Flag).
### 11.	Other QC and Non-Detect Processing: Identify which samples were uploaded as non-detects or suspect by the original data source. 
   a.	Suspect Samples: Tags records with suspect qualifier codes (TADA.MeasureQualifierCode.Flag).
### 12.	Non-Detect Values: Adds columns for censored data tags (TADA.CensoredData.Flag) and replaces non-detects with calculated values (e.g., 50% of the detection limit).
### 13.	Column and Data Cleanup: Remove NA columns to tidy up the result .CSV file and dataset. 
   a.	Remove All-NA Columns: Drops columns that contain only NA values.
### 14.	Filter Negative Values: Removes records with negative result values.
### 15.	Detection Limit QC: Manual QC check to review units used for detection limits 
### 16.	Harmonizing detection limit units. Also evaluating if any samples were reported at a concentration that is lower than their corresponding detection limit. 
   a.	Detection Limit Tags:
      i.	Tags samples based on detection limits and units.
      ii.	Converts and harmonizes detection limit units (e.g., converting µg/L to ng/L).
   b.	Detection Limit Statistics:
      i.	Calculates statistical summaries for detection limits (average, median, standard deviation).
      ii. Tags outliers based on calculated thresholds.
### 17.	Detection Limit User Error: Tag samples that are entered as ‘uncensored’ but are lower than the reported detection limit.
   a.	Labeling: 
      i.	Uncensored: Reported concentration is higher than reported detection limit 
      ii. Non-detect: Concentration is reported as non-detect or below detection limit
      iii. Unknown: Reported concentration is lower than reported detection limit 
### 18.	EPA Method Validation: Add a tag if the sample can be identified as using an EPA analytical method for PFAS in water. 
   a.	EPA Method Tags: Tags records based on accepted EPA analytical methods.
### 19.	Export Processed Data: Export this processed, cleaned, and tagged data into shareable file. 
   a.	With Tags: Exports the dataset with all tags to a CSV (EPATADA_Original_data_with_flags_tags.csv).
   b.	Filtered Data: Applies a series of filters to exclude invalid data, retaining only acceptable samples. Exports this filtered dataset (EPATADA_priority_filtered_data.csv).
### 20.	Preparing Initial Summary Maps with Filtered Data 
   a.	Filter by States with data: Create new dataset containing only states with available surface water and/or tissue data 
   b.	Create scatterpie map with circle radius representing number of samples present per state

  
