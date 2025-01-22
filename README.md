# Welcome to TADA-PFAS-SW: Tools for Automated Data Analysis of PFAS in Surface Water!

This is a quality assurance procedure for cleaning up Per- and polyfluoroalkyl substances (PFAS) surface water concentration data and can be adapted for other variables.

## Purpose

The purpose of this document is to provide an order and general description of the steps taken to access, download, clean, and tag PFAS surface water data supplied by the US EPA’s Water Quality Portal. This document demonstrates the use of the R package “EPATADA” to assist with coding processes used to generate the results of a nationwide study of ambient surface water PFAS concentrations in the US. The code, data, and supporting documentation are all available free of charge. The document is broken down into sections that outline how each attached .R file can be used sequentially to evaluate surface water data in a reproducible manner. 

## Authors and Affiliations

Authors: Hannah Ferriby1, Kateri Salk1, Matt Dunn1, Christopher Wharton1, Susan Cormier2, Tammy Newcomer-Johnson2

Affiliations: 1. Tetra Tech Inc. 2. United States Environmental Protection Agency 

Corresponding Author: Tammy Newcomer-Johnson – newcomber-johnson.tammy@epa.gov 

Preferred citation: H. Ferriby, K. Salk, M. Dunn, C. Wharton, S. Cormier, and T. Newcomer-Johnson. 2025. TADA-PFAS-SW. https://github.com/TammyNewcomerJohnson/EPATADA-PFAS-SW

# How to Use TADA-PFAS-SW

Function Reference Glossary: https://usepa.github.io/EPATADA/reference/index.html

## Included .R Files

data_pull.R
EPA_data_processing_SW_ONLY.R

## Outline of data_pull.R: 

### 1.	Setup and Package Installation
   
A. Check and Install Dependencies:
  	
  1. Checks for the remotes package and installs it if not already present.
    	
  2. Installs the EPATADA package from GitHub.
  	
B.	Load Required Libraries:
  
  1. Required R libraries (EPATADA, dplyr, readr) loaded.
    
### 2.	Data Retrieval
   
A.	Using TADA_BigDataRetrieval:

  1. Pulls data for PFAS-related compounds from a specified source using predefined characteristics:
    
      a.	characteristicType: Includes broad PFAS categories (e.g., "PFAS, Perfluorinated Alkyl Substance").
      
      b.	characteristicName: Targets specific PFAS compounds (e.g., “Perfluorooctanoic acid”, “Perfluorooctanesulfonate”).
      
### 3.	Data Cleaning with TADA_AutoClean
   
A.	Automatic Cleaning and Transformation:
  
  1. Runs the TADA_AutoClean function to clean and standardize the dataset.
    
  2. Subpoints: The cleaning process includes:
    
      a.	Column Capitalization for WQX Interoperability: Adds "TADA."-prefixed columns with uppercase values for select attributes.
      
      b.	Special Character Conversion: Converts special characters in measurement values and creates new "TADA." columns.
      
      c.	Latitude and Longitude Conversion: Converts these fields to numeric types and adds "TADA."-prefixed columns.
      
      d.	Standardize Unit Labels: Replaces "meters" with "m" in depth-related columns.
      
      e.	Replace Deprecated Characteristic Names: Updates deprecated names using the WQX domain table.
      
      f.	Result and Detection Limit Unit Harmonization: Converts result and detection limit units to WQX-compliant or user-defined targets.
      
      g.	Depth Unit Conversion: Converts depth measures to meters and adds new "TADA." columns.
      
      h.	Create Comparable Data Group IDs: Generates a concatenated ID for grouped data comparison.
      
  3.	Ensures the dataset is standardized and ready for downstream analysis.
    
### 4.	Export Cleaned Data

A.	Write Cleaned Data to CSV: The cleaned dataset is exported as a CSV file (data_export.csv) for further processing or analysis.

## Outline of EPA_data_processing_SW_ONLY.R:

### 1.	Setup and Initial Data Import
   
A.	Load Packages: Required R libraries (EPATADA, dplyr, readr, purrr) loaded.
  
B.	Data Import: The PFAS dataset from data pull (data_export.csv) was imported.
  
### 2.	Filter Data for PFAS Compounds
   
A.	Filter by Media: Retain only records from surface water samples.
  
B.	Filter by Compound: Ensure samples are from a predefined list of PFAS compounds.
  
c.	Harmonize Names: Abbreviate specific PFAS compound names for consistency.
  
### 3.	Quality Control (QC) Tagging
   
A.  Result Unit Validity: Adds a tag (TADA.ResultUnit.Flag) to validate measurement units.
  
B.  Sample Fraction Validity: Tags sample fraction issues (TADA.SampleFraction.Flag).
  
C.	Method Speciation Validity: Tags issues with method speciation (TADA.MethodSpeciation.Flag).
  
D.	Harmonization: Harmonizes synonyms across records, adding tags like TADA.Harmonized.Flag.
  
### 4.	Tagging for Outliers and Thresholds
   
A.	Unrealistic Values: Tags values above upper or below lower thresholds.
  
B.	Continuous Data: Identifies continuous data (commented out in this code).
  
### 5.	Method and Duplicate Checks

A.	Analytical Methods: Tags data based on the validity of analytical methods used (TADA.AnalyticalMethod.Flag).
  
B.	Duplicates:
  
  1.	Tags potential duplicates across multiple organizations (TADA.MultipleOrgDupGroupID).
    
  2.	Tags single-organization duplicates (TADA.SingleOrgDup.Flag).
    
### 6.	Quality Control Activity Identification
    
A.	QC Samples: Tags data from QC-related activities (TADA.ActivityType.Flag).
  
### 7.	Coordinate Validation
    
A.	Invalid Coordinates: Tags records with problematic coordinates (TADA.InvalidCoordinates.Flag).
  
### 8.	Other QC and Non-Detect Processing
    
A.	Suspect Samples: Tags records with suspect qualifier codes (TADA.MeasureQualifierCode.Flag).
  
B.	Non-Detect Values: Adds columns for censored data tags (TADA.CensoredData.Flag) and replaces non-detects with calculated values (e.g., 50% of the detection limit).
  
### 9.	Column and Data Cleanup
    
A.	Remove All-NA Columns: Drops columns that contain only NA values.
  
B.	Filter Negative Values: Removes records with negative result values.
  
### 10.	Detection Limit QC
    
A.	Detection Limit Tags:
  
  1.	Tags samples based on detection limits and units.
    
  2.	Converts and harmonizes detection limit units (e.g., converting µg/L to ng/L).
    
B.	Detection Limit Statistics:
  
  1.	Calculates statistical summaries for detection limits (average, median, standard deviation).
    
  2.	Tags outliers based on calculated thresholds.
    
C.	Detection Limit User Error
  
  1.	Tag samples that are entered as ‘uncensored’ but are lower than the reported detection limit.
    
### 11.	EPA Method Validation

A.	EPA Method Tags: Tags records based on accepted EPA analytical methods.
  
### 12.	Export Processed Data

A.	With Tags: Exports the dataset with all tags to a CSV (EPATADA_Original_data_with_flags_SW_ONLY.csv).
  
B.	Filtered Data: Applies a series of filters to exclude invalid data, retaining only acceptable samples. Exports this filtered dataset (EPATADA_filtered_data_SW_ONLY.csv).
