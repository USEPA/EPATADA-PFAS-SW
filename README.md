# Welcome to TADA-PFAS-SW: Tools for Automated Data Analysis of PFAS in Surface Water!

This is a quality assurance procedure for cleaning up Per- and polyfluoroalkyl substances (PFAS) surface water concentration data and can be adapted for other variables.

## Purpose

The purpose of this document is to provide an order and general description of the steps taken to access, download, clean, and tag PFAS surface water data supplied by the US EPA’s Water Quality Portal. This document demonstrates the use of the R package “EPATADA” to assist with coding processes used to generate the results of a nationwide study of ambient surface water PFAS concentrations in the US. The code, data, and supporting documentation are all available free of charge. The document is broken down into sections that outline how each attached .R file can be used sequentially to evaluate surface water data in a reproducible manner. 

## Authors and Affiliations

Authors: Hannah Ferriby1, Kateri Salk1, Matt Dunn1, Christopher Wharton1, Susan Cormier2, Tammy Newcomer-Johnson2

Affiliations: 1. Tetra Tech Inc. 2. United States Environmental Protection Agency 

Corresponding Author: Tammy Newcomer-Johnson – newcomber-johnson.tammy@epa.gov 

Preferred citation: H. Ferriby, K. Salk, M. Dunn, C. Wharton, S. Cormier, and T. Newcomer-Johnson. 2025. TADA-PFAS-SW. https://github.com/TammyNewcomerJohnson/EPATADA-PFAS-SW

## Included .R Files

data_pull.R
EPA_data_processing_SW_ONLY.R

## How to Use TADA-PFAS-SW

Function Reference Glossary: https://usepa.github.io/EPATADA/reference/index.html

### Outline of data_pull.R: 

1.	Setup and Package Installation
  a.	Check and Install Dependencies:
    i.	Checks for the remotes package and installs it if not already present.
    ii.	Installs the EPATADA package from GitHub.
  b.	Load Required Libraries:
    i.	Required R libraries (EPATADA, dplyr, readr) loaded.
2.	Data Retrieval
  a.	Using TADA_BigDataRetrieval:
    i.	Pulls data for PFAS-related compounds from a specified source using predefined characteristics:
      1.	characteristicType: Includes broad PFAS categories (e.g., "PFAS, Perfluorinated Alkyl Substance").
      2.	characteristicName: Targets specific PFAS compounds (e.g., “Perfluorooctanoic acid”, “Perfluorooctanesulfonate”).
3.	Data Cleaning with TADA_AutoClean
  a.	Automatic Cleaning and Transformation:
    i.	Runs the TADA_AutoClean function to clean and standardize the dataset.
    ii.	Subpoints: The cleaning process includes:
      1.	Column Capitalization for WQX Interoperability: Adds "TADA."-prefixed columns with uppercase values for select attributes.
      2.	Special Character Conversion: Converts special characters in measurement values and creates new "TADA." columns.
      3.	Latitude and Longitude Conversion: Converts these fields to numeric types and adds "TADA."-prefixed columns.
      4.	Standardize Unit Labels: Replaces "meters" with "m" in depth-related columns.
      5.	Replace Deprecated Characteristic Names: Updates deprecated names using the WQX domain table.
      6.	Result and Detection Limit Unit Harmonization: Converts result and detection limit units to WQX-compliant or user-defined targets.
      7.	Depth Unit Conversion: Converts depth measures to meters and adds new "TADA." columns.
      8.	Create Comparable Data Group IDs: Generates a concatenated ID for grouped data comparison.
    iii.	Ensures the dataset is standardized and ready for downstream analysis.
4.	Export Cleaned Data
  a.	Write Cleaned Data to CSV: The cleaned dataset is exported as a CSV file (data_export.csv) for further processing or analysis.

### Outline of EPA_data_processing_SW_ONLY.R:
1.	Setup and Initial Data Import
  a.	Load Packages: Required R libraries (EPATADA, dplyr, readr, purrr) loaded.
  b.	Data Import: PFAS dataset from data pull (data_export.csv) imported.
2.	Filter Data for PFAS Compounds
  a.	Filter by Media: Retain only records from surface water samples.
  b.	Filter by Compound: Ensure samples are from a predefined list of PFAS compounds.
  c.	Harmonize Names: Abbreviate specific PFAS compound names for consistency.
3.	Quality Control (QC) Tagging
  a.	Result Unit Validity: Adds a tag (TADA.ResultUnit.Flag) to validate measurement units.
  b.	Sample Fraction Validity: Tags sample fraction issues (TADA.SampleFraction.Flag).
  c.	Method Speciation Validity: Tags issues with method speciation (TADA.MethodSpeciation.Flag).
  d.	Harmonization: Harmonizes synonyms across records, adding tags like TADA.Harmonized.Flag.
4.	Tagging for Outliers and Thresholds
  a.	Unrealistic Values: Tags values above upper or below lower thresholds.
  b.	Continuous Data: Identifies continuous data (commented out in this code).
5.	Method and Duplicate Checks
  a.	Analytical Methods: Tags data based on the validity of analytical methods used (TADA.AnalyticalMethod.Flag).
  b.	Duplicates:
    i.	Tags potential duplicates across multiple organizations (TADA.MultipleOrgDupGroupID).
    ii.	Tags single-organization duplicates (TADA.SingleOrgDup.Flag).
6.	Quality Control Activity Identification
  a.	QC Samples: Tags data from QC-related activities (TADA.ActivityType.Flag).
7.	Coordinate Validation
  a.	Invalid Coordinates: Tags records with problematic coordinates (TADA.InvalidCoordinates.Flag).
8.	Other QC and Non-Detect Processing
  a.	Suspect Samples: Tags records with suspect qualifier codes (TADA.MeasureQualifierCode.Flag).
  b.	Non-Detect Values: Adds columns for censored data tags (TADA.CensoredData.Flag) and replaces non-detects with calculated values (e.g., 50% of the detection limit).
9.	Column and Data Cleanup
  a.	Remove All-NA Columns: Drops columns that contain only NA values.
  b.	Filter Negative Values: Removes records with negative result values.
10.	Detection Limit QC
  a.	Detection Limit Tags:
    i.	Tags samples based on detection limits and units.
    ii.	Converts and harmonizes detection limit units (e.g., converting µg/L to ng/L).
  b.	Detection Limit Statistics:
    i.	Calculates statistical summaries for detection limits (average, median, standard deviation).
    ii.	Tags outliers based on calculated thresholds.
  c.	Detection Limit User Error
    i.	Tag samples that are entered as ‘uncensored’ but are lower than the reported detection limit.
11.	EPA Method Validation
  a.	EPA Method Tags: Tags records based on accepted EPA analytical methods.
12.	Export Processed Data
  a.	With Tags: Exports the dataset with all tags to a CSV (EPATADA_Original_data_with_flags_SW_ONLY.csv).
  b.	Filtered Data: Applies a series of filters to exclude invalid data, retaining only acceptable samples. Exports this filtered dataset (EPATADA_filtered_data_SW_ONLY.csv).
