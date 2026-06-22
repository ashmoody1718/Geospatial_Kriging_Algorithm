# Geospatial_Kriging_Algorithm

## Table of contents
* [General info](#general-info)
* [Technologies](#technologies)
* [Dependencies](#dependencies)
* [Setup](#setup)

## General info
This is a kriging algorithm I created during my CEE7980 Geospatial Statistics course in 2024.
	
## Technologies
Project is created with:
* R >= 4.6.0
* Excel 2024

## Dependencies
* readxl 1.4.5
* ggplot2 4.0.3
* dplyr 1.2.1
* tidyverse 2.0.0
* matlib 1.0.1

## File Descriptions
### CEE7980_HW6_Moody.r
Main kriging script. Includes package installation, data setup, matrix creation, and graphing.

### variogram_mf.r
Script performs calculations needed to create a variogram using the provided data.

### N5Samples.xlsx
xlxs file containing sample geostatistics data to create variogram.

## Setup
To run this project, install CEE7980_HW6_Moody.r, variogram_mf.r, and N5Samples.xlsx within the same working directory.
