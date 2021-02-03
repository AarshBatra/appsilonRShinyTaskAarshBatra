## R Shiny Data Task===========================================================

# metadata---------------------------------------------------------------------
# author: Aarsh Batra
# date: February 2, 2021
# R version: 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 18363)
# R Studio version: 1.4.1103

# libraries--------------------------------------------------------------------
library(tidyverse)
library(knitr)
library(devtools)
library(stringr)
library(tidyr)
library(dplyr)
library(magrittr)
library(data.table)
library(lubridate)
library(roxygen2)
library(testthat)
library(shiny)
library(ggplot2)
library(leaflet)
library(shiny.semantic)
library(geodist) # for calculating distances given Lat and Long

# read in data-----------------------------------------------------------------

#' reading raw data into R
#'
#' This functions reads in raw data into R (after performing a few
#' sanity checks) and throws an error if for any reason data cannot be read.
#'
#'
#' @importFrom readr read_csv
#' @importFrom base file.exists stop
#' @param path_to_file path to the file where the data is stored
#'
#' @return This function returns an R object which contains the raw dataset.
#'
#' @examples
#' read_raw_data(path_to_file = "path/to/data.csv")
#'
#' @export

read_raw_data <- function(path_to_file){

  if(!is.character(path_to_file)){
    stop("path_to_file should be of type character")
  }

  if(!file.exists(path_to_file)){
    stop("file not present in the specified location. Please make sure
         that you point the path to the raw data csv file.")
  }
  raw_data <- readr::read_csv(file = path_to_file)
  raw_data
}


# cleaning and rearranging raw dataset for analysis----------------------------

#' cleaning and rearranging Raw Data for analysis
#'
#' This function takes in the raw dataset and does some basic cleaning (if any)
#' rearranging (if any).
#'
#' @importFrom dplyr arrange select
#' @param raw_data raw dataset that is returned by the \code{read_raw_data}
#'        function
#'
#' @return this function returns the cleaned dataset which is ready
#'         for analysis.
#'
#' @examples
#' clean_raw_data(raw_data = raw_dataset)
#'
#' @export


clean_raw_data <- function(raw_data){

  # rearrange dates into chronological order
  cleaned_data <- dplyr::arrange(raw_data, DATETIME)

  # rearrange columns such that LON column is the first column
  cleaned_data <- dplyr::select(cleaned_data, LON, everything())

  cleaned_data
}






# exploring: raw dataset-------------------------------------------------------
raw_data <- read_raw_data(path_to_file = "data-raw/ships.csv")
dim(raw_data)
length(unique(raw_data$SHIP_ID))
base::summary(raw_data)


# exploring: cleaned dataset---------------------------------------------------
cleaned_data <- clean_raw_data(raw_data)

View(cleaned_data)

filter_cleaned_data <- function(cleaned_data, ship_type = "cargo",
                                ship_name = "MERI"){
  filtered_cleaned_data <- cleaned_data %>%
    dplyr::filter(ship_type == ship_type,
                  SHIPNAME == ship_name)
  filtered_cleaned_data
}

filtered_cleaned_data <- filter_cleaned_data(cleaned_data)
#
# foo <- geodist(cleaned_ships_data[1, 2:1], cleaned_ships_data[2, 2:1],
#         measure = "geodesic")
# foo/1000
#
# geodist(foo, sequential = TRUE, measure = "haversine")/1000
















