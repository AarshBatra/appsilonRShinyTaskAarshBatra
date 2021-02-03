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

# finding the ship_type/ship_name group with most variation in lat and long
# to use as a work case for testing code.

cleaned_data %>%
  dplyr::group_by(ship_type, SHIPNAME) %>%
  dplyr::summarise(var_in_lat = sd(LAT), var_in_lon = sd(LON)) %>%
  dplyr::filter(var_in_lat == max(var_in_lat), var_in_lon == max(var_in_lon))

# it turns out that the duo with ship_type = "Tanker" and ship_name = "MARINUS"
# has the most variation in Latitude and Longitude, so for the filtering
# function I will set these values as the default values.


filter_cleaned_data <- function(cleaned_data, ship_type = "Tanker",
                                ship_name = "MARINUS"){
  filtered_cleaned_data <- cleaned_data %>%
    dplyr::filter(ship_type == ship_type,
                  SHIPNAME == ship_name)
  filtered_cleaned_data
}

filtered_cleaned_data <- filter_cleaned_data(cleaned_data)

# main for loop that calculates max distance travelled for each ship.

# setting empty data structures
num_col_needed_to_calc_dist <- 2 # LON and LAT
max_tibble <- as.data.frame(matrix(NA, nrow = num_col_needed_to_calc_dist,
                     ncol = ncol(filtered_cleaned_data)))
colnames(max_tibble) <- colnames(filtered_cleaned_data)

max_dist <- -1 # This is the default placeholder which will be updated to
               # 0 (or more, given distance calculations) once the main
               # for loop starts calculating distances. This as of now
               # just serves as a dummy. Don't get confused by the negative
               # sign as this is just a dummy placeholder variable. The unit
               # in which this variable is measured is meters.

for(i in 1 : (nrow(filtered_cleaned_data) - 1)){
 current_tibble <- filtered_cleaned_data[(i:(i+1)), ]
 current_tibble_dist_cols <- current_tibble[, (1 : num_col_needed_to_calc_dist)]
 curr_dist <- geodist::geodist(current_tibble_dist_cols,
                               sequential = TRUE,
                             measure = "geodesic")
 print(current_tibble)
 print(current_tibble_dist_cols)
 print(dim(current_tibble_dist_cols))
 print(curr_dist)
 print(i)
 if(abs(curr_dist) >= max_dist){
   max_dist <- curr_dist
   max_tibble <- current_tibble
 } else {
   next
 }
}

#
#
# foo <- geodist(cleaned_ships_data[1, 2:1], cleaned_ships_data[2, 2:1],
#         measure = "geodesic")
# foo/1000
#
# geodist(foo, sequential = TRUE, measure = "haversine")/1000














