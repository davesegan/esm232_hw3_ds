# Function to calculate the yield anomaly for almonds
# ESM 232
# Function developed by Kat Leigh, David Segan, Alex Milward and Mauricio Collado
######################################


# 1. We upload the packages
library(tidyverse)
library(here)

# 2. Dataframe format
#
#' Dataframe requires the following columns: date, day, month, year, 
#' daily precipitations,and daily minimum temperature
#' The columns must be named exactly: day, month, year, tmin_c, and precip 
#' The variable units are Celsius (tmin_c) and mm (precip)
#
# Read in data
climate_df <- read.delim(here('clim.txt'), sep = " ")

# 3. Model for yield anomaly for almonds
#
# Function uses a polynomic model with 2 climate variables
# to estimate the yield anomaly for almonds (tons acre)
#' @param climate_df name of the dataset with the climate variables over time
#' @param var1_mon month for minimum temperature Default is 2
#' @param var2_mon month for precipitation Default is 1
#' @param coef_t1 coefficient for min temperatue Default is -0.015
#' @param coef_t2 coefficient for squared min temperatue Default is -0.0046
#' @param coef_p1 coefficient for precipitation Default is -0.07
#' @param coef_p2 coefficient for squared precipitation Default is 0.0043
#' @param intercept Intercept of the relationship Default 0.28
#' @references
#' Lobell (2006).

almond_anomaly = function(climate_df, #dataframe
                          var1_mon=2, #month for variable 1
                          var2_mon=1, #month for variable 1
                          coef_t1=-0.015, #coeficient for tmin
                          coef_t2=-0.0046, #coeficient for squared tmin
                          coef_p1=-0.07, #coeficient for precip
                          coef_p2=0.0043, #coeficient for squared precip
                          intercept=0.28){ #contant
  
  # We filter the dataframe for monthly average precipitation and minimun temp
  
  filt_clim_df <- climate_df %>%
    group_by(year, month) %>% #we group by month and year
    summarize(mean_tmin = mean(tmin_c), #mean t_min within each month
              sum_p = sum(precip)) %>%  #sum of precip within each month
    filter(month %in% c(var1_mon,var2_mon)) # we keep the relevant months
  
  # first and last year
  firstyear=min(filt_clim_df$year) # first year of filtered dataset
  lastyear=max(filt_clim_df$year) # last year of filtered dataset
  
  # We filter the dataframe for the average minimum temperatures for February of each year
  clim_var1_df <- filt_clim_df %>% 
    filter(month == var1_mon) %>% #filter month for tmin
    select(mean_tmin)
  
  # We filter the dataframe for total precipitation for January of each year
  clim_var2_df <- filt_clim_df %>% 
    filter(month == var2_mon) %>% #filter month for precip
    select(sum_p)
  
  # We process the information and save it into a dataframe
  yield_anom <- data.frame(year= seq(firstyear, lastyear, by = 1), # year
                           yield=coef_t1*clim_var1_df$mean_tmin # estimation
                           + coef_t2*(clim_var1_df$mean_tmin^2) 
                           + coef_p1*clim_var2_df$sum_p 
                           + coef_p2*(clim_var2_df$sum_p^2) 
                           + intercept)
  
  return(yield_anom) # result as dataset
  
}

######################################
# The end

test  <- almond_anomaly(climate_df)

