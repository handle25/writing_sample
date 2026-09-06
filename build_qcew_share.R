### This script allows users to create custom crosswalks for their needs.  Modifying the baseline crosswalks created  ###
### in CW_Builder_Forward and CW_Builder_Backward, users can bridge to whatever code system they need, and aggregate  ###
### up to whatever coarser bridge they desire.                                                                        ###
### Note that some custom crosswalks won't make a lot of sense.  Forcing NAICS 1-digit codes to split to the full set ###
### of SIC 4-digit codes is less natural than, say, NAICS 5 to SIC 3, or NAICS 3 to SIC 2.  Use good judgement.       ###

# Necessary packages
library(tidyverse)
library(data.table)
library(haven)

# The next three sections are not for user interaction, they just set up the tool.  Run all lines of code from the
# top and then create your crosswalks using the section "User Interface"

####===================================== Crosswalking Forward in Time/Versions =====================================####
setwd(paste0(path, "/145101-V3"))
## Load the crosswalks
CW_8797 <- read.csv("CW_SIC87_97.csv", colClasses = c(SIC87="character", NAICS97="character"))
CW_9702 <- read.csv("CW_9702.csv", colClasses = c(NAICS97="character", NAICS02="character"))
CW_0207 <- read.csv("CW_0207.csv", colClasses = c(NAICS02="character", NAICS07="character"))
CW_0712 <- read.csv("CW_0712.csv", colClasses = c(NAICS07="character", NAICS12="character"))
CW_1217 <- read.csv("CW_1217.csv", colClasses = c(NAICS12="character", NAICS17="character"))
CW_1722 <- read.csv("CW_1722.csv", colClasses = c(NAICS17="character", NAICS22="character"))


## Define forward bridge function
Build_a_Bridge_Forward <- function(start, end) {
  
  cwv <- c(1987,1997,2002,2007,2012,2017,2022) # The sequence of crosswalk version years
  cws <- c("87","97","02","07","12","17","22") # The sequence of crosswalk version shorthands
  
  
  # Throw an error if user enters a start year that isn't a version year
  if (!start %in% cwv) {
    stop("Invalid Start Date Entered, Use Version Years")
  }
  
  # Throw an error if user enters an end year that isn't a version year
  if (!end %in% cwv) {
    stop("Invalid End Date Entered, Use Version Years")
  }
  
  # Throw an error if user enters a combination that goes backward or does not need an extended crosswalk.
  # for example, 2022 to 2017, or 2002 to 2002, or 2012 to 2017
  start_idx <- match(start, cwv)
  end_idx <- match(end, cwv)
  if (end_idx - start_idx <= 1) {
    stop("Custom Forward Crosswalk Not Needed, Use Those Provided")
  }
  
  # Initialize loop variables and set the counter backstop
  counter <- match(start, cwv)
  quit <- match(end, cwv)-1
  cw <- get(paste0("CW_", cws[counter], cws[counter+1]))
  start_titles <- cw[ ,c(1,2)] %>% distinct()
  
  while (counter<quit) {
    new <- get(paste0("CW_", cws[counter+1], cws[counter+2])) # Load the next crosswalk link
    
    # Join in the next link and rescale weighting variables by new split shares
    cw <- cw[ ,-c(4)] %>% # Recycle the previous output and remove anachronistic target titles
      left_join(new[ ,-c(2, 5:7)], by=names(new)[1], relationship = "many-to-many") %>%
      mutate(Employment = Employment*emp_weight.y,
             Establishments = Establishments*est_weight.y,
             Payroll = Payroll*pay_weight.y)
    
    # Collapse by unique origin-target pairs, e.g., NAICS97 NAICS17 (origin is column 1, target is column 10)
    cw <- cw %>%
      group_by(across(all_of(names(cw)[c(1,10)]))) %>%
      summarise(across(c(Employment, Establishments, Payroll), ~ sum(.x, na.rm = FALSE)), .groups = "drop") %>%
      
      # Recalculate child code shares based off rescaled weighting data
      group_by(!!sym(names(cw)[1])) %>%
      mutate(emp_weight = round(Employment/sum(Employment), 2),
             est_weight = round(Establishments/sum(Establishments), 2),
             pay_weight = round(Payroll/sum(Payroll), 2)) %>%
      ungroup() %>%
      
      # Add back code titles
      left_join(start_titles, by=names(cw)[1]) %>%
      left_join(distinct(new, !!sym(names(new)[3]), !!sym(names(new)[4])), by=names(new)[3])
    
    # Rearrange for convenience
    cw <- cw[ ,c(1,9,2,10,3:8)]
    
    # Increase loop counter by 1
    counter <- counter+1
  }
  return(cw)
}




####===================================== Crosswalking Backward in Time/Versions =====================================####

# Load the crosswalks
CW_2217 <- read.csv("CW_2217.csv", colClasses = c(NAICS22="character", NAICS17="character"))
CW_1712 <- read.csv("CW_1712.csv", colClasses = c(NAICS17="character", NAICS12="character"))
CW_1207 <- read.csv("CW_1207.csv", colClasses = c(NAICS12="character", NAICS07="character"))
CW_0702 <- read.csv("CW_0702.csv", colClasses = c(NAICS07="character", NAICS02="character"))
CW_0297 <- read.csv("CW_0297.csv", colClasses = c(NAICS02="character", NAICS97="character"))
CW_9787 <- read.csv("CW_97_SIC87.csv", colClasses = c(NAICS97="character", SIC87="character"))

# Workaround for zeros in CW_2217 code 517122
CW_2217$Employment[CW_2217$NAICS22=="517122" & CW_2217$NAICS17=="517312"] <- 0.7
CW_2217$Employment[CW_2217$NAICS22=="517122" & CW_2217$NAICS17=="517911"] <- 0.3
CW_2217$Establishments[CW_2217$NAICS22=="517122" & CW_2217$NAICS17=="517312"] <- 0.61
CW_2217$Establishments[CW_2217$NAICS22=="517122" & CW_2217$NAICS17=="517911"] <- 0.39
CW_2217$Payroll[CW_2217$NAICS22=="517122" & CW_2217$NAICS17=="517312"] <- 0.7
CW_2217$Payroll[CW_2217$NAICS22=="517122" & CW_2217$NAICS17=="517911"] <- 0.3



## Define backward bridge function
Build_a_Bridge_Backward <- function(start, end) {
  
  cwv <- c(2022,2017,2012,2007,2002,1997,1987) # The sequence of crosswalk version years
  cws <- c("22","17","12","07","02","97","87") # The sequence of crosswalk version shorthands
  
  
  # Throw an error if user enters a start year that isn't a version year
  if (!start %in% cwv) {
    stop("Invalid Start Date Entered, Use Version Years")
  }
  
  # Throw an error if user enters an end year that isn't a version year
  if (!end %in% cwv) {
    stop("Invalid End Date Entered, Use Version Years")
  }
  
  # Throw an error if user enters a combination that goes forward or does not need an extended crosswalk.
  # for example, 2017 to 2022, or 2002 to 2002, or 2012 to 2007
  start_idx <- match(start, cwv)
  end_idx <- match(end, cwv)
  if (end_idx - start_idx <= 1) {
    stop("Custom Backward Crosswalk Not Needed, Use Those Provided")
  }
  
  # Initialize loop variables and set the counter backstop
  counter <- match(start, cwv)
  quit <- match(end, cwv)-1
  cw <- get(paste0("CW_", cws[counter], cws[counter+1]))
  start_titles <- cw[ ,c(1,2)] %>% distinct()
  
  while (counter<quit) {
    new <- get(paste0("CW_", cws[counter+1], cws[counter+2])) # Load the next crosswalk link
    
    # Join in the next link and rescale weighting variables by new split shares
    cw <- cw[ ,-c(4)] %>% # Recycle the previous output and remove anachronistic target titles
      left_join(new[ ,-c(2, 5:7)], by=names(new)[1], relationship = "many-to-many") %>%
      mutate(Employment = Employment*emp_weight.y,
             Establishments = Establishments*est_weight.y,
             Payroll = Payroll*pay_weight.y)
    
    # Collapse by unique origin-target pairs, e.g., NAICS17 NAICS97 (origin is column 1, target is column 10)
    cw <- cw %>%
      group_by(across(all_of(names(cw)[c(1,10)]))) %>%
      summarise(across(c(Employment, Establishments, Payroll), ~ sum(.x, na.rm = FALSE)), .groups = "drop") %>%
      
      # Recalculate child code shares based off rescaled weighting data
      group_by(!!sym(names(cw)[1])) %>%
      mutate(emp_weight = round(Employment/sum(Employment), 2),
             est_weight = round(Establishments/sum(Establishments), 2),
             pay_weight = round(Payroll/sum(Payroll), 2)) %>%
      ungroup() %>%
      
      # Add back code titles
      left_join(start_titles, by=names(cw)[1]) %>%
      left_join(distinct(new, !!sym(names(new)[3]), !!sym(names(new)[4])), by=names(new)[3])
    
    # Rearrange for convenience
    cw <- cw[ ,c(1,9,2,10,3:8)]
    
    # Increase loop counter by 1
    counter <- counter+1
  }
  return(cw)
}




####================================ Crosswalking to Different Levels of Aggregation ================================####

## Define aggregating function
start_version <- 1987
end_version <- 2012
Bridge_Aggregator <- function(crosswalk, origin_digits, target_digits) {
  
  # Throw an error if user enters something other than 1-6
  if (!origin_digits %in% c(1:6)) {
    stop("Invalid Aggregation Level Entered, Use 1-4 for SIC or 1-6 for NAICS")
  }
  
  # Create custom trimming of codes
  cw <- crosswalk %>%
    mutate(new_origin = substr(.[[1]], 1, origin_digits),
           new_target = substr(.[[3]], 1, target_digits)) %>%
    
    # Aggregate using trimmed codes
    group_by(new_origin, new_target) %>%
    summarise(across(c(Employment, Establishments, Payroll), ~ sum(.x, na.rm = FALSE)), .groups = "drop") %>%
    
    # Recalculate child code shares based off aggregated weighting data
    group_by(new_origin) %>%
    mutate(emp_weight = round(Employment/sum(Employment), 5),
           est_weight = round(Establishments/sum(Establishments), 2),
           pay_weight = round(Payroll/sum(Payroll), 2)) %>%
    ungroup()
  
  # Add back column names
  names(cw)[c(1,2)] <- names(crosswalk)[c(1,3)]
  
  return(cw)
}




####============================================== User Interface ==============================================####

## Here is where you create your custom crosswalk.  First you will span the versions you need, then you will
## aggregate to whatever digit levels you need


# Start here if you need to crosswalk FORWARD across time/versions.  Replace start_version with the SIC or NAICS
# version you want to bridge from, and replace end_version with the NAICS version you want to bridge to

Your_CW <- Build_a_Bridge_Forward(start_version, end_version)



# Start here if you need to crosswalk BACKWARD across time/versions.  Replace start_version with the NAICS
# version you want to bridge from, and replace end_version with the SIC or NAICS version you want to bridge to

# Your_CW <- Build_a_Bridge_Backward(start_version, end_version)



# Now go here to aggregate up to coarser digits if you need to.  Where it says origin_digits, replace with
# the number of digits you want to bridge from.  Where it says target_digits, replace with the number of digits
# you want to bridge to.  For example, to create a crosswalk from 1997 NAICS 3-digit level to a 1987 SIC 2-digit level,
# the line should be Your_CW <- Bridge_Aggregator(CW_9787, 3, 2).  Leave the first argument as Your_CW if you used
# either of the steps above.
origin_digits <- 4
target_digits <- 6

Your_CW <- Bridge_Aggregator(Your_CW, origin_digits, target_digits)



# Finally, if you absolutely must have a one-to-one crosswalk, you can use the lines below to map each code
# to only its highest weight correspondence.  If you want your crosswalk to be weighted, skip this and go to export
Your_CW %>%
  count(SIC87) %>%
  count(n)



Your_CW <- Your_CW %>%
  group_by(!!sym(names(Your_CW)[1])) %>%
  mutate(max_flag = ifelse(emp_weight==max(emp_weight), 1, 0)) %>%
  ungroup() %>%
  filter(max_flag==1) %>% select(-max_flag)

Your_CW <- Your_CW %>%
  group_by(!!sym(names(Your_CW)[1])) %>%
  mutate(max_flag = ifelse(emp_weight == max(emp_weight), 1, 0)) %>%
  ungroup() %>%
  filter(max_flag == 1) %>%
  select(-max_flag)

write.csv(Your_CW, paste0(path, "/my_crosswalk.csv"), row.names = FALSE)

# Export your custom crosswalk as a csv or dta file
#write.csv(Your_CW, "MyCustomCrosswalk.csv", row.names = F) # Uncomment and run this line when ready to export the crosswalk
#write_dta(Your_CW, "MyCustomCrosswalk.dta") # Or use this line if you prefer a Stata file