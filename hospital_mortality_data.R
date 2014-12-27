##### utility function for loading data from Outcome of Care Measures dataset

## Load Outcome of Care Measures dataset and return dataframe with the following mortality columns
##      Hospital.Name
##      State
##      Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
##      Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
##      Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
get_hospital_mortality_dataframe <- function() {

    ## Read outcome data
    hospital_outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## reduce to hospital state, name, and mortality metric we want
    col_hospital_name <- "Hospital.Name"
    col_hospital_state <- "State"
    col_heart_attack <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    col_heart_failure <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    col_pneumonia <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    
    my_columns <- c(col_hospital_name, col_hospital_state, col_heart_attack, col_heart_failure, col_pneumonia)
    mortality <- subset(hospital_outcome, select = my_columns)
    
    ## convert metric columns to numeric
    suppressWarnings(mortality[,3:5] <- sapply(mortality[,3:5], as.numeric))
    
    mortality
}