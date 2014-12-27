source("hospital_mortality_data.R")

### selects the num'th ranked best hospital in given state for given outcome
###  num can also be "best" or "worst"
rankhospital <- function(state, outcome, num = "best") {
    
    ## sanity check input params
    if (!is.character(state)) {
        stop("invalid state")
    }
    if (!is.character(outcome)) {
        stop("invalid outcome")
    }
    if (!is.character(num) && !is.numeric(num)) {
        stop("invalid num")
    }   
    
    ## Only accept 2 char for State; trim spaces
    state <- str_trim(state)
    if (nchar(state)!=2) {
        stop("invalid state")        
    }
    
    # define valid outcome inputs
    outcome_heart_attack <- "heart attack"
    outcome_heart_failure <- "heart failure"
    outcome_pneumonia <- "pneumonia"
    
    ## Only accept the 3 outcomes above; ignore case and trim spaces
    outcome <- str_trim(outcome)
    if (!(tolower(outcome) %in% c(outcome_heart_attack, outcome_heart_failure, outcome_pneumonia))) {
        stop("invalid outcome")
    }
    
    ## acceptable text values for num
    num_best <- "best"
    num_worst <- "worst"
    
    ## Only accept "best", "worst", or numerical value for num
    num <- tolower(str_trim(num))
    if (suppressWarnings(!is.numeric(as.numeric(num))) && !(num %in% c(num_best, num_worst))) {
        stop("invalid num")
    }
    
    ############ start main processing ##########
    
    ## load mortality dataframe
    mortality <- get_hospital_mortality_dataframe()
    
    ## make it easier to locate appropriate metric column from outcome name
    names(mortality)[3:5] <- make.names(c(outcome_heart_attack, outcome_heart_failure, outcome_pneumonia))
    
    
    ## Filter by State, ignoring case
    mortality_state <- mortality[toupper(mortality$State)==toupper(state),]
    
    if(nrow(mortality_state) < 1) {
        stop("invalid state")
    }
    
    # Create column name from outcome, ignoring case
    col_outcome = make.names(tolower(outcome))
    
    # filter NA and ascend sort based on desired outcome
    mortality_state_outcome <- mortality_state[!is.na(mortality_state[col_outcome]),]
    mortality_state_outcome <- mortality_state_outcome[order(mortality_state_outcome[col_outcome], mortality_state_outcome["Hospital.Name"]),]
    
    if(nrow(mortality_state_outcome) < 1) {
        stop("no data available for outcome")
    }  
    
    ## Return num-th ranked hospital name in that state with lowest 30-day death rate
    
    # by default, return NA
    best_hospital <- NA
    
    if (!is.na(suppressWarnings(as.numeric(num)))) {
        index <- suppressWarnings(as.numeric(num))
        if (index <= nrow(mortality_state_outcome)) {
            best_hospital <- mortality_state_outcome$Hospital.Name[index]
        }
    } else if (num == num_best) {
        best_hospital <- mortality_state_outcome$Hospital.Name[1]
    } else if (num == num_worst) {
        best_hospital <- mortality_state_outcome$Hospital.Name[nrow(mortality_state_outcome)]
    }
    
    best_hospital
}