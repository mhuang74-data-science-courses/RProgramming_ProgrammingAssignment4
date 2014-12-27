source("hospital_mortality_data.R")

### selects the num'th ranked best hospital across all states for given outcome
###  num can also be "best" or "worst"
rankall <- function(outcome, num = "best") {
    
    ## sanity check input params
    if (!is.character(outcome)) {
        stop("invalid outcome")
    }
    if (!is.character(num) && !is.numeric(num)) {
        stop("invalid num")
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
    
    # Create column name from outcome, ignoring case
    col_outcome = make.names(tolower(outcome))
    
    # filter NA and ascend sort based on desired outcome
    mortality_outcome <- mortality[!is.na(mortality[col_outcome]),]
    
    if(nrow(mortality_outcome) < 1) {
        stop("no data available for outcome")
    }  
    
    # sort by State, Outcome, then Hospital Name
    mortality_outcome <- mortality_outcome[order(mortality_outcome["State"], mortality_outcome[col_outcome], mortality_outcome["Hospital.Name"]),]

    # split results by States
    mortality_outcome_split_state <- split(mortality_outcome, mortality_outcome$State)
    
    ## Function to return num-th ranked hospital name; or best or worst hospital
    get_nth_hospital <- function(ranked_hospitals, num) {
        # by default, return NA
        selected_hospital <- NA
        
        if (!is.na(suppressWarnings(as.numeric(num)))) {
            index <- suppressWarnings(as.numeric(num))
            if (index <= nrow(ranked_hospitals)) {
                selected_hospital <- ranked_hospitals$Hospital.Name[index]
            }
        } else if (num == num_best) {
            selected_hospital <- ranked_hospitals$Hospital.Name[1]
        } else if (num == num_worst) {
            selected_hospital <- ranked_hospitals$Hospital.Name[nrow(ranked_hospitals)]
        }
        
        selected_hospital
    }
    
    # loop over each State to get nth hospital we want
    state_hospitals <- lapply(mortality_outcome_split_state, get_nth_hospital, num)
    # convert back to dataframe
    state_hospitals_df <- ldply(state_hospitals, function(x) c(x, names(x)))
    names(state_hospitals_df) <- c("state", "hospital")
    
    # make sure order is: hospital, state
    state_hospitals_df <- state_hospitals_df[,c(2,1)]

    return(state_hospitals_df)
}