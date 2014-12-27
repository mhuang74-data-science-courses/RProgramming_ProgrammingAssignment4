source("rankhospital.R")

### selects the best hospital in given state for given outcome
best <- function(state, outcome) {
    rankhospital(state, outcome, "best")
}