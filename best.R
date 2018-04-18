best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   # head(outcome_data)
    ## Check that state and outcome are valid
    
    states <- outcome_data[ , 7]
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if ((state %in% states) == FALSE) {
        stop(print("Invalid State"))
    }
    else if ((outcome %in% outcomes) == FALSE) {
        stop(print("Invalid Outcome"))
    }
    

    ## Return hospital name in that state with lowest 30-day death
        ## rate
    #get the subset of the data with the desired state
    new_data <- subset(outcome_data, State == state)
    
    #get the desired outcome column from the data file
    if (outcome == "heart attack") {
        outcome_column <- 11
    }
    else if (outcome == "heart failure") {
        outcome_column <- 17
    }
    else {
        outcome_column <- 23
    }
    
    #get rid of the NA's in the desired outcome column
    required_columns <- suppressWarnings(as.numeric(new_data[,outcome_column]))
    bad <- is.na(required_columns)
    desired_data <- new_data[!bad, ]
    
    #find the hospitals in the rows with the minimum outcome value
    columns_considered <- as.numeric(desired_data[, outcome_column])
    desired_rows <- which(columns_considered == min(columns_considered))
    desired_hospitals <- desired_data[desired_rows, 2]
    
    #if there are multiple hospitals with the minimum outcome value, then
    #return the first hospital name from the alphabetically sorted hospital
    #names list
    if (length(desired_hospitals) > 1) {
        hospitals_sorted <- sort(desired_hospitals)
        hospitals_sorted[1]
    }
    else {
        desired_hospitals
    }
    
}
