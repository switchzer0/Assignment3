best <- function(state, outcome) {
        ## Read outcome data, identify columns
        outcome_data <- read.csv("outcome-of-care-measures.csv")
        outcome_options <- c("heart attack", "heart failure", "pneumonia")
        colnames(outcome_data)[c(11,17,23)] <- outcome_options
        
        
        #check that state and outcome are valid
        if(!state %in% state.abb) stop("invalid state")
        if(!outcome %in% outcome_options) stop("invalid outcome")


        ## subset data to state indicated in function and then select min hospital for that outcome
        state_subset <- outcome_data[outcome_data$State == state,]
        outcome_subset <- state_subset[state_subset$outcome == !is.na]
        sorted_outcome <-  state_subset[order(state_subset$Hospital.Name),]
        ##sorted_outcome[sorted_outcome$]
        best_row <- which.min(sorted_outcome[[outcome]])
        as.character(sorted_outcome$Hospital.Name[best_row])
}