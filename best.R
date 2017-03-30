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

                ##get rid of NAs in outcome column
        
                ##order alphabetically so that first column is selected by "best.row" function
                sorted_outcome <-  state_subset[order( "Hospital.Name", na.last = NA),]
        
        ##select min value row and print as char vector
        best_row <- which.min(sorted_outcome[[outcome]]s)
        as.character(sorted_outcome$Hospital.Name[best_row])
}