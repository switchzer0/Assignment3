best <- function(state, outcome) {
        ## Read outcome data, identify columns
        outcome_data <- read.csv("outcome-of-care-measures.csv",  colClasses = "character")
        outcome_options <- c("heart attack", "heart failure", "pneumonia")
        colnames(outcome_data)[c(11,17,23)] <- outcome_options
        
        #check that state and outcome are valid
        if(!state %in% state.abb) stop("invalid state")
        if(!outcome %in% outcome_options) stop("invalid outcome")

        ## subset data to state indicated in function and then order data outcome min first, then by alpha
        state_subset <- outcome_data[outcome_data$State == state,]
        sorted_outcome <-  state_subset[order(as.numeric(state_subset[[outcome]]),state_subset$Hospital.Name, na.last = NA),]
        
        ##print as char
        as.character(sorted_outcome$Hospital.Name[1])
}