rankhospital <-function(state, outcome, num = "best") {
        
        #read OOCM.csv
        outcome_data <- read.csv("outcome-of-care-measures.csv")
        outcome_options <- c("heart attack", "heart failure", "pneumonia")
        colnames(outcome_data)[c(11,17,23)] <- outcome_options
        
        ## Check that state and outcome are valid: exact messages "invalid state" and "invalid outcome"
        if(!state %in% state.abb) stop("invalid state")
        if(!outcome %in% outcome_options) stop("invalid outcome")
        
        #remove hospitals w/ NA on the given outcome
        state_subset <- outcome_data[outcome_data$State == state,]
        sorted_outcome <-  state_subset[order(state_subset[outcome],state_subset$Hospital.Name, na.last = NA),]
        
        
        #define num parameters: best = 1, worst = nrows, interger = that row number, too-large number = NA
                if (num == "best") {index <- 1
                } else if (num == "worst") {index <- nrow(sorted_outcome)
                } else index <- num
        
        as.character(sorted_outcome$Hospital.Name[index])
        #return char vector w/ name of hospital per ranking "num", tie breaking by alphabetica
}
        