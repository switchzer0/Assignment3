rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcome_options <- c("heart attack", "heart failure", "pneumonia")
        colnames(outcome_data)[c(11,17,23)] <- outcome_options
        
        ## Check that state and outcome are valid: exact messages "invalid state" and "invalid outcome"
        if(!outcome %in% outcome_options) stop("invalid outcome")
        
        #order data and remove hospitals w/ NA on the given outcome
        outcome_subset <-  outcome_data[order(as.numeric(outcome_data[[outcome]]),outcome_data$Hospital.Name, na.last = NA),]
 
        #define index parameters
        if (num == "best") {index <- 1
                } else if (num == "worst") {index <- nrow(sorted_outcome)
                } else index <- num
        
        #loop through state list, pulling the "outcome" at "num" rank from each one and adding it to a data frame
        ##alternatively, order() by state and then by rank (ascending) and loop selecting 4, 4+50, etc.
        dat <- data.frame()
        #loop through files, running cor() on those those greater than threshold
        #does the assignment need state names in alpha order, or the abbrevs? will need to rank state.abb if the former
        for (i in 1:length(state.abb)) {
                #get just the next state subset in the list
                ranked <- outcome_subset[outcome_subset$State == state.abb[i],]
                #grab the hospital name per specified rank ("num)
                hosp_name <- as.character(ranked$Hospital.Name[index])
                
                #append the hospital name and state name to data frame
                dat <- rbind(dat, c(hosp_name, state.abb[i]))
                }
        dat
}


## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name