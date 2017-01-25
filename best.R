best <- function(state, outcome) {
        #set outcome options and check that state and outcome are valid
        outcome_options <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
        if(!state %in% state.abb) stop("invalid state")
        if(!outcome %in% names(outcome_options)) stop("invalid outcome")


        ## Read outcome data subsetting selecting relevant columns
        outcome_data <- read.csv("outcome-of-care-measures.csv")[,c(2,7,outcome_options[outcome])]


        ## subset data to state indicated in function and then select min hospital for that outcome
        
        data_by_state <- outcome_data[outcome_data$State == state,]
        sortorder <- c(names(data_by_state[3]),names(data_by_state[1]))
        
        sorted <- data_by_state[order(names(data_by_state[3]),names(data_by_state[1])),]
                
                
                ##template: with(score, score[order(sex, y, x),])
                sortorder <- c(names(data_by_state[2]),names(data_by_state[3]),names(data_by_state[1]))
                with(data_by_state, data_by_state[order(sortorder),])
                
                
                best_row <- which.min(data_by_state[,3])
        data_by_state$Hospital.Name[best_row]
}