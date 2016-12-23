best <- function(state, outcome) {
        ## Read outcome data subsetting selecting relevant columns
        outcome_options <- c("Heart Attack"=11, "Heart Failure"=17, "Pneumonia"=23)
        outcome_data <- read.csv("outcome-of-care-measures.csv")[,c(2,7,outcome_options[outcome])]
        
        
        ## Check that state and outcome are valid
        if(!state %in% state.abb) stop("invalid state")
        if(!outcome %in% names(outcome_options)) stop("invalid outcome")
        

        ## subset data to state indicated in function and then select min hospital for that outcome
        state_select <- outcome_data[outcome_data$State == state,]
        state_select[,outcome],na.rm = TRUE
        
        ##find the row, save to variable, print the name col of that row?
                ##outcome options are 
                #Column 11 is heart attack mortality
                #Column 17 is heart failure mortality
                #Column 23 is pneumonia mortality
        
                #drop NA values before ranking options
                                        
        ## Return hospital name in that state with lowest 30-day death rate
                ## Column 2 is hospital name
                                        
                                        
}