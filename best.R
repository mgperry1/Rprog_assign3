
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=".")
}

best <- function(state, outcome) {
  ## Read outcome data
  if (!exists("outcomeDF") ) {
  outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  out_cols <- names(outcomeDF)
  }
  ## Check that state and outcome are valid
  checkST <- outcomeDF[outcomeDF$State == state,]
  # Test for valid outcome by pasting the text to the column name and then
  #checking the length of subsetting the colnames on that phrase
  #"Hospital.30.Day.Death..Mortality..Rates.from.
  test_outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from." , simpleCap(outcome), sep="")
  
  if (nrow(checkST) == 0 ) stop("invalid state")
  if (length(out_cols[out_cols %in% test_outcome]) == 0 ) stop("invalid outcome")
  ## Return hospital name in that state with lowest 30-day death
  ## rate X[order(X$var1,X$var3),]
  #out_cols[out_cols %in% test_outcome]
  #Convert the Outcome column selected to numeric
  checkST[,test_outcome] <- suppressWarnings(as.numeric(checkST[,test_outcome]))
  #Order by the Outcome then the State for ties..
  state_df <- checkST[order(checkST[,test_outcome],checkST[,"Hospital.Name"]),]
  state_df[1,2]
}
