# Checks that x is a numeric vector without NA values.
check_num <- function(x) {
    
    # check for NULL values
    if (is.null(x)) 
        return(FALSE)
    
    # check if x is numeric without NA values
    return(is.vector(x, "numeric") & !any(is.na(x)))
}




# Checks that x and y have the same length
check_len <- function(x, y) {
    return(length(x) == length(y))
}



# Checks if observed and predicted vectors are valid for computations
check_vectors <- function(observed, predicted) {
    return(check_num(observed) & check_num(predicted) & check_len(observed, predicted))
}

