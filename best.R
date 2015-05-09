#Thic code is to print the hospital which is have lowest dealth base on types of dealth and states in USA#

outcomedata <- read.csv("hospitaldata/outcome-of-care-measures.csv", colClasses ="character")
decease <- data.frame(a = c(1, 2, 3), b = c("heart attack", 'heart failure', "pneumonia"))
best <- function(state, outcome) {
    if(any(any(state == outcomedata[, 7]), any(outcome == decease[, 2]))) {
        if(any(outcome == decease[, 2])) { 
            if(any(state == outcomedata[, 7])) { a <- c("go ahead")} else {
                stop("Invalid state")} 
        } else {stop ("Invalid outcome")}
    } else {stop("Invalid outcome and state")}
    #Typing function in here#
    
    ha <- as.numeric(outcomedata[, 11])
    hf <- as.numeric(outcomedata[, 17])
    pn <- as.numeric(outcomedata[, 23])
    bha <- cbind( outcomedata[,c(1,2,7)],ha)
    good  <- complete.cases(bha)
    bnha <- bha[good, ]
    sha <- split(bnha,bnha[[3]])
    bhf <- cbind( outcomedata[,c(1,2,7)],hf)
    good  <- complete.cases(bhf)
    bnhf <- bhf[good, ]
    shf <- split(bnhf,bnhf[[3]])
    bpn <- cbind( outcomedata[,c(1,2,7)],pn)
    good  <- complete.cases(bpn)
    bnpn <- bpn[good, ]
    spn <- split(bnpn,bnpn[[3]])
    if (outcome == "heart attack") {
        ss1 <- lapply(sha, function(x) x[which(x$ha == min(x$ha)),"Hospital.Name"])
        as.character(ss1[[state]])   
    } else {
        if(outcome == "heart failure") {
            ss1 <- lapply(shf, function(x) x[which(x$hf == min(x$hf)),"Hospital.Name"])
            as.character(ss1[[state]])
        } else {if ( outcome == "pneumonia") {
            ss1 <- lapply(spn, function(x) x[which(x$pn == min(x$pn)),"Hospital.Name"])
            as.character(ss1[[state]])
        }
        } 
    }    
}