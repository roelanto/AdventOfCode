readplan <- function(filename) {
    con=file(filename, open="r")
    str <- readChar(con, nchars=1024*1024)
    close(con)
    ## determine number of newlines (= number of rows)
    numnewlines <- length(which(strsplit(str, c())[[1]] == "\n"))
    matrix(strsplit(str, c())[[1]], nrow=numnewlines, byrow=TRUE)
}    


inside <- function(pos, plan) {
#     message(pos[1], " ", pos[2], " ", dim(plan)[1], " ", dim(plan)[2])
    if (pos[1] > 0 && pos[1] <=dim(plan)[1] &&
        pos[2] > 0 && pos[2] <=dim(plan)[2]) 
        return(TRUE)
        
    return(FALSE)
}

nextmove <- function(pos, dir, plan) {
    if (dir == "SOUTH") {
        nextmove <- c(pos[1]+1, pos[2])
    } else if (dir == "EAST") {
        nextmove <- c(pos[1], pos[2]+1)
    } else if (dir == "WEST") {
        nextmove <- c(pos[1], pos[2]-1)
    } else if (dir == "NORTH") {
        nextmove <- c(pos[1]-1, pos[2])
    }
    if (inside(nextmove, plan)) {
        return (list(nextmove = nextmove, status="OK"))
    } else {
        return (list(nextmove = pos, status="OOB"))
    }
            

}

changedirection <- function(currentdir, lastdir) {
    newdir <- currentdir
    possibledirs <- c("NORTH", "SOUTH")
    if (lastdir %in% c("NORTH", "SOUTH")) possibledirs <- c("EAST", "WEST")
    
    while (newdir == currentdir) {
        newdir <- sample(possibledirs, size=1)
    }
    newdir
}


plan <- readplan("~/AdventOfCode/day19/input.txt")


startpos <- c(1,1)
for (x in c(1:(dim(plan)[2]-1))) {
    message(x)
    if (plan[1,x] == '|') startpos<-c(1,x)
}
pos <- startpos
direction<-"SOUTH"
lastdir <- "SOUTH"
tried <- c()
while(length(tried) <= 2) {
    status <- "RUNNING"
    while(status == "RUNNING") {
        proposedmove <- nextmove(pos, direction, plan)
        if (proposedmove$status == "OK") {
            if (!(plan[proposedmove$nextmove[1], proposedmove$nextmove[2]] %in% c(" ", "\n"))) {
                pos <- proposedmove$nextmove
                tried <- c()
                if (plan[proposedmove$nextmove[1], proposedmove$nextmove[2]] %in% LETTERS) {
                    message(plan[proposedmove$nextmove[1], proposedmove$nextmove[2]])
                }
                lastdir <- direction
            } else {
                status <- "OOB"
            }
        } else {
            status <- "OOB"
        }
    }
    direction <- changedirection(direction, lastdir)
    tried <- append(tried, direction)
}
