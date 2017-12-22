

directions <- c("N", "E", "S", "W")
directionsmp <- list(c(-1,0), c(0,1), c(1,0), c(0,-1))
currentdir <- directions[1]

expandgrid <- function(grid, current) {
    col <- matrix(rep(".", times=dim(grid)[1]), ncol=1)
    row <- matrix(rep(".", times=dim(grid)[2]), nrow=1)
    
    if (current[1] == 0) {
        grid <- rbind(row, grid)
        current <- current + c(1,0)
    } else if (current[1] > dim(grid)[1]) {
        grid <- rbind(grid, row)
    }
    if (current[2] == 0) {
        grid <- cbind(col, grid)
        current <- current + c(0,1)
    } else if (current[2] > dim(grid)[2]) {
        grid <- cbind(grid, col)
    }
    list(grid=grid, current=current)
}

        
updateposition <- function(grid, current, currentdiridx, numinfections) {
    if (grid[current[1], current[2]] == "#") {
        mod <- 1
        newval <- "."
    } else {
        mod <- -1
        newval <- "#"
        numinfections <- numinfections + 1
    }
                                        #    browser()
    grid[current[1], current[2]] <- newval
#    message("Change ", current, " to ", newval)
    currentdiridx <- currentdiridx + mod
#    message("currentdiridx ", currentdiridx)
    currentdir <- directions[1 + (currentdiridx %% 4 )]
#    message("Step to ", directions[1 + (currentdiridx %% 4 )])
    return(list(current=current+directionsmp[[1 + (currentdiridx %% 4 )]], currentdir=currentdir, currentdiridx=currentdiridx, grid=grid, numinfections=numinfections))
}

lines <- read.delim(file="~/AdventOfCode/day22/input.txt", stringsAsFactors=FALSE, sep="Q", header=FALSE)
grid <- matrix(as.character(unlist(apply(lines, 1, strsplit, ""))), nrow=nrow(lines), byrow=TRUE)

# grid <- matrix(strsplit("..##.....", "")[[1]], nrow=3, byrow=TRUE)
current <- c(1+((dim(grid)[1]-1)/2),1+((dim(grid)[2]-1)/2))

currentdiridx <- 0
numinfections <- 0
for (i in c(1:10000)) {
    res <- updateposition(grid, current, currentdiridx, numinfections)
    current <- res[["current"]]
    currentdiridx <- res[["currentdiridx"]]
    numinfections <- res[["numinfections"]]
    grid <- res[["grid"]]
    res <- expandgrid(grid, current)
    grid <- res[["grid"]]
    current <- res[["current"]]
    message(i)
#    print(grid)
#    message(current)
}
message("# infections: ", numinfections)

current <- updateposition(grid, current)
current
current <- updateposition(grid, current)
current
current <- updateposition(grid, current)
current

    
    
