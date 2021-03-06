setwd("~/AdventOfCode/day9")
input <- read.table("input.txt", sep="Q")
con<-file("input.txt",open="r")
input<-readLines(con)
close(con)

markgarbage <- function(char, status, prevstatus) {
    if (status != "I") {
        if (char == "<" && !(prevstatus == "g" || prevstatus == "G")) return("g")
        if (prevstatus == "G" || prevstatus=="g") {
            if (char != ">") return("G")
            return("h")
        } else {
            return("?")
        }
    } else {
        return("I")
    }
    return("?")
}

status="O"
string <- strsplit("<>","")[[1]]
string <- strsplit("<{!>}>", "")[[1]]
string <- strsplit("<!!!>>", "")[[1]]
string <- strsplit("<{o\"i!a,<{i<a>", "")[[1]]
string <- strsplit("{<a>,<a>,<a>,<a>}", "")[[1]]
string <- strsplit("{{<!!>},{<!!>},{<!!>},{<!!>}}", "")[[1]]
string <- strsplit("{{{}}}", "")[[1]]
#string <- strsplit("{{},{}}", "")[[1]]
#string <- strsplit("{{{},{},{{}}}}", "")[[1]]
                                        #string <- strsplit("{{<a!>},{<a!>},{<a!>},{<ab>}}", "")[[1]]

#string <- strsplit(input, "")[[1]]
status <- rep("?", length(string))
score <- rep(0, length(string))
for (i in c(1:length(string))) {
    if (string[i] == '!' && status[i] != "I") {
        status[i] <- "I"
        status[i+1] <- "I"
    }
}
#status
string[status=="I"] <- " "
#status[status=="I"] <- "?"
for (i in c(1:length(string))) {
    status[i] <- markgarbage(string[i], status[max(1, i-1)])
}
string[status=="g" | status=="G"] <- " "
status[status=="g" | status=="G"] <- "?"
level <- 0
numgroups <- 0
oldlevel <- 0
finalscore <- 0
for (i in c(1:length(string))) {
    if (string[i] == "{") {
        level <- level+1
    } else if (string[i] == "}") {
        level <- level -1
    }
    if (level > oldlevel) {
        numgroups <- numgroups + 1
        message("Score + ", level)
        finalscore <- finalscore + level
    }
    oldlevel <- level
    score[i] <- level
}
finalscore

duplicated(score)

test <- function(str, expected) {
    status="O"
    string <- strsplit(str,"")[[1]]
    status <- rep("?", length(string))
    score <- rep(0, length(string))
    for (i in c(1:length(string))) {
        if (string[i] == '!' && status[i] != "I") {
            status[i] <- "I"
            status[i+1] <- "I"
        }
    }
    string[status=="I"] <- " "
#    print(status)
    for (i in c(1:length(string))) {
        ctr <- 1
        prevstatus <- "I"
        while(prevstatus == "I") {
            prevstatus <- status[max(1,i-ctr)]
            ctr <- ctr+1
        }
        status[i] <- markgarbage(string[i], status[i], prevstatus)
    }
    string[status=="g" | status=="G"] <- " "
#    print(string)
#    print(status)
    return(list(status=status, result=sum((status=="G"))))
}
    

test("<>", 0)
test("<random characters>", 17)
test("<<<<>", 3)
test("<{!>}>",2)
test("<!!>", 0)
test("<!!!>>", 0)
test("<{o\"i!a,<{i<a>", 10)

test("{{{},{{{<,u,!{}!>,<'e<',>,", 2)

test(input, 1)
