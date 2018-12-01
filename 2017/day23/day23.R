argname <- function(str) {
    ifelse (is.numeric(type.convert(str, as.is=TRUE)),
            str,
            paste0("regs[[\"", str, "\"]]"))
}

createFunction <- function(str, lineno) {
    header <- "function(regs) {
"
    footer <- "        regs
    }
"
    parts <- strsplit(str, " ")
    fun <- parts[[1]][1]
    arg1 <- argname(parts[[1]][2])
    arg2 <- argname(parts[[1]][3])
    body <- "NOOP"
    
    if (fun == "set") {
        body <- paste(arg1, "<-", arg2, "
                      regs[[\"nxt\"]] <- regs[[\"nxt\"]] + 1")
        
    } else if (fun == "mul") {
        body <- paste(arg1, "<-", arg2, "*", arg1, "
recursive <<- recursive + 1
                      regs[[\"nxt\"]] <- regs[[\"nxt\"]] + 1")
    } else if (fun == "snd") {
        body <- paste(argname("snd"), "<-", arg1)
    } else if (fun == "rcv") {
        body <- paste("if (", arg1, " > 0) {", argname("rcv"), "==", arg1, "\nmessage(\"done, snd = \",", argname("snd"),")\nquit()\n}")
    } else if (fun == "sub") {
        body <- paste0(arg1, "<-", arg1, "-", arg2, "
                      regs[[\"nxt\"]] <- regs[[\"nxt\"]] + 1")
    } else if (fun == "mod") {
        body <- paste0(arg1, "<-", arg1, "%%", arg2)
    } else if (fun == "jnz") {
        body <- paste("if (",arg1," != 0) {
                      regs[[\"nxt\"]] <- regs[[\"nxt\"]] + ", arg2, "} else { regs[[\"nxt\"]] <- regs[[\"nxt\"]] + 1}")
    }
    if (1==2 && substr(str, 1, 8) == "jnz g -8") {
        paste(header, "message(\"",str,"\", regs[[\"g\"]])\n", body, "\n", footer)
    } else {
        paste(header, body, "\n", footer)
    }
}

## create script from input given by organizers

setwd("~/AdventOfCode/day23")

lines <- read.table("input.txt", sep="Q")

fun <- paste("all <- list(")
lineno <- 1
for (line in as.character(lines[,1])) {
    res <- createFunction(line, lineno)
    fun <- paste(fun, res, ",")
    lineno <- lineno + 1
}
fun <- paste(fun, "function(regs) {regs})")

# easy debugging by writing to a file.
file <- "script.R"
con <- file(description=file, open="w")
write(fun, file)

# read the generated functions 
source(file)

recursive <<- 0
# initialize all registers
regs <- list(a=1, b=0, c=0, d=0, e=0, i=0, p=0, f=0, g=0, h=0, snd=0, rcv=0, nxt=1)


                                        # run the generated functions (one for each line).
prev <- -1
while (regs$nxt <= 32) {
    regs <- lapply(list(all[[regs$nxt]]), function(f) f(regs))[[1]]
    if (regs$h != prev ) {
        
        print(paste0(paste0(rep(".", regs$nxt), collapse=""), "a=", regs$a," b=", regs$b, " c=", regs$c, " d=", regs$d, " e=", regs$e, " f=", regs$f, " g=", regs$g, " h=", regs$h, " nxt=", regs$nxt))
        prev <- regs$b
    }
}
print(paste0(paste0(rep(".", regs$nxt), collapse=""), "a=", regs$a," b=", regs$b, " c=", regs$c, " d=", regs$d, " e=", regs$e, " f=", regs$f, " g=", regs$g, " h=", regs$h, " nxt=", regs$nxt))
