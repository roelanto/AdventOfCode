argname <- function(str) {
    ifelse (is.numeric(type.convert(str, as.is=TRUE)),
            str,
            paste0("regs[[\"", str, "\"]]"))
}

createFunction <- function(str, lineno) {
    header <- "function(regs, instance) {
"
    footer <- paste("\n        regs
    }
")
    parts <- strsplit(str, " ")
    fun <- parts[[1]][1]
    arg1 <- argname(parts[[1]][2])
    arg2 <- argname(parts[[1]][3])
    body <- "NOOP"
    
    if (fun == "set") {
        body <- paste(arg1, "<-", arg2)
    } else if (fun == "mul") {
        body <- paste(arg1, "<-", arg2, "*", arg1)
    } else if (fun == "snd") {
        body <- paste(
            "regs[[\"numsnd\"]] <- regs[[\"numsnd\"]] + 1 \nif (instance == 1) message(\"send: \", ", arg1, ")\n", argname("sendbuffer")," <- append(", argname("sendbuffer"), " , ", arg1, ")"
        )
    } else if (fun == "rcv") {
        body <- paste("if (length(regs[[\"recvbuffer\"]]) > 0) {
            received <- regs[[\"recvbuffer\"]][1]
            regs[[\"recvbuffer\"]] <- regs[[\"recvbuffer\"]][-1]
            
            ", arg1, " <- received
        } else {
            regs[[\"state\"]] <- \"WAITING\"
regs[[\"last\"]] <- ", lineno, "
        }")
    } else if (fun == "add") {
        body <- paste0(arg1, "<-", arg1, "+", arg2)
    } else if (fun == "mod") {
        body <- paste0(arg1, "<-", arg1, "%%", arg2)
    } else if (fun == "jgz") {
        body <- paste("if (",arg1," > 0) {
               jmpseq <- ", lineno, " + ",arg2,"\
for (c in c(jmpseq:length(all))) {\
if (regs[[\"state\"]] != \"WAITING\") {
regs <- lapply(list(all[[c]]), function(f) f(regs, instance))[[1]]
}
}
        }")

    }
#    paste(header, "message(\"",str,", p = \",", argname("p"), ")\nif (regs[[\"state\"]] == \"RUNNING\") {\n", body, "\n}", footer)
    paste(header, "if (regs[[\"state\"]] == \"RUNNING\") {\n", body, "\n}", footer)
}

## create script from input given by organizers
lines <- read.table("input.txt", sep="Q")

fun <- paste("all <- list(")
lineno <- 1
for (line in as.character(lines[,1])) {
    res <- createFunction(line, lineno)
    fun <- paste(fun, res, ",")
    lineno <- lineno + 1
}
fun <- paste(fun, "function(regs, instance) {regs})")

# easy debugging by writing to a file.
file <- "script.R"
con <- file(description=file, open="w")
write(fun, file)

# read the generated functions 
source(file)

# initialize all registers
regs.0 <- list(state="RUNNING", sendbuffer=c(), a=0, b=0, c=0, d=0, e=0, i=0, p=0, f=0, snd=0, rcv=0, last=1, numsnd=0)
regs.1 <- list(state="RUNNING", sendbuffer=c(), a=0, b=0, c=0, d=0, e=0, i=0, p=1, f=0, snd=0, rcv=0, last=1, numsnd=0)

## run the generated functions (one for each line).

                                        #while (!(prevstate.0 == "WAITING" && prevstate.1 == "WAITING")) {
exitcond <- FALSE

while (!exitcond) {
regs.0[["state"]] <- "RUNNING"
regs.1[["state"]] <- "RUNNING"
allfuncs.0 <- seq(from=regs.0[["last"]], to=length(all))
for (funcnum in allfuncs.0) {
    if (regs.0[["state"]] == "RUNNING") {
        regs.0 <- lapply(list(all[[funcnum]]), function(f) f(regs.0, 0))[[1]]
    }
}    
regs.1$recvbuffer <- append(regs.1$recvbuffer, regs.0[["sendbuffer"]])
regs.0[["sendbuffer"]] <- c()
message("recvbuffer1 == ", length(regs.0[["recvbuffer"]]), " 2 == ", length(regs.1[["recvbuffer"]]))
allfuncs.1 <- seq(from=regs.1[["last"]], to=length(all))
for (funcnum in allfuncs.1) {
    if (regs.1[["state"]] == "RUNNING") {
        regs.1 <- lapply(list(all[[funcnum]]), function(f) f(regs.1, 1))[[1]]
    }
}
regs.0$recvbuffer <- append(regs.0$recvbuffer, regs.1[["sendbuffer"]])
regs.1[["sendbuffer"]] <- c()
message("recvbuffer1 == ", length(regs.0[["recvbuffer"]]), " 2 == ", length(regs.1[["recvbuffer"]]))
if (length(regs.0[["recvbuffer"]]) == length(regs.1[["recvbuffer"]])) {exitcond <- TRUE}
}

message("Num sends from instance 1: ", regs.1$numsnd)
                                        #}
