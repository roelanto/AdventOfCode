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
        body <- paste(arg1, "<-", arg2)
    } else if (fun == "mul") {
        body <- paste(arg1, "<-", arg2, "*", arg1)
    } else if (fun == "snd") {
        body <- paste(argname("snd"), "<-", arg1)
    } else if (fun == "rcv") {
        body <- paste("if (", arg1, " > 0) {", argname("rcv"), "==", arg1, "\nmessage(\"done, snd = \",", argname("snd"),")\nquit()\n}")
    } else if (fun == "add") {
        body <- paste0(arg1, "<-", arg1, "+", arg2)
    } else if (fun == "mod") {
        body <- paste0(arg1, "<-", arg1, "%%", arg2)
    } else if (fun == "jgz") {
        body <- paste("if (",arg1," > 0) {
               jmpseq <- ", lineno, " + ",arg2,"\
for (c in c(jmpseq:length(all))) {\
regs <- lapply(list(all[[c]]), function(f) f(regs))[[1]]
}
        }")

    }
    paste(header, "message(\"",str,"\")\n", body, "\n", footer)
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
fun <- paste(fun, "function(regs) {regs})")

# easy debugging by writing to a file.
file <- "script.R"
con <- file(description=file, open="w")
write(fun, file)

# read the generated functions 
source(file)

# initialize all registers
regs <- list(a=0, b=0, c=0, d=0, e=0, i=0, p=0, f=0, snd=0, rcv=0)

# run the generated functions (one for each line).
for (fun in all) {
    regs <- lapply(list(fun), function(f) f(regs))[[1]]
}


        
