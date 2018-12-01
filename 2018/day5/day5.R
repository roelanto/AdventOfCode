instructions <- as.vector(read.table("~/AdventOfCode/day5/input.txt", sep="Q")[,1])

ptr <- 1
stepctr <- 0
while (ptr <= length(instructions)) {
    jmp <- instructions[ptr]
    instructions[ptr] <- instructions[ptr]+1
    ptr <- ptr+jmp
#    message("instructions: ", paste(instructions, " "))
    message("ptr: ", ptr)
    stepctr <- stepctr + 1
}
stepctr
