instructions <- as.vector(read.table("~/AdventOfCode/day5/input.txt", sep="Q")[,1])

# instructions <- c(0,3,0,1,-3)
ptr <- 1
stepctr <- 0
while (ptr <= length(instructions)) {
    jmp <- instructions[ptr]
    oldptr <- ptr
    ptr <- ptr+jmp
    if (instructions[oldptr] >= 3) {
        instructions[oldptr] <- instructions[oldptr]-1
    } else {
        instructions[oldptr] <- instructions[oldptr]+1
    }
                                        #  message("instructions: ", paste(instructions, " "))
    if (stepctr %% 100000 == 0) {
        message("ptr: ", ptr, " stepctr: ", stepctr)
    }
    stepctr <- stepctr + 1
}
stepctr
