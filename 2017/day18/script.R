all <- list( function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["i"]] <- 31 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["a"]] <- 1 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["p"]] <- 17 * regs[["p"]] 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if ( regs[["p"]]  > 0) {
               jmpseq <-  4  +  regs[["p"]] 
for (c in c(jmpseq:length(all))) {
if (regs[["state"]] != "WAITING") {
regs <- lapply(list(all[[c]]), function(f) f(regs, instance))[[1]]
}
}
        } 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["a"]] <- 2 * regs[["a"]] 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["i"]]<-regs[["i"]]+-1 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if ( regs[["i"]]  > 0) {
               jmpseq <-  7  +  -2 
for (c in c(jmpseq:length(all))) {
if (regs[["state"]] != "WAITING") {
regs <- lapply(list(all[[c]]), function(f) f(regs, instance))[[1]]
}
}
        } 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["a"]]<-regs[["a"]]+-1 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["i"]] <- 127 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["p"]] <- 622 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["p"]] <- 8505 * regs[["p"]] 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["p"]]<-regs[["p"]]%%regs[["a"]] 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["p"]] <- 129749 * regs[["p"]] 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["p"]]<-regs[["p"]]+12345 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["p"]]<-regs[["p"]]%%regs[["a"]] 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["b"]] <- regs[["p"]] 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["b"]]<-regs[["b"]]%%10000 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["numsnd"]] <- regs[["numsnd"]] + 1 
if (instance == 1) message("send: ",  regs[["b"]] )
 regs[["sendbuffer"]]  <- append( regs[["sendbuffer"]]  ,  regs[["b"]] ) 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["i"]]<-regs[["i"]]+-1 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if ( regs[["i"]]  > 0) {
               jmpseq <-  20  +  -9 
for (c in c(jmpseq:length(all))) {
if (regs[["state"]] != "WAITING") {
regs <- lapply(list(all[[c]]), function(f) f(regs, instance))[[1]]
}
}
        } 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if ( regs[["a"]]  > 0) {
               jmpseq <-  21  +  3 
for (c in c(jmpseq:length(all))) {
if (regs[["state"]] != "WAITING") {
regs <- lapply(list(all[[c]]), function(f) f(regs, instance))[[1]]
}
}
        } 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if (length(regs[["recvbuffer"]]) > 0) {
            received <- regs[["recvbuffer"]][1]
            regs[["recvbuffer"]] <- regs[["recvbuffer"]][-1]
            
             regs[["b"]]  <- received
        } else {
            regs[["state"]] <- "WAITING"
regs[["last"]] <-  22 
        } 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if ( regs[["b"]]  > 0) {
               jmpseq <-  23  +  -1 
for (c in c(jmpseq:length(all))) {
if (regs[["state"]] != "WAITING") {
regs <- lapply(list(all[[c]]), function(f) f(regs, instance))[[1]]
}
}
        } 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["f"]] <- 0 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["i"]] <- 126 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if (length(regs[["recvbuffer"]]) > 0) {
            received <- regs[["recvbuffer"]][1]
            regs[["recvbuffer"]] <- regs[["recvbuffer"]][-1]
            
             regs[["a"]]  <- received
        } else {
            regs[["state"]] <- "WAITING"
regs[["last"]] <-  26 
        } 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if (length(regs[["recvbuffer"]]) > 0) {
            received <- regs[["recvbuffer"]][1]
            regs[["recvbuffer"]] <- regs[["recvbuffer"]][-1]
            
             regs[["b"]]  <- received
        } else {
            regs[["state"]] <- "WAITING"
regs[["last"]] <-  27 
        } 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["p"]] <- regs[["a"]] 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["p"]] <- -1 * regs[["p"]] 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["p"]]<-regs[["p"]]+regs[["b"]] 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if ( regs[["p"]]  > 0) {
               jmpseq <-  31  +  4 
for (c in c(jmpseq:length(all))) {
if (regs[["state"]] != "WAITING") {
regs <- lapply(list(all[[c]]), function(f) f(regs, instance))[[1]]
}
}
        } 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["numsnd"]] <- regs[["numsnd"]] + 1 
if (instance == 1) message("send: ",  regs[["a"]] )
 regs[["sendbuffer"]]  <- append( regs[["sendbuffer"]]  ,  regs[["a"]] ) 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["a"]] <- regs[["b"]] 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if ( 1  > 0) {
               jmpseq <-  34  +  3 
for (c in c(jmpseq:length(all))) {
if (regs[["state"]] != "WAITING") {
regs <- lapply(list(all[[c]]), function(f) f(regs, instance))[[1]]
}
}
        } 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["numsnd"]] <- regs[["numsnd"]] + 1 
if (instance == 1) message("send: ",  regs[["b"]] )
 regs[["sendbuffer"]]  <- append( regs[["sendbuffer"]]  ,  regs[["b"]] ) 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["f"]] <- 1 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["i"]]<-regs[["i"]]+-1 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if ( regs[["i"]]  > 0) {
               jmpseq <-  38  +  -11 
for (c in c(jmpseq:length(all))) {
if (regs[["state"]] != "WAITING") {
regs <- lapply(list(all[[c]]), function(f) f(regs, instance))[[1]]
}
}
        } 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 regs[["numsnd"]] <- regs[["numsnd"]] + 1 
if (instance == 1) message("send: ",  regs[["a"]] )
 regs[["sendbuffer"]]  <- append( regs[["sendbuffer"]]  ,  regs[["a"]] ) 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if ( regs[["f"]]  > 0) {
               jmpseq <-  40  +  -16 
for (c in c(jmpseq:length(all))) {
if (regs[["state"]] != "WAITING") {
regs <- lapply(list(all[[c]]), function(f) f(regs, instance))[[1]]
}
}
        } 
} 
        regs
    }
 , function(regs, instance) {
 if (regs[["state"]] == "RUNNING") {
 if ( regs[["a"]]  > 0) {
               jmpseq <-  41  +  -19 
for (c in c(jmpseq:length(all))) {
if (regs[["state"]] != "WAITING") {
regs <- lapply(list(all[[c]]), function(f) f(regs, instance))[[1]]
}
}
        } 
} 
        regs
    }
 , function(regs, instance) {regs})
