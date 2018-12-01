all <- list( function(regs) {
 regs[["b"]] <- 79 
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["c"]] <- regs[["b"]] 
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 if ( regs[["a"]]  != 0) {
                      regs[["nxt"]] <- regs[["nxt"]] +  2 } else { regs[["nxt"]] <- regs[["nxt"]] + 1} 
         regs
    }
 , function(regs) {
 if ( 1  != 0) {
                      regs[["nxt"]] <- regs[["nxt"]] +  5 } else { regs[["nxt"]] <- regs[["nxt"]] + 1} 
         regs
    }
 , function(regs) {
 regs[["b"]] <- 100 * regs[["b"]] 
recursive <<- recursive + 1
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["b"]]<-regs[["b"]]--100000
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["c"]] <- regs[["b"]] 
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["c"]]<-regs[["c"]]--17000
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["f"]] <- 1 
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["d"]] <- 2 
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["e"]] <- 2 
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["g"]] <- regs[["d"]] 
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["g"]] <- regs[["e"]] * regs[["g"]] 
recursive <<- recursive + 1
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["g"]]<-regs[["g"]]-regs[["b"]]
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 if ( regs[["g"]]  != 0) {
                      regs[["nxt"]] <- regs[["nxt"]] +  2 } else { regs[["nxt"]] <- regs[["nxt"]] + 1} 
         regs
    }
 , function(regs) {
 regs[["f"]] <- 0 
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["e"]]<-regs[["e"]]--1
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["g"]] <- regs[["e"]] 
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["g"]]<-regs[["g"]]-regs[["b"]]
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 if ( regs[["g"]]  != 0) {
                      regs[["nxt"]] <- regs[["nxt"]] +  -8 } else { regs[["nxt"]] <- regs[["nxt"]] + 1} 
         regs
    }
 , function(regs) {
 regs[["d"]]<-regs[["d"]]--1
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["g"]] <- regs[["d"]] 
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["g"]]<-regs[["g"]]-regs[["b"]]
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 if ( regs[["g"]]  != 0) {
                      regs[["nxt"]] <- regs[["nxt"]] +  -13 } else { regs[["nxt"]] <- regs[["nxt"]] + 1} 
         regs
    }
 , function(regs) {
 if ( regs[["f"]]  != 0) {
                      regs[["nxt"]] <- regs[["nxt"]] +  2 } else { regs[["nxt"]] <- regs[["nxt"]] + 1} 
         regs
    }
 , function(regs) {
 regs[["h"]]<-regs[["h"]]--1
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["g"]] <- regs[["b"]] 
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 regs[["g"]]<-regs[["g"]]-regs[["c"]]
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 if ( regs[["g"]]  != 0) {
                      regs[["nxt"]] <- regs[["nxt"]] +  2 } else { regs[["nxt"]] <- regs[["nxt"]] + 1} 
         regs
    }
 , function(regs) {
 if ( 1  != 0) {
                      regs[["nxt"]] <- regs[["nxt"]] +  3 } else { regs[["nxt"]] <- regs[["nxt"]] + 1} 
         regs
    }
 , function(regs) {
 regs[["b"]]<-regs[["b"]]--17
                      regs[["nxt"]] <- regs[["nxt"]] + 1 
         regs
    }
 , function(regs) {
 if ( 1  != 0) {
                      regs[["nxt"]] <- regs[["nxt"]] +  -23 } else { regs[["nxt"]] <- regs[["nxt"]] + 1} 
         regs
    }
 , function(regs) {regs})
