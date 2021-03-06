library(rgl)

filename <- "~/AdventOfCode/day20/input.txt"

parsecoords <- function (str) {
    parts <- strsplit(strsplit(strsplit(as.character(str), "<")[[1]][2], ">")[[1]][1], ",")[[1]]
    as.numeric(parts)
}
parsecoords("p=<3,0,0>,")

update <- function(p) {
    newvel <- p@v + p@a
    p@v <- newvel
    p@coords <- p@coords + p@v
    p
}

distance <- function(p) {
    sum(abs(p@coords))
}

determineduplicated <- function(points) {
    return(!(duplicated(lapply(points, function(x) {x@coords})) | duplicated(lapply(points, function(x) {x@coords}), fromLast=TRUE)))
}

setClass("point", representation(coords="vector", v="vector", a="vector"))


lines <- read.table(filename)
points <- vector("list", length=nrow(lines))
for (i in c(1:nrow(lines))) {
    line <- lines[i,]
    point <- new("point",
                 coords=c(parsecoords(line[[1]])),
                 a=c(parsecoords(line[[3]])),
                 v=c(parsecoords(line[[2]])))
    points[[i]] <- point
}

minx <- min(unlist(lapply(points, function(x) {x@coords[1]})))
maxx <- max(unlist(lapply(points, function(x) {x@coords[1]})))
miny <- min(unlist(lapply(points, function(x) {x@coords[2]})))
maxy <- max(unlist(lapply(points, function(x) {x@coords[2]})))
minz <- min(unlist(lapply(points, function(x) {x@coords[3]})))
maxz <- max(unlist(lapply(points, function(x) {x@coords[3]})))

plotpoints <- function(points) {
                                        #    options(rgl.printRglwidget = TRUE)
                                        #    open3d()
    xcoords <- unlist(lapply(points, function(x) {x@coords[1]}))
    ycoords <- unlist(lapply(points, function(x) {x@coords[2]}))
    zcoords <- unlist(lapply(points, function(x) {x@coords[3]}))
    plot3d(x=xcoords, y=ycoords, z=zcoords, xlim=c(minx,maxx), ylim=c(miny,maxy), zlim=c(minz,maxz), add=FALSE)
#    for (point in points) {
#        points3d(x=xcoords, y=ycoords, z=zcoords)
#    }
    rgl.pop()
}
plotpoints(points)


distorder  <- order(sapply(points, distance))
prevdistorder <- order(sapply(points, distance), decreasing=TRUE)
iter <- 1
while ((iter < 10) && (length(prevdistorder) != length(distorder)) || sum(prevdistorder == distorder) != length(distorder)) {
    prevdistorder <- distorder
                                        #    message(paste(distorder, " "))
    for (i in c(1:10)) {
        points <- lapply(points, update)
        points <- points[determineduplicated(points)]
        clear3d()
        plotpoints(points)
        rgl.postscript(paste0("file", iter), fmt="pdf")
        rgl.pop()
        distances <- sapply(points, distance)
        distorder <- order(distances)
        iter <- iter+1
    }
}

length(distorder)


