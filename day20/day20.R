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

distorder  <- order(sapply(points, distance))
prevdistorder <- order(sapply(points, distance), decreasing=TRUE)
while ((length(prevdistorder) != length(distorder)) || sum(prevdistorder == distorder) != length(distorder)) {
    prevdistorder <- distorder
#    message(paste(distorder, " "))
    for (i in c(1:10)) {
        points <- lapply(points, update)
        points <- points[determineduplicated(points)]
        distances <- sapply(points, distance)
        distorder <- order(distances)
    }
}

length(distorder)


