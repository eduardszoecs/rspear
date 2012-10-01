#' Match data with Trait-Database
#'
#' 
match_traits <- function(x, y, takex, takey, s = seq(0,0.5,.1), exact.first = TRUE) {
  ## merged holds the matched data
  merged <- NULL
  xtake <- as.character(x[ ,takex])
  ytake <- y[ ,takey]
  ## first we transform x to help in the matching: ignore cases and kill multiple white space
  xtake <- gsub(" +"," ",tolower(as.character(xtake)))
  ytake <- gsub(" +"," ",tolower(as.character(ytake)))  
  ## create indexes
  x.id <- 1:length(xtake)
  y.id <- 1:length(ytake)
  ## match exactly first
  if (exact.first) {
    tmp <- match(xtake, ytake)
    if (length(tmp) > 0) {
      ## save in merged what matches 
      merged <- na.omit(rbind(merged,
                              data.frame(x.id=x.id,
                                         y.id=y.id[tmp],
                                         threshold=-1)))
      ##save the unmatched obs indexes in x.id and y.id
      x.id <- x.id[!(x.id %in% merged[,1])]
    }
  }
  ## match approximately for each threshold in s
  for (i in s) {
    ##match both ways
    ##x->y
    tmp <- sapply(x.id,function(x) agrep(xtake[x],ytake[y.id],
                                         max.distance=i)[1])
    ## xm is a index of tmp with the non missing data
    xm <- !is.na(tmp)
    if (sum(xm) > 0) {
      ## put in merged 
      merged <- na.omit(rbind(merged,
                              data.frame(x.id=x.id[xm],
                                         y.id=y.id[tmp[xm]],
                                         threshold=i)[!duplicated(tmp),]))
      x.id <- x.id[!(x.id %in% merged[,1])]
      y.id <- y.id[!(y.id %in% merged[,2])]
    }
    ##y->x
    tmp <- sapply(y.id,function(x) agrep(paste(ytake[x]," "),xtake[x.id],
                                         max.distance=list(all=i,substitutions=i,deletions=i,insertions=i))[1])
    xm <- !is.na(tmp)
    if (sum(xm) > 0) {
      merged <- na.omit(rbind(merged,
                              data.frame(y.id=y.id[xm],
                                         x.id=x.id[tmp[xm]],
                                         threshold=i)))
      x.id <- x.id[!(x.id%in%merged[,1])]
      y.id <- y.id[!(y.id%in%merged[,2])]
    }
  }
  merged <- data.frame(merged)
  out <- data.frame(x[merged$x.id, ], y[merged$y.id, ])
  print(data.frame(data = x[merged$x.id, takex], traits = y[merged$y.id, takey], threshold = merged$threshold))
  return(out)
}