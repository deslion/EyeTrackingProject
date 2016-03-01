IDT <- function(t, x, y, settings) {
  #Reading settings
  filterMarkers       <- settings$filterMarkers
  dispersionThreshold <- settings$dispersionThreshold # in px or degrees
  durationThreshold   <- settings$durationThreshold # in milliseconds
  durationThreshold   <- durationThreshold/1000 # now in seconds
  angular             <- settings$angular
  screenDist          <- settings$screenDist
  screenResolution    <- settings$screenResolution
  screenSize          <- settings$screenSize
  #Dispersion measure
  disp <- function(coords, left, right) {
    return((max(coords$x[left:right]) - min(coords$x[left:right])) + 
           (max(coords$y[left:right]) - min(coords$y[left:right])))
  }
  #Converting px to angle
  if (angular)
  {
    angPositions <- calcAngPos(x = x, y = y, screenDist, screenResolution, screenSize)
    x <- angPositions$xAng
    y <- angPositions$yAng
  }
  
  size <- length(t)
  rawEvM <- c()
  rawEvM[1:(size - 1)] <- sacMarker
  coords <- data.frame(x = x[-size], 
                       y = y[-size], 
                       dur = t[-1] - t[-size])
  left <- 1; right <- 2
  while (right < size) {
    if (sum(coords$dur[left:right]) < durationThreshold) {
      right <- right + 1
      next  
    }
    d <- disp(coords, left, right)
    if (d > dispersionThreshold) {
      left <- right + 1; right <- left + 1
    }
    else {
      while ((d <= dispersionThreshold) & (right < size)) {
        right <- right + 1
        d <- disp(coords, left, right)
      }
      right <- right - 1
      rawEvM[left:right] <- fixMarker
      left <- right + 1
      right <- left + 1
    }
  }
  rawEvM[which(filterMarkers != filterOkMarker)[-size]] <- gapMarker
  rawEvM <- append(rawEvM, gapMarker)
  group <- markersGroups(rawEvM)
  return(list(eventMarkers = rawEvM, eventGroups = group))
}