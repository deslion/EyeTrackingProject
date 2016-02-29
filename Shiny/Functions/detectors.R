source("Shiny\\Functions\\detectors\\ANH.R", local = T)
source("Shiny\\Functions\\detectors\\IDT.R", local = T)
source("Shiny\\Functions\\detectors\\IVT.R", local = T)

createDetector <- function(id, name, description, fun, settings)
{
  detector <- new(Class = "OculomotorEventDetector", id = id, name = name, fun = fun, description = description, settings = settings)
  return(detector)
}

markersGroups <- function(markers) {
  evmarks <- data.frame(firstEv = markers[-length(markers)], secondEv = markers[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  return(group)
}

## CORE DETECTOR ##
# This detector uses specified function (IVT, IDT, Ada-NH, ...) to detect oculomotor events
coreDetector <- function(DataRecord, settings)
{
  eventDetectorID <- settings$detectorID
  t <- DataRecord@eyesDataObject@time@time
  algorithm <- settings$subfun
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    filterMarkers <- DataRecord@eyesDataObject@leftEventsMarkers$filterMarkers@markers
    settings <- append(settings, list(filterMarkers = filterMarkers))
    res <- algorithm(t = t, x = leftX, y = leftY, settings)
    oculomotorEventMarkers <- new(Class = "OculomotorEventMarkers", 
                                  detectorID = eventDetectorID, 
                                  markers = res$eventMarkers,
                                  groups = res$eventGroups,
                                  eventClass = "OculomotorEvent")
    DataRecord@eyesDataObject@leftEventsMarkers$oculomotorEventMarkers <- oculomotorEventMarkers
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
  {
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    filterMarkers <- DataRecord@eyesDataObject@rightEventsMarkers$filterMarkers@markers
    settings <- append(settings, list(filterMarkers = filterMarkers))
    res <- algorithm(t = t, x = rightX, y = rightY, settings)
    oculomotorEventMarkers <- new(Class = "OculomotorEventMarkers", 
                                  detectorID = eventDetectorID, 
                                  markers = res$eventMarkers,
                                  groups = res$eventGroups,
                                  eventClass = "OculomotorEvent")
    DataRecord@eyesDataObject@rightEventsMarkers$oculomotorEventMarkers <- oculomotorEventMarkers
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    
    leftFilterMarkers <- DataRecord@eyesDataObject@leftEventsMarkers$filterMarkers@markers
    rightFilterMarkers <- DataRecord@eyesDataObject@rightEventsMarkers$filterMarkers@markers
    
    resLeft <- algorithm(t = t, x = leftX, y = leftY, append(settings, list(filterMarkers = leftFilterMarkers)))
    leftOculomotorEventMarkers <- new(Class = "OculomotorEventMarkers", 
                                      detectorID = eventDetectorID, 
                                      markers = resLeft$eventMarkers,
                                      groups = resLeft$eventGroups,
                                      eventClass = "OculomotorEvent")
    resRight <- algorithm(t = t, x = rightX, y = rightY, append(settings, list(filterMarkers = rightFilterMarkers)))
    rightOculomotorEventMarkers <- new(Class = "OculomotorEventMarkers", 
                                       detectorID = eventDetectorID, 
                                       markers = resRight$eventMarkers,
                                       groups = resRight$eventGroups,
                                       eventClass = "OculomotorEvent")
    
    DataRecord@eyesDataObject@leftEventsMarkers$oculomotorEventMarkers <- leftOculomotorEventMarkers
    DataRecord@eyesDataObject@rightEventsMarkers$oculomotorEventMarkers <- rightOculomotorEventMarkers
  }
  return(DataRecord)
}