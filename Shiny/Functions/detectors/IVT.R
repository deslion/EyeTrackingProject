IVT <- function(t, x, y, settings)
{
  filterMarkers <- settings$filterMarkers

  VT <- settings$VT
  angular <- settings$angular
  screenDist <- settings$screenDistance
  screenResolution <- settings$screenResolution
  screenSize <- settings$screenSize
  postProcess <- settings$postProcess
  markerNames <- settings$markerNames
  # 1. Velocities and accelerations estimation
  vel <- calcVel(t, x, y, settings)
  accels <- vel$accels
  
  # 2. Classification stage: getting raw event markers
  gapMarkers <- ifelse(filterMarkers != filterOkMarker, gapMarker, filterOkMarker)
  rawEventMarkers <- ifelse(gapMarkers[1:length(accels)] == gapMarker, gapMarker, ifelse(vel$vels[1:length(accels)] <= VT, fixMarker, sacMarker))
  eventMarkers <- rawEventMarkers
  group <- markersGroups(eventMarkers)

  if (postProcess)
  {
    classifyGaps <- settings$classifyGaps
    MaxTBetFix <- settings$MaxTBetFix
    MaxDistBetFix <- settings$MaxDistBetFix
    minFixLen <- settings$minFixLen
    maxGapLen <- settings$maxGapLen 
    maxVel <- settings$maxVel
    maxAccel <- settings$maxAccel
    
    events <- data.frame(t = t[1:length(accels)], 
                         x = x[1:length(accels)], 
                         y = y[1:length(accels)], 
                         dls = vel$dists[1:length(accels)], 
                         dts = vel$dts[1:length(accels)], 
                         vel = vel$vels[1:length(accels)], 
                         accel = accels, 
                         evm = rawEventMarkers, 
                         gr = group)
    eventGroups <- split(events, group)
    
    fixationGroups <- list()
    saccadeGroups <- list()
    gapGroups <- list()
    artifactGroups <- list()
    eventMarkersGroups <- list()
    group <- 0
    # newGroups <- c()
    # newEvents <- c()
    lastGroup = NA
    for (gr in 1:length(eventGroups))
    {
      currentGroup <- eventGroups[[gr]]$evm[1]
      # Если текущая группа сэмплов - фиксация
      if (currentGroup == fixMarker)
      {
        # то вычисляем её длительность
        fixLen <- (eventGroups[[gr]]$t[nrow(eventGroups[[gr]])]-eventGroups[[gr]]$t[1])
        # если фиксация короткая, то рассматриваем её как артефакт
        if (fixLen < minFixLen)
        {
          artifactGroups <- append(artifactGroups, eventGroups[gr])
          group <- group + 1
          # newGroups <- c(newGroups, rep(group, nrow(eventGroups[[gr]])))
          # newEvents <- c(newEvents, rep(markerNames$artifact, nrow(eventGroups[[gr]])))
          eventMarkersGroups <- append(eventMarkersGroups, list(rep(artMarker, nrow(eventGroups[[gr]]))))
        }
        # если фиксация не короткая
        if (fixLen >= minFixLen)
        {
          anyGroupBefore <- !is.na(lastGroup)
          prevGroupIsSaccade <- F
          anyFixBefore <- F
          fixCloseInTime <- F
          fixCloseInSpace <- F
          
          if (anyGroupBefore) {prevGroupIsSaccade <- (lastGroup == sacMarker)}
          if (prevGroupIsSaccade) {anyFixBefore <- (length(fixationGroups) != 0)}
          if (anyFixBefore) 
          {
            currentFixOnset <- eventGroups[[gr]]$t[1]
            lastFixation <- tail(fixationGroups, n = 1)[[1]]
            lastFixOffset <- tail(lastFixation$t, 1)
            fixCloseInTime <- (lastFixOffset - currentFixOnset) <= MaxTBetFix
          }
          if (fixCloseInTime) 
          {
            currentFixPos <- c(eventGroups[[gr]]$x[1], eventGroups[[gr]]$y[1])
            lastFixPos <- c(tail(lastFixation$x, 1), tail(lastFixation$y, 1))
            dist <- sqrt((lastFixPos[1]-currentFixPos[1])^2 + (lastFixPos[2]-currentFixPos[2])^2)
            fixCloseInSpace <- (dist <= MaxDistBetFix)
          }
          if (fixCloseInSpace)
          {
            # то предыдущую саккаду рассматриваем как артефакт записи
            # newEvents[tail(newEvents, nrow(saccadeGroups[[length(saccadeGroups)]]))] <- rep(markerNames$artifact, nrow(saccadeGroups[[length(saccadeGroups)]]))
            artifactGroups <- append(artifactGroups, saccadeGroups[length(saccadeGroups)])
            eventMarkersGroups[length(eventMarkersGroups)] <- list(rep(artMarker, length(eventMarkersGroups[[length(eventMarkersGroups)]])))
            saccadeGroups <- saccadeGroups[-length(saccadeGroups)]
            
            # а текущую фиксацию рассматриваем как продолжение предыдущей
            lastFixation <- list(rbind(lastFixation, eventGroups[[gr]]))
            fixationGroups[length(fixationGroups)] <- lastFixation
            lastGroup <- fixMarker
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(fixMarker, nrow(eventGroups[[gr]]))))
          }
          
          if (!anyGroupBefore | !prevGroupIsSaccade | !anyFixBefore | !fixCloseInTime | !fixCloseInSpace)
          {
            # то дополняем список фиксаций текущей фиксацией
            fixationGroups <- append(fixationGroups, eventGroups[gr])
            lastGroup <- fixMarker
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(fixMarker, nrow(eventGroups[[gr]]))))
          }
        }
      }
      # Если текущая группа сэмплов - саккада
      if (currentGroup == sacMarker)
      {
        # то вычисляем параметры maxVel и maxAccel
        maxSaccadeVel <- max(eventGroups[[gr]]$vel, na.rm = T)
        maxSaccadeAccel <- max(eventGroups[[gr]]$accel, na.rm = T)
        
        # и если саккада аномальна (включает сэмплы с аномальными значениями скорости или ускорения), 
        # то дополняем список артефактов этой саккадой
        if (maxSaccadeVel > maxVel | maxSaccadeAccel > maxAccel)
        {
          artifactGroups <- append(artifactGroups, eventGroups[gr])
          eventMarkersGroups <- append(eventMarkersGroups, list(rep(artMarker, nrow(eventGroups[[gr]]))))
        }
        #	если саккада не аномальна
        else
        {
          # то если предыдущая группа - саккада, то дополняем последнюю саккаду сэмплами текущей саккады
          if (!is.na(lastGroup) & lastGroup == sacMarker)
          {
            lastSaccade <- list(rbind(saccadeGroups[[length(saccadeGroups)]], eventGroups[[gr]]))
            saccadeGroups[length(saccadeGroups)] <- lastSaccade
            lastGroup <- sacMarker
            lastMarkers <- list(c(eventMarkersGroups[[length(eventMarkersGroups)]], rep(sacMarker, nrow(eventGroups[[gr]]))))
            eventMarkersGroups[length(eventMarkersGroups)] <- lastMarkers
          }
          else
            # иначе дополняем список саккад текущей саккадой
          {
            saccadeGroups <- append(saccadeGroups, eventGroups[gr])
            lastGroup <- sacMarker
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(sacMarker, nrow(eventGroups[[gr]]))))
          }
        }    
      }
      # Если текущая группа сэмплов - пропуск
      if (currentGroup == gapMarker)
      {
        if (classifyGaps)
        {
          # то проверяем по параметру maxGapLen, длинный ли пропуск или не длинный
          gapLen <- eventGroups[[gr]]$t[nrow(eventGroups[[gr]])]-eventGroups[[gr]]$t[1]
          # если длинный, или lastGroup = NA, или группа является последней
          # то классифицируем его как пропуск
          if (gapLen > maxGapLen | is.na(lastGroup) | gr == length(eventGroups))
          {
            gapClass <- gapMarker
          }
          # если не длинный, и группа не является последней, и группа не является первой
          # то классифицируем пропуск по близости сэмплов, смежных с пропуском 
          if (gapLen <= maxGapLen & gr != length(eventGroups) & !is.na(lastGroup))
          {
            
            if (lastGroup != gapMarker & eventGroups[[gr+1]]$evm[1] != gapMarker)
            {
              lastSmpBeforeGap <- eventGroups[[gr-1]][nrow(eventGroups[[gr-1]]),]
              firstSmpAfterGap <- eventGroups[[gr+1]][1,]
              t1 <- lastSmpBeforeGap$t
              t2 <- firstSmpAfterGap$t
              pos1 <- c(lastSmpBeforeGap$x, lastSmpBeforeGap$y)
              pos2 <- c(firstSmpAfterGap$x, firstSmpAfterGap$y)
              if (t2-t1 <= MaxTBetFix)
              {
                dist <- sqrt((pos1[1]-pos2[1])^2 + (pos1[2]-pos2[2])^2)
                # если смежные с пропуском сэмплы близки во времени и пространстве, 
                # то он классифицируется как фиксация
                if (dist <= MaxDistBetFix)
                {
                  gapClass <- fixMarker
                }
                else
                  # если не близки в пространстве, то пропуск классифицируется как саккада
                {
                  gapClass <- sacMarker
                }
              }
              else
                # если не близки во времени, то пропуск классифицируется как саккада
              {
                gapClass <- sacMarker
              }
            }
            if (lastGroup == markerNames$gap | eventGroups[[gr+1]]$evm[1] == markerNames$gap)
            {
              gapClass <- gapMarker
            }
          }
        }
        else
        {
          gapClass <- gapMarker
        }
        
        # результат классификации пропуска позволяет отнести его сэмплы к тому или иному списку событий
        # если пропуск - длинный пропуск, то пополняем список пропусков
        if (gapClass == gapMarker)
        {
          gapGroups <- append(gapGroups, eventGroups[gr])
          lastGroup <- gapMarker
          eventMarkersGroups <- append(eventMarkersGroups, list(rep(gapMarker, nrow(eventGroups[[gr]]))))
        }
        # если пропуск - фиксация
        if (gapClass == fixMarker)
        {
          # то если последняя группа - фиксация, то добавляем сэмплы пропуска в эту группу
          if (lastGroup == fixMarker)
          {
            lastFixation <- rbind(eventGroups[[gr-1]], eventGroups[[gr]])
            fixationGroups[length(fixationGroups)] <- list(lastFixation)
            lastGroup <- fixMarker
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(fixMarker, nrow(eventGroups[[gr]]))))
          }
          # иначе добавляем новую группу в список фиксаций
          else
          {
            fixationGroups <- append(fixationGroups, eventGroups[gr])
            lastGroup <- fixMarker
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(fixMarker, nrow(eventGroups[[gr]]))))
          }
        }
        # если пропуск - саккада
        if (gapClass == sacMarker)
        {
          # то если последняя группа - саккада, то добавляем сэмплы пропуска в эту группу
          if (lastGroup == sacMarker)
          {
            lastSaccade <- rbind(eventGroups[[gr-1]], eventGroups[[gr]])
            saccadeGroups[length(saccadeGroups)] <- list(lastSaccade)
            lastGroup <- sacMarker
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(sacMarker, nrow(eventGroups[[gr]]))))
          }
          # иначе добавляем новую группу в список саккад
          if (lastGroup == fixMarker | lastGroup == gapMarker)
          {
            saccadeGroups <- append(saccadeGroups, eventGroups[gr])
            lastGroup <- sacMarker
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(sacMarker, nrow(eventGroups[[gr]]))))
          }
        }
      }
    }
    eventMarkers <- unlist(eventMarkersGroups)
  }
  return(list(eventMarkers = eventMarkers, eventGroups = group))
}