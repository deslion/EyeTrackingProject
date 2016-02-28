ANH <- function(t, x, y, settings) {
  # Чтение настроек
  screenDist     <- settings$screenDist
  screenDim      <- settings$screenSize
  screenSize     <- settings$screenResolution
  # Возможно, стоит убрать фильтр из настроек
  filter         <- settings$filter
  filterOrder    <- settings$filterOrder
  # -----------------------------------------
  maxSaccadeVel  <- settings$maxSaccadeVel
  maxSaccadeAcc  <- settings$maxSaccadeAcc
  minSaccadeDur  <- settings$minSaccadeDur
  minFixationDur <- settings$minFixationDur
  PT0            <- settings$PT0
  tolerance      <- settings$tolerance
  markerNames    <- settings$markerNames
  print("ANH started work correctly.")
  
  filterMarkers <- settings$filterMarkers
  
  filterOkMarker <- 1
  filterGapMarker <- 2
  filterArtMarker <- 3
  
  fixMarker <- 1
  sacMarker <- 2
  gliMarker <- 3
  smpMarker <- 4
  gapMarker <- 5
  artMarker <- 6
  
  size <- length(t)
  vel <- calcVel(t, x, y, settings)
  windowSize <- as.integer(minFixationDur/mean(vel$dts))
  rawEvM <- ifelse(filterMarkers != filterOkMarker, gapMarker, filterOkMarker) 
  
  
  print("CHKPNT 1")
  
  
  
  #########################################################
  ###Peak velocity Threshold. Data-driven algorithm     ###
  ###Функция вычисляет порог скорости по исходным данным###
  #########################################################
  getThreshold <- function(Vel, sigmas) {
    print("getThreshold started work correctly")
    PT <- PT0
    repeat {
      PTcur <- PT; velos <- Vel[which(Vel<PTcur)]
      if (length(velos) > windowSize) {
        m <- mean(velos); s <- sd(velos)
      }
      else {
        m <- mean(Vel); s <- sd(Vel)
      }
      PT <- m + sigmas*s
      if (abs(PT - PTcur) < tolerance) break
    }
    print("getThreshold finished work correctly")
    return(PT)
  }
  
  print("CHKPNT 2")
  print(table(rawEvM))
  
  PT <- getThreshold(vel$vels[which(rawEvM==filterOkMarker)],6); 
  
  print("CHKPNT 3")
  #######################################################
  ###              Определение саккад                 ###
  ###Поиск пиков скорости, onset-ов и offset-ов саккад###
  #######################################################
  #Находим номера сэмплов начала пика и конца пика
  above_Threshold <- (vel$vels > PT)
  peaks <- which(above_Threshold[-1]!=above_Threshold[-length(above_Threshold)])
  if (peaks[1]<=windowSize) peaks <- peaks[-c(1:2)]
  #Находим onset-ы
  STon <- getThreshold(vel$vels[which(rawEvM==filterOkMarker)],3)
 
  print("CHKPNT 4")
  
  
  leftmost_peaks <- peaks[seq(1,length(peaks),2)]
  onsets <- c()
  for (i in leftmost_peaks){
    nsample <- i
    if (nsample <= windowSize) break
    repeat {
      if (vel$vels[nsample] < STon) 
        if ((vel$vels[nsample]-vel$vels[nsample-1]) <= 0) {
          onsets <- c(onsets, nsample)
          break 
        }
        else if((nsample-1)==0) break
        else nsample <- nsample - 1
      else nsample <- nsample - 1
    }
  }
  
  
  print("CHKPNT 5")
  
  
  #Находим offset-ы
  rightmost_peaks <- peaks[seq(2,length(peaks),2)]
  offsets <- c()
  offset_Thresholds <- c()
  alpha <- 0.7; beta <- 0.3
  for (i in 1:length(rightmost_peaks)) {
    nsample <- rightmost_peaks[i]
    from <- (onsets[i]-windowSize)
    to <- onsets[i]
    LNT <- getThreshold(vel$vels[from:to],3)
    
    SToff <- alpha*STon + beta*LNT
    
    offset_Thresholds <- c(offset_Thresholds, SToff)
    
    repeat {
      if (vel$vels[nsample] < SToff) {
        if ((vel$vels[nsample] - vel$vels[nsample+1]) <=0) {
          offsets <- c(offsets, nsample)
          break
        }
        else {
          nsample <- nsample + 1
        }
      }
      else {
        nsample <- nsample + 1
      }
      if (nsample >= size) {
        break
      }
    }
  }

  
  print("CHKPNT 6")
  
  print(paste(length(onsets),length(offsets)))
  #Определяем саккады
  for (i in 1:length(offsets))
  {
    indexes <- onsets[i]:offsets[i]
    print(paste("First index is", indexes[1], "last is", tail(indexes,1)))
    correctDuration <- (sum(vel$dts[indexes])>=minSaccadeDur)
    print(paste("Duration is correct:", correctDuration))
    correctHighestVel <- (max(vel$vels[indexes])<=maxSaccadeVel)
    print(paste("Highest velocity is correct:", correctHighestVel))
    correctHighestAcc <- (max(vel$accels[indexes])<=maxSaccadeAcc)
    print(paste("Highest acceleration is correct:", correctHighestAcc))
    if (correctHighestVel & correctHighestAcc)
      if (correctDuration){
        print(rawEvM[indexes])
        print(gapMarker)
        rawEvM[indexes] <- sacMarker
      }
      else {}
    else
      rawEvM[indexes] <- gapMarker
  }
  
  
  print("CHKPNT 7")
  
  
  ###########################
  ### Определение глиссад ###
  ###########################
  #Определяем глиссады
  for (i in 1:length(offsets)) {
    n <- ifelse((offsets[i]+windowSize)>=size,size-1,offsets[i]+windowSize)
    for (j in offsets[i]:n) {
      if (rawEvM[j]==filterOkMarker) 
        rawEvM[j] <- ifelse((vel$vels[j] > PT), 
                            gliMarker, 
                            ifelse(vel$vels[j] > offset_Thresholds[i], 
                                   gliMarker, 
                                   gapMarker))
    }
  }
  ############################
  ### Определение фиксаций ###
  ############################
  rawEvM <- ifelse(rawEvM == filterOkMarker, fixMarker, rawEvM)
#   rawEvM <- append(rawEvM, gapMarker)
  #Итоговый набор маркеров
  print("ANH finished work correctly!")
  group <- markersGroups(rawEvM)
  return(list(eventMarkers = rawEvM, eventGroups = group))
}