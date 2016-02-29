## It's meant ..\\EyeTrackingProject\\ is working directory
## Source
source("Shiny\\Functions\\miscFunctions.R", local = T)
source("Shiny\\Functions\\dataLoaders.R", local = T)
source("Shiny\\Classes\\optionsAndSettingsClasses.R", local = T)
source("Shiny\\Classes\\baseEyeDataClasses.R", local = T)
source("Shiny\\Classes\\baseClasses.R", local = T)
source("Shiny\\Classes\\listsAndTablesClasses.R", local = T)
source("Shiny\\Classes\\Events\\filterEvent.R", local = T)
source("Shiny\\Classes\\Events\\oculomotorEvent.R", local = T)
source("Shiny\\Classes\\Events\\AOIEvent.R", local = T)
source("Shiny\\Classes\\Events\\syncEvent.R", local = T)
source("Shiny\\Classes\\Events\\frameEvent.R", local = T)
source("Shiny\\Classes\\Events\\windowEvent.R", local = T)
source("Shiny\\Classes\\representations.R", local = T)
source("Shiny\\Functions\\dataParsers.R", local = T)
source("Shiny\\Functions\\filters.R", local = T)
source("Shiny\\Functions\\smoothers.R", local = T)
source("Shiny\\Functions\\detectors.R", local = T)
source("Shiny\\Functions\\eventAnalyzersNew.R", local = T)
source("Shiny\\Functions\\estimatorsNew.R", local = T)
source("Shiny\\Functions\\DataRecordSubFunctions.R", local = T)
source("Shiny\\Functions\\GeneralEventSubFunctions.R", local = T)
source("Shiny\\CoreSubFunctionsInit.R", local = T)
source("Shiny\\Functions\\eventAnalyzersNew.R", local = T)
source("Shiny\\Methods\\Methods_v_1_7.R", local = T)
## Some troubles with loading eventSelector
# source("Shiny\\Functions\\eventSelector.R", local = T)

## Libraries
library(grDevices)
library(data.table)
library(signal)

## Variables
maindir <- ""