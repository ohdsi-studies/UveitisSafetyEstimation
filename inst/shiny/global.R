# source("inst/shiny/DataPulls.R")
# source("inst/shiny/PlotsAndTables.R")

source("DataPulls.R")
source("PlotsAndTables.R")

shinyData <- "data"

shinySettings <- list(dataFolder = shinyData, blind = FALSE)
dataFolder <- shinySettings$dataFolder
blind <- shinySettings$blind
connection <- NULL
positiveControlOutcome <- NULL

load(file.path(dataFolder, "PreMergedShinyData.RData"))

source("DataClean.R")



# code for compressing Shiny data ==============================================
#
# blind = FALSE
# dataFolder <- "G:/epi_964_7/shinyData"
# 
# splitTables <- c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")
# files <- list.files(dataFolder, pattern = ".rds")
# files <- files[!(files = grepl("ta_balance", files))] # drop ta_balance files from here
# 
# 
# # Find part to remove from all file names (usually databaseId):
# databaseFileName <- files[grepl("^database", files)]
# removeParts <- paste0(gsub("database", "", databaseFileName), "$")
# 
# # Remove data already in global environment:
# for (removePart in removeParts) {
#   tableNames <- gsub("_t[0-9]+_c[0-9]+$", "", gsub(removePart, "", files[grepl(removePart, files)]))
#   camelCaseNames <- SqlRender::snakeCaseToCamelCase(tableNames)
#   camelCaseNames <- unique(camelCaseNames)
#   camelCaseNames <- camelCaseNames[!(camelCaseNames %in% SqlRender::snakeCaseToCamelCase(splitTables))]
#   suppressWarnings(
#     rm(list = camelCaseNames)
#   )
# }
# 
# # Load data from data folder. R data objects will get names derived from the filename:
# 
# loadFile <- function(file,          # file = files[18]
#                      removePart) {  # removePart = "_Meta-analysis.rds$"
# 
#   tableName <- gsub("_t[0-9]+_c[0-9]+$", "", gsub(removePart, "", file))
#   camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
#   if (!(tableName %in% splitTables)) {
#     newData <- readRDS(file.path(dataFolder, file))
#     if (camelCaseName == "cohortMethodResult") {
#       if (removePart != "_Meta-analysis.rds$") {
#         newData$sources <- rep("", nrow(newData))
#       }
#     }
#     colnames(newData) <- SqlRender::snakeCaseToCamelCase(colnames(newData))
#     if (exists(camelCaseName, envir = .GlobalEnv)) {
#       existingData <- get(camelCaseName, envir = .GlobalEnv)
#       newData <- rbind(existingData, newData)
#       newData <- unique(newData)
#     }
#     assign(camelCaseName, newData, envir = .GlobalEnv)
#   }
#   invisible(NULL)
# }
# 
# for (removePart in removeParts) {
#   lapply(files[grepl(removePart, files)], loadFile, removePart)
# }
# 
# tcos <- unique(cohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
# tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]
# 
# 
# taFiles <- list.files(dataFolder, pattern = ".rds")
# taFiles <- taFiles[taFiles = grepl("ta_balance", taFiles)]
# 
# taBalance <- data.frame()
# for (taFile in taFiles) { # taFile <- taFiles[2]
#   taBal <- readRDS(file.path(dataFolder, taFile))
#   taBalance <- dplyr::bind_rows(taBalance, taBal)
# }
# taBalance <- unique(taBalance)
# rm(taBal)
# 
#  
# dfs <- Filter(function(x) is.data.frame(get(x)) , ls())
# save(list = dfs,
#      file = file.path(dataFolder, "PreMergedShinyData.RData"),
#      compress = TRUE,
#      compression_level = 2)
#
# # Copy splittable tables ------------------------------------------------------------------------------
# toLocation <- "S:/Git/Bitbucket/epi_964/inst/shiny/data"
# toCopy <- files[grepl(paste(splitTables, collapse = "|"), files)]
# file.copy(from = file.path(dataFolder, toCopy),
#           to = file.path(toLocation, toCopy))


