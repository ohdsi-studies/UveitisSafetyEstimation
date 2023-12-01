#' @export
runEstimation <- function(connectionDetails,
                          cdmDatabaseSchema,
                          cohortDatabaseSchema,
                          cohortTable,
                          outputFolder,
                          maxCores) {

  # Run estimation =============================================================
  cmOutputFolder <- file.path(outputFolder, "cmOutput")
  if (!file.exists(cmOutputFolder)) {
    dir.create(cmOutputFolder, recursive = TRUE)
  }

  cmAnalysisListFile <- system.file("settings", "cmAnalysisList.json", package = "epi964")
  cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
  tcosList <- createTcosList()

  ParallelLogger::logError("runCmAnalyses")
  results <- CohortMethod::runCmAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    exposureDatabaseSchema = cohortDatabaseSchema,
    exposureTable = cohortTable,
    outcomeDatabaseSchema = cohortDatabaseSchema,
    outcomeTable = cohortTable,
    outputFolder = cmOutputFolder,
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = tcosList,
    getDbCohortMethodDataThreads = 1, #min(3, maxCores),
    createStudyPopThreads = min(3, maxCores),
    createPsThreads = 3, #max(1, round(maxCores / 10)),
    psCvThreads = min(10, maxCores),
    trimMatchStratifyThreads = min(5, maxCores),
    fitOutcomeModelThreads = max(1, round(maxCores / 4)),
    outcomeCvThreads = min(4, maxCores),
    refitPsForEveryOutcome = FALSE,
    outcomeIdsOfInterest = 8466
  )

  ParallelLogger::logInfo("---- Creating analysis summary -- -------------------")
  analysisSummary <- CohortMethod::summarizeAnalyses(
    referenceTable = results,
    outputFolder = cmOutputFolder
  )
  analysisSummary <- addCohortNames(analysisSummary, "outcomeId", "outcomeName")
  analysisSummary <- addCohortNames(analysisSummary, "comparatorId", "comparatorName")
  analysisSummary <- addCohortNames(analysisSummary, "targetId", "targetName")
  analysisSummary <- addAnalysisDescription(analysisSummary, "analysisId", "analysisDescription")
  analysisSummary <- dplyr::arrange(
    analysisSummary,
    analysisId,
    targetId,
    comparatorId,
    outcomeId
  )
  readr::write_csv(analysisSummary, file.path(outputFolder, "analysisSummary.csv"))

  # Compute covariate balance ==================================================

  balanceFolder <- file.path(cmOutputFolder, "balance")
  if (!file.exists(balanceFolder)) {
    dir.create(balanceFolder)
  }
  subset <- results[results$outcomeId %in% 8466, ]
  subset <- subset[subset$strataFile != "", ]
  if (nrow(subset) > 0) {
    subset <- split(subset, seq(nrow(subset)))
    cluster <- ParallelLogger::makeCluster(min(3, maxCores))
    ParallelLogger::clusterApply(
      cluster,
      subset,
      computeCovariateBalance,
      cmOutputFolder = cmOutputFolder,
      balanceFolder = balanceFolder
    )
    ParallelLogger::stopCluster(cluster)
  }
}

getNegativeControls <- function() {
  pathToCsv <- system.file("settings", "NegativeControlOutcomes.csv", package = "epi964")
  negativeControls <- readr::read_csv(pathToCsv, col_types = readr::cols())
  negativeControls$targetEffectSize <- rep(1, nrow(negativeControls))
  return(negativeControls)
}

createTcosList <- function() {
  pathToCsv <- system.file("settings", "TcosOfInterest.csv", package = "epi964")
  tcos <- readr::read_csv(pathToCsv, col_types = readr::cols())
  tcs <- unique(tcos[, c("targetId", "comparatorId"), ])
  negativeControls <- getNegativeControls()
  negativeControlsOutcomeIds <- negativeControls$outcomeId

  createTco <- function(i) {
    targetId <- tcs$targetId[i]
    comparatorId <- tcs$comparatorId[i]
    outcomeId <- tcos$outcomeId[tcos$targetId == targetId & tcos$comparatorId == comparatorId]
    outcomeIds <- c(outcomeId, negativeControlsOutcomeIds)
    excludeConceptIds <- tcos$excludedCovariateConceptIds[tcos$targetId == targetId & tcos$comparatorId == comparatorId]
    if (!is.na(excludeConceptIds)) {
      excludeConceptIds <- as.numeric(strsplit(excludeConceptIds, split = ";")[[1]])
    } else {
      excludeConceptIds <- c()
    }
    tco <- CohortMethod::createTargetComparatorOutcomes(
      targetId = targetId,
      comparatorId = comparatorId,
      outcomeIds = outcomeIds,
      excludedCovariateConceptIds = excludeConceptIds,
      includedCovariateConceptIds = c()
    )
    return(tco)
  }
  tcosList <- lapply(1:nrow(tcs), createTco)
  class(tcosList) <- "targetComparatorOutcomes"
  return(tcosList)
}

addCohortNames <- function(data,
                           IdColumnName,
                           nameColumnName) {
  pathToCsv <- system.file("settings", "exposureIndicationCohortsToCreate.csv", package = "epi964")
  exposureRef <- readr::read_csv(pathToCsv, col_types = readr::cols())
  pathToCsv <- system.file("settings", "outcomeCohortsToCreate.csv", package = "epi964")
  outcomeRef <- readr::read_csv(pathToCsv, col_types = readr::cols())
  negativeControls <- getNegativeControls()
  idToName <- data.frame(cohortId = c(exposureRef$exposureIndicationCohortId, outcomeRef$cohortId, negativeControls$outcomeId),
                         cohortName = c(exposureRef$exposureIndicationCohortName, outcomeRef$cohortName, negativeControls$outcomeName))
  idToName <- idToName[order(idToName$cohortId), ]
  idToName <- idToName[!duplicated(idToName$cohortId), ]
  names(idToName)[1] <- IdColumnName
  names(idToName)[2] <- nameColumnName
  data <- merge(data, idToName, all.x = TRUE)
  idCol <- which(colnames(data) == IdColumnName)
  if (idCol < ncol(data) - 1) {
    data <- data[, c(1:idCol, ncol(data), (idCol+1):(ncol(data)-1))]
  }
  return(data)
}

addAnalysisDescription <- function(data,
                                   IdColumnName = "analysisId",
                                   nameColumnName = "analysisDescription") {
  cmAnalysisListFile <- system.file("settings", "cmAnalysisList.json", package = "epi964")
  cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
  idToName <- lapply(cmAnalysisList, function(x) data.frame(analysisId = x$analysisId, description = as.character(x$description)))
  idToName <- do.call("rbind", idToName)
  names(idToName)[1] <- IdColumnName
  names(idToName)[2] <- nameColumnName
  data <- merge(data, idToName, all.x = TRUE)
  idCol <- which(colnames(data) == IdColumnName)
  if (idCol < ncol(data) - 1) {
    data <- data[, c(1:idCol, ncol(data), (idCol+1):(ncol(data)-1))]
  }
  return(data)
}

computeCovariateBalance <- function(row,
                                    cmOutputFolder,
                                    balanceFolder) {
  outputFileName <- file.path(balanceFolder,
                              sprintf("bal_t%s_c%s_o%s_a%s.rds",
                                      row$targetId,
                                      row$comparatorId,
                                      row$outcomeId,
                                      row$analysisId))
  if (!file.exists(outputFileName)) {
    cohortMethodDataFile <- file.path(cmOutputFolder, row$cohortMethodDataFile)
    cohortMethodData <- CohortMethod::loadCohortMethodData(cohortMethodDataFile)
    strataFile <- file.path(cmOutputFolder, row$strataFile)
    strata <- readRDS(strataFile)
    if (nrow(strata) > 0) {
      balance <- CohortMethod::computeCovariateBalance(population = strata,
                                                       cohortMethodData = cohortMethodData)
      saveRDS(balance, outputFileName)
    }
  }
}
