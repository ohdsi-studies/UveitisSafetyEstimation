#' @export
assessDiagnostics <- function(studyFolder) {

  shinyDataFolder <- file.path(studyFolder, "shinyData")
  source("inst/shiny/global.R")

  os <- cohortMethodResult$outcomeId %in% outcomeOfInterest$outcomeId
  rows <- unique(cohortMethodResult[os, c("databaseId",
                                          "analysisId",
                                          "targetId",
                                          "comparatorId",
                                          "outcomeId")])
  rowsList <- split(rows, seq(nrow(rows)))


  # 0 events ===================================================================
  alpha <- 0.05
  power <- 0.8
  z1MinAlpha <- qnorm(1 - alpha/2)
  zBeta <- -qnorm(1 - power)
  pA <- cohortMethodResult$targetSubjects/(cohortMethodResult$targetSubjects + cohortMethodResult$comparatorSubjects)
  pB <- 1 - pA
  totalEvents <- abs(cohortMethodResult$targetOutcomes) + abs(cohortMethodResult$comparatorOutcomes)
  cohortMethodResult$mdrr <- exp(sqrt((zBeta + z1MinAlpha)^2/(totalEvents * pA * pB)))
  mdrr <- merge(rows, cohortMethodResult[, c("databaseId",
                                             "analysisId",
                                             "targetId",
                                             "comparatorId",
                                             "outcomeId",
                                             "targetOutcomes",
                                             "comparatorOutcomes",
                                             "mdrr")])
  mdrr$mdrrPass <- ifelse(mdrr$targetOutcomes == 0 | mdrr$comparatorOutcomes == 0, 0, 1)


  # Covariate balance ============================================================
  getAsmd <- function(row) {
    balance <- getCovariateBalance(
      connection = connection,
      targetId = row$targetId,
      comparatorId = row$comparatorId,
      databaseId = row$databaseId,
      analysisId = row$analysisId,
      outcomeId = row$outcomeId
    )
    maxAsmd <- max(balance$absAfterMatchingStdDiff)
    row$maxAsmd <- maxAsmd
    row$asmdPass <- ifelse(maxAsmd > 0.1, 0, 1)
    return(row)
  }
  asmd <- lapply(rowsList, getAsmd)
  asmd <- dplyr::bind_rows(asmd)

  # Equipoise ==================================================================
  getEquipoise <- function(row) { # row <- rowsList[[1]]
    patientCounts <- getAttrition(
      connection = connection,
      targetId = row$targetId,
      comparatorId = row$comparatorId,
      outcomeId = row$outcomeId,
      analysisId = row$analysisId,
      databaseId = row$databaseId
    )
    targetSize <- patientCounts$targetPersons[patientCounts$description == "Have at least 1 days at risk"]
    comparatorSize <- patientCounts$comparatorPersons[patientCounts$description == "Have at least 1 days at risk"]
    ps <- getPs(
      connection = connection,
      targetIds = row$targetId,
      comparatorIds = row$comparatorId,
      analysisId = row$analysisId,
      databaseId = row$databaseId
    )
    if (nrow(ps) == 0) {
      row$inEquipoise <- NA
      row$equipoisePass <- NA
      return(row)
    } else {
      psFiltered <- ps[ps$preferenceScore >= 0.3 & ps$preferenceScore <= 0.7, ]
      targetFraction <- sum(psFiltered$targetDensity) / sum(ps$targetDensity)
      comparatorFraction <- sum(psFiltered$comparatorDensity) / sum(ps$comparatorDensity)
      totalFraction <- (targetFraction * targetSize + comparatorFraction * comparatorSize) / (targetSize + comparatorSize)
      row$inEquipoise <- totalFraction
      row$equipoisePass <- ifelse(totalFraction > 0.35, 1, 0)
      return(row)
    }
  }
  equipoise <- lapply(rowsList, getEquipoise)
  equipoise <- dplyr::bind_rows(equipoise)

  # EASE =======================================================================
  getEase <- function(row) {
    ncResults <- cohortMethodResult %>%
      dplyr::filter(
        databaseId == row$databaseId &
        analysisId == row$analysisId &
        targetId == row$targetId &
        comparatorId == row$comparatorId &
        outcomeId != outcomeOfInterest$outcomeId
      ) %>%
      dplyr::select(
        logRr,
        seLogRr
      )
    ncResults <- ncResults[!is.na(ncResults$seLogRr), ]
    nullDist <- EmpiricalCalibration::fitNull(ncResults$logRr, ncResults$seLogRr)
    ease <- EmpiricalCalibration::computeExpectedAbsoluteSystematicError(nullDist)
    row$ease <- ease
    row$easePass <- ifelse(ease > 0.25, 0, 1)
    return(row)
  }
  ease <- lapply(rowsList, getEase)
  ease <- dplyr::bind_rows(ease)

  # Combine ====================================================================

  diagnosticsResults <- dplyr::inner_join(mdrr, asmd)
  diagnosticsResults <- dplyr::inner_join(diagnosticsResults, equipoise)
  diagnosticsResults <- dplyr::inner_join(diagnosticsResults, ease)

  diagnosticsResults$diagnosticsPassed <- diagnosticsResults$mdrrPass +
    diagnosticsResults$asmdPass + diagnosticsResults$equipoisePass +
    diagnosticsResults$easePass

  diagnosticsResults <- addCohortNames(diagnosticsResults, "outcomeId", "outcomeName")
  diagnosticsResults <- addCohortNames(diagnosticsResults, "comparatorId", "comparatorName")
  diagnosticsResults <- addCohortNames(diagnosticsResults, "targetId", "targetName")
  diagnosticsResults <- addAnalysisDescription(diagnosticsResults, "analysisId", "analysisDescription")
  diagnosticsResults <- dplyr::arrange(diagnosticsResults, databaseId, analysisId, targetId, comparatorId, outcomeId) %>%
    dplyr::relocate(databaseId)

  # save for viewing as spreadsheet
  readr::write_csv(diagnosticsResults, file.path(studyFolder, "diagnosticsResults.csv"))

  # save for filtering in shiny app
  diagnosticsResults <- SqlRender::camelCaseToSnakeCaseNames(diagnosticsResults)
  saveRDS(diagnosticsResults, file.path(shinyDataFolder, "diagnostics_results.rds"))
}


getNegativeControls <- function() {
  pathToCsv <- system.file("settings", "NegativeControlOutcomes.csv", package = "epi964")
  negativeControls <- readr::read_csv(pathToCsv, col_types = readr::cols())
  negativeControls$targetEffectSize <- rep(1, nrow(negativeControls))
  return(negativeControls)
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

