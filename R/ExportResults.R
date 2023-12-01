#' @export
exportResults <- function(outputFolder,
                          databaseId,
                          minCellCount = 5) {

  cmOutputFolder <- file.path(outputFolder, "cmOutput")
  exportFolder <- file.path(cmOutputFolder, "export")
  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }

  # Run export functions =======================================================

  exportAnalyses(cmOutputFolder, exportFolder, databaseId)
  exportExposures(cmOutputFolder, exportFolder, databaseId)
  exportOutcomes(cmOutputFolder, exportFolder, databaseId)
  exportMetaData(cmOutputFolder, exportFolder, databaseId, minCellCount)
  exportMainResults(cmOutputFolder, exportFolder, databaseId, minCellCount)
  exportDiagnostics(cmOutputFolder, exportFolder, databaseId, minCellCount)
  exportTargetAnalyticBalance(cmOutputFolder, exportFolder, databaseId)

}


  # Export functions ===========================================================

exportAnalyses <- function(cmOutputFolder,
                           exportFolder,
                           databaseId) {
  ParallelLogger::logInfo(" ==== Exporting analyses ==========================")

  ParallelLogger::logInfo(" ---- cohort_method_analysis table ----------------")
  tempFileName <- tempfile()
  cmAnalysisListFile <- system.file("settings", "cmAnalysisList.json", package = "epi964")
  cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)

  cmAnalysisToRow <- function(cmAnalysis) {
    ParallelLogger::saveSettingsToJson(cmAnalysis, tempFileName)
    row <- tibble::tibble(analysisId = cmAnalysis$analysisId,
                          description = cmAnalysis$description)
    return(row)
  }
  cohortMethodAnalysis <- lapply(cmAnalysisList, cmAnalysisToRow)
  cohortMethodAnalysis <- do.call("rbind", cohortMethodAnalysis)
  cohortMethodAnalysis <- unique(cohortMethodAnalysis)
  unlink(tempFileName)
  colnames(cohortMethodAnalysis) <- SqlRender::camelCaseToSnakeCase(colnames(cohortMethodAnalysis))
  fileName <- file.path(exportFolder, "cohort_method_analysis.csv")
  readr::write_csv(cohortMethodAnalysis, fileName)

  ParallelLogger::logInfo(" ---- covariate_analysis table --------------------")
  reference <- readRDS(file.path(cmOutputFolder, "outcomeModelReference.rds"))

  getCovariateAnalyses <- function(cmAnalysis) {
    cmDataFile <- reference$cohortMethodDataFile[reference$analysisId == cmAnalysis$analysisId][1]
    cmData <- CohortMethod::loadCohortMethodData(file.path(cmOutputFolder, cmDataFile))
    covariateAnalysis <- dplyr::collect(cmData$analysisRef)
    covariateAnalysis <- covariateAnalysis[, c("analysisId", "analysisName")]
    colnames(covariateAnalysis) <- c("covariate_analysis_id", "covariate_analysis_name")
    covariateAnalysis$analysis_id <- cmAnalysis$analysisId
    return(covariateAnalysis)
  }
  covariateAnalysis <- lapply(cmAnalysisList, getCovariateAnalyses)
  covariateAnalysis <- do.call("rbind", covariateAnalysis)
  fileName <- file.path(exportFolder, "covariate_analysis.csv")
  readr::write_csv(covariateAnalysis, fileName)
}

exportExposures <- function(cmOutputFolder,
                            exportFolder,
                            databaseId) {
  ParallelLogger::logInfo(" ==== Exporting exposures ===========================")

  ParallelLogger::logInfo(" ---- exposure_of_interest table --------------------")
  tcoRefFile <- system.file("settings", "TcosOfInterest.csv", package = "epi964")
  tcosRef <- readr::read_csv(tcoRefFile, col_types = readr::cols())

  exposureRefFile <- system.file("settings", "exposureIndicationCohortsToCreate.csv", package = "epi964")
  exposureRef <- readr::read_csv(exposureRefFile, col_types = readr::cols())

  createExposureRow <- function(exposureIndicationCohortId) {
    exposureIndicationCohortName <- as.character(exposureRef$exposureIndicationCohortName[exposureRef$exposureIndicationCohortId == exposureIndicationCohortId])
    return(tibble::tibble(exposureId = exposureIndicationCohortId,
                          exposureName = exposureIndicationCohortName))
  }
  exposureOfInterest <- unique(c(tcosRef$targetId, tcosRef$comparatorId))
  exposureOfInterest <- lapply(exposureOfInterest, createExposureRow)
  exposureOfInterest <- do.call("rbind", exposureOfInterest)
  colnames(exposureOfInterest) <- SqlRender::camelCaseToSnakeCase(colnames(exposureOfInterest))
  fileName <- file.path(exportFolder, "exposure_of_interest.csv")
  readr::write_csv(exposureOfInterest, fileName)
}

exportOutcomes <- function(cmOutputFolder,
                           exportFolder,
                           databaseId) {
  ParallelLogger::logInfo(" ==== Exporting outcomes ============================")

  ParallelLogger::logInfo(" ---- outcome_of_interest table ---------------------")
  outcomeRefFile <- system.file("settings", "outcomeCohortsToCreate.csv", package = "epi964")
  outcomeRef <- readr::read_csv(outcomeRefFile, col_types = readr::cols())

  createOutcomeRow <- function(outcomeCohortId) {
    outcomeCohortName <- as.character(outcomeRef$cohortName[outcomeRef$cohortId == outcomeCohortId])
    return(tibble::tibble(outcomeId = outcomeCohortId,
                          outcomeName = outcomeCohortName))
  }
  outcomeOfInterest <- lapply(outcomeRef$cohortId, createOutcomeRow)
  outcomeOfInterest <- do.call("rbind", outcomeOfInterest)
  colnames(outcomeOfInterest) <- SqlRender::camelCaseToSnakeCase(colnames(outcomeOfInterest))
  fileName <- file.path(exportFolder, "outcome_of_interest.csv")
  readr::write_csv(outcomeOfInterest, fileName)

  ParallelLogger::logInfo(" ---- negative_control_outcome table ---------------------")
  negControlsFile <- system.file("settings", "NegativeControlOutcomes.csv", package = "epi964")
  negativeControls <- readr::read_csv(negControlsFile, col_types = readr::cols())
  negativeControls <- negativeControls[, c("outcomeId", "outcomeName")]
  colnames(negativeControls) <- SqlRender::camelCaseToSnakeCase(colnames(negativeControls))
  fileName <- file.path(exportFolder, "negative_control_outcome.csv")
  readr::write_csv(negativeControls, fileName)
}

exportMetaData <- function(cmOutputFolder,
                           exportFolder,
                           databaseId,
                           minCellCount) {
  ParallelLogger::logInfo(" ==== Exporting metadata ==========================")

  ParallelLogger::logInfo(" ---- database table ------------------------------")
  database <- tibble::tibble(database_id = databaseId,
                             database_name = databaseId,
                             is_meta_analysis = 0)
  fileName <- file.path(exportFolder, "database.csv")
  readr::write_csv(database, fileName)

  ParallelLogger::logInfo("---- attrition table ------------------------------")
  fileName <- file.path(exportFolder, "attrition.csv")
  if (file.exists(fileName)) {
    unlink(fileName)
  }
  outcomeRefFile <- system.file("settings", "outcomeCohortsToCreate.csv", package = "epi964")
  outcomeRef <- readr::read_csv(outcomeRefFile, col_types = readr::cols())

  outcomesOfInterest <- outcomeRef$cohortId
  reference <- readRDS(file.path(cmOutputFolder, "outcomeModelReference.rds"))
  reference <- reference[reference$outcomeId %in% outcomesOfInterest, ]
  first <- !file.exists(fileName)
  pb <- txtProgressBar(style = 3)

  for (i in 1:nrow(reference)) {
    outcomeModel <- readRDS(file.path(cmOutputFolder, reference$outcomeModelFile[i]))
    attrition <- outcomeModel$attrition[, c("description", "targetPersons", "comparatorPersons")]
    attrition$sequenceNumber <- 1:nrow(attrition)
    attrition1 <- attrition[, c("sequenceNumber", "description", "targetPersons")]
    colnames(attrition1)[3] <- "subjects"
    attrition1$exposureId <- reference$targetId[i]
    attrition2 <- attrition[, c("sequenceNumber", "description", "comparatorPersons")]
    colnames(attrition2)[3] <- "subjects"
    attrition2$exposureId <- reference$comparatorId[i]
    attrition <- rbind(attrition1, attrition2)
    attrition$targetId <- reference$targetId[i]
    attrition$comparatorId <- reference$comparatorId[i]
    attrition$analysisId <- reference$analysisId[i]
    attrition$outcomeId <- reference$outcomeId[i]
    attrition$databaseId <- databaseId
    attrition <- attrition[, c("databaseId",
                               "exposureId",
                               "targetId",
                               "comparatorId",
                               "outcomeId",
                               "analysisId",
                               "sequenceNumber",
                               "description",
                               "subjects")]
    attrition <- enforceMinCellValue(attrition, "subjects", minCellCount, silent = TRUE)

    colnames(attrition) <- SqlRender::camelCaseToSnakeCase(colnames(attrition))
    write.table(x = attrition,
                file = fileName,
                row.names = FALSE,
                col.names = first,
                sep = ",",
                dec = ".",
                qmethod = "double",
                append = !first)
    first <- FALSE
    if (i %% 100 == 10) {
      setTxtProgressBar(pb, i/nrow(reference))
    }
  }
  setTxtProgressBar(pb, 1)
  close(pb)


  ParallelLogger::logInfo(" ---- covariate table -----------------------------")
  reference <- readRDS(file.path(cmOutputFolder, "outcomeModelReference.rds"))

  getCovariates <- function(analysisId) {
    cmDataFile <- reference$cohortMethodDataFile[reference$analysisId == analysisId][1]
    cmData <- CohortMethod::loadCohortMethodData(file.path(cmOutputFolder, cmDataFile))
    covariateRef <- dplyr::collect(cmData$covariateRef)
    covariateRef <- covariateRef[, c("covariateId", "covariateName", "analysisId")]
    colnames(covariateRef) <- c("covariateId", "covariateName", "covariateAnalysisId")
    covariateRef$analysisId <- analysisId
    return(covariateRef)
  }
  covariates <- lapply(unique(reference$analysisId), getCovariates)
  covariates <- do.call("rbind", covariates)
  covariates$databaseId <- databaseId
  colnames(covariates) <- SqlRender::camelCaseToSnakeCase(colnames(covariates))
  fileName <- file.path(exportFolder, "covariate.csv")
  readr::write_csv(covariates, fileName)
  rm(covariates)

  ParallelLogger::logInfo(" ---- cm_follow_up_dist table --------------------")
  getCmFollowUpDist <- function(i) {
    if (reference$strataFile[i] == "") {
      strataPop <- readRDS(file.path(cmOutputFolder, reference$studyPopFile[i]))
    } else {
      strataPop <- readRDS(file.path(cmOutputFolder, reference$strataFile[i]))
    }
    targetDist <- quantile(strataPop$survivalTime[strataPop$treatment == 1],
                           c(0, 0.1, 0.25, 0.5, 0.85, 0.9, 1))
    comparatorDist <- quantile(strataPop$survivalTime[strataPop$treatment == 0],
                               c(0, 0.1, 0.25, 0.5, 0.85, 0.9, 1))
    row <- tibble::tibble(target_id = reference$targetId[i],
                          comparator_id = reference$comparatorId[i],
                          outcome_id = reference$outcomeId[i],
                          analysis_id = reference$analysisId[i],
                          target_min_days = targetDist[1],
                          target_p10_days = targetDist[2],
                          target_p25_days = targetDist[3],
                          target_median_days = targetDist[4],
                          target_p75_days = targetDist[5],
                          target_p90_days = targetDist[6],
                          target_max_days = targetDist[7],
                          comparator_min_days = comparatorDist[1],
                          comparator_p10_days = comparatorDist[2],
                          comparator_p25_days = comparatorDist[3],
                          comparator_median_days = comparatorDist[4],
                          comparator_p75_days = comparatorDist[5],
                          comparator_p90_days = comparatorDist[6],
                          comparator_max_days = comparatorDist[7])
    return(row)
  }
  outcomesOfInterest <- 8466
  reference <- readRDS(file.path(cmOutputFolder, "outcomeModelReference.rds"))
  reference <- reference[reference$outcomeId %in% outcomesOfInterest, ]
  results <- plyr::llply(1:nrow(reference), getCmFollowUpDist, .progress = "text")
  results <- do.call("rbind", results)
  results$database_id <- databaseId
  fileName <- file.path(exportFolder, "cm_follow_up_dist.csv")
  readr::write_csv(results, fileName)
  rm(results)
}

exportMainResults <- function(cmOutputFolder,
                              exportFolder,
                              databaseId,
                              minCellCount) {
  ParallelLogger::logInfo(" ==== Exporting main results ======================")

  ParallelLogger::logInfo("---- cohort_method_result table -------------------")

  analysesSum <- readr::read_csv(file.path(outputFolder, "analysisSummary.csv"), col_types = readr::cols())
  negativeControls <- getNegativeControls()

  ParallelLogger::logInfo("---- performing empirical calibration on main effets")
  cluster <- ParallelLogger::makeCluster(1)
  subsets <- split(analysesSum,
                   paste(analysesSum$targetId, analysesSum$comparatorId, analysesSum$analysisId))
  rm(analysesSum)
  results <- ParallelLogger::clusterApply(cluster,
                                          subsets,
                                          calibrate,
                                          allControls = negativeControls)
  ParallelLogger::stopCluster(cluster)
  rm(subsets)
  results <- dplyr::bind_rows(results)
  results$databaseId <- databaseId
  results <- enforceMinCellValue(results, "targetSubjects", minCellCount)
  results <- enforceMinCellValue(results, "comparatorSubjects", minCellCount)
  results <- enforceMinCellValue(results, "targetOutcomes", minCellCount)
  results <- enforceMinCellValue(results, "comparatorOutcomes", minCellCount)
  colnames(results) <- SqlRender::camelCaseToSnakeCase(colnames(results))
  fileName <- file.path(exportFolder, "cohort_method_result.csv")
  readr::write_csv(results, fileName)
  rm(results)
}

exportDiagnostics <- function(cmOutputFolder,
                              exportFolder,
                              databaseId,
                              minCellCount) {
  ParallelLogger::logInfo(" ==== Exporting diagnostics =======================")

  ParallelLogger::logInfo("---- covariate_balance table ----------------------")
  fileName <- file.path(exportFolder, "covariate_balance.csv")
  if (file.exists(fileName)) {
    unlink(fileName)
  }
  first <- TRUE
  balanceFolder <- file.path(cmOutputFolder, "balance")
  files <- list.files(balanceFolder, pattern = "bal_.*.rds", full.names = TRUE)
  pb <- txtProgressBar(style = 3)
  for (i in 1:length(files)) {
    ids <- gsub("^.*bal_t", "", files[i])
    targetId <- as.numeric(gsub("_c.*", "", ids))
    ids <- gsub("^.*_c", "", ids)
    comparatorId <- as.numeric(gsub("_[aso].*$", "", ids))
    if (grepl("_s", ids)) {
      subgroupId <- as.numeric(gsub("^.*_s", "", gsub("_a[0-9]*.rds", "", ids)))
    } else {
      subgroupId <- NA
    }
    if (grepl("_o", ids)) {
      outcomeId <- as.numeric(gsub("^.*_o", "", gsub("_a[0-9]*.rds", "", ids)))
    } else {
      outcomeId <- NA
    }
    ids <- gsub("^.*_a", "", ids)
    analysisId <- as.numeric(gsub(".rds", "", ids))
    balance <- readRDS(files[i])

    inferredTargetBeforeSize <- mean(balance$beforeMatchingSumTarget/balance$beforeMatchingMeanTarget, na.rm = TRUE)
    inferredComparatorBeforeSize <- mean(balance$beforeMatchingSumComparator/balance$beforeMatchingMeanComparator, na.rm = TRUE)
    inferredTargetAfterSize <- mean(balance$afterMatchingSumTarget/balance$afterMatchingMeanTarget, na.rm = TRUE)
    inferredComparatorAfterSize <- mean(balance$afterMatchingSumComparator/balance$afterMatchingMeanComparator, na.rm = TRUE)

    balance$databaseId <- databaseId
    balance$targetId <- targetId
    balance$comparatorId <- comparatorId
    balance$outcomeId <- outcomeId
    balance$analysisId <- analysisId
    balance$interactionCovariateId <- subgroupId
    balance <- balance[, c("databaseId",
                           "targetId",
                           "comparatorId",
                           "outcomeId",
                           "analysisId",
                           "interactionCovariateId",
                           "covariateId",
                           "beforeMatchingMeanTarget",
                           "beforeMatchingMeanComparator",
                           "beforeMatchingStdDiff",
                           "afterMatchingMeanTarget",
                           "afterMatchingMeanComparator",
                           "afterMatchingStdDiff")]
    colnames(balance) <- c("databaseId",
                           "targetId",
                           "comparatorId",
                           "outcomeId",
                           "analysisId",
                           "interactionCovariateId",
                           "covariateId",
                           "targetMeanBefore",
                           "comparatorMeanBefore",
                           "stdDiffBefore",
                           "targetMeanAfter",
                           "comparatorMeanAfter",
                           "stdDiffAfter")
    balance$targetMeanBefore[is.na(balance$targetMeanBefore)] <- 0
    balance$comparatorMeanBefore[is.na(balance$comparatorMeanBefore)] <- 0
    balance$stdDiffBefore <- round(balance$stdDiffBefore, 3)
    balance$targetMeanAfter[is.na(balance$targetMeanAfter)] <- 0
    balance$comparatorMeanAfter[is.na(balance$comparatorMeanAfter)] <- 0
    balance$stdDiffAfter <- round(balance$stdDiffAfter, 3)
    balance <- enforceMinCellValue(balance, "targetMeanBefore", minCellCount/inferredTargetBeforeSize, TRUE)
    balance <- enforceMinCellValue(balance, "comparatorMeanBefore", minCellCount/inferredComparatorBeforeSize, TRUE)
    balance <- enforceMinCellValue(balance, "targetMeanAfter", minCellCount/inferredTargetAfterSize, TRUE)
    balance <- enforceMinCellValue(balance, "comparatorMeanAfter", minCellCount/inferredComparatorAfterSize, TRUE)
    balance$targetMeanBefore <- round(balance$targetMeanBefore, 3)
    balance$comparatorMeanBefore <- round(balance$comparatorMeanBefore, 3)
    balance$targetMeanAfter <- round(balance$targetMeanAfter, 3)
    balance$comparatorMeanAfter <- round(balance$comparatorMeanAfter, 3)
    balance <- balance[balance$targetMeanBefore != 0 &
                         balance$comparatorMeanBefore != 0 &
                         balance$targetMeanAfter != 0 &
                         balance$comparatorMeanAfter != 0 &
                         balance$stdDiffBefore != 0 &
                         balance$stdDiffAfter != 0, ]
    balance <- balance[!is.na(balance$targetId), ]
    colnames(balance) <- SqlRender::camelCaseToSnakeCase(colnames(balance))
    write.table(x = balance,
                file = fileName,
                row.names = FALSE,
                col.names = first,
                sep = ",",
                dec = ".",
                qmethod = "double",
                append = !first)
    first <- FALSE
    setTxtProgressBar(pb, i/length(files))
  }
  close(pb)


  ParallelLogger::logInfo("---- preference_score_dist table ---------")
  reference <- readRDS(file.path(cmOutputFolder, "outcomeModelReference.rds"))
  preparePlot <- function(row,
                          reference) {
    idx <- reference$analysisId == row$analysisId &
      reference$targetId == row$targetId &
      reference$comparatorId == row$comparatorId
    psFileName <- file.path(cmOutputFolder, reference$sharedPsFile[idx][1])
    if (file.exists(psFileName)) {
      ps <- readRDS(psFileName)
      if (min(ps$propensityScore) < max(ps$propensityScore)) {
        ps <- CohortMethod:::computePreferenceScore(ps)

        d1 <- density(ps$preferenceScore[ps$treatment == 1], from = 0, to = 1, n = 100)
        d0 <- density(ps$preferenceScore[ps$treatment == 0], from = 0, to = 1, n = 100)

        result <- tibble::tibble(databaseId = databaseId,
                                 targetId = row$targetId,
                                 comparatorId = row$comparatorId,
                                 analysisId = row$analysisId,
                                 preferenceScore = d1$x,
                                 targetDensity = d1$y,
                                 comparatorDensity = d0$y)
        return(result)
      }
    }
    return(NULL)
  }
  subset <- unique(reference[reference$sharedPsFile != "", c("targetId", "comparatorId", "analysisId")])
  data <- plyr::llply(split(subset, 1:nrow(subset)),
                      preparePlot,
                      reference = reference,
                      .progress = "text")
  data <- dplyr::bind_rows(data)
  fileName <- file.path(exportFolder, "preference_score_dist.csv")
  if (!is.null(data)) {
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  }
  readr::write_csv(data, fileName)


  ParallelLogger::logInfo("---- propensity_model table ---------")
  getPsModel <- function(row, reference) {
    idx <- reference$analysisId == row$analysisId &
      reference$targetId == row$targetId &
      reference$comparatorId == row$comparatorId
    psFileName <- file.path(cmOutputFolder, reference$sharedPsFile[idx][1])
    if (file.exists(psFileName)) {
      ps <- readRDS(psFileName)
      metaData <- attr(ps, "metaData")
      if (is.null(metaData$psError)) {
        cmDataFile <- file.path(cmOutputFolder, reference$cohortMethodDataFile[idx][1])
        cmData <- CohortMethod::loadCohortMethodData(cmDataFile)
        model <- CohortMethod::getPsModel(ps, cmData)
        model$covariateId[is.na(model$covariateId)] <- 0
        Andromeda::close(cmData)
        model$databaseId <- databaseId
        model$targetId <- row$targetId
        model$comparatorId <- row$comparatorId
        model$analysisId <- row$analysisId
        model <- model[, c("databaseId", "targetId", "comparatorId", "analysisId", "covariateId", "coefficient")]
        return(model)
      }
    }
    return(NULL)
  }
  subset <- unique(reference[reference$sharedPsFile != "", c("targetId", "comparatorId", "analysisId")])
  data <- plyr::llply(split(subset, 1:nrow(subset)),
                      getPsModel,
                      reference = reference,
                      .progress = "text")
  data <- dplyr::bind_rows(data)
  fileName <- file.path(exportFolder, "propensity_model.csv")
  if (!is.null(data)) {
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  }
  readr::write_csv(data, fileName)


  ParallelLogger::logInfo("---- kaplan_meier_dist table ----------------------")
  ParallelLogger::logInfo("---- computing KM curves --------------------------")
  reference <- readRDS(file.path(cmOutputFolder, "outcomeModelReference.rds"))
  outcomesOfInterest <- 8466

  reference <- reference[reference$outcomeId %in% outcomesOfInterest, ]
  reference <- reference[, c("strataFile",
                             "studyPopFile",
                             "targetId",
                             "comparatorId",
                             "outcomeId",
                             "analysisId")]
  tempFolder <- file.path(exportFolder, "temp")
  if (!file.exists(tempFolder)) {
    dir.create(tempFolder)
  }
  cluster <- ParallelLogger::makeCluster(1)
  ParallelLogger::clusterRequire(cluster, "epi964")
  tasks <- split(reference, seq(nrow(reference)))
  ParallelLogger::clusterApply(cluster,
                               tasks,
                               prepareKm,
                               cmOutputFolder = cmOutputFolder,
                               tempFolder = tempFolder,
                               databaseId = databaseId,
                               minCellCount = minCellCount)
  ParallelLogger::stopCluster(cluster)
  ParallelLogger::logInfo("---- writing to single csv file ------------------")
  saveKmToCsv <- function(file,
                          first,
                          outputFile) {
    data <- readRDS(file)
    if (!is.null(data)) {
      colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
    }
    write.table(x = data,
                file = outputFile,
                row.names = FALSE,
                col.names = first,
                sep = ",",
                dec = ".",
                qmethod = "double",
                append = !first)
  }
  outputFile <- file.path(exportFolder, "kaplan_meier_dist.csv")
  files <- list.files(tempFolder, "km_.*.rds", full.names = TRUE)
  saveKmToCsv(files[1], first = TRUE, outputFile = outputFile)
  if (length(files) > 1) {
    plyr::l_ply(files[2:length(files)],
                saveKmToCsv,
                first = FALSE,
                outputFile = outputFile,
                .progress = "text")
  }
  unlink(tempFolder, recursive = TRUE)
}


exportTargetAnalyticBalance <- function(cmOutputFolder,
                                        exportFolder,
                                        databaseId) {

  targetAnalyticBalanceFolder <- file.path(cmOutputFolder, "targetAnalyticBalance")

  loadFile <- function(file) { # file = files[1]
    taBalance <- readRDS(file)
    return(file)
  }

  files <- list.files(targetAnalyticBalanceFolder, full.names = TRUE)

  taBalanceResults <- data.frame()
  for (file in files) {
    taBalance <- readRDS(file)
    taBalanceResults <- dplyr::bind_rows(taBalanceResults, taBalance)
  }
  readr::write_csv(taBalanceResults, file.path(exportFolder, "ta_balance.csv"))
}



enforceMinCellValue <- function(data,
                                fieldName,
                                minValues,
                                silent = FALSE) {
  toCensor <- !is.na(dplyr::pull(data, fieldName)) & dplyr::pull(data, fieldName) < minValues & dplyr::pull(data, fieldName) != 0
  if (!silent) {
    percent <- round(100 * sum(toCensor)/nrow(data), 1)
    message("   censoring ",
            sum(toCensor),
            " values (",
            percent,
            "%) from ",
            fieldName,
            " because value below minimum")
  }
  if (length(minValues) == 1) {
    data[toCensor, fieldName] <- -minValues
  } else {
    data[toCensor, fieldName] <- -minValues[toCensor]
  }
  return(data)
}

calibrate <- function(subset,
                      allControls) {
  ncs <- subset[subset$outcomeId %in% allControls$outcomeId[allControls$targetEffectSize == 1], ]
  ncs <- ncs[!is.na(ncs$seLogRr), ]
  if (nrow(ncs) <= 5) {
    subset$calibratedP <- rep(NA, nrow(subset))
    subset$calibratedRr <- rep(NA, nrow(subset))
    subset$calibratedCi95Lb <- rep(NA, nrow(subset))
    subset$calibratedCi95Ub <- rep(NA, nrow(subset))
    subset$calibratedLogRr <- rep(NA, nrow(subset))
    subset$calibratedSeLogRr <- rep(NA, nrow(subset))
  } else {
    null <- EmpiricalCalibration::fitMcmcNull(ncs$logRr, ncs$seLogRr)
    calibratedP <- EmpiricalCalibration::calibrateP(null = null,
                                                    logRr = subset$logRr,
                                                    seLogRr = subset$seLogRr)
    subset$calibratedP <- calibratedP$p

    model <- EmpiricalCalibration::convertNullToErrorModel(null)
    calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = subset$logRr,
                                                                      seLogRr = subset$seLogRr,
                                                                      model = model)
    subset$calibratedRr <- exp(calibratedCi$logRr)
    subset$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
    subset$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
    subset$calibratedLogRr <- calibratedCi$logRr
    subset$calibratedSeLogRr <- calibratedCi$seLogRr
  }
  subset$i2 <- rep(NA, nrow(subset))
  subset <- subset[, c("targetId",
                       "comparatorId",
                       "outcomeId",
                       "analysisId",
                       "rr",
                       "ci95lb",
                       "ci95ub",
                       "p",
                       "i2",
                       "logRr",
                       "seLogRr",
                       "target",
                       "comparator",
                       "targetDays",
                       "comparatorDays",
                       "eventsTarget",
                       "eventsComparator",
                       "calibratedP",
                       "calibratedRr",
                       "calibratedCi95Lb",
                       "calibratedCi95Ub",
                       "calibratedLogRr",
                       "calibratedSeLogRr")]
  colnames(subset) <- c("targetId",
                        "comparatorId",
                        "outcomeId",
                        "analysisId",
                        "rr",
                        "ci95Lb",
                        "ci95Ub",
                        "p",
                        "i2",
                        "logRr",
                        "seLogRr",
                        "targetSubjects",
                        "comparatorSubjects",
                        "targetDays",
                        "comparatorDays",
                        "targetOutcomes",
                        "comparatorOutcomes",
                        "calibratedP",
                        "calibratedRr",
                        "calibratedCi95Lb",
                        "calibratedCi95Ub",
                        "calibratedLogRr",
                        "calibratedSeLogRr")
  return(subset)
}


prepareKm <- function(task,
                      cmOutputFolder,
                      tempFolder,
                      databaseId,
                      minCellCount) {
  ParallelLogger::logTrace("Preparing KM plot for target ", task$targetId,
                           ", comparator ", task$comparatorId,
                           ", outcome ", task$outcomeId,
                           ", analysis ", task$analysisId)
  outputFileName <- file.path(tempFolder, sprintf("km_t%s_c%s_o%s_a%s.rds",
                                                  task$targetId,
                                                  task$comparatorId,
                                                  task$outcomeId,
                                                  task$analysisId))
  if (file.exists(outputFileName)) {
    return(NULL)
  }
  popFile <- task$strataFile
  if (popFile == "") {
    popFile <- task$studyPopFile
  }
  population <- readRDS(file.path(cmOutputFolder, popFile))
  if (nrow(population) == 0) {
    # Can happen when matching and treatment is predictable
    return(NULL)
  }
  data <- prepareKaplanMeier(population)
  if (is.null(data)) {
    # No shared strata
    return(NULL)
  }
  data$targetId <- task$targetId
  data$comparatorId <- task$comparatorId
  data$outcomeId <- task$outcomeId
  data$analysisId <- task$analysisId
  data$databaseId <- databaseId
  data <- enforceMinCellValue(data, "targetAtRisk", minCellCount)
  data <- enforceMinCellValue(data, "comparatorAtRisk", minCellCount)
  saveRDS(data, outputFileName)
}

prepareKaplanMeier <- function(population) {
  dataCutoff <- 0.9
  population$y <- 0
  population$y[population$outcomeCount != 0] <- 1
  if (is.null(population$stratumId) || length(unique(population$stratumId)) == nrow(population)/2) {
    sv <- survival::survfit(survival::Surv(survivalTime, y) ~ treatment, population, conf.int = TRUE)
    idx <- summary(sv, censored = T)$strata == "treatment=1"
    survTarget <- tibble::tibble(time = sv$time[idx],
                                 targetSurvival = sv$surv[idx],
                                 targetSurvivalLb = sv$lower[idx],
                                 targetSurvivalUb = sv$upper[idx])
    idx <- summary(sv, censored = T)$strata == "treatment=0"
    survComparator <- tibble::tibble(time = sv$time[idx],
                                     comparatorSurvival = sv$surv[idx],
                                     comparatorSurvivalLb = sv$lower[idx],
                                     comparatorSurvivalUb = sv$upper[idx])
    data <- merge(survTarget, survComparator, all = TRUE)
  } else {
    population$stratumSizeT <- 1
    strataSizesT <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 1, ], sum)
    if (max(strataSizesT$stratumSizeT) == 1) {
      # variable ratio matching: use propensity score to compute IPTW
      if (is.null(population$propensityScore)) {
        stop("Variable ratio matching detected, but no propensity score found")
      }
      weights <- aggregate(propensityScore ~ stratumId, population, mean)
      if (max(weights$propensityScore) > 0.99999) {
        return(NULL)
      }
      weights$weight <- weights$propensityScore / (1 - weights$propensityScore)
    } else {
      # stratification: infer probability of treatment from subject counts
      strataSizesC <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 0, ], sum)
      colnames(strataSizesC)[2] <- "stratumSizeC"
      weights <- merge(strataSizesT, strataSizesC)
      if (nrow(weights) == 0) {
        warning("No shared strata between target and comparator")
        return(NULL)
      }
      weights$weight <- weights$stratumSizeT/weights$stratumSizeC
    }
    population <- merge(population, weights[, c("stratumId", "weight")])
    population$weight[population$treatment == 1] <- 1
    idx <- population$treatment == 1
    survTarget <- CohortMethod:::adjustedKm(weight = population$weight[idx],
                                            time = population$survivalTime[idx],
                                            y = population$y[idx])
    survTarget$targetSurvivalUb <- survTarget$s^exp(qnorm(0.975)/log(survTarget$s) * sqrt(survTarget$var)/survTarget$s)
    survTarget$targetSurvivalLb <- survTarget$s^exp(qnorm(0.025)/log(survTarget$s) * sqrt(survTarget$var)/survTarget$s)
    survTarget$targetSurvivalLb[survTarget$s > 0.9999] <- survTarget$s[survTarget$s > 0.9999]
    survTarget$targetSurvival <- survTarget$s
    survTarget$s <- NULL
    survTarget$var <- NULL
    idx <- population$treatment == 0
    survComparator <- CohortMethod:::adjustedKm(weight = population$weight[idx],
                                                time = population$survivalTime[idx],
                                                y = population$y[idx])
    survComparator$comparatorSurvivalUb <- survComparator$s^exp(qnorm(0.975)/log(survComparator$s) * sqrt(survComparator$var)/survComparator$s)
    survComparator$comparatorSurvivalLb <- survComparator$s^exp(qnorm(0.025)/log(survComparator$s) * sqrt(survComparator$var)/survComparator$s)
    survComparator$comparatorSurvivalLb[survComparator$s > 0.9999] <- survComparator$s[survComparator$s > 0.9999]
    survComparator$comparatorSurvival <- survComparator$s
    survComparator$s <- NULL
    survComparator$var <- NULL
    data <- merge(survTarget, survComparator, all = TRUE)
  }
  data <- data[, c("time", "targetSurvival", "targetSurvivalLb", "targetSurvivalUb", "comparatorSurvival", "comparatorSurvivalLb", "comparatorSurvivalUb")]
  cutoff <- quantile(population$survivalTime, dataCutoff)
  data <- data[data$time <= cutoff, ]
  if (cutoff <= 300) {
    xBreaks <- seq(0, cutoff, by = 50)
  } else if (cutoff <= 600) {
    xBreaks <- seq(0, cutoff, by = 100)
  } else {
    xBreaks <- seq(0, cutoff, by = 250)
  }

  targetAtRisk <- c()
  comparatorAtRisk <- c()
  for (xBreak in xBreaks) {
    targetAtRisk <- c(targetAtRisk, sum(population$treatment == 1 & population$survivalTime >= xBreak))
    comparatorAtRisk <- c(comparatorAtRisk, sum(population$treatment == 0 & population$survivalTime >= xBreak))
  }
  data <- merge(data, tibble::tibble(time = xBreaks,
                                     targetAtRisk = targetAtRisk,
                                     comparatorAtRisk = comparatorAtRisk), all = TRUE)
  if (is.na(data$targetSurvival[1])) {
    data$targetSurvival[1] <- 1
    data$targetSurvivalUb[1] <- 1
    data$targetSurvivalLb[1] <- 1
  }
  if (is.na(data$comparatorSurvival[1])) {
    data$comparatorSurvival[1] <- 1
    data$comparatorSurvivalUb[1] <- 1
    data$comparatorSurvivalLb[1] <- 1
  }
  idx <- which(is.na(data$targetSurvival))
  while (length(idx) > 0) {
    data$targetSurvival[idx] <- data$targetSurvival[idx - 1]
    data$targetSurvivalLb[idx] <- data$targetSurvivalLb[idx - 1]
    data$targetSurvivalUb[idx] <- data$targetSurvivalUb[idx - 1]
    idx <- which(is.na(data$targetSurvival))
  }
  idx <- which(is.na(data$comparatorSurvival))
  while (length(idx) > 0) {
    data$comparatorSurvival[idx] <- data$comparatorSurvival[idx - 1]
    data$comparatorSurvivalLb[idx] <- data$comparatorSurvivalLb[idx - 1]
    data$comparatorSurvivalUb[idx] <- data$comparatorSurvivalUb[idx - 1]
    idx <- which(is.na(data$comparatorSurvival))
  }
  data$targetSurvival <- round(data$targetSurvival, 4)
  data$targetSurvivalLb <- round(data$targetSurvivalLb, 4)
  data$targetSurvivalUb <- round(data$targetSurvivalUb, 4)
  data$comparatorSurvival <- round(data$comparatorSurvival, 4)
  data$comparatorSurvivalLb <- round(data$comparatorSurvivalLb, 4)
  data$comparatorSurvivalUb <- round(data$comparatorSurvivalUb, 4)

  # Remove duplicate (except time) entries:
  data <- data[order(data$time), ]
  data <- data[!duplicated(data[, -1]), ]
  return(data)
}
