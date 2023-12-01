#' @export
runCohortDiagnostics <- function(connectionDetails,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 outputFolder,
                                 databaseId) {

  exportFolder <- file.path(outputFolder, "cohortDiagnostics")
  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }

  pathToCsv <- system.file("settings", "indicationCohortsToCreate.csv", package = "epi964")
  indicationCohortsToCreate <- readr::read_csv(pathToCsv, col_types = readr::cols()) %>%
    dplyr::select(
      cohortId,
      cohortName,
      json,
      sql,
      atlasId
    )

  pathToCsv <- system.file("settings", "outcomeCohortsToCreate.csv", package = "epi964")
  outcomeCohortsToCreate <- readr::read_csv(pathToCsv, col_types = readr::cols()) %>%
    dplyr::select(
      cohortId,
      cohortName,
      json,
      sql,
      atlasId
    )

  # outcomes
  CohortDiagnostics::executeDiagnostics(
    cohortDefinitionSet = outcomeCohortsToCreate,
    exportFolder = file.path(exportFolder, "outcomes"),
    databaseId = databaseId,
    cohortDatabaseSchema = cohortDatabaseSchema,
    databaseName = databaseId,
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = cohortTable),
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    runInclusionStatistics = FALSE,
    runIncludedSourceConcepts = TRUE,
    runOrphanConcepts = TRUE,
    runTimeSeries = FALSE,
    runVisitContext = TRUE,
    runBreakdownIndexEvents = TRUE,
    runIncidenceRate = TRUE,
    runCohortRelationship = TRUE,
    runTemporalCohortCharacterization = TRUE,
    temporalCovariateSettings = CohortDiagnostics::getDefaultCovariateSettings(),
    minCellCount = 5,
    minCharacterizationMean = 0.01,
    incremental = TRUE,
    incrementalFolder = file.path(exportFolder, "incremental")
  )

  # indications
  CohortDiagnostics::executeDiagnostics(
    cohortDefinitionSet = indicationCohortsToCreate,
    exportFolder = file.path(exportFolder, "indications"),
    databaseId = databaseId,
    cohortDatabaseSchema = cohortDatabaseSchema,
    databaseName = databaseId,
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = cohortTable),
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    runInclusionStatistics = FALSE,
    runIncludedSourceConcepts = FALSE,
    runOrphanConcepts = FALSE,
    runVisitContext = TRUE,
    runBreakdownIndexEvents = TRUE,
    runIncidenceRate = TRUE,
    runTimeSeries = FALSE,
    runCohortRelationship = TRUE,
    runTemporalCohortCharacterization = TRUE,
    temporalCovariateSettings = CohortDiagnostics::getDefaultCovariateSettings(),
    minCellCount = 5,
    minCharacterizationMean = 0.01,
    incremental = TRUE,
    incrementalFolder = file.path(exportFolder, "incremental")
  )
}

.getCohortJson <- function(file) {
  path <- sprintf("inst/cohorts/%s", file)
  cohortJson <- SqlRender::readSql(path)
  cohortJson <- RJSONIO::fromJSON(cohortJson)
  return(cohortJson)
}
