#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cohortDatabaseSchema,
                    cohortTable,
                    outputFolder,
                    databaseId,
                    createCohortTable = FALSE, # TRUE will delete existing cohorts
                    createExposureCohorts = FALSE,
                    createIndicationsCohorts = FALSE,
                    createOutcomeCohorts = FALSE,
                    createExposureIndicationCohorts = FALSE,
                    createValidationCohorts = FALSE,
                    runCohortDiagnostics = FALSE,
                    runPheValuator = FALSE,
                    runEstimation = FALSE,
                    compareTargetAnalyticCohorts = FALSE,
                    exportResults = FALSE,
                    maxCores = 1,
                    minCellCount = 5) {

  if (createCohortTable) {
    cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
    CohortGenerator::createCohortTables(
      connectionDetails = connectionDetails,
      cohortTableNames = cohortTableNames,
      cohortDatabaseSchema = cohortDatabaseSchema
    )
  }

  if (createExposureCohorts) {
    cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
    pathToCsv <- system.file("settings", "exposureCohortsToCreate.csv", package = "epi964")
    exposureCohortDefinitionSet <- readr::read_csv(pathToCsv, col_types = readr::cols())
    cohortsGenerated <- CohortGenerator::generateCohortSet(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTableNames = cohortTableNames,
      cohortDefinitionSet = exposureCohortDefinitionSet
    )
  }

  if (createIndicationsCohorts) {
    cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
    pathToCsv <- system.file("settings", "indicationCohortsToCreate.csv", package = "epi964")
    indicationCohortDefinitionSet <- readr::read_csv(pathToCsv, col_types = readr::cols())
    cohortsGenerated <- CohortGenerator::generateCohortSet(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTableNames = cohortTableNames,
      cohortDefinitionSet = indicationCohortDefinitionSet
    )
  }

  if (createOutcomeCohorts) {
    cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
    pathToCsv <- system.file("settings", "outcomeCohortsToCreate.csv", package = "epi964")
    outcomeCohortDefinitionSet <- readr::read_csv(pathToCsv, col_types = readr::cols())
    cohortsGenerated <- CohortGenerator::generateCohortSet(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTableNames = cohortTableNames,
      cohortDefinitionSet = outcomeCohortDefinitionSet
    )

    pathToCsv <- system.file("settings", "NegativeControlOutcomes.csv", package = "epi964")
    negativeControlOutcomes <- readr::read_csv(pathToCsv, col_types = readr::cols())
    sql <- SqlRender::loadRenderTranslateSql(
      sqlFilename = "NegativeControlOutcomes.sql",
      packageName = "epi964",
      dbms = connectionDetails$dbms,
      cdm_database_schema = cdmDatabaseSchema,
      target_database_schema = cohortDatabaseSchema,
      target_cohort_table = cohortTable,
      outcome_ids = unique(negativeControlOutcomes$outcomeId)
    )
    connection <- DatabaseConnector::connect(connectionDetails)
    DatabaseConnector::executeSql(connection, sql)
    DatabaseConnector::disconnect(connection)
  }

  if (createExposureIndicationCohorts) {
    epi964::createExposureIndicationCohorts(
      connectionDetails = connectionDetails,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable
    )
  }

  if (createValidationCohorts) {
    cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
    pathToCsv <- system.file("settings", "validationCohortsToCreate.csv", package = "epi964")
    validationCohortDefinitionSet <- readr::read_csv(pathToCsv, col_types = readr::cols())
    cohortsGenerated <- CohortGenerator::generateCohortSet(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTableNames = cohortTableNames,
      cohortDefinitionSet = validationCohortDefinitionSet
    )
  }

  if (runCohortDiagnostics) {
    epi964::runCohortDiagnostics(
      connectionDetails = connectionDetails,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      outputFolder = outputFolder,
      databaseId = databaseId
    )
  }

  if (runPheValuator) {
    epi964::runPheValuator(
      baseUrl = baseUrl,
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      outputFolder = outputFolder,
      phenotypeName = "Non infectious uveitis",
      databaseId = databaseId,
      xSpecCohortId = 13747,
      prevalenceCohortId = 13748,
      excludedCovariateConceptIds = c(374035,379019,436108,4029812,4132493,4218590,4312493),
      phenotypeCohortIds = c(8468, 8467, 8466)
    )
  }

  if (runEstimation) {
    epi964::runEstimation(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      outputFolder = outputFolder,
      maxCores = maxCores
    )
  }

  if (compareTargetAnalyticCohorts) {
    epi964::compareTargetAnalyticCohorts(outputFolder)
  }

  if (exportResults) {
    epi964::exportResults(outputFolder,
                          databaseId)
  }

}
