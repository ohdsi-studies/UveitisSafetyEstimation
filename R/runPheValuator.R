#' @export
runPheValuator <- function(baseUrl,
                           connectionDetails,
                           cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           cohortTable,
                           outputFolder,
                           phenotypeName,
                           databaseId,
                           xSpecCohortId,
                           prevalenceCohortId,
                           excludedCovariateConceptIds,
                           phenotypeCohortIds) {

  pvFolder <- file.path(outputFolder, "pheValuator", phenotypeName)
  if (!file.exists(pvFolder)) {
    dir.create(pvFolder, recursive = TRUE)
  }

  covariateSettingsAcute <- PheValuator::createDefaultCovariateSettings(
    excludedCovariateConceptIds = excludedCovariateConceptIds,
    addDescendantsToExclude = TRUE,
    startDayWindow1 = 0,
    endDayWindow1 = 10,
    startDayWindow2 = 11,
    endDayWindow2 = 20,
    startDayWindow3 = 21,
    endDayWindow3 = 30
  )

  CohortArgsAcute <- PheValuator::createCreateEvaluationCohortArgs(
    xSpecCohortId = xSpecCohortId,
    daysFromxSpec = 1,
    xSensCohortId = xSpecCohortId,
    prevalenceCohortId = prevalenceCohortId,
    modelBaseSampleSize = 25000,
    xSpecCohortSize = 5000,
    covariateSettings = covariateSettingsAcute,
    baseSampleSize = 2000000,
    lowerAgeLimit = 18,
    upperAgeLimit = 100,
    startDate = "20150101",
    endDate = "21000101",
    excludeModelFromEvaluation = FALSE
  )

  testArgs1 <- PheValuator::createTestPhenotypeAlgorithmArgs(
    cutPoints = c("EV"),
    phenotypeCohortId = xSpecCohortId,
    washoutPeriod = 0
  )

  analysis1 <- PheValuator::createPheValuatorAnalysis(
    analysisId = 1,
    description = ROhdsiWebApi::getCohortDefinition(xSpecCohortId, baseUrl)$name,
    createEvaluationCohortArgs = CohortArgsAcute,
    testPhenotypeAlgorithmArgs = testArgs1
  )

  testArgs2 <- PheValuator::createTestPhenotypeAlgorithmArgs(
    cutPoints =  c("EV"),
    phenotypeCohortId = prevalenceCohortId,
    washoutPeriod = 0
  )

  analysis2 <- PheValuator::createPheValuatorAnalysis(
    analysisId = 2,
    description = ROhdsiWebApi::getCohortDefinition(prevalenceCohortId, baseUrl)$name,
    createEvaluationCohortArgs = CohortArgsAcute,
    testPhenotypeAlgorithmArgs = testArgs2
  )

  pheValuatorAnalysisList <- list(analysis1, analysis2)

  for (phenotypeCohortId in phenotypeCohortIds) {

    testArgs <- PheValuator::createTestPhenotypeAlgorithmArgs(
      cutPoints = c("EV"),
      phenotypeCohortId = phenotypeCohortId,
      washoutPeriod = 0
    )
    analysis <- PheValuator::createPheValuatorAnalysis(
      analysisId = length(pheValuatorAnalysisList) + 1,
      description = ROhdsiWebApi::getCohortDefinition(phenotypeCohortId, baseUrl)$name,
      createEvaluationCohortArgs = CohortArgsAcute,
      testPhenotypeAlgorithmArgs = testArgs
    )
    pheValuatorAnalysisList[[length(pheValuatorAnalysisList) + 1]] <- analysis
  }

  referenceTable <- PheValuator::runPheValuatorAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    workDatabaseSchema = cohortDatabaseSchema,
    outputFolder = pvFolder,
    pheValuatorAnalysisList = pheValuatorAnalysisList
  )

  results <- PheValuator::summarizePheValuatorAnalyses(
    referenceTable = referenceTable,
    outputFolder = pvFolder
  )
  readr::write_csv(results, file.path(pvFolder, "pvResults.csv"))
}
