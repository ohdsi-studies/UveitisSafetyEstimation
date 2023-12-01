# Study specifications =========================================================

## Get cmData ==================================================================

excludedCovariateConceptIds <- c(
  912263,937368,967823,1305058,1511348,1592513,1593700,2314216,2314229,2314231,19041065,21600857,21600884,21600885,21601153,21601160,21601179,21601194,21601195,21601386,21601421,21601422,21603890,21603891,21603892,21603914,35200139,35603563,40161532,40171288,45774639,45888764,45889779,45892883,21601119,38003196,1129625,4203722
)

getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
  studyStartDate = "",
  studyEndDate = "",
  firstExposureOnly = FALSE,
  removeDuplicateSubjects = FALSE,
  restrictToCommonPeriod = FALSE, # set FALSE here, TRUE in study pop args
  covariateSettings = FeatureExtraction::createDefaultCovariateSettings(
    excludedCovariateConceptIds = excludedCovariateConceptIds,
    addDescendantsToExclude = TRUE
  )
)

## Define study populations ====================================================

createStudyPopArgsOnTreatment <- CohortMethod::createCreateStudyPopulationArgs(
  firstExposureOnly = FALSE,
  restrictToCommonPeriod = TRUE,
  washoutPeriod = 0,
  removeDuplicateSubjects = FALSE,
  removeSubjectsWithPriorOutcome = TRUE,
  priorOutcomeLookback = 99999,
  minDaysAtRisk = 1,
  maxDaysAtRisk = 99999,
  startAnchor = "cohort start",
  riskWindowStart = 1,
  endAnchor = "cohort end",
  riskWindowEnd = 0,
  censorAtNewRiskWindow = FALSE # no T vs C patient overlap or TAR overlap by cohort defn design
)

createStudyPopArgsItt <- CohortMethod::createCreateStudyPopulationArgs(
  firstExposureOnly = FALSE,
  restrictToCommonPeriod = TRUE,
  washoutPeriod = 0,
  removeDuplicateSubjects = FALSE,
  removeSubjectsWithPriorOutcome = TRUE,
  priorOutcomeLookback = 99999,
  minDaysAtRisk = 1,
  maxDaysAtRisk = 99999,
  startAnchor = "cohort start",
  riskWindowStart = 1,
  endAnchor = "cohort start",
  riskWindowEnd = 99999,
  censorAtNewRiskWindow = FALSE # no T vs C patient overlap or TAR overlap by cohort defn design
)

## PS specifications ===========================================================

createPsArgs <- CohortMethod::createCreatePsArgs(
  maxCohortSizeForFitting = 250000,
  errorOnHighCorrelation = TRUE,
  stopOnError = TRUE, # note these
  prior = Cyclops::createPrior(
    priorType = "laplace",
    useCrossValidation = TRUE
  ),
  control = Cyclops::createControl(
    maxIterations = 5000,
    fold = 10,
    cvRepetitions = 10,
    noiseLevel = "silent",
    seed = 123
  )
)

matchOnPsArgs1To1 <- CohortMethod::createMatchOnPsArgs(
  caliper = 0.2,
  caliperScale = "standardized logit",
  allowReverseMatch = FALSE,
  maxRatio = 1
)

matchOnPsArgs1To10 <- CohortMethod::createMatchOnPsArgs(
  caliper = 0.2,
  caliperScale = "standardized logit",
  allowReverseMatch = FALSE,
  maxRatio = 10
)

stratifyByPsArgs10Strata <- CohortMethod::createStratifyByPsArgs(
  numberOfStrata = 10,
  baseSelection = "all"
)

trimByIptwArgs <- CohortMethod::createTrimByIptwArgs(
  maxWeight = 0,
  estimator = "att"
)


## Outcome model specifications ================================================

fitOutcomeModelArgsConditional <- CohortMethod::createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = TRUE,
  useCovariates = FALSE,
  profileGrid = NULL
)

# fitOutcomeModelArgsIptw <- CohortMethod::createFitOutcomeModelArgs(
#   modelType = "cox",
#   stratified = FALSE,
#   inversePtWeighting = TRUE,
#   estimator = "att",
#   maxWeight = 0,
#   useCovariates = FALSE,
#   profileGrid = NULL
# )

## CM analysis specifications ==================================================

cmAnalysis1 <- CohortMethod::createCmAnalysis(
  analysisId = 1,
  description = "1:1 PS matched, on-treatment",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgsOnTreatment,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs1To1,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional
)

cmAnalysis2 <- CohortMethod::createCmAnalysis(
  analysisId = 2,
  description = "1:1 PS matched, ITT",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgsItt,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs1To1,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional
)

cmAnalysis3 <- CohortMethod::createCmAnalysis(
  analysisId = 3,
  description = "1:10 PS matched, on-treatment",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgsOnTreatment,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs1To10,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional
)

cmAnalysis4 <- CohortMethod::createCmAnalysis(
  analysisId = 4,
  description = "1:10 PS matched, ITT",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgsItt,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs1To10,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional
)

# cmAnalysis5 <- CohortMethod::createCmAnalysis(
#   analysisId = 5,
#   description = "10 PS stratified, on-treatment",
#   getDbCohortMethodDataArgs = getDbCmDataArgs,
#   createStudyPopArgs = createStudyPopArgsOnTreatment,
#   createPs = TRUE,
#   createPsArgs = createPsArgs,
#   stratifyByPs = TRUE,
#   stratifyByPsArgs = stratifyByPsArgs10Strata,
#   fitOutcomeModel = TRUE,
#   fitOutcomeModelArgs = fitOutcomeModelArgsConditional
# )
#
# cmAnalysis6 <- CohortMethod::createCmAnalysis(
#   analysisId = 6,
#   description = "10 PS stratified, ITT",
#   getDbCohortMethodDataArgs = getDbCmDataArgs,
#   createStudyPopArgs = createStudyPopArgsItt,
#   createPs = TRUE,
#   createPsArgs = createPsArgs,
#   stratifyByPs = TRUE,
#   stratifyByPsArgs = stratifyByPsArgs10Strata,
#   fitOutcomeModel = TRUE,
#   fitOutcomeModelArgs = fitOutcomeModelArgsConditional
# )
#
# cmAnalysis5 <- CohortMethod::createCmAnalysis(
#   analysisId = 5,
#   description = "IPTW, on-treatment",
#   getDbCohortMethodDataArgs = getDbCmDataArgs,
#   createStudyPopArgs = createStudyPopArgsOnTreatment,
#   createPs = TRUE,
#   createPsArgs = createPsArgs,
#   trimByIptw = TRUE,
#   trimByIptwArgs = trimByIptwArgs,
#   fitOutcomeModel = TRUE,
#   fitOutcomeModelArgs = fitOutcomeModelArgsIptw
# )
#
# cmAnalysis6 <- CohortMethod::createCmAnalysis(
#   analysisId = 6,
#   description = "IPTW, ITT",
#   getDbCohortMethodDataArgs = getDbCmDataArgs,
#   createStudyPopArgs = createStudyPopArgsItt,
#   createPs = TRUE,
#   createPsArgs = createPsArgs,
#   trimByIptw = TRUE,
#   trimByIptwArgs = trimByIptwArgs,
#   fitOutcomeModel = TRUE,
#   fitOutcomeModelArgs = fitOutcomeModelArgsIptw
# )

CohortMethod::saveCmAnalysisList(
  list(cmAnalysis1,
       cmAnalysis2,
       cmAnalysis3,
       cmAnalysis4),
  file = "inst/settings/cmAnalysisList.json"
)

## Create TCOs =================================================================

pathToCsv <- system.file("settings", "exposureIndicationCohortsToCreate.csv", package = "epi964")
exposureCohortRef <- readr::read_csv(pathToCsv, col_types = readr::cols())

remicadeExposureIds <- c(9367, 9369, 9371, 9373)
comparatorExposureIds <- exposureCohortRef$exposureCohortId[!(exposureCohortRef$exposureCohortId %in% remicadeExposureIds)]
indicationCohortIds <- unique(exposureCohortRef$indicationCohortId)

# these are not properly excluding via TCOs, had to remove at feature extraction during createGetDbCohortMethodDataArgs
excludedCovariateConceptIds <- c(912263,937368,967823,1305058,1511348,1592513,1593700,2314216,2314229,2314231,19041065,21600857,21600884,21600885,21601153,21601160,21601179,21601194,21601195,21601386,21601421,21601422,21603890,21603891,21603892,21603914,35200139,35603563,40161532,40171288,45774639,45888764,45889779,45892883,21601119,38003196,1129625,4203722)
excludedCovariateConceptIds <- paste(excludedCovariateConceptIds, collapse = ";")

tcos <- tibble::tibble()
for (indicationCohortId in indicationCohortIds) { # indicationCohortId <- indicationCohortIds[1]
  indicationSubset <- exposureCohortRef[exposureCohortRef$indicationCohortId == indicationCohortId, ]
  targetId <- indicationSubset$exposureIndicationCohortId[indicationSubset$exposureCohortId %in% remicadeExposureIds]
  comparatorIds <- indicationSubset$exposureIndicationCohortId[indicationSubset$exposureCohortId %in% comparatorExposureIds]
  tcIndication <- merge(targetId, comparatorIds)
  names(tcIndication) <- c("targetId", "comparatorId")
  tcos <- dplyr::bind_rows(tcos, tcIndication)
}
tcos$outcomeId <- rep(8466, nrow(tcos))

tcos$excludedCovariateConceptIds <- rep(excludedCovariateConceptIds, nrow(tcos))
readr::write_csv(tcos, file.path("inst/settings/TcosOfInterest.csv"))
