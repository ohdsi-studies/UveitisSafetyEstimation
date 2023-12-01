# Global settings ==============================================================

library(magrittr)
options(andromedaTempFolder = "G:/andromedaTemp", spipen = 999)
maxCores <- parallel::detectCores()

baseUrl <- Sys.getenv("BASE_URL")
ROhdsiWebApi::authorizeWebApi(baseUrl, "windows")

studyFolder <- "G:/epi_964"
cohortTable <- "epi_964"

cohortDatabaseSchema <- "scratch_jweave17"

source("extras/CohortStudySettings.R") # then rebuild
source("extras/EstimationStudySettings.R") # then rebuild


# Optum DOD settings ===========================================================

databaseId <- "optum_extended_dod"
outputFolder <- file.path(studyFolder, databaseId)
cdmDatabaseSchema <- "cdm_optum_extended_dod_v2050"

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("DBMS"),
  server = paste0(Sys.getenv("OHDA_SERVER"), databaseId),
  extraSettings = Sys.getenv("EXTRA_SETTINGS"),
  port = Sys.getenv("port"),
  user = Sys.getenv("OHDA_USER"),
  password = Sys.getenv("OHDA_PASSWORD")
)

epi964::execute(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  outputFolder = outputFolder,
  databaseId = databaseId,
  createCohortTable = FALSE,
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
  maxCores = maxCores,
  minCellCount = 5
)


# CCAE settings ===============================================================

databaseId <- "truven_ccae"
outputFolder <- file.path(studyFolder, databaseId)
cdmDatabaseSchema <- "cdm_truven_ccae_v2044"

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("DBMS"),
  server = paste0(Sys.getenv("OHDA_SERVER"), databaseId),
  extraSettings = Sys.getenv("EXTRA_SETTINGS"),
  port = Sys.getenv("port"),
  user = Sys.getenv("OHDA_USER"),
  password = Sys.getenv("OHDA_PASSWORD")
)

epi964::execute(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  outputFolder = outputFolder,
  databaseId = databaseId,
  createCohortTable = FALSE,
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
  maxCores = maxCores,
  minCellCount = 5
)


# Optum EHR settings ===========================================================

databaseId <- "optum_ehr"
outputFolder <- file.path(studyFolder, databaseId)
cdmDatabaseSchema <- "cdm_optum_ehr_v1990"

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("DBMS"),
  server = paste0(Sys.getenv("OHDA_SERVER"), databaseId),
  extraSettings = Sys.getenv("EXTRA_SETTINGS"),
  port = Sys.getenv("port"),
  user = Sys.getenv("OHDA_USER"),
  password = Sys.getenv("OHDA_PASSWORD")
)

epi964::execute(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  outputFolder = outputFolder,
  databaseId = databaseId,
  createCohortTable = FALSE,
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
  maxCores = maxCores,
  minCellCount = 5
)


# Pharmetrics settings =========================================================

databaseId <- "iqvia_pharmetrics_plus"
outputFolder <- file.path(studyFolder, databaseId)
cdmDatabaseSchema <- "cdm_iqvia_pharmetrics_plus_v2001"

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("DBMS"),
  server = paste0(Sys.getenv("OHDA_SERVER"), databaseId),
  extraSettings = Sys.getenv("EXTRA_SETTINGS"),
  port = Sys.getenv("port"),
  user = Sys.getenv("OHDA_USER"),
  password = Sys.getenv("OHDA_PASSWORD")
)

epi964::execute(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  outputFolder = outputFolder,
  databaseId = databaseId,
  createCohortTable = FALSE,
  createExposureCohorts = FALSE,
  createIndicationsCohorts = FALSE,
  createOutcomeCohorts = FALSE,
  createExposureIndicationCohorts = FALSE,
  createValidationCohorts = FALSE,
  runCohortDiagnostics = TRUE,
  runPheValuator = FALSE,
  runEstimation = FALSE,
  compareTargetAnalyticCohorts = FALSE,
  exportResults = FALSE,
  maxCores = maxCores,
  minCellCount = 5
)


# Amb EMR settings =============================================================

databaseId <- "iqvia_amb_emr"
outputFolder <- file.path(studyFolder, databaseId)
cdmDatabaseSchema <- "cdm_iqvia_amb_emr_v1979"

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("DBMS"),
  server = paste0(Sys.getenv("OHDA_SERVER"), databaseId),
  extraSettings = Sys.getenv("EXTRA_SETTINGS"),
  port = Sys.getenv("port"),
  user = Sys.getenv("OHDA_USER"),
  password = Sys.getenv("OHDA_PASSWORD")
)

epi964::execute(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  outputFolder = outputFolder,
  databaseId = databaseId,
  createCohortTable = FALSE,
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
  maxCores = maxCores,
  minCellCount = 5
)


# outcomes explorer ============================================================

cdOutcomesDataFolder <- file.path(studyFolder, "cdOutcomesData")
if (!file.exists(cdOutcomesDataFolder)) {
  dir.create(cdOutcomesDataFolder)
}
file.copy(
  from = c(
    list.files(file.path(studyFolder, "optum_extended_dod", "cohortDiagnostics", "outcomes"), full.names = TRUE, pattern = ".zip"),
    list.files(file.path(studyFolder, "truven_ccae", "cohortDiagnostics", "outcomes"), full.names = TRUE, pattern = ".zip"),
    list.files(file.path(studyFolder, "iqvia_amb_emr", "cohortDiagnostics", "outcomes"), full.names = TRUE, pattern = ".zip"),
    list.files(file.path(studyFolder, "iqvia_pharmetrics_plus", "cohortDiagnostics", "outcomes"), full.names = TRUE, pattern = ".zip"),
    list.files(file.path(studyFolder, "optum_ehr", "cohortDiagnostics", "outcomes"), full.names = TRUE, pattern = ".zip")
  ),
  to = cdOutcomesDataFolder,
  overwrite = TRUE
)

CohortDiagnostics::createMergedResultsFile(
  dataFolder = cdOutcomesDataFolder,
  sqliteDbPath = file.path(cdOutcomesDataFolder, "MergedCohortDiagnosticsData.sqlite"),
  overwrite = TRUE,
  tablePrefix = ""
)

CohortDiagnostics::launchDiagnosticsExplorer(
  sqliteDbPath = file.path(cdOutcomesDataFolder, "MergedCohortDiagnosticsData.sqlite"),
  makePublishable = TRUE,
  publishDir = file.path(getwd(), "DiagnosticsExplorerOutcomes")
)


# indications explorer =========================================================

cdInidcationsDataFolder <- file.path(studyFolder, "cdInidcationsData")
if (!file.exists(cdInidcationsDataFolder)) {
  dir.create(cdInidcationsDataFolder)
}
file.copy(
  from = c(
    list.files(file.path(studyFolder, "optum_extended_dod", "cohortDiagnostics", "indications"), full.names = TRUE, pattern = ".zip"),
    list.files(file.path(studyFolder, "truven_ccae", "cohortDiagnostics", "indications"), full.names = TRUE, pattern = ".zip"),
    list.files(file.path(studyFolder, "iqvia_amb_emr", "cohortDiagnostics", "indications"), full.names = TRUE, pattern = ".zip"),
    list.files(file.path(studyFolder, "iqvia_pharmetrics_plus", "cohortDiagnostics", "indications"), full.names = TRUE, pattern = ".zip"),
    list.files(file.path(studyFolder, "optum_ehr", "cohortDiagnostics", "indications"), full.names = TRUE, pattern = ".zip")
  ),
  to = cdInidcationsDataFolder,
  overwrite = TRUE
)

CohortDiagnostics::createMergedResultsFile(
  dataFolder = cdInidcationsDataFolder,
  sqliteDbPath = file.path(cdInidcationsDataFolder, "MergedCohortDiagnosticsData.sqlite"),
  overwrite = TRUE,
  tablePrefix = ""
)

CohortDiagnostics::launchDiagnosticsExplorer(
  sqliteDbPath = file.path(cdInidcationsDataFolder, "MergedCohortDiagnosticsData.sqlite"),
  makePublishable = TRUE,
  publishDir = file.path(getwd(), "DiagnosticsExplorerIndications"),
  overwritePublishDir = TRUE
)


# complie PV results ===========================================================

pvFolder <- file.path(studyFolder, "pvData")
if (!file.exists(pvFolder)) {
  dir.create(pvFolder, recursive = TRUE)
}

databaseNames <- c("cdm_iqvia_amb_emr_v1979",
                   "cdm_iqvia_pharmetrics_plus_v2001",
                   "cdm_optum_ehr_v1990",
                   "cdm_optum_extended_dod_v2050",
                   "cdm_truven_ccae_v2044")

databaseIds <- gsub("\\_v....", "", databaseNames)
databaseIds <- gsub("cdm_", "", databaseIds)

valSummary <- tibble::tibble()
for (databaseId in databaseIds) {
  valSummaryDbFile <- file.path(studyFolder, databaseId, "pheValuator", "Non infectious uveitis", "pvResults.csv")
  valSummaryDb <- readr::read_csv(valSummaryDbFile, show_col_types = FALSE) %>%
    dplyr::select(
      cdm,
      description,
      cohortId,
      truePositives,
      trueNegatives,
      falsePositives,
      falseNegatives,
      estimatedPrevalence,
      sensitivity,
      specificity,
      ppv,
      npv
    )
  valSummary <- dplyr::bind_rows(valSummary, valSummaryDb)
}
readr::write_csv(valSummary, file.path(pvFolder, "pvSummary.csv"))


# Review estimation diagnostics ================================================

shinyDataFolder <- file.path(studyFolder, "shinyData")
if (!file.exists(shinyDataFolder)) {
  dir.create(shinyDataFolder)
}
file.copy(
  from = c(
    list.files(file.path(studyFolder, "optum_extended_dod", "cmOutput", "shinyData"), full.names = TRUE),
    list.files(file.path(studyFolder, "truven_ccae", "cmOutput", "shinyData"), full.names = TRUE),
    list.files(file.path(studyFolder, "iqvia_amb_emr", "cmOutput", "shinyData"), full.names = TRUE),
    list.files(file.path(studyFolder, "iqvia_pharmetrics_plus", "cmOutput", "shinyData"), full.names = TRUE),
    list.files(file.path(studyFolder, "optum_ehr", "cmOutput", "shinyData"), full.names = TRUE)
  ),
  to = shinyDataFolder,
  overwrite = TRUE
)


# Apply unblinding criteria ====================================================
epi964::assessDiagnostics(studyFolder) # produces diagnostics_results.rds in shinyFolder


# Meta-analysis ================================================================

epi964::doMetaAnalysis(
  studyFolder = studyFolder,
  outputFolders = c(
    file.path(studyFolder, "optum_extended_dod"),
    file.path(studyFolder, "truven_ccae"),
    file.path(studyFolder, "iqvia_amb_emr"),
    file.path(studyFolder, "iqvia_pharmetrics_plus"),
    file.path(studyFolder, "optum_ehr")
  ),
  maOutputFolder = file.path(studyFolder, "meta_analysis")
)

shinyDataFolder <- file.path(studyFolder, "shinyData")
if (!file.exists(shinyDataFolder)) {
  dir.create(shinyDataFolder)
}
file.copy(
  from = list.files(file.path(studyFolder, "meta_analysis", "shinyData"), full.names = TRUE),
  to = shinyDataFolder,
  overwrite = TRUE
)

# create figures ===============================================================

epi964::CreateFigures(
  shinyDataFolder = "S:/Git/Bitbucket/epi_964/inst/shiny/data",
  figuresFolder = "G:/epi_964_7/figures"
)

