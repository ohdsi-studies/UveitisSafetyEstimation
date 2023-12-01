#' @export
createExposureIndicationCohorts <- function(connectionDetails,
                                            cohortDatabaseSchema,
                                            cohortTable) {

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  pathToCsv <- system.file("settings/exposureIndicationCohortsToCreate.csv", package = "epi964")
  exposureIndicationCohortRef <- readr::read_csv(pathToCsv, col_types = readr::cols()) %>%
    dplyr::select(
      exposureIndicationCohortId,
      exposureCohortId,
      indicationCohortId
    )

  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = NULL,
    tableName = "#exposure_indication_cohort_ref",
    data = exposureIndicationCohortRef,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = TRUE,
    bulkLoad = FALSE,
    camelCaseToSnakeCase = TRUE
  )

  sqlFile <- system.file("sql/sql_server/intersectExposureIndicationsCohorts.sql", package = "epi964")
  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "intersectExposureIndicationsCohorts.sql",
    packageName = "epi964",
    dbms = connectionDetails$dbms,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable)
  DatabaseConnector::executeSql(connection, sql)
}
