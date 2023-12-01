#' @export
prepareShinyData <- function(outputFolder) {

  exportFolder <- file.path(outputFolder, "cmOutput", "export")
  shinyDataFolder <- file.path(outputFolder, "cmOutput", "shinyData")
  if (!file.exists(shinyDataFolder)) {
    dir.create(shinyDataFolder, recursive = TRUE)
  }

  databaseId <- basename(outputFolder)
  splitTables <- c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")

  processSubet <- function(subset, tableName) {
    targetId <- subset$target_id[1]
    comparatorId <- subset$comparator_id[1]
    fileName <- sprintf("%s_t%s_c%s_%s.rds", tableName, targetId, comparatorId, databaseId)
    saveRDS(subset, file.path(shinyDataFolder, fileName))
  }

  processFile <- function(file) {  # file <- files[15]
    tableName <- gsub(".csv$", "", file)
    table <- readr::read_csv(file.path(exportFolder, file), col_types = readr::cols())
    if (tableName %in% splitTables) {
      subsets <- split(table, list(table$target_id, table$comparator_id))
      plyr::l_ply(subsets, processSubet, tableName = tableName)
    } else {
      saveRDS(table, file.path(shinyDataFolder, sprintf("%s_%s.rds", tableName, databaseId)))
    }
  }
  files <- list.files(exportFolder, ".*.csv")
  plyr::l_ply(files, processFile, .progress = "text")
}

