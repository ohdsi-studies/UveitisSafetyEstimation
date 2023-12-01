#' @export
compareTargetAnalyticCohorts <- function(outputFolder) {

  # population
  # treatment = 1: analytic cohort
  # treatment = 0: target cohort

  cohortMethodDataFolder <- file.path(outputFolder, "cmOutput")
  targetAnalyticBalanceFolder <- file.path(cohortMethodDataFolder, "targetAnalyticBalance")
  if (!file.exists(targetAnalyticBalanceFolder)) {
    dir.create(targetAnalyticBalanceFolder)
  }

  reference <- readRDS(file.path(cohortMethodDataFolder, "outcomeModelReference.rds")) %>%
    dplyr::filter(outcomeOfInterest == TRUE)


  prepareCharacteristics <- function(row,
                                     reference) {

    analysisId <- row$analysisId
    targetId <- row$targetId
    comparatorId <- row$comparatorId
    databaseId <- basename(outputFolder)

    cohortMethodDataFile <- file.path(cohortMethodDataFolder, row$cohortMethodDataFile)
    cohortMethodData <- CohortMethod::loadCohortMethodData(cohortMethodDataFile)

    targetCohort <- as.data.frame(cohortMethodData$cohorts) %>%
      dplyr::filter(treatment == 1) %>%
      dplyr::mutate(treatment = 0)

    stratPopFile <- file.path(cohortMethodDataFolder, row$strataFile)
    stratPop <- readRDS(stratPopFile)

    analyticCohort <- stratPop %>%
      dplyr::filter(treatment == 1)

    population <- dplyr::bind_rows(targetCohort, analyticCohort)
    balance <- computeCovariateBalance2(population, cohortMethodData)
    balance$analysisId <- analysisId
    balance$targetId <- targetId
    balance$comparatorId <- comparatorId
    balance$databaseId <- databaseId

    targetAnalysisBalanceFile <- file.path(targetAnalyticBalanceFolder,
                                           sprintf("ta_balance_a%s_t%s_c%s.rds", analysisId, targetId, comparatorId))
    saveRDS(balance, targetAnalysisBalanceFile)
  }
  dummy <- lapply(X = split(reference, 1:nrow(reference)), prepareCharacteristics, reference = reference)
}

computeCovariateBalance2 <- function(population,
                                     cohortMethodData) {

  cohortMethodData$tempCohortTarget <- cohortMethodData$cohorts %>%
    dplyr::select(rowId, treatment) %>%
    dplyr::filter(treatment == 0)

  targetCohort <- tibble::as_tibble(cohortMethodData$tempCohortTarget)
  targetChars <- computeMeansPerGroup2(targetCohort, cohortMethodData) %>%
    dplyr::select(-treatment, sumTarget = sum, meanTarget = mean, sdTarget = sd)

  cohortMethodData$tempCohortAnalytic <- population %>%
    dplyr::select(rowId, treatment, stratumId) %>%
    dplyr::filter(treatment == 1)

  analyticCohort <- tibble::as_tibble(cohortMethodData$tempCohortAnalytic)
  analyticChars <- computeMeansPerGroup2(analyticCohort, cohortMethodData) %>%
    dplyr::select(-treatment, sumAnalytic = sum, meanAnalytic = mean, sdAnalytic = sd)

  balance <- targetChars %>%
    dplyr::full_join(analyticChars, by = "covariateId") %>%
    dplyr::mutate(sd = sqrt((sdTarget^2 + sdAnalytic^2) / 2))

  balance <- balance %>%
    dplyr::inner_join(collect(cohortMethodData$covariateRef), by = "covariateId") %>%
    dplyr::inner_join(cohortMethodData$analysisRef %>%
                        dplyr::select(analysisId, domainId, isBinary) %>%
                        dplyr::collect() %>%
                        dplyr::mutate(domainId = as.factor(domainId)), by = "analysisId") %>%
    dplyr::mutate(stdDiff = (meanTarget - meanAnalytic) / sd)

  return(balance)
}


computeMeansPerGroup2 <- function(cohort,
                                  cohortMethodData) {

  hasStrata <- "stratumId" %in% colnames(cohort)

  if (hasStrata) {
    stratumSize <- cohort %>%
      dplyr::group_by(stratumId, treatment) %>%
      dplyr::count() %>%
      dplyr::ungroup()
  }

  if (hasStrata && any(stratumSize %>% dplyr::pull(n) > 1)) {

    w <- stratumSize %>%
      dplyr::mutate(weight = 1 / n) %>%
      dplyr::inner_join(cohort, by = c("stratumId", "treatment")) %>%
      dplyr::select(rowId, treatment, weight)

    wSum <- w %>%
      dplyr::group_by(treatment) %>%
      dplyr::summarize(wSum = dplyr::sum(weight, na.rm = TRUE)) %>%
      dplyr::ungroup()

    cohortMethodData$w <- w %>%
      dplyr::inner_join(wSum, by = "treatment") %>%
      dplyr::mutate(weight = weight / wSum) %>%
      select(rowId, treatment, weight)

    sumW <- 1

    result <- cohortMethodData$covariates %>%
      dplyr::inner_join(cohortMethodData$w, by = c("rowId")) %>%
      dplyr::group_by(covariateId, treatment) %>%
      dplyr::summarize(
        sum = sum(as.numeric(covariateValue), na.rm = TRUE),
        mean = sum(weight * as.numeric(covariateValue), na.rm = TRUE),
        sumSqr = sum(weight * as.numeric(covariateValue)^2, na.rm = TRUE),
        sumWSqr = sum(weight^2, na.rm = TRUE),
        .groups = "keep") %>%
      dplyr::mutate(
        sd = sqrt(abs(sumSqr - mean^2) * sumW/(sumW^2 - sumWSqr))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        covariateId,
        treatment,
        sum,
        mean,
        sd
      ) %>%
      dplyr::collect()

    cohortMethodData$w <- NULL

  } else {

    cohortCounts <- cohort %>%
      dplyr::group_by(treatment) %>%
      dplyr::count()

    covariates <- tibble::as_tibble(cohortMethodData$covariates)

    result <- covariates %>%
      dplyr::inner_join(dplyr::select(cohort, rowId, treatment), by = "rowId") %>%
      dplyr::group_by(covariateId, treatment) %>%
      dplyr::summarize(
        sum = sum(as.numeric(covariateValue), na.rm = TRUE),
        sumSqr = sum(as.numeric(covariateValue)^2, na.rm = TRUE),
        .groups = "keep"
      ) %>%
      dplyr::inner_join(cohortCounts, by = "treatment") %>%
      dplyr::mutate(
        sd = sqrt((sumSqr - (sum^2 / n)) / n),
        mean = sum / n
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        covariateId,
        treatment,
        sum,
        mean,
        sd
      ) %>%
      dplyr::collect()
  }

  return(result)
}

plotCovariateBalanceScatterPlot2 <- function(balance,
                                             absolute = TRUE,
                                             threshold = 0,
                                             title = "Standardized difference of mean",
                                             fileName = NULL,
                                             analyticLabel = "Analytic covariate prevalence",
                                             targetLabel = "Target covariate prevalence",
                                             showCovariateCountLabel = FALSE,
                                             showMaxLabel = FALSE) {
  analyticLabel <- as.character(analyticLabel)
  targetLabel <- as.character(targetLabel)

  if (absolute) {
    balance$stdDiff <- abs(balance$stdDiff)
  }

  limits <- c(0, 1)

  plot <- ggplot2::ggplot(balance,
                          ggplot2::aes(x = .data$meanTarget, y = .data$meanAnalytic)) +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_x_continuous(analyticLabel, limits = limits) +
    ggplot2::scale_y_continuous(targetLabel, limits = limits)
  if (threshold != 0) {
    plot <- plot + ggplot2::geom_hline(yintercept = c(threshold,
                                                      -threshold), alpha = 0.5, linetype = "dotted")
  }
  if (showCovariateCountLabel || showMaxLabel) {
    labels <- c()
    if (showCovariateCountLabel) {
      labels <- c(labels, sprintf("Number of covariates: %s", format(nrow(balance), big.mark = ",", scientific = FALSE)))
    }
    if (showMaxLabel) {
      labels <- c(labels, sprintf("%s max(absolute): %.2f", analyticLabel, max(abs(balance$stdDiff), na.rm = TRUE)))
    }
    dummy <- data.frame(text = paste(labels, collapse = "\n"))
    plot <- plot + ggplot2::geom_label(x = limits[1] + 0.01, y = limits[2], hjust = "left", vjust = "top", alpha = 0.8, ggplot2::aes(label = text), data = dummy, size = 3.5)

  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 4, height = 4, dpi = 400)
  }
  return(plot)
}

