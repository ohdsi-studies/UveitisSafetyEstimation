#' @export
CreateFigures <- function(shinyDataFolder,
                          figuresFolder) {

  if (!file.exists(figuresFolder)) {
    dir.create(figuresFolder, recursive = TRUE)
  }

  load(file.path(shinyDataFolder, "PreMergedShinyData.RData"))

  databaseIds <- database$databaseId[!database$databaseId == "Meta-analysis"]
  analysisIds <- cohortMethodAnalysis$analysisId

  for (i in nrow(tcos)) {

    targetId <- tcos$targetId[i]
    targetName <- shortenExposureName(targetId)

    comparatorId <- tcos$comparatorId[i]
    comparatorName <- shortenExposureName(comparatorId)

    for (analysisId in analysisIds) {

      plots <- list()

      for (databaseId in databaseIds) {

       # preference score =====================================================

        ps <- getPs(shinyDataFolder = shinyDataFolder,
                    targetId = targetId,
                    comparatorId = comparatorId,
                    analysisId = analysisId,
                    databaseId = databaseId)

        targetSize <- attrition$subjects[attrition$databaseId == databaseId &
                                           attrition$exposureId == targetId &
                                           attrition$targetId == targetId &
                                           attrition$comparatorId == comparatorId &
                                           attrition$analysisId == analysisId &
                                           attrition$description == "Matched on propensity score"]

        comparatorSize <- attrition$subjects[attrition$databaseId == databaseId &
                                               attrition$exposureId == comparatorId &
                                               attrition$targetId == targetId &
                                               attrition$comparatorId == comparatorId &
                                               attrition$analysisId == analysisId &
                                               attrition$description == "Matched on propensity score"]

        psPlot <- plotPs(ps = ps,
                         targetName = targetName,
                         comparatorName = comparatorName,
                         targetSize = targetSize,
                         comparatorSize = comparatorSize)

        plots[[length(plots) + 1]] <- psPlot

        # covariate balance ====================================================

        balance <- getCovariateBalance(shinyDataFolder = shinyDataFolder,
                                       covariate = covariate,
                                       targetId = targetId,
                                       comparatorId = comparatorId,
                                       databaseId = databaseId,
                                       analysisId = analysisId)

        balancePlot <- plotCovariateBalance(balance = balance,
                                            beforeLabel = "ASMD before matching",
                                            afterLabel = "ASMD after matching")

        plots[[length(plots) + 1]] <- balancePlot

        # calibration ==========================================================

        controlResults <- getControlResults(cohortMethodResult = cohortMethodResult,
                                            targetId = targetId,
                                            comparatorId = comparatorId,
                                            analysisId = analysisId,
                                            databaseId = databaseId)
        if (sum(!is.na(controlResults$rr)) < 5) {
          calibrationPlot <- ggplot2::ggplot()
        } else {
          calibrationPlot <- plotCalibration(d = controlResults)
        }

        plots[[length(plots) + 1]] <- calibrationPlot

        # target-analytic balance ==============================================

        taBal <- getTaBalance(taBalance = taBalance,
                              targetId = targetId,
                              comparatorId = comparatorId,
                              analysisId = analysisId,
                              databaseId = databaseId)

        taBalPlots <- plotTaBalance(balance = taBal,
                                    absolute = TRUE,
                                    threshold = 0,
                                    title = "Target vs analytic cohort covariate prevalence",
                                    analyticLabel = "Analytic covariate prevalence",
                                    targetLabel = "Target covariate prevalence",
                                    showCovariateCountLabel = FALSE,
                                    showMaxLabel = FALSE)

        plots[[length(plots) + 1]] <- taBalPlots
      }

      row1 <- grid::textGrob("Amb. EMR", rot = 90, gp = grid::gpar(fontsize = 30))
      row2 <- grid::textGrob("Pharmetrics", rot = 90, gp = grid::gpar(fontsize = 30))
      row3 <- grid::textGrob("Optum® EHR", rot = 90, gp = grid::gpar(fontsize = 30))
      row4 <- grid::textGrob("Clinformatics®", rot = 90, gp = grid::gpar(fontsize = 30))
      row5 <- grid::textGrob("CCAE", rot = 90, gp = grid::gpar(fontsize = 30))
      col0 <- grid::textGrob("")
      col1 <- grid::textGrob("Equipoise", gp = grid::gpar(fontsize = 30))
      col2 <- grid::textGrob("Covariate balance", gp = grid::gpar(fontsize = 30))
      col3 <- grid::textGrob("Calibration", gp = grid::gpar(fontsize = 30))
      col4 <- grid::textGrob("Representativeness", gp = grid::gpar(fontsize = 30))

      plotGrob <- gridExtra::arrangeGrob(col0, col1,        col2,        col3,        col4,
                                         row1, plots[[1]],  plots[[2]],  plots[[3]],  plots[[4]],
                                         row2, plots[[5]],  plots[[6]],  plots[[7]],  plots[[8]],
                                         row3, plots[[9]],  plots[[10]], plots[[11]], plots[[12]],
                                         row4, plots[[13]], plots[[14]], plots[[15]], plots[[16]],
                                         row5, plots[[17]], plots[[18]], plots[[19]], plots[[20]],
                                         heights = c(0.5, 4, 4, 4, 4, 4),
                                         widths = c(0.5, 4, 4, 4, 4),
                                         nrow = 6)

      plotName <- sprintf("diagnostics_t%s_c%s_a%s.png",
                          targetId,
                          comparatorId,
                          analysisId)
      ggplot2::ggsave(filename = file.path(figuresFolder, plotName),
                      plot = plotGrob,
                      height = 25.5,
                      width = 40)
    }
  }
}

# data grab and plotting functions =============================================

shortenExposureName <- function(exposureId) {
  if (exposureId == 93678449) {
    exposureName <- "RA - Remicade®(m)"
  }
  if (exposureId == 93699028) {
    exposureName <- "IBD - Remicade®"
  }
  if (exposureId == 93719029) {
    exposureName <- "PsO/PsA - Remicade®"
  }
  if (exposureId == 93739048) {
    exposureName <- "AS - Remicade®"
  }
  if (exposureId == 93688449) {
    exposureName <- "RA - comparator"
  }
  if (exposureId == 93709028) {
    exposureName <- "IBD - comparator"
  }
  if (exposureId == 93729029) {
    exposureName <- "PsO/PsA - comparator"
  }
  if (exposureId == 93749048) {
    exposureName <- "AS - comparator"
  }
  return(exposureName)
}

# ps functions =================================================================

getPs <- function(shinyDataFolder,
                  targetId,
                  comparatorId,
                  analysisId,
                  databaseId) {
  file <- sprintf("preference_score_dist_t%s_c%s_%s.rds", targetId, comparatorId, databaseId)
  filePath <- file.path(shinyDataFolder, file)
  if (!file.exists(filePath)) {
    return(NULL)
  }
  ps <- readRDS(filePath)
  colnames(ps) <- SqlRender::snakeCaseToCamelCase(colnames(ps))
  ps <- ps[ps$analysisId == analysisId, ]
  return(ps)
}

plotPs <- function(ps,
                   targetName = "T",
                   comparatorName = "C",
                   targetSize,
                   comparatorSize) {
  psDensity <- ps
  ps <- rbind(data.frame(x = ps$preferenceScore, y = ps$targetDensity, group = targetName),
              data.frame(x = ps$preferenceScore, y = ps$comparatorDensity, group = comparatorName))
  ps$group <- factor(ps$group, levels = c(as.character(targetName), as.character(comparatorName)))
  theme <- ggplot2::element_text(colour = "#000000", size = 20, margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm"))
  plot <- ggplot2::ggplot(ps,
                          ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
    ggplot2::geom_vline(xintercept = c(0, 0.25, 0.5, 0.75, 1), colour = "white", lty = 1, size = 0.5) +
    ggplot2::geom_density(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                           rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.text = theme,
                   axis.text.y = theme,
                   axis.text = theme,
                   axis.title = theme)
  psFiltered <- psDensity[psDensity$preferenceScore >= 0.3 & psDensity$preferenceScore <= 0.7, ]
  targetFraction <- sum(psFiltered$targetDensity) / sum(psDensity$targetDensity)
  comparatorFraction <- sum(psFiltered$comparatorDensity) / sum(psDensity$comparatorDensity)
  totalFraction <- (targetFraction * targetSize + comparatorFraction * comparatorSize) / (targetSize + comparatorSize)
  labelsRight <- sprintf("Equipoise: %2.1f%%", totalFraction * 100)
  plot <- plot + ggplot2::geom_label(x = 1,
                                     y = max(ps$y),
                                     hjust = "right",
                                     vjust = "top",
                                     alpha = 0.7,
                                     ggplot2::aes(label = text),
                                     data = data.frame(text = labelsRight), size = 7, inherit.aes = FALSE)
  return(plot)
}


# covariate balance functions ==================================================

getCovariateBalance <- function(shinyDataFolder,
                                covariate = covariate,
                                targetId,
                                comparatorId,
                                databaseId,
                                analysisId) {
  file <- sprintf("covariate_balance_t%s_c%s_%s.rds", targetId, comparatorId, databaseId)
  file <- file.path(shinyDataFolder, file)
  if (!file.exists(file)) {
    return(NULL)
  } else {
    balance <- readRDS(file)
    colnames(balance) <- SqlRender::snakeCaseToCamelCase(colnames(balance))
    balance <- balance[balance$analysisId == analysisId, ]
    balance <- merge(balance, covariate[covariate$databaseId == databaseId & covariate$analysisId == analysisId,
                                        c("covariateId", "covariateAnalysisId", "covariateName")])
    balance <- balance[ c("covariateId",
                          "covariateName",
                          "covariateAnalysisId",
                          "targetMeanBefore",
                          "comparatorMeanBefore",
                          "stdDiffBefore",
                          "targetMeanAfter",
                          "comparatorMeanAfter",
                          "stdDiffAfter")]
    colnames(balance) <- c("covariateId",
                           "covariateName",
                           "analysisId",
                           "beforeMatchingMeanTreated",
                           "beforeMatchingMeanComparator",
                           "beforeMatchingStdDiff",
                           "afterMatchingMeanTreated",
                           "afterMatchingMeanComparator",
                           "afterMatchingStdDiff")
    balance$absBeforeMatchingStdDiff <- abs(balance$beforeMatchingStdDiff)
    balance$absAfterMatchingStdDiff <- abs(balance$afterMatchingStdDiff)
    return(balance)
  }
}

plotCovariateBalance <- function(balance,
                                 beforeLabel = "Before matching",
                                 afterLabel = "After matching") {

  nCovars <- nrow(balance)
  nImbalancedCovars <- nrow(balance[balance$absAfterMatchingStdDiff > 0.1, ])
  labelsRight <- sprintf("# Covariates ASMD > 0.1: %1.0f", nImbalancedCovars)
  limits <- c(min(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff), na.rm = TRUE),
              max(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff), na.rm = TRUE))
  theme <- ggplot2::element_text(colour = "#000000", size = 20)

  plot <- ggplot2::ggplot(balance, ggplot2::aes(x = absBeforeMatchingStdDiff, y = absAfterMatchingStdDiff)) +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16, size = 2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 1) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
    ggplot2::scale_y_continuous(afterLabel, limits = limits) +
    ggplot2::theme(text = ggplot2::element_text(colour = "#000000", size = 12),
                   axis.title = theme,
                   axis.text.y = theme,
                   axis.text.x = theme,
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::geom_hline(yintercept = 0.1, linetype = "dashed", color = "red", size = 1) +
    ggplot2::geom_label(x = limits[2],
                        y = limits[2],
                        hjust = "right",
                        vjust = "top",
                        alpha = 1,
                        ggplot2::aes(label = text),
                        data = data.frame(text = labelsRight), size = 7, inherit.aes = FALSE)

  return(plot)
}


# calibration functions ========================================================

getControlResults <- function(cohortMethodResult,
                              targetId,
                              comparatorId,
                              analysisId,
                              databaseId) {
  results <- cohortMethodResult[cohortMethodResult$targetId == targetId &
                                  cohortMethodResult$comparatorId == comparatorId &
                                  cohortMethodResult$analysisId == analysisId &
                                  cohortMethodResult$databaseId == databaseId, ]
  results$effectSize <- NA
  ncIds <- unique(cohortMethodResult$outcomeId[!cohortMethodResult$outcomeId == 8466])
  idx <- results$outcomeId %in% ncIds
  results$effectSize[idx] <- 1
  results <- results[!is.na(results$effectSize), ]
  return(results)
}


plotCalibration <- function(d) {
  d <- d[!is.na(d$seLogRr), ]
  d$significant  <- d$ci95Lb > 1 | d$ci95Ub < 1
  estimates <- length(d$seLogRr)

  uncalibratedNull <- EmpiricalCalibration::fitNull(d$logRr, d$seLogRr)
  easeUncalibrated <- EmpiricalCalibration::computeExpectedAbsoluteSystematicError(uncalibratedNull)
  easeUncalibratedLabel <- sprintf("EASE: %.2f", easeUncalibrated)
  d$easeLabel <- easeUncalibratedLabel

  oneRow <- data.frame(nLabel = paste0(formatC(estimates, big.mark = ","), " estimates"),
                       meanLabel = paste0(formatC(100 * mean(!d$significant, na.rm = TRUE), digits = 1, format = "f"), "% of CIs includes 1"),
                       easeLabel = d$easeLabel)
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 20)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  themeLA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 0)
  alpha <- 1 - min(0.95 * (nrow(d)/50000)^0.1, 0.95)

  plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y = seLogRr)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "white", lty = 1, size = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1/qnorm(0.025)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1.5,
                         alpha = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1/qnorm(0.975)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1.5,
                         alpha = 0.5) +
    ggplot2::geom_point(size = 2, color = rgb(0, 0, 0, alpha = 0.05), alpha = alpha, shape = 16) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_label(x = log(0.11),
                        y = 1,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = nLabel),
                        size = 7,
                        data = oneRow) +
    ggplot2::geom_label(x = log(0.11),
                        y = 0.9,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = meanLabel),
                        size = 7,
                        data = oneRow) +
    ggplot2::geom_label(x = log(0.11),
                        y = 0.8,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = easeLabel),
                        size = 7,
                        data = oneRow) +

    ggplot2::scale_x_continuous("HR", limits = log(c(0.1, 10)), breaks = log(breaks), labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = theme,
                   axis.text.x = theme,
                   axis.title = theme,
                   legend.key = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   legend.position = "none")
  return(plot)
}


# target-analytic balance functions ============================================

getTaBalance <- function(taBalance,
                         targetId,
                         comparatorId,
                         analysisId,
                         databaseId) {
  taBal <- taBalance[taBalance$targetId == targetId &
                       taBalance$comparatorId == comparatorId &
                       taBalance$analysisId == analysisId &
                       taBalance$databaseId == databaseId, ]
  return(taBal)
}

plotTaBalance <- function(balance,
                          absolute = TRUE,
                          threshold = 0,
                          title = "Target vs after matching cohort covariate prevalence",
                          analyticLabel = "After matching covariate prevalence",
                          targetLabel = "Target covariate prevalence",
                          showCovariateCountLabel = FALSE,
                          showMaxLabel = FALSE) {
  analyticLabel <- as.character(analyticLabel)
  targetLabel <- as.character(targetLabel)

  if (absolute) {
    balance$stdDiff <- abs(balance$stdDiff)
  }

  limits <- c(0, 1)
  theme <- ggplot2::element_text(colour = "#000000", size = 20)
  plot <- ggplot2::ggplot(balance,
                          ggplot2::aes(x = .data$meanTarget, y = .data$meanAnalytic)) +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_x_continuous(analyticLabel, limits = limits) +
    ggplot2::scale_y_continuous(targetLabel, limits = limits) +
    ggplot2::theme(text = ggplot2::element_text(colour = "#000000", size = 20),
                   axis.text.y = theme,
                   axis.text.x = theme,
                   axis.title = theme,
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank())

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
  return(plot)
}


# HR forest plots ==============================================================

getMainResults <- function(cohortMethodResult,
                           targetIds = c(),
                           comparatorIds = c(),
                           outcomeIds = c(),
                           databaseIds = c(),
                           analysisIds = c()) {
  idx <- rep(TRUE, nrow(cohortMethodResult))
  if (length(targetIds) != 0) {
    idx <- idx & cohortMethodResult$targetId %in% targetIds
  }
  if (length(comparatorIds) != 0) {
    idx <- idx & cohortMethodResult$comparatorId %in% comparatorIds
  }
  if (length(outcomeIds) != 0) {
    idx <- idx & cohortMethodResult$outcomeId %in% outcomeIds
  }
  if (length(databaseIds) != 0) {
    idx <- idx & cohortMethodResult$databaseId %in% databaseIds
  }
  if (length(analysisIds) != 0) {
    idx <- idx & cohortMethodResult$analysisId %in% analysisIds
  }
  return(cohortMethodResult[idx, ])
}


plotForest <- function(cohortMethodResult,
                       cohortMethodAnalysis) {

  plotResults <- getMainResults(cohortMethodResult = cohortMethodResult,
                                targetIds = 93699028,
                                comparatorIds = 93709028,
                                outcomeIds = 8466)

  plotResults <- merge(x = plotResults,
                       y = cohortMethodAnalysis)
  names(plotResults)[26] <- "analysisName"

  plotResults$tIr1kpy <- format(round(plotResults$targetOutcomes / (plotResults$targetDays / 365.25) * 1000, 2), nsmall = 2)
  plotResults$cIr1kpy <- format(round(plotResults$comparatorOutcomes / (plotResults$comparatorDays / 365.25) * 1000, 2), nsmall = 2)

  breaks <- c(0.175, 0.25, 0.5, 1, 2, 4)
  plotLabels <- c(0.175, paste("0.25\nFavors target"), 0.5, 1, paste("2\nFavors comparator"), 4)

  d <- plotResults[, c("analysisName",
                       "databaseId",
                       "targetSubjects",
                       "targetOutcomes",
                       "tIr1kpy",
                       "comparatorSubjects",
                       "comparatorOutcomes",
                       "cIr1kpy",
                       "calibratedRr",
                       "calibratedCi95Lb",
                       "calibratedCi95Ub",
                       "i2")]
  d[, c("calibratedRr", "calibratedCi95Lb", "calibratedCi95Ub")] <- log(d[, c("calibratedRr", "calibratedCi95Lb", "calibratedCi95Ub")])
  d$type <- NA
  d$type[d$databaseId == "Meta-analysis"] <- "ma"
  d$type[is.na(d$type)] <- 'db'
  d$databaseId[d$databaseId == "Meta-analysis"] <- paste0("Summary (I2", ifelse(d$i2[d$databaseId == "Meta-analysis"] < 0.01, "<0.01", paste0("=", d$i2[d$databaseId == "Meta-analysis"])), ")")
  d$i2 <- NULL
  names(d) <- c("analysisName",
                "databaseId",
                "t",
                "tEvents",
                "tIr",
                "c",
                "cEvents",
                "cIr",
                "logRr",
                "logLb95Ci",
                "logUb95Ci",
                "type")

  d$aOrder <- match(d$analysisName, c("1:10 PS matched, on-treatment",
                                      "1:10 PS matched, ITT",
                                      "1:1 PS matched, on-treatment",
                                      "1:1 PS matched, ITT"))
  d$dbOrder <- match(d$databaseId, c("Database",
                                     "iqvia_amb_emr",
                                     "iqvia_pharmetrics_plus",
                                     "optum_ehr",
                                     "truven_ccae"))
  d <- d[order(d$aOrder, d$dbOrder, d$type), ]
  d$aOrder <- NULL
  d$dbOrder <- NULL

  d$databaseId[d$databaseId == "iqvia_amb_emr"] <- "Amb EMR"
  d$databaseId[d$databaseId == "iqvia_pharmetrics_plus"] <- "Pharmetrics"
  d$databaseId[d$databaseId == "optum_ehr"] <- "Optum® EHR"
  d$databaseId[d$databaseId == "truven_ccae"] <- "CCAE"
  d$databaseId[d$databaseId == "optum_extended_dod"] <- "Clinformatics®"

  d$analysisName[d$analysisName == "1:10 PS matched, on-treatment"] <- "1:10 PS matched, OT"
  d$analysisName[d$analysisName == "1:1 PS matched, on-treatment"] <- "1:1 PS matched, OT"

  d$t <- formatC(d$t, big.mark = ",", format = "d")
  d$c <- formatC(d$c, big.mark = ",", format = "d")

  header <- data.frame(analysisName = "Analysis",
                       databaseId = "Database",
                       t = "patients",
                       tEvents = "T events",
                       tIr = "T IR/1,000 PYs",
                       c = "patients",
                       cEvents = "C events",
                       cIr = "C IR/1,000 PYs",
                       logRr = -100,
                       logLb95Ci = -100,
                       logUb95Ci = -100,
                       type = "header")
  d <- rbind(header, d)
  d$lcl <- ifelse(d$logLb95Ci < log(0.175), log(0.175), d$logLb95Ci)
  d$ucl <- ifelse(d$logUb95Ci > log(6), log(6), d$logUb95Ci)
  d$lcl[d$type == "header"] <- -100
  d$ucl[d$type == "header"] <- -100
  d$lcl[is.na(d$lcl)] <- -100
  d$ucl[is.na(d$ucl)] <- -100
  d$logRr[is.na(d$logRr)] <- -100
  d$logLb95Ci[is.na(d$logLb95Ci)] <- -100
  d$logUb95Ci[is.na(d$logUb95Ci)] <- -100
  dropRows <- (d$logRr == -100 | d$logLb95Ci == -100 | d$logUb95Ci == -100) & d$type %in% c("db", "ma")
  d <- d[!dropRows, ]
  d$row <- rev(1:nrow(d))

  rr <- ifelse(exp(d$logRr) > 100, ">100", formatC(exp(d$logRr), digits = 2, format = "f"))
  rr[rr == "0.00"] <- "<0.01"

  rrLb95Ci <- ifelse(exp(d$logLb95Ci) < 0.01, "<0.01", formatC(exp(d$logLb95Ci), digits = 2, format = "f"))
  rrUb95Ci <- ifelse(exp(d$logUb95Ci) > 100, ">100", formatC(exp(d$logUb95Ci), digits = 2, format = "f"))
  rrUb95Ci[rrUb95Ci == "0.00"] <- "<0.01"

  estimateLabels <- paste0(rr, " (", rrLb95Ci, ", ", rrUb95Ci, ")")
  tEventLabels <- paste0(d$tEvents, "/", d$t)
  cEventLabels <- paste0(d$cEvents, "/", d$c)

  labels <- data.frame(y = rep(d$row, 7), # 95
                       x = rep(c(-15.5, -13.5, -11.5, -9.5, -7.5, -5.5, -3.5), each = nrow(d)), # 57
                       label = c(as.character(d$analysisName),
                                 as.character(d$databaseId),
                                 tEventLabels,
                                 as.character(d$tIr),
                                 cEventLabels,
                                 as.character(d$cIr),
                                 estimateLabels), # 57
                       stringsAsFactors = FALSE)

  labels$label[labels$x == -15.5 & duplicated(labels$label)] <- "" # here for remove redundant labels
  labels$label[labels$label == "<0.01 (<0.01, <0.01)" & labels$comparison == "T vs C"] <- "cHR (95% CI)"
  labels$label[labels$label == "<0.01 (<0.01, <0.01)"] <- ""
  labels$label[labels$label == ">100 (<0.01, <0.01)"] <- ""
  labels$label[labels$y == max(labels$y) & labels$x == -3.5] <- "cHR (CI 95%)"

  if (length(d$row[d$logLb95Ci < d$lcl]) > 0) {
    lclData <- data.frame(x = log(0.175),
                          xend = log(0.175),
                          y = d$row[d$logLb95Ci < d$lcl],
                          yend = d$row[d$logLb95Ci < d$lcl])
  } else {
    lclData <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  if (length(d$row[d$logUb95Ci > d$ucl]) > 0) {
    uclData <- data.frame(x = log(6),
                          xend = log(6),
                          y = d$row[d$logUb95Ci > d$ucl],
                          yend = d$row[d$logUb95Ci > d$ucl])
  } else {
    uclData <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }

  plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y = row)) +

    ggplot2::scale_fill_manual(values = c('#f7f7f7','#cccccc','#f7f7f7','#cccccc', '#f7f7f7'), breaks = levels(d$analysisName)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -15.5, xmax = 10, ymin = row - 0.5, ymax = row + 0.5, fill = analysisName), alpha =0.5) +

    ggplot2::geom_vline(xintercept = log(breaks), colour = "light gray", lty = 1, size = 0.2) +
    ggplot2::geom_vline(xintercept = 0, size = 0.5) +
    ggplot2::geom_errorbarh(height = 0, ggplot2::aes(xmin = lcl, xmax = ucl)) +
    ggplot2::geom_segment(data = lclData,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 30, type = "open", length = ggplot2::unit(0.05, "inches"))) +
    ggplot2::geom_segment(data = uclData,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 210, type = "open", length = ggplot2::unit(0.05, "inches"))) +
    ggplot2::geom_point(size = 3, ggplot2::aes(shape = type), fill = "white", show.legend = FALSE) +
    ggplot2::scale_shape_manual(values = c(18, 16, 23)) +
    ggplot2::scale_x_continuous(breaks = log(breaks), labels = plotLabels) +
    ggplot2::coord_cartesian(xlim = c(-15.5, log(5)), ylim = c(min(d$row), max(d$row))) +
    ggplot2::geom_text(size = 4.5, hjust = 0, vjust = 0.5, ggplot2::aes(x = x, y = y, label = label), data = labels) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))

  plotName <- sprintf("hr_plot_t%s_c%s.png",
                      targetId,
                      comparatorId)
  ggplot2::ggsave(filename = file.path(figuresFolder, plotName),
                  plot = plot,
                  height = 8,
                  width = 17.5)

  return(plot)
}
