library(magrittr)
baseUrl <- Sys.getenv("BASE_URL")
ROhdsiWebApi::authorizeWebApi(baseUrl, "windows")

# Cohort specifications ========================================================

exposureCohortIds <- c(
  9367, # [964_5] Remicade and methotrexate (RA)
  9368, # [964_5] certolizumab pegol, tocilizumab (RA)
  9369, # [964_5] Remicade (CD-UC)
  9370, # [964_5] golimumab, certolizumab pegol, ustekinumab, vedolizumab (CD-UC)
  9371, # [964_5] Remicade (PP-PsA)
  9372, # [964_5] golim, certz, guselk, risankiz, tildrakiz, brodal, ixekiz, secukin, ustekin (PsO-PsA)
  9373, # [964_5] Remicade (AS)
  9374  # [964_5] certolizumab pegol, golimumab, ixekizumab, secukinumab (AS)
)

indicationCohortIds <- c(
  8449, # [964] Rheumatoid Arthritis
  9048, # [964] Ankylosing Spondylitis
  9028, # [964] Crohn's disease or ulcerative colitis
  9029  # [964] Psoriatic arthritis or plaque psoriasis
)

outcomeCohortId <- c(8466) # [964] Non-infectious uveitis (primary)

cohortIds <- c(
  exposureCohortIds,
  indicationCohortIds,
  outcomeCohortId
)

cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = cohortIds
)

CohortGenerator::saveCohortDefinitionSet(
  cohortDefinitionSet = cohortDefinitionSet,
  settingsFileName = file.path("inst/settings/CohortsToCreate.csv"),
  jsonFolder = file.path("inst/cohorts"),
  sqlFolder = file.path("inst/sql/sql_server")
)

exposureCohortDefinitionSet <- cohortDefinitionSet %>%
  dplyr::filter(cohortId %in% exposureCohortIds) %>%
  dplyr::mutate(cohortName = gsub("\\[964_5\\] ", "", cohortName)) %>%
  readr::write_csv(file.path("inst/settings/exposureCohortsToCreate.csv"))

indicationCohortDefinitionSet <- cohortDefinitionSet %>%
  dplyr::filter(cohortId %in% indicationCohortIds) %>%
    dplyr::mutate(cohortName = gsub("\\[964\\] ", "", cohortName)) %>%
  readr::write_csv(file.path("inst/settings/indicationCohortsToCreate.csv"))

outcomeCohortDefinitionSet <- cohortDefinitionSet %>%
  dplyr::filter(cohortId %in% outcomeCohortId) %>%
  dplyr::mutate(cohortName = gsub("\\[964\\] ", "", cohortName)) %>%
  readr::write_csv(file.path("inst/settings/outcomeCohortsToCreate.csv"))


# epi_964_3: Exposure indication cohorts intersection settings =================

# indicationRef <- indicationCohortDefinitionSet %>%
#   dplyr::select(cohortId, cohortName) %>%
#   dplyr::rename(indicationCohortId = cohortId,
#                 indicationCohortName = cohortName)
#
# remMethExposureRef <- exposureCohortDefinitionSet %>%
#   dplyr::select(cohortId, cohortName) %>%
#   dplyr::filter(cohortId %in% exposureCohortIds[1]) %>%
#   dplyr::rename(exposureCohortId = cohortId,
#                 exposureCohortName = cohortName)
#
# remExposureRef <- exposureCohortDefinitionSet %>%
#   dplyr::select(cohortId, cohortName) %>%
#   dplyr::filter(cohortId %in% exposureCohortIds[2]) %>%
#   dplyr::rename(exposureCohortId = cohortId,
#                 exposureCohortName = cohortName)
#
# otherExposureRef <- exposureCohortDefinitionSet %>%
#   dplyr::select(cohortId, cohortName) %>%
#   dplyr::filter(!(cohortId %in% exposureCohortIds[1:2])) %>%
#   dplyr::rename(exposureCohortId = cohortId,
#                 exposureCohortName = cohortName)
#
#
# remMethExposureIndicationRef <- merge(remMethExposureRef,
#                                       indicationRef[indicationRef$indicationCohortId == 8449, ]) %>%
#   dplyr::mutate(exposureIndicationCohortId = as.numeric(paste0(exposureCohortId, indicationCohortId)),
#                 exposureIndicationCohortName = sprintf("%s w %s", exposureCohortName, indicationCohortName)) %>%
#   dplyr::mutate(exposureIndicationCohortName = gsub("\\[964_4\\] ", "", exposureIndicationCohortName))
#
#
# remExposureIndicationRef <- merge(remExposureRef,
#                                   indicationRef[indicationRef$indicationCohortId != 8449, ]) %>%
#   dplyr::mutate(exposureIndicationCohortId = as.numeric(paste0(exposureCohortId, indicationCohortId)),
#                 exposureIndicationCohortName = sprintf("%s w %s", exposureCohortName, indicationCohortName)) %>%
#   dplyr::mutate(exposureIndicationCohortName = gsub("\\[964_3\\] ", "", exposureIndicationCohortName))
#
#
# otherExposureIndicationRef <- merge(otherExposureRef,
#                                     indicationRef) %>%
#   dplyr::mutate(exposureIndicationCohortId = as.numeric(paste0(exposureCohortId, indicationCohortId)),
#                 exposureIndicationCohortName = sprintf("%s w %s", exposureCohortName, indicationCohortName)) %>%
#   dplyr::mutate(exposureIndicationCohortName = gsub("\\[964_3\\] ", "", exposureIndicationCohortName))
#
#
# exposureIndicationRef <- dplyr::bind_rows(remMethExposureIndicationRef,
#                                           remExposureIndicationRef,
#                                           otherExposureIndicationRef) %>%
#   readr::write_csv(file.path("inst/settings/exposureIndicationCohortsToCreate.csv"))


# epi_964_5: Exposure indication cohorts intersection settings =================

indicationRef <- indicationCohortDefinitionSet %>%
  dplyr::select(cohortId, cohortName) %>%
  dplyr::rename(indicationCohortId = cohortId,
                indicationCohortName = cohortName)

raExposureRef <- exposureCohortDefinitionSet %>%
  dplyr::select(cohortId, cohortName) %>%
  dplyr::filter(cohortId %in% exposureCohortIds[1:2]) %>%
  dplyr::rename(exposureCohortId = cohortId,
                exposureCohortName = cohortName)

cdUcExposureRef <- exposureCohortDefinitionSet %>%
  dplyr::select(cohortId, cohortName) %>%
  dplyr::filter(cohortId %in% exposureCohortIds[3:4]) %>%
  dplyr::rename(exposureCohortId = cohortId,
                exposureCohortName = cohortName)

ppPsaExposureRef  <- exposureCohortDefinitionSet %>%
  dplyr::select(cohortId, cohortName) %>%
  dplyr::filter(cohortId %in% exposureCohortIds[5:6]) %>%
  dplyr::rename(exposureCohortId = cohortId,
                exposureCohortName = cohortName)

asExposureRef  <- exposureCohortDefinitionSet %>%
  dplyr::select(cohortId, cohortName) %>%
  dplyr::filter(cohortId %in% exposureCohortIds[7:8]) %>%
  dplyr::rename(exposureCohortId = cohortId,
                exposureCohortName = cohortName)


raExposureIndicationRef <- merge(raExposureRef,
                                 indicationRef[indicationRef$indicationCohortId == 8449, ]) %>%
  dplyr::mutate(exposureIndicationCohortId = as.numeric(paste0(exposureCohortId, indicationCohortId)),
                exposureIndicationCohortName = sprintf("%s w %s", exposureCohortName, indicationCohortName)) %>%
  dplyr::mutate(exposureIndicationCohortName = gsub("\\[964_5\\] ", "", exposureIndicationCohortName))

cdUcExposureIndicationRef <- merge(cdUcExposureRef,
                                   indicationRef[indicationRef$indicationCohortId == 9028, ]) %>%
  dplyr::mutate(exposureIndicationCohortId = as.numeric(paste0(exposureCohortId, indicationCohortId)),
                exposureIndicationCohortName = sprintf("%s w %s", exposureCohortName, indicationCohortName)) %>%
  dplyr::mutate(exposureIndicationCohortName = gsub("\\[964_5\\] ", "", exposureIndicationCohortName))


ppPsaExposureIndicationRef <- merge(ppPsaExposureRef,
                                    indicationRef[indicationRef$indicationCohortId == 9029, ]) %>%
  dplyr::mutate(exposureIndicationCohortId = as.numeric(paste0(exposureCohortId, indicationCohortId)),
                exposureIndicationCohortName = sprintf("%s w %s", exposureCohortName, indicationCohortName)) %>%
  dplyr::mutate(exposureIndicationCohortName = gsub("\\[964_5\\] ", "", exposureIndicationCohortName))


asExposureIndicationRef <- merge(asExposureRef,
                                 indicationRef[indicationRef$indicationCohortId == 9048, ]) %>%
  dplyr::mutate(exposureIndicationCohortId = as.numeric(paste0(exposureCohortId, indicationCohortId)),
                exposureIndicationCohortName = sprintf("%s w %s", exposureCohortName, indicationCohortName)) %>%
  dplyr::mutate(exposureIndicationCohortName = gsub("\\[964_5\\] ", "", exposureIndicationCohortName))

exposureIndicationRef <- dplyr::bind_rows(raExposureIndicationRef,
                                          cdUcExposureIndicationRef,
                                          ppPsaExposureIndicationRef,
                                          asExposureIndicationRef) %>%
  readr::write_csv(file.path("inst/settings/exposureIndicationCohortsToCreate.csv"))



# uveitis phenotype evaluation settings ========================================

uveitisNarrowCohortId <- 8467
uveitisBroadCohortId <- 8468
uveitisPrimaryCohortId <- 8466

uveitisXSpecCohortId <- 13747
uveitisPrevCohortId <- 13748

validationCohortIds <- c(
  uveitisNarrowCohortId,
  uveitisBroadCohortId,
  uveitisPrimaryCohortId,
  uveitisXSpecCohortId,
  uveitisPrevCohortId
)

validationCohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = validationCohortIds
)

validationCohortDefinitionSet <- validationCohortDefinitionSet %>%
  dplyr::mutate(cohortName = gsub("\\[964\\] ", "", cohortName)) %>%
  readr::write_csv(file.path("inst/settings/validationCohortsToCreate.csv"))
