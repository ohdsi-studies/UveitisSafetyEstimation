library(dplyr)

.getCohortJson <- function(file) {
  path <- sprintf(paste0(jsonLocation,"%s"), file)
  cohortJson <- SqlRender::readSql(path)
  cohortJson <- RJSONIO::fromJSON(cohortJson)
  return(cohortJson)
}

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                server = "ohda-prod-1.cldcoxyrkflo.us-east-1.redshift.amazonaws.com/truven_ccae",
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"),
                                                                port = 5439,
                                                                extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory")

connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

vocabularyDatabaseSchema <- "cdm_truven_ccae_v2044"
outputFolder <- "extras/"
jsonLocation <- "inst/cohorts/"
jsons <- list.files(path = jsonLocation)
jsons <- jsons[!jsons == "dummyCohort.json"]

httr::set_config(httr::config(ssl_verifypeer = FALSE))
baseUrl <- "https://epi.jnj.com:8443/WebAPI"
ROhdsiWebApi::authorizeWebApi(baseUrl, "windows")

dfOuter <-data.frame()
for(z in 1:length(jsons)){ #get cohorts in folder
  cohort <- .getCohortJson(jsons[z])
  df <- data.frame()

  for(i in 1:length(cohort$ConceptSets)){ #get concept sets inside cohort
    conceptIds <- ROhdsiWebApi::resolveConceptSet(conceptSetDefinition = cohort$ConceptSets[[i]]$expression , baseUrl = baseUrl)
    conceptName <- cohort$ConceptSets[[i]]$name
    if(length(conceptIds)==0){conceptIds <- c(0)} #if there are not concepts returned
    concepts <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                           sql = "SELECT '@conceptName' AS CONCEPT_SET_NAME, c.CONCEPT_ID, c.CONCEPT_NAME FROM @vocabularyDatabaseSchema.CONCEPT c
                                                                  WHERE c.CONCEPT_ID IN (@conceptIds);",
                                                           vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                                           conceptIds = conceptIds,
                                                           conceptName = conceptName)
    df <- rbind(df,concepts)
  }
  dfOuter <- rbind(dfOuter, df)
}

DatabaseConnector::disconnect(connection)

dfOuter$CONCEPT_SET_NAME <- stringr::str_replace_all(dfOuter$CONCEPT_SET_NAME,stringr::fixed("[921] "),"")      #trim out garbage text
dfOuter$CONCEPT_SET_NAME <- stringr::str_replace_all(dfOuter$CONCEPT_SET_NAME,stringr::fixed("[964] "),"")      #trim out garbage text
dfOuter$CONCEPT_SET_NAME <- stringr::str_replace_all(dfOuter$CONCEPT_SET_NAME,stringr::fixed("[EPI_964] "),"")      #trim out garbage text
dfOuter$CONCEPT_SET_NAME <- stringr::str_replace_all(dfOuter$CONCEPT_SET_NAME,stringr::fixed("[PheValuator] "),"")      #trim out garbage text
dfOuter$CONCEPT_SET_NAME <- stringr::str_replace_all(dfOuter$CONCEPT_SET_NAME,stringr::fixed("[LEGEND HTN] "),"")   #trim out garbage text

allConcepts <- dfOuter %>% distinct()
allConcepts <- allConcepts[order(allConcepts$CONCEPT_SET_NAME,allConcepts$CONCEPT_NAME),]
write.csv(x = allConcepts,file = paste0(outputFolder,"/CONCEPT_SETS.csv"),row.names = FALSE)
