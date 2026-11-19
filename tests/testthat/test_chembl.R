local_chembl_responses <- function(.env = parent.frame()) {
  testthat::local_mocked_bindings(
    .cache_enabled = function() FALSE,
    .perform_request = function(request) {
      body <- if (grepl("/schema", request$url, fixed = TRUE)) {
        '{"fields":{"molecule_chembl_id":{},"target_chembl_id":{},"mechanism_of_action":{}}}'
      } else {
        '{"mechanisms":[{"molecule_chembl_id":"CHEMBL1413","target_chembl_id":"CHEMBL2363058","mechanism_of_action":"Example mechanism"}],"page_meta":{"total_count":1}}'
      }
      httr2::response(
        url = request$url,
        headers = list(`content-type` = "application/json"),
        body = charToRaw(body)
      )
    },
    .package = "AnnotationGx",
    .env = .env
  )
}

test_that("ChEMBL requests encode fields and validate inputs", {
  local_chembl_responses()
  request <- .build_chembl_request(
    "mechanism",
    "mechanism_of_action",
    "icontains",
    "Example mechanism"
  )
  expect_equal(
    request$url,
    "https://www.ebi.ac.uk/chembl/api/data/mechanism?mechanism_of_action__icontains=Example%20mechanism&format=json"
  )
  expect_error(.build_chembl_request("invalid"))
  expect_error(.build_chembl_request("mechanism", "invalid"))
  expect_error(.build_chembl_request("mechanism", filter_type = "invalid"))
  expect_error(.build_chembl_request("mechanism", format = "invalid"))
})

test_that("ChEMBL schema and query responses are parsed", {
  local_chembl_responses()
  expect_equal(
    getChemblResourceFields("mechanism"),
    c("molecule_chembl_id", "target_chembl_id", "mechanism_of_action")
  )
  response <- queryChemblAPI(
    "mechanism",
    "molecule_chembl_id",
    "in",
    "CHEMBL1413"
  )
  expect_equal(response$page_meta$total_count, 1)
  expect_equal(response$mechanisms$molecule_chembl_id, "CHEMBL1413")
})

test_that("mechanism results preserve return modes and fill missing columns", {
  local_chembl_responses()
  mechanism <- getChemblMechanism("CHEMBL1413")
  expect_s3_class(mechanism, "data.table")
  expect_equal(nrow(mechanism), 1L)
  expect_setequal(names(mechanism), .chembl_mechanism_cols())
  expect_true(is.na(mechanism$action_type))
  expect_equal(mechanism$target_chembl_id, "CHEMBL2363058")
  expect_equal(
    getChemblMechanism("CHEMBL1413", returnURL = TRUE)[[1]],
    "https://www.ebi.ac.uk/chembl/api/data/mechanism?molecule_chembl_id__in=CHEMBL1413&format=json"
  )
  expect_named(
    getChemblMechanism("CHEMBL1413", raw = TRUE)[[1]],
    c("mechanisms", "page_meta")
  )
})

test_that("HTML schema failures are not cached as successful results", {
  old_options <- options(annotationgx.cache.dir = tempfile("chembl-cache-"))
  on.exit(options(old_options), add = TRUE)
  calls <- new.env()
  calls$n <- 0L
  local_mocked_bindings(
    .perform_request = function(request) {
      calls$n <- calls$n + 1L
      httr2::response(
        url = request$url,
        headers = list(`content-type` = "text/html"),
        body = charToRaw("<html>Service unavailable</html>")
      )
    },
    .package = "AnnotationGx"
  )
  expect_error(getChemblMechanism("CHEMBL1413"), "Unexpected content type")
  expect_error(getChemblMechanism("CHEMBL1413"), "Unexpected content type")
  expect_equal(calls$n, 2L)
})

test_that("ChEMBL resource and filter lists are available offline", {
  expect_true("mechanism" %in% getChemblResources())
  expect_true("in" %in% getChemblFilterTypes())
})

test_that("the evaluated ChEMBL vignette survives HTML responses", {
  source <- test_path("..", "..", "vignettes", "ChEMBL.Rmd")
  if (!file.exists(source)) {
    source <- system.file("doc", "ChEMBL.Rmd", package = "AnnotationGx")
  }
  skip_if(!file.exists(source), "Vignette source is unavailable")
  local_mocked_bindings(
    .cache_enabled = function() FALSE,
    .perform_request = function(request) {
      httr2::response(
        url = request$url,
        headers = list(`content-type` = "text/html"),
        body = charToRaw("<html>Service unavailable</html>")
      )
    },
    .package = "AnnotationGx"
  )
  output <- tempfile(fileext = ".md")
  knitr::knit(
    text = c(
      "```{r, include=FALSE}",
      "knitr::opts_chunk$set(error = FALSE)",
      "```",
      readLines(source)
    ),
    output = output,
    quiet = TRUE,
    envir = new.env()
  )
  rendered <- paste(readLines(output), collapse = "\n")
  expect_match(rendered, "ChEMBL example could not complete")
  expect_match(rendered, "Unexpected content type")
  expect_match(rendered, "R version")
  expect_false(grepl("#> Error", rendered, fixed = TRUE))
})

test_that("live ChEMBL returns a mechanism table", {
  skip_if_offline("www.ebi.ac.uk")
  old_options <- options(
    annotationgx.cache.use = FALSE,
    annotationgx.request.timeout = 5,
    annotationgx.request.max_tries = 1
  )
  on.exit(options(old_options), add = TRUE)
  mechanism <- tryCatch(
    getChemblMechanism("CHEMBL1413"),
    error = function(error) {
      if (
        inherits(error, "httr2_failure") ||
          inherits(error, "annotationgx_chembl_unavailable")
      ) {
        skip(paste("ChEMBL unavailable:", conditionMessage(error)))
      }
      stop(error)
    }
  )
  expect_s3_class(mechanism, "data.table")
  expect_true(nrow(mechanism) > 0L)
  expect_true(all(mechanism$molecule_chembl_id == "CHEMBL1413"))
})

test_that("ChEMBL distinguishes service outages from other errors", {
  response <- function(status = 200L, type = "application/json", body = "{}") {
    httr2::response(
      status_code = status,
      headers = list(`content-type` = type),
      body = charToRaw(body)
    )
  }
  for (status in c(429L, 500L, 502L, 503L)) {
    expect_error(
      .parse_chembl_json(response(status)),
      class = "annotationgx_chembl_unavailable"
    )
  }
  expect_error(
    .parse_chembl_json(response(type = "text/html", body = "<html>")),
    class = "annotationgx_chembl_unavailable"
  )
  expect_error(.parse_chembl_json(response(400L)), class = "httr2_http_400")
  expect_error(.parse_chembl_json(response(401L)), class = "httr2_http_401")
  expect_error(.parse_chembl_json(response(body = "not JSON")), "lexical error")
  expect_equal(.parse_chembl_json(response(body = '{"ok":true}')), list(ok = TRUE))
})

test_that("evaluated Rd examples handle outages but retain programming errors", {
  local_mocked_bindings(.cache_enabled = function() FALSE,
    .package = "AnnotationGx")
  mode <- "html"
  local_mocked_bindings(
    .perform_request = function(request) {
      if (mode == "bug") stop("deliberate programming error")
      if (mode == "network") {
        stop(errorCondition("connection failed", class = "httr2_failure"))
      }
      httr2::response(
        headers = list(`content-type` = "text/html"),
        body = charToRaw("<html>Service unavailable</html>")
      )
    }, .package = "AnnotationGx"
  )
  for (topic in c("getChemblMechanism", "getChemblResourceFields", "queryChemblAPI")) {
    rd <- test_path("..", "..", "man", paste0(topic, ".Rd"))
    skip_if(!file.exists(rd), "Rd sources are unavailable")
    script <- tempfile(fileext = ".R")
    tools::Rd2ex(rd, out = script)
    for (failure in c("html", "network")) {
      mode <- failure
      expect_message(sys.source(script, envir = new.env()), "ChEMBL unavailable")
    }
    mode <- "bug"
    expect_error(sys.source(script, envir = new.env()), "deliberate programming error")
  }
})
