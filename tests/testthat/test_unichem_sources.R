test_that("UniChem source metadata supports current and legacy schemas", {
  source <- list(
    UCICount = 1L,
    baseIdUrl = "https://example.org/compound/",
    description = "Synthetic test source",
    name = "example",
    nameLabel = "Example",
    nameLong = "Example database",
    sourceID = 1L,
    srcDetails = "",
    srcUrl = "https://example.org",
    updateComments = ""
  )
  response <- new.env()
  response$sources <- c(
    source,
    list(
      srcLastUpdated = "2026-09-12",
      created = NA_character_,
      private = FALSE
    )
  )
  local_mocked_bindings(
    .cache_enabled = function() FALSE,
    .perform_request = function(request) NULL,
    .parse_unichem_response = function(...) {
      list(response = "Success", totalSources = 1L, sources = response$sources)
    },
    .package = "AnnotationGx"
  )

  current <- getUnichemSources(all_columns = TRUE)
  expect_equal(current$LastUpdated, "2026-09-12")
  expect_identical(current$ReleaseDate, NA_character_)
  expect_identical(current$ReleaseNumber, NA_integer_)
  expect_equal(ncol(current), 13L)
  expect_named(getUnichemSources(), c("Name", "SourceID"))

  response$sources <- c(
    source,
    list(
      lastUpdated = "2025-01-01",
      srcReleaseDate = "2024-12-01",
      srcReleaseNumber = 2L
    )
  )
  legacy <- getUnichemSources(all_columns = TRUE)
  expect_named(legacy, names(current))
  expect_equal(legacy$LastUpdated, "2025-01-01")
  expect_equal(legacy$ReleaseDate, "2024-12-01")
  expect_equal(legacy$ReleaseNumber, 2L)

  response$sources <- source
  expect_identical(
    getUnichemSources(all_columns = TRUE)$LastUpdated,
    NA_character_
  )
})
