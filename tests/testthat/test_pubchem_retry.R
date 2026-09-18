test_that("PubChem annotation requests recover from a temporary 503", {
  calls <- new.env()
  calls$n <- 0L
  request <- .build_pubchem_view_query(id = 176870)
  # Mock transport below httr2's retry loop so the real retry policy runs.
  local_mocked_bindings(
    req_perform1 = function(req, ...) {
      calls$n <- calls$n + 1L
      httr2::response(
        status_code = if (calls$n == 1L) 503L else 200L,
        url = req$url,
        headers = list(`content-type` = "application/json"),
        body = charToRaw('{"Record":{"RecordNumber":176870}}')
      )
    },
    .package = "httr2"
  )
  response <- httr2::req_perform(request)
  expect_equal(calls$n, 2L)
  expect_equal(httr2::resp_status(response), 200L)
  expect_equal(.parse_resp_json(response)$Record$RecordNumber, 176870)
})
