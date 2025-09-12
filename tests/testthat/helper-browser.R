skip_if_no_browser <- function() {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    testthat::skip("Package 'chromote' not installed; skipping browser-based tests")
  }

  path <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  if (is.null(path) || !nzchar(path)) {
    testthat::skip("No Chrome/Chromium found by chromote::find_chrome(); skipping")
  }

  # Try to launch a short-lived chromote session to verify connectivity
  can_start <- tryCatch({
    s <- chromote::ChromoteSession$new()
    on.exit(try(s$close(), silent = TRUE), add = TRUE)
    TRUE
  }, error = function(e) {
    message("Chromote failed to start: ", conditionMessage(e))
    FALSE
  })
  if (!can_start) {
    testthat::skip("Chromote could not start a debugging session; skipping")
  }
}

