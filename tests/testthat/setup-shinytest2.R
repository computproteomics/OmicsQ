# Load application support files into testing environment
shinytest2::load_app_env()

# Make Chromote startup more robust without hardwiring a browser path.
# These options are no-ops if already set by the user/CI.
op <- options()
if (is.null(op$chromote.startup_timeout)) {
  options(chromote.startup_timeout = 30)
}

# Add conservative flags for container/root environments only if not set
want_args <- c("--headless=new", "--no-first-run", "--no-default-browser-check")
is_root <- tryCatch(identical(Sys.info()["effective_user"], "root"), error = function(e) FALSE)
if (is_root) {
  want_args <- c(want_args, "--no-sandbox", "--disable-dev-shm-usage")
}
have_args <- getOption("chromote.chrome_args", default = character())
options(chromote.chrome_args = unique(c(have_args, want_args)))
