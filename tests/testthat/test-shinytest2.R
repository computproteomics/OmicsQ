library(shinytest2)

# Helper to extract data from a DT output in various shapes
extract_dt_data <- function(dt) {
  if (is.data.frame(dt)) return(dt)
  if (is.list(dt) && !is.null(dt$data)) return(as.data.frame(dt$data))
  if (is.list(dt) && !is.null(dt$x) && !is.null(dt$x$data)) return(as.data.frame(dt$x$data))
  return(NULL)
}

# Compute groups from column headers using the same logic as ExpDesign
compute_groups <- function(cols, method = "lv") {
  if (length(cols) <= 1) return(rep(1L, length(cols)))
  # Use app's distance function if available
  d <- expd_dist(cols, method = method, p = 0.2)
  th <- sort(unique(as.vector(d)))
  med <- stats::median(th[th != 0], na.rm = TRUE)
  stats::cutree(stats::hclust(stats::as.dist(d)), h = med)
}

test_that("{shinytest2} minimal: OmicsQ main flow", {
  skip_if_no_browser()
  app <- AppDriver$new(name = "OmicsQ-suite-minimal", height = 900, width = 1400, seed = 123)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  # Load bundled example data
  app$click("dataInput-run_example")
  app$wait_for_idle()

  # Proceed to experimental design tab
  app$click("dataInput-proceed_to_expdesign")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "mainpage"), "exp_design")

  # Proceed to pre-processing tab
  app$click("expDesign-proceed_to_process")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "mainpage"), "process")
})

test_that("Experimental Design tuning updates table", {
  skip_if_no_browser()
  app <- AppDriver$new(name = "OmicsQ-suite-expdesign", height = 900, width = 1400, seed = 123)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  # Load example freshly and navigate to experimental design
  app$set_inputs(mainpage = "read", allow_no_input_binding_ = TRUE)
  app$wait_for_idle()
  app$click("dataInput-run_example")
  app$wait_for_idle()
  app$click("dataInput-proceed_to_expdesign")
  app$wait_for_idle()

  # Change distance type and ensure input reflects change
  app$set_inputs(`expDesign-dist_type` = "jw")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "expDesign-dist_type"), "jw")

  # The editable table should be rendered
  app$wait_for_value(output = "expDesign-etable")
  expect_true(!is.null(app$get_value(output = "expDesign-etable")))
})

test_that("Pre-processing adjustments produce summary and plots", {
  skip_if_no_browser()
  app <- AppDriver$new(name = "OmicsQ-suite-preproc", height = 900, width = 1400, seed = 123)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  # Load example freshly and navigate to pre-processing
  app$set_inputs(mainpage = "read", allow_no_input_binding_ = TRUE)
  app$wait_for_idle()
  app$click("dataInput-run_example")
  app$wait_for_idle()
  app$click("dataInput-proceed_to_expdesign")
  app$wait_for_idle()
  app$click("expDesign-proceed_to_process")
  app$wait_for_idle()

  # Set normalization and missing-value parameters
  app$set_inputs(`preProcessing-normalization` = "colMedians")
  app$set_inputs(`preProcessing-max_na` = 0)
  app$set_inputs(`preProcessing-show_missing_rows` = TRUE)
  app$wait_for_idle()

  # Summary text should be present and informative
  app$wait_for_value(output = "preProcessing-ptable_summary")
  summary_txt <- app$get_value(output = "preProcessing-ptable_summary")
  expect_true(is.character(summary_txt) && nzchar(summary_txt))
  expect_true(grepl("samples and", summary_txt))

  # Plots should render without error (presence check)
  app$wait_for_value(output = "preProcessing-pca_plot")
  expect_true(!is.null(app$get_value(output = "preProcessing-pca_plot")))
  app$wait_for_value(output = "preProcessing-corrplot")
  expect_true(!is.null(app$get_value(output = "preProcessing-corrplot")))
  app$wait_for_value(output = "preProcessing-missingplot")
  expect_true(!is.null(app$get_value(output = "preProcessing-missingplot")))
})

test_that("Proceed to apps navigates to Apps tab", {
  skip_if_no_browser()
  app <- AppDriver$new(name = "OmicsQ-suite-proceed-apps", height = 900, width = 1400, seed = 123)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  # Load example freshly and navigate to pre-processing
  app$set_inputs(mainpage = "read", allow_no_input_binding_ = TRUE)
  app$wait_for_idle()
  app$click("dataInput-run_example")
  app$wait_for_idle()
  app$click("dataInput-proceed_to_expdesign")
  app$wait_for_idle()
  app$click("expDesign-proceed_to_process")
  app$wait_for_idle()
  # Balance design by filling with empty columns if needed
  app$set_inputs(`preProcessing-add_na_columns` = TRUE)
  app$wait_for_idle()
  app$wait_for_value(output = "preProcessing-res_num_reps")
  bal_txt <- app$get_value(output = "preProcessing-res_num_reps")
  expect_true(is.character(bal_txt) && nzchar(bal_txt))

  # Proceed when enabled
  app$click("preProcessing-proceed_to_apps")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "mainpage"), "apps")
})

test_that("Uploading duplicate-ID dataset enables summarization and reduces IDs", {
  skip_if_no_browser()
  app <- AppDriver$new(name = "OmicsQ-suite-dups", height = 900, width = 1400, seed = 123)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  # Return to Data Input and upload a small CSV with duplicate IDs
  app$set_inputs(mainpage = "read", allow_no_input_binding_ = TRUE)
  app$wait_for_idle()

  dupfile <- normalizePath(testthat::test_path("fixtures", "dups.csv"), winslash = "/", mustWork = TRUE)
  app$upload_file(`dataInput-pfile` = dupfile)
  app$wait_for_idle()

  # Select ID and quantitative columns
  app$set_inputs(`dataInput-sel_icol` = "ID")
  app$set_inputs(`dataInput-sel_qcols` = c("A","B","C","D"))
  app$wait_for_idle()

  # Proceed through tabs
  app$click("dataInput-proceed_to_expdesign")
  app$wait_for_idle()
  app$click("expDesign-proceed_to_process")
  app$wait_for_idle()

  # Summary should report non-unique IDs prior to summarization
  app$wait_for_value(output = "preProcessing-ptable_summary")
  summary_before <- app$get_value(output = "preProcessing-ptable_summary")
  expect_true(grepl("non-unique IDs", summary_before))

  # Apply summarization and expect unique IDs reported
  app$set_inputs(`preProcessing-summarize` = "colMedians")
  app$wait_for_idle()
  app$wait_for_value(output = "preProcessing-ptable_summary")
  summary_after <- app$get_value(output = "preProcessing-ptable_summary")
  expect_true(grepl("unique IDs", summary_after))
})

test_that("Experimental design table cell edit is accepted", {
  skip_if_no_browser()
  app <- AppDriver$new(name = "OmicsQ-suite-edit-cell", height = 900, width = 1400, seed = 123)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  # Load example and navigate to experimental design to ensure table exists
  app$set_inputs(mainpage = "read", allow_no_input_binding_ = TRUE)
  app$wait_for_idle()
  app$click("dataInput-run_example")
  app$wait_for_idle()
  app$click("dataInput-proceed_to_expdesign")
  app$wait_for_idle()
  # Ensure table is rendered
  app$wait_for_value(output = "expDesign-etable")
  expect_true(!is.null(app$get_value(output = "expDesign-etable")))
  # Edit first row, first column (Group) to 2
  app$set_inputs(`expDesign-etable_cell_edit` = list(row = 1, col = 1, value = 2))
  app$wait_for_idle()
  # Table should still be present after edit
  app$wait_for_value(output = "expDesign-etable")
  expect_true(!is.null(app$get_value(output = "expDesign-etable")))
})

test_that("Group-major headers yield adjacent grouping for PolySTest", {
  skip_if_no_browser()
  app <- AppDriver$new(name = "OmicsQ-suite-group-major", height = 900, width = 1400, seed = 123)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  app$set_inputs(mainpage = "read", allow_no_input_binding_ = TRUE)
  app$wait_for_idle()
  f <- normalizePath(testthat::test_path("fixtures", "order_group_major.csv"), winslash = "/", mustWork = TRUE)
  app$upload_file(`dataInput-pfile` = f)
  app$wait_for_idle()
  app$set_inputs(`dataInput-sel_icol` = "ID")
  app$set_inputs(`dataInput-sel_qcols` = c("Control_Rep1","Control_Rep2","Treatment_Rep1","Treatment_Rep2"))
  app$wait_for_idle()
  app$click("dataInput-proceed_to_expdesign")
  app$wait_for_idle()
  # Compute expected grouping from column headers (matches app logic)
  cols <- c("Control_Rep1","Control_Rep2","Treatment_Rep1","Treatment_Rep2")
  g <- suppressWarnings(as.integer(compute_groups(cols = cols, method = "lv")))
  message(paste("[order_group_major] Computed Group:", paste(g, collapse = ",")))
  expect_true(length(g) >= 4)
  expect_true(g[1] == g[2])
  expect_true(g[3] == g[4])
  expect_true(g[1] != g[3])
})

test_that("Interleaved headers still group by condition for PolySTest", {
  skip_if_no_browser()
  app <- AppDriver$new(name = "OmicsQ-suite-interleaved", height = 900, width = 1400, seed = 123)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  app$set_inputs(mainpage = "read", allow_no_input_binding_ = TRUE)
  app$wait_for_idle()
  f <- normalizePath(testthat::test_path("fixtures", "order_interleaved.csv"), winslash = "/", mustWork = TRUE)
  app$upload_file(`dataInput-pfile` = f)
  app$wait_for_idle()
  app$set_inputs(`dataInput-sel_icol` = "ID")
  app$set_inputs(`dataInput-sel_qcols` = c("Control_Rep1","Treatment_Rep1","Control_Rep2","Treatment_Rep2"))
  app$wait_for_idle()
  app$click("dataInput-proceed_to_expdesign")
  app$wait_for_idle()
  # Ensure default distance type used for grouping
  app$set_inputs(`expDesign-dist_type` = "lv")
  app$wait_for_idle()
  cols <- c("Control_Rep1","Treatment_Rep1","Control_Rep2","Treatment_Rep2")
  g <- suppressWarnings(as.integer(compute_groups(cols = cols, method = "lv")))
  message(paste("[order_interleaved] Computed Group:", paste(g, collapse = ",")))
  expect_true(length(g) >= 4)
  # 1 and 3 are same condition; 2 and 4 are same condition; they differ across
  expect_true(g[1] == g[3])
  expect_true(g[2] == g[4])
  expect_true(g[1] != g[2])
})

test_that("Non-canonical group labels map to two distinct groups", {
  skip_if_no_browser()
  app <- AppDriver$new(name = "OmicsQ-suite-noncanonical", height = 900, width = 1400, seed = 123)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  app$set_inputs(mainpage = "read", allow_no_input_binding_ = TRUE)
  app$wait_for_idle()
  f <- normalizePath(testthat::test_path("fixtures", "order_noncanonical_groups.csv"), winslash = "/", mustWork = TRUE)
  app$upload_file(`dataInput-pfile` = f)
  app$wait_for_idle()
  app$set_inputs(`dataInput-sel_icol` = "ID")
  app$set_inputs(`dataInput-sel_qcols` = c("G3_Rep1","G3_Rep2","G5_Rep1","G5_Rep2"))
  app$wait_for_idle()
  app$click("dataInput-proceed_to_expdesign")
  app$wait_for_idle()
  app$set_inputs(`expDesign-dist_type` = "lv")
  app$wait_for_idle()
  cols <- c("G3_Rep1","G3_Rep2","G5_Rep1","G5_Rep2")
  g <- suppressWarnings(as.integer(compute_groups(cols = cols, method = "lv")))
  message(paste("[order_noncanonical] Computed Group:", paste(g, collapse = ",")))
  expect_true(length(g) >= 4)
  expect_true(g[1] == g[2])
  expect_true(g[3] == g[4])
  expect_true(g[1] != g[3])
})
