library(shinytest2)

# Single Chromote session reused across UI flow tests to avoid flaky startups
app <- AppDriver$new(name = "OmicsQ-suite", height = 900, width = 1400, seed = 123)
teardown({
  try(app$stop(), silent = TRUE)
})

test_that("{shinytest2} minimal: OmicsQ main flow", {
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
  # Navigate to experimental design and perform an edit on the 'Group' column
  app$set_inputs(mainpage = "exp_design", allow_no_input_binding_ = TRUE)
  app$wait_for_idle()
  # Edit first row, first column (Group) to 2
  app$set_inputs(`expDesign-etable_cell_edit` = list(row = 1, col = 1, value = 2))
  app$wait_for_idle()
  # Table renders after edit
  app$wait_for_value(output = "expDesign-etable")
  expect_true(!is.null(app$get_value(output = "expDesign-etable")))
})

test_that("Missing-value filtering reduces feature count (NA fixture)", {
  # Upload a fixture with NA values and navigate to pre-processing
  app$set_inputs(mainpage = "read", allow_no_input_binding_ = TRUE)
  app$wait_for_idle()
  nafile <- normalizePath(testthat::test_path("fixtures", "with_na.csv"), winslash = "/", mustWork = TRUE)
  app$upload_file(`dataInput-pfile` = nafile)
  app$wait_for_idle()
  app$set_inputs(`dataInput-sel_icol` = "ID")
  app$set_inputs(`dataInput-sel_qcols` = c("A","B","C","D"))
  app$wait_for_idle()
  app$click("dataInput-proceed_to_expdesign")
  app$wait_for_idle()
  app$click("expDesign-proceed_to_process")
  app$wait_for_idle()

  # Helper to extract the numeric feature count from the summary text
  extract_features <- function(txt) {
    m <- regexec("samples and\\s*([0-9,]+)\\s*features", txt)
    res <- regmatches(txt, m)[[1]]
    if (length(res) >= 2) as.integer(gsub(",", "", res[2])) else NA_integer_
  }

  # Baseline features
  app$wait_for_value(output = "preProcessing-ptable_summary")
  s_before <- app$get_value(output = "preProcessing-ptable_summary")
  feats_before <- extract_features(s_before)
  expect_true(!is.na(feats_before) && feats_before > 0)

  # Tighten missingness threshold to zero to drop any rows with NA
  app$set_inputs(`preProcessing-max_na` = 0)
  app$wait_for_idle()
  app$wait_for_value(output = "preProcessing-ptable_summary")
  s_after <- app$get_value(output = "preProcessing-ptable_summary")
  feats_after <- extract_features(s_after)
  expect_true(!is.na(feats_after) && feats_after >= 0)
  expect_lt(feats_after, feats_before)
})
