# State manager to restore full sessions safely + input reassertion
make_state_manager <- function(session) {
    registry   <- list()                    # { ns_id -> list(entries...) }
    ns_order   <- NULL                      # optional: user-specified restore order
    restoring  <- shiny::reactiveVal(FALSE)
    obs_reg    <- list()                    # registered observers (optional)
    stash      <- NULL                      # modules payload held between onRestore and onRestored
    
    # ---- Input reassert configuration ------------------------------------------
    input_cfg <- list(
        enabled    = FALSE,
        ids        = character(0),            # inputs to reassert to saved values
        dispatcher = NULL,                    # optional function(session, id, value)
        delay_tick = TRUE,                     # use a zero-delay tick after hydration
        delay_ms   = 50L          # small delay to let pickers finish rendering
    )
    stash_inputs <- NULL                    # saved input snapshot from bookmark
    
    # --- Registration API --------------------------------------------------------
    register_values <- function(ns_id, rv) {
        stopifnot(!is.null(ns_id))
        entry <- list(kind = "values", obj = rv)
        registry[[ns_id]] <<- c(registry[[ns_id]], list(entry))
        invisible(TRUE)
    }
    
    register_vals <- function(ns_id, vals_named_list) {
        stopifnot(!is.null(ns_id), is.list(vals_named_list), !is.null(names(vals_named_list)))
        entry <- list(kind = "val", obj = vals_named_list)
        registry[[ns_id]] <<- c(registry[[ns_id]], list(entry))
        invisible(TRUE)
    }
    
    register_observer <- function(o) {
        obs_reg <<- c(obs_reg, list(o)); invisible(o)
    }
    
    set_ns_order <- function(order_vec) {
        stopifnot(is.null(order_vec) || is.character(order_vec))
        ns_order <<- order_vec; invisible(TRUE)
    }
    
    # Opt-in: which inputs to reassert to saved values after hydration
    set_input_reassert <- function(ids, delay_tick = TRUE, delay_ms = 50L)  {
        input_cfg$enabled    <<- length(ids) > 0
        input_cfg$ids        <<- unique(as.character(ids))
        input_cfg$delay_tick <<- isTRUE(delay_tick)
        input_cfg$delay_ms   <<- as.integer(delay_ms)
        message("Input reassert enabled for: ", paste(input_cfg$ids, collapse=", "))
        invisible(TRUE)
    }
    
    # Optional: custom dispatcher for updating inputs
    # Signature: function(session, id, value) { updateSelectInput(...); ... }
    set_input_dispatcher <- function(fun) {
        stopifnot(is.null(fun) || is.function(fun))
        input_cfg$dispatcher <<- fun
        invisible(TRUE)
    }
    
    # --- Helpers ----------------------------------------------------------------
    assign_into_values <- function(rv, values) {
        for (nm in names(values)) {
            old <- shiny::isolate(rv[[nm]])
            if (!identical(old, values[[nm]])) rv[[nm]] <- values[[nm]]
        }
    }
    
    assign_into_vals <- function(vals_named_list, values) {
        common <- intersect(names(vals_named_list), names(values))
        for (nm in common) {
            rv  <- vals_named_list[[nm]]
            old <- shiny::isolate(rv())
            if (!identical(old, values[[nm]])) rv(values[[nm]])
        }
    }
    
    suspend_all_obs <- function() for (o in obs_reg) o$suspend()
    resume_all_obs  <- function() for (o in obs_reg) o$resume()
    
    hydrate_ns <- function(ns_id, saved_entries) {
        reg_entries <- registry[[ns_id]]
        if (is.null(reg_entries) || is.null(saved_entries)) return(invisible())
        n <- min(length(saved_entries), length(reg_entries))
        for (i in seq_len(n)) {
            e_saved <- saved_entries[[i]]
            e_reg   <- reg_entries[[i]]
            if (e_reg$kind == "values" && e_saved$kind == "values") {
                assign_into_values(e_reg$obj, e_saved$data)
            } else if (e_reg$kind == "val" && e_saved$kind == "val") {
                assign_into_vals(e_reg$obj, e_saved$data)
            }
        }
    }
    
    compute_order <- function(payload) {
        regs <- names(registry); pays <- names(payload)
        base <- if (!is.null(ns_order)) ns_order else regs
        unique(c(intersect(base, pays), setdiff(intersect(regs, pays), base)))
    }
    
    # Snapshot tracked inputs at bookmark time (values to be reasserted later)
    snapshot_inputs <- function() {
        if (!input_cfg$enabled || !length(input_cfg$ids)) return(list())
        vals <- lapply(input_cfg$ids, function(id) {
            # may be NULL during bookmark; that's OK, we reassert only non-NULL later
            tryCatch(shiny::isolate(session$input[[id]]), error = function(e) NULL)
        })
        names(vals) <- input_cfg$ids
        vals
    }
    
    # Generic updater if no dispatcher is provided:
    default_update_input <- function(session, id, value) {
        # Try common updaters; silently continue on errors
        try(updateSelectInput(session, id, selected = value), silent = TRUE)
        try(updateCheckboxGroupInput(session, id, selected = value), silent = TRUE)
        try(updateCheckboxInput(session, id, value = isTRUE(value)), silent = TRUE)
        try(shinyWidgets::updatePickerInput(session, id, selected = value), silent = TRUE)
        try(updateRadioButtons(session, id, selected = value), silent = TRUE)
        try(updateNumericInput(session, id, value = value), silent = TRUE)
        try(updateSliderInput(session, id, value = value), silent = TRUE)
        try(updateDateInput(session, id, value = value), silent = TRUE)
        try(updateDateRangeInput(session, id, start = value[[1]], end = value[[2]]), silent = TRUE)
        try(updateTextInput(session, id, value = as.character(value)), silent = TRUE)
    }
    
    reassert_inputs <- function(saved) {
        message("Reassert ids: ", paste(names(saved), collapse=", "))
        for (id in names(saved)) {
            val <- saved[[id]]
            if (is.null(val)) next
            cur <- tryCatch(shiny::isolate(session$input[[id]]), error = function(e) NA)
            if (identical(cur, val)) next
            # freezeReactiveValue(session$input, id)
            # on.exit(unfreezeReactiveValue(session$input, id), add = TRUE)
            if (is.function(input_cfg$dispatcher)) {
                input_cfg$dispatcher(session, id, val)
            } else {
                default_update_input(session, id, val)
            }
        }
    }
    
    # --- Hooks ------------------------------------------------------------------
    onBookmark <- function(state) {
        # Save module/reactive state
        payload <- list()
        for (ns_id in names(registry)) {
            saved_entries <- lapply(registry[[ns_id]], function(e) {
                if (e$kind == "values") {
                    list(kind = "values", data = shiny::reactiveValuesToList(e$obj))
                } else {
                    dat <- lapply(e$obj, function(rv) shiny::isolate(rv()))
                    names(dat) <- names(e$obj)
                    list(kind = "val", data = dat)
                }
            })
            payload[[ns_id]] <- saved_entries
        }
        state$values$modules <- payload
        state$values$schema_version <- 1L
        
        # Save tracked input values for later reassertion (optional)
        if (input_cfg$enabled) {
            state$values$._inputs <- snapshot_inputs()
            message("Snapshotting inputs: ", paste(names(state$values$._inputs), collapse=", "))
        }
    }
    
    # Phase 1: capture only; avoid assignments here
    onRestore <- function(state) {
        restoring(TRUE)
        stash <<- state$values$modules %||% list()
        # bring forward saved inputs if present
        stash_inputs <<- state$values$._inputs %||% list()
        message("Restore inputs present: ", paste(names(stash_inputs), collapse = ", "))
    }
    
    # Phase 2: hydrate AFTER UI is flushed, then (optionally) reassert inputs
    onRestored <- function(state) {
        print("onRestored")
        session$onFlushed(function() {
            if (length(obs_reg)) suspend_all_obs()
            
            payload <- stash %||% list()
            for (ns_id in compute_order(payload)) hydrate_ns(ns_id, payload[[ns_id]])
            
            if (length(obs_reg)) resume_all_obs()
            
            # Reassert only the inputs that drifted from the saved snapshot
            maybe_reassert <- function() {
                if (input_cfg$enabled && length(stash_inputs)) {
                    shiny::withReactiveDomain(session, {
                        message("in reactive domain session")
                        reassert_inputs(stash_inputs)
                    })
                }
                # cleanup & finish
                stash <<- NULL
                stash_inputs <<- NULL
                restoring(FALSE)
            }
            
            # Give one tick so UI in lazy tabs exists before updates
            if (isTRUE(input_cfg$delay_tick) && requireNamespace("later", quietly = TRUE)) {
                later::later(maybe_reassert, delay = max(0, input_cfg$delay_ms)/1000)
            } else {
                maybe_reassert()
            }
        }, once = TRUE)
    }
    
    # Public API
    list(
        # registration
        register_values      = register_values,
        register_vals        = register_vals,
        register_observer    = register_observer,
        set_ns_order         = set_ns_order,
        
        # input reassert (optional)
        set_input_reassert   = set_input_reassert,
        set_input_dispatcher = set_input_dispatcher,
        
        # state
        restoring            = restoring,
        
        # hooks
        onBookmark           = onBookmark,
        onRestore            = onRestore,
        onRestored           = onRestored
    )
}
