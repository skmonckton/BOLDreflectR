showModal(
  modalDialog(
    title = "Settings & Defaults",
    tagList(
      passwordInput( 
        "api_key", 
        div("BOLD API key:",
            bslib::tooltip(
              icon("circle-question"),
              paste0("The API key is saved to your system's credential store (",keystore,") when the 'Get data' button is pressed."))
        ),
        value = tryCatch({
          key_get("BOLD.apikey")
        }, error = function(e){
          ""
        })
      ),
      inputGroup(title = "Customize output",
                 groupId = "customize_defaults",
                 selectizeInput("filt_opt_def",
                                tagList(div("Columns to show:"),
                                        actionLink(
                                          "save_filter_set",
                                          icon("bookmark"))),
                                filter_options(),
                                multiple = TRUE,
                                options = list(plugins = list("drag_drop"))
                 ),
                 selectizeInput("filt_seq_def",
                                "Filter by marker:",
                                list("COI-5P" = "COI-5P", 
                                     "COI-3P" = "COI-3P",
                                     "28S" = "28S",
                                     "28S-D2-D3" = "28S-D2-D3",
                                     "18S" = "18S",
                                     "18S-5P" = "18S-5P"),
                                multiple = TRUE
                 ),
                 bslib::input_switch(
                   "collapse_mrkrs_def",
                   "Collapse multiple markers into single rows"
                 )),
      inputGroup(title = "Consensus BIN taxonomy",
                 groupId = "bin_cons_def",
                 numericInput(
                   "bc_threshold_def",
                   div("Consensus threshold:",
                       bslib::tooltip(
                         icon("circle-question"),
                         paste0("Minimum proportion of records in a BIN that must have a concordant identification in order to establish consensus."))),
                   1, min = 0.5, max = 1.0, step = 0.05),
                 numericInput(
                   "bc_minids_def",
                   div("Minimum IDs:",
                       bslib::tooltip(
                         icon("circle-question"),
                         paste0("Minimum number of concordant identifications needed to establish a consensus identification. If a BIN contains fewer records than this number, then they must all have matching identifications to reach consensus."))),
                   2, min = 1, step = 1),
                 checkboxInput(
                   "bc_enforcesci_def",
                   "Limit to scientific names",
                   value = TRUE
                 ),
                 checkboxInput(
                   "bc_portal_stats_def",
                   div("Include BOLD Portal stats",
                       bslib::tooltip(
                         icon("circle-question"),
                         HTML("<strong>Please note</strong>: This can take <strong>several minutes</strong> for more than a few dozen BINs. Additional statistics include member counts (total, private, public), pairwise distances (average, maximum), and nearest-neighbour information (NN distance, NN BIN).")
                       )))
                 )
    )))
