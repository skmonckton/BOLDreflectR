ui <- bslib::page_fillable(
  title = "BOLDreflectR",
  tags$head(
    # uses asset_url() to load a fresh copy instead of cached
    tags$link(rel = "stylesheet", type = "text/css", href = asset_url("brstyles.css")),
    tags$link(rel = "stylesheet", type = "text/css", href = asset_url("viewers.css")),
    tags$script(src = asset_url("viewer-common.js")),
    tags$script(src = asset_url("alignment-viewer.js")),
    tags$script(src = asset_url("tree-viewer.js")),
    tags$script(HTML(r"(
      // while the server is busy with a long task (> 0.5 s), block buttons, links and Ctrl+Enter that enqueue more (tabs, scrolling, viewing data still work)
      (function() {
        var timer = null, busy = false;
        var ACTIONS = '.action-button, .shiny-download-link, .easy-button-button';
        function setBusy(on) { busy = on; document.body.classList.toggle('app-busy', on); }
        $(document).on('shiny:busy', function() {
          clearTimeout(timer);
          timer = setTimeout(function() { setBusy(true); }, 500);
        });
        $(document).on('shiny:idle', function() {
          clearTimeout(timer);
          setBusy(false);
        });
        // only clicks by the user; app's "phantom clicks" (e.g. starting a PDF download) are allowed
        document.addEventListener('click', function(e) {
          if (busy && e.isTrusted && e.target.closest && e.target.closest(ACTIONS)) { e.preventDefault(); e.stopPropagation(); }
        }, true);
        document.addEventListener('keydown', function(e) {
          if (busy && e.ctrlKey && e.key === 'Enter') { e.preventDefault(); e.stopPropagation(); }
        }, true);
      })();
      // bottom bar shows the current tab's buttons (tab_data in server.R); done here so it changes as soon as the tab does
      (function() {
        var map = {}, all = [], inputs = {};
        function update() {
          var b = map[inputs.tabs] || [];
          if (!Array.isArray(b)) b = b[inputs[b.nav]] || [];
          b = [].concat(b);
          all.forEach(function(id) { $('#' + id).removeClass('shinyjs-hide').toggle(b.indexOf(id) >= 0); });
          $('#table_buttons').toggleClass('bar-empty', b.length === 0);
        }
        Shiny.addCustomMessageHandler('tab_buttons', function(m) { map = m.tabs; all = [].concat(m.all); update(); });
        $(document).on('shiny:inputchanged', function(e) {
          if (e.name === 'tabs' || e.name === 'div_nav') { inputs[e.name] = e.value; update(); }
        });
      })();
      // Ctrl+Enter starts data retrieval with 'change' event triggered to use latest value of field
      $(document).on('keydown', 'div[data-value=fetchdata] div.tabbable input, div[data-value=fetchdata] div.tabbable textarea', function(e) {
          if (e.ctrlKey && e.key === 'Enter') {
            e.preventDefault();
            $(this).trigger('change');
            Shiny.setInputValue('fetch_ctrl_enter', Math.random(), {priority: 'event'});
          }
        });
      // sequence length range validation: when a value changes, keep it within the limits and to the correct side of its partner
      $(document).on('change', '.single-marker input[type=number]', function() {
        var v = parseFloat(this.value);
        if (isNaN(v)) return;
        var isMin = /_min/.test(this.id);
        var partner = document.getElementById(isMin ? this.id.replace('_min', '_max') : this.id.replace('_max', '_min'));
        var lo = this.min === '' ? -Infinity : parseFloat(this.min);
        var hi = this.max === '' ? Infinity : parseFloat(this.max);
        var fixed = Math.round(Math.min(Math.max(v, lo), hi));
        var p = partner ? parseFloat(partner.value) : NaN;
        if (!isNaN(p)) fixed = isMin ? Math.min(fixed, p) : Math.max(fixed, p);
        if (fixed !== v) {
          this.value = fixed;
          $(this).trigger('change');
        }
      });
      // coordinate range validation: slider value is used for search, number boxes are kept in sync with it by the browser
      (function() {
        var limits = {lon: 180, lat: 90};
        function parts(axis) {
          var slider = $('#node_' + axis + '_range');
          return {slider: slider, ion: slider.data('ionRangeSlider'),
                  min: document.getElementById('node_' + axis + '_min'), max: document.getElementById('node_' + axis + '_max')};
        }
        // full range is dimmed, since it is ignored in the search
        function dim(axis, from, to) {
          $('.coord-range:has(#node_' + axis + '_range)').toggleClass('dim-input', from <= -limits[axis] && to >= limits[axis]);
        }
        function fromSlider(axis) {
          var p = parts(axis);
          if (!p.ion) return;
          p.min.value = p.ion.result.from;
          p.max.value = p.ion.result.to;
          dim(axis, p.ion.result.from, p.ion.result.to);
        }
        function fromBoxes(axis, editedMin) {
          var p = parts(axis);
          if (!p.ion) return;
          var clamp = function(x, fallback) { x = parseFloat(x); return isNaN(x) ? fallback : Math.min(Math.max(x, -limits[axis]), limits[axis]); };
          var from = clamp(p.min.value, -limits[axis]), to = clamp(p.max.value, limits[axis]);
          if (from > to) { if (editedMin) from = to; else to = from; }
          p.min.value = from;
          p.max.value = to;
          p.ion.update({from: from, to: to});
          p.slider.trigger('change');
          dim(axis, from, to);
        }
        ['lon', 'lat'].forEach(function(axis) {
          $(document).on('change', '#node_' + axis + '_range', function() { fromSlider(axis); });
          $(document).on('change', '#node_' + axis + '_min', function() { fromBoxes(axis, true); });
          $(document).on('change', '#node_' + axis + '_max', function() { fromBoxes(axis, false); });
        });
        $(document).on('shiny:sessioninitialized', function() {
          setTimeout(function() { fromSlider('lon'); fromSlider('lat'); }, 0);
        });
      })();
      Shiny.addCustomMessageHandler("initLookupMap", function(data) {
        window.mapBySID = {};
        data.forEach(function(row) {
          window.mapBySID[row.sampleid] = row.processid;
        });
      });
      Shiny.addCustomMessageHandler("resize_map", function(mapId) {
        setTimeout(function() {
          var widget = HTMLWidgets.find(mapId);
          var map = widget && widget.getMap();
          if (map) map.invalidateSize();
        }, 100);
      });
      Shiny.addCustomMessageHandler("open_path", function(path) {
        if (!(window.electronAPI && window.electronAPI.openPath)) return;
        window.electronAPI.openPath(path).then(function(err) {
          if (err) Shiny.setInputValue('open_path_error', err, {priority: 'event'});
        });
      });
      // copy text to the clipboard (sent by cb() in global.R); outside the desktop app, the browser clipboard is used (only on secure origins such as localhost)
      Shiny.addCustomMessageHandler("copy_text", function(text) {
        var copy = (window.electronAPI && window.electronAPI.copyText)
          ? window.electronAPI.copyText(text)
          : (navigator.clipboard
              ? navigator.clipboard.writeText(text)
              : Promise.reject(new Error('the browser clipboard is not available here')));
        copy.catch(function(err) {
          Shiny.setInputValue('clipboard_error', String(err), {priority: 'event'});
        });
      });
      Shiny.addCustomMessageHandler("choose_file", function(opts) {
        if (!(window.electronAPI && window.electronAPI.openFile)) return;
        window.electronAPI.openFile({title: opts.title, filters: opts.filters}).then(function(path) {
          if (path) Shiny.setInputValue(opts.inputId, path, {priority: 'event'});
        });
      });
      Shiny.addCustomMessageHandler('click_link', function(id) {
        document.getElementById(id).click();
      });
      Shiny.addCustomMessageHandler('clear_input', function(inputId) {
        Shiny.setInputValue(inputId, null);
      });
      Shiny.addCustomMessageHandler('reset-dt', function(outputId) {
        var tbl = $('#' + outputId).find('table.dataTable').first();
        if (tbl.length === 0) return;
        var dt = tbl.DataTable();
        dt.order([]);
        dt.search('');
        dt.columns().search('');
        var container = $(dt.table().container());
        container.find('input').val('');
        container.find('select').prop('selectedIndex', 0);
        container.find('.glyphicon-remove-circle').css('display', 'none');
        dt.draw();
      });
    )"))
  ),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      div(id = "side-top",
          div(id = "side-title",
              span(img(src = "reflectR-name.png", alt = "BOLDreflectR", style = "height: 3rem;")),
              `data-ver` = paste0("v", ver)),
          div(
            actionLink("app_info", icon("circle-info")),
            actionLink("settings", icon("gear"))),
          ),
      bslib::accordion(
        id="optpanels",
        bslib::accordion_panel(
          value="fetchdata",
          "Get data",
          icon = bsicons::bs_icon("cloud-arrow-down"),
                      hidden(
                        tagAppendChild(
                          passwordInput(
                            "api_key",
                            "BOLD API key:"),
                          checkboxInput("remember_key",
                            div("Remember key", 
                                bslib::tooltip(
                                  # (clicks on the icon don't tick the checkbox)
                                  tags$span(icon("circle-question"), onclick = "event.preventDefault()"),
                                  paste0("When 'Remember' is ticked, the API key is saved to your system's credential store (",keystore,") when the 'Get data' button is pressed. Otherwise, it is used until the app is closed. Once set, it can be updated from the settings panel."),
                                  id = "api_key_tip")),
                            value = TRUE)
                        )
                      ),
                      bslib::navset_pill(
                        id="query_type",
                        bslib::nav_panel(value="fetch_opts",
                                         "Fetch",
                                         inputGroup(title = "Parameters:",
                                         selectInput("fetch_by",
                                                     "Fetch by:",
                                                     list("Process ID" = "processid",
                                                          "Sample ID" = "sampleid",
                                                          "BIN" = "bin_uris",
                                                          "Dataset codes" = "dataset_codes",
                                                          "Project codes" = "project_codes"
                                                     )
                                         ),
                                         textAreaInput( 
                                           "fetch_id_list", 
                                           div("Identifiers:",
                                               bslib::tooltip(
                                                 icon("circle-question"),
                                                 "Comma-separated or one per line.",
                                                 id = "query_tip"))
                                         ))), 
                        bslib::nav_panel(value="search_opts",
                                         "Search",
                                         inputGroup(title = "Parameters:",
                                         textAreaInput( 
                                           "search_tax", 
                                           div("Taxonomy:",
                                               bslib::tooltip(
                                                 icon("circle-question"),
                                                 paste0("Comma-separated or one taxon per line, no quotes."),
                                                 id = "tax_search_tip"))
                                         ),
                                         textAreaInput( 
                                           "search_geo", 
                                           div("Geography:",
                                               bslib::tooltip(
                                                 icon("circle-question"),
                                                 HTML("Country, ocean, province, state, region, sector, realm*, biome*, or ecoregion*. One term per line, no quotes needed.<br><span style=\"font-size: 0.7rem;\">*As defined by RESOLVE; case-sensitive, spaces replaced by underscores.</span>"),
                                                 id = "geo_search_tip"))
                                         ),
                                         div(class="form-group shiny-input-container marker-select",
                                             div(class="control-label", "Marker:"),
                                             div(class="form-group shiny-input-container single-marker",
                                                 selectInput(
                                                   "seq_marker",
                                                   label = "",
                                                   selected = "",
                                                   marker_options),
                                                 numericInput(
                                                   "seq_min",
                                                   label = "",
                                                   value = NULL,
                                                   min = 5, max = 2000, step = 1),
                                                 numericInput(
                                                   "seq_max",
                                                   label = "",
                                                   value = NULL,
                                                   min = 5, max = 2000, step = 1))
                                         ),
                                         shinyWidgets::airDatepickerInput(
                                           "search_dates",
                                           label = "Collection date range:",
                                           value = NULL,
                                           range = TRUE,
                                           view = "years",
                                           clearButton = TRUE,
                                           addon = "none",
                                           placeholder = "Start - End",
                                           minDate = "2000-01-01", maxDate = "2075-01-01"),
                                         textAreaInput( 
                                           "search_inst", 
                                           div("Institutes:",
                                               bslib::tooltip(icon("circle-question"),
                                                              "Institute where physical specimen is stored. One per line, no quotes needed."))
                                         ))),
                        bslib::nav_panel(value = "source-local",
                                         "Local data",
                                         inputGroup(title = "Source data:",
                                                    groupId = "node_source_pkg",
                                                    div(class = "shiny-input-container",
                                                        HTML('<label class="control-label">Select a local data package:</label>'),
                                                        div(class = "file-input-wrapper input-group",
                                                            selectInput("datapackage_id", "",
                                                                        choices = stats::setNames(datapkgs$path, datapkgs$id)),
                                                            actionButton("datapackage_btn", "Add/Edit...")))),
                                         inputGroup(title = "Search terms:",
                                                    groupId = "node_source_terms",
                                                    textAreaInput("node_id_list", div("Process IDs and/or sample IDs:", bslib::tooltip(icon("circle-question"), "Comma-separated or one per line."))),
                                                    textAreaInput("node_bin_list", div("BINs:", bslib::tooltip(icon("circle-question"), "Comma-separated or one per line."))),
                                                    textAreaInput("node_recset_list", div("Project and/or dataset codes:", bslib::tooltip(icon("circle-question"), "Comma-separated or one per line."))),
                                                    textAreaInput("node_tax", div("Taxonomy:", bslib::tooltip(icon("circle-question"), "Comma-separated or one per line."))),
                                                    textAreaInput("node_geo", div("Geography:", bslib::tooltip(icon("circle-question"), "Country, ocean, province, state, region, sector, or site. One term per line, no quotes needed."))),
                                                    bslib::accordion(
                                                      id = "more_opts",
                                                      bslib::accordion_panel(
                                                        "Additional fields",
                                                        textAreaInput("node_ider_list", div("Identified by:", bslib::tooltip(icon("circle-question"), "Comma-separated or one per line."))),
                                                        textAreaInput("node_biogeo_list", div("Biome, realm, or ecoregion:", bslib::tooltip(icon("circle-question"), "Biogeographic realm, biome, or ecoregion (as defined by RESOLVE; case-sensitive, spaces replaced by underscores). One term per line, no quotes needed."))),
                                                        textAreaInput("node_inst_list", div("Institutes:", bslib::tooltip(icon("circle-question"), "Institute where physical specimen is stored. One term per line, no quotes needed."))),
                                                        textAreaInput("node_seqinst_list", div("Sequence run sites:", bslib::tooltip(icon("circle-question"), "Institute where sequencing was performed, or database from which the sequence was retrieved. One term per line, no quotes needed.")))))),
                                         inputGroup(title = "Markers:",
                                                    groupId = "node_markers",
                                                    groupClass = "marker-select",
                                                    div(class="form-group shiny-input-container single-marker",
                                                        selectInput(
                                                          "node_seq_marker_1",
                                                          label = "",
                                                          selected = "",
                                                          marker_options),
                                                        numericInput(
                                                          "node_seq_min_1",
                                                          label = "",
                                                          value = NULL,
                                                          min = 5, max = 2000, step = 1),
                                                        numericInput(
                                                          "node_seq_max_1",
                                                          label = "",
                                                          value = NULL,
                                                          min = 5, max = 2000, step = 1)),
                                                    div(class="flex-div",
                                                        actionButton("node_add_marker", icon("plus")),
                                                        actionButton("node_del_marker", icon("minus")))
                                         ),
                                         inputGroup(title = "Coordinates:",
                                                    groupId = "node_bounding_box",
                                                    div(class="form-group shiny-input-container",
                                                        div(class="control-label", "Longitude:"),
                                                        div(class="flex-div coord-range",
                                                            numericInput("node_lon_min", "", value = -180),
                                                            sliderInput(
                                                              "node_lon_range", "",
                                                              min = -180, max = 180,
                                                              step = 0.01,
                                                              ticks = FALSE,
                                                              post = "°",
                                                              value = c(-180, 180)),
                                                            numericInput("node_lon_max", "", value = 180)),
                                                        div(class="control-label", "Latitude:"),
                                                        div(class="flex-div coord-range",
                                                            numericInput("node_lat_min", "", value = -90),
                                                            sliderInput(
                                                              "node_lat_range", "",
                                                              min = -90, max = 90,
                                                              step = 0.01,
                                                              ticks = FALSE,
                                                              post = "°",
                                                              value = c(-90, 90)),
                                                            numericInput("node_lat_max", "", value = 90)))))
                      ),
          checkboxInput(
            "fetch_binmates",
            "Find additional BIN members"
          ),
          actionButton(
            "fetch_btn",
            "Get data"
          )
        ),  
        bslib::accordion_panel(
          value="customize",
          "Customize",
          icon = bsicons::bs_icon("sliders"),
          selectizeInput("filt_opt",
                         tagList(div("Columns to show:"),
                                 actionLink(
                                   "save_filter_set",
                                   icon("bookmark"))),
                         choices = filter_options(),
                         selected = try(user_config$settings$col_set, silent = TRUE),
                         multiple = TRUE,
                         options = list(plugins = list("drag_drop", "remove_button"))
          ),
          selectizeInput("filt_seq",
                         "Filter by marker:",
                         choices = list("COI-5P" = "COI-5P", 
                              "COI-3P" = "COI-3P",
                              "28S" = "28S",
                              "28S-D2-D3" = "28S-D2-D3",
                              "18S" = "18S",
                              "18S-5P" = "18S-5P"),
                         selected = try(user_config$settings$filt_seq, silent = TRUE),
                         multiple = TRUE,
                         options = list(plugins = list("remove_button"))
          ),
          div(class="form-group shiny-input-container",
              actionButton(
                "reset_btn",
                "Reset"
                )),
          div(class="flex-div",
            bslib::input_switch(
              "include_binmates",
              "Include additional BIN members"
              ),
            actionLink(
              "view_binmates",
              icon("square-arrow-up-right")
              )),
          hidden(bslib::input_switch(
            "include_nts",
            div("Show NTS records in fetched data",
                bslib::tooltip(
                  icon("circle-question"),
                  "Shows or hides non-target sequence records within existing results (i.e., NTS records matching initial query terms).")),
            value = isTRUE(user_config$settings$include_nts)
          )),
          bslib::input_switch(
            "collapse_mrkrs",
            "Collapse multiple markers into single rows",
            value = isTRUE(user_config$settings$collapse_mrkrs)
          )
        ),  
        bslib::accordion_panel(
          value="summarize",
          "Summarize",
          icon = bsicons::bs_icon("bar-chart"),
          inputGroup(title = "Value counts",
                     groupId = "sum-opts",
                     selectInput("ana_opt",
                                 "Summarize by:",
                                 list(
                                   "Taxonomy" = list(
                                     "Taxonomic summary" = "tax_summary"),
                                   "Unique values - selected fields" = list(
                                     "Taxon" = "identification",
                                     "BIN" = "bin_uri",
                                     "Identification dates (parsed)" = "id_date_parsed",
                                     "Projects" = "projects",
                                     "Datasets" = "datasets",
                                     "Country" = "country.ocean"),
                                   "Unique values - all fields" = config$fieldsets$bcdm[!config$fieldsets$bcdm %in% c("identification","bin_uri","country.ocean","bold_recordset_code_arr")]
                                 ), multiple = FALSE),
                     div(class="form-group shiny-input-container",
                         actionButton(
                           "ana_btn",
                           "Generate"))),
          inputGroup(title = "Query hit report",
                     groupId = "qhits-opts",
                     div(class="form-group",
                         "Generate a report of query terms that were found or not:",
                         bslib::tooltip(
                           icon("circle-question"),
                           HTML("This reflects the original query only, not additional BIN members."))),
                     div(actionButton(
                       "query_hits_btn",
                       "Generate"
                     ))),
          inputGroup(title = "Distribution map",
                     groupId = "map-opts",
                     div(class="form-group",
                         "Plot filtered records on a map:",
                         bslib::tooltip(
                           icon("circle-question"),
                           HTML("Only rows matching current filters that have valid coordinates will be included on the map."))),
                     div(actionButton(
                       "map_btn",
                       "Generate"
                     )))),
        bslib::accordion_panel(
          value="analyze",
          "Analyze",
          icon = bsicons::bs_icon("graph-up"),
          inputGroup(title = "Sequence alignment",
                     groupId = "aln-opts",
                     alignment_options("aln"),
                     div(actionButton("aln_btn", "Align"))),
          inputGroup(title = "Neighbour-joining tree",
                     groupId = "tree-opts",
                     # marker is set to match alignment marker and disabled while "Use current alignment" is checked
                     alignment_marker_input("tree"),
                     selectInput("tree_model", "Distance model:",
                                 list("Kimura 2-parameter (K2P)" = "K80",
                                      "p-distance" = "raw",
                                      "Tamura-Nei (TN93)" = "TN93")),
                     conditionalPanel("!input.tree_from_aln", alignment_method_inputs("tree")),
                     # disabled and unchecked while there is no alignment; also unchecked automatically when the filtered records change
                     checkboxInput("tree_from_aln",
                                   div("Use current alignment",
                                       bslib::tooltip(
                                         icon("circle-question"),
                                         "Build the tree using the alignment from the Alignment tab, including any manual edits.")),
                                   value = FALSE),
                     # shown once BIN reps have been selected (server.R)
                     hidden(div(id = "tree_bin_reps_wrap",
                                checkboxInput("tree_bin_reps",
                                              div("BIN representatives only",
                                                  bslib::tooltip(
                                                    icon("circle-question"),
                                                    "Build the tree from the records in the BIN reps tab (plus its table filters). With 'Use current alignment' checked, their sequences are taken from the existing alignment.")),
                                              value = FALSE))),
                     checkboxInput("tree_collapse",
                                   div("Collapse identical sequences",
                                       bslib::tooltip(
                                         icon("circle-question"),
                                         "Build the tree from unique sequences only, then add identical sequences back as zero-length tips. Much faster for large datasets (e.g. 5,800 COI barcodes: about 1 minute instead of 9), but approximate, because NJ gives some weight to every copy; arrangement of tips may differ slightly.")),
                                   value = FALSE),
                     checkboxInput("tree_midpoint", "Midpoint root", value = TRUE),
                     # tip labels and colours can be changed after the tree is built (server.R)
                     tree_label_input("tree_labels",
                                      div("Tip labels:",
                                          bslib::tooltip(
                                            icon("circle-question"),
                                            "Labels can also be modified on the live tree after it is built. Default tip labels can be changed in Settings."))),
                     selectInput("tree_colour",
                                 div("Colour by:",
                                     bslib::tooltip(
                                       icon("circle-question"),
                                       "Colour tip labels (and uniform clusters) by one of the label fields; tips without a value are grey (colours are reused after 64 values). Can also be modified on the live tree after it is built.")),
                                 choices = c("None" = "none")),
                     div(actionButton("tree_btn", "Build tree"))),
          inputGroup(title = tagList("Diversity profile", actionLink("div_profile_info", icon("circle-info"))),
                     groupId = "div-analysis",
                     div(id = "div-analysis-opts",
                       selectInput("div_rank", "Analyze by:", 
                                   c("Species if known, BIN if not" = "bin_fallback",
                                     "BIN" = "bin_uri",
                                     "Species" = "species")),
                       selectInput("div_site_type", "Assemblage type:",
                                   list("Locations" = "locations",
                                        "Grid squares" = "grids",
                                        "Higher taxa" = "taxa",
                                        "None (overall alpha diversity only)" = "none")),
                       hidden(selectInput("div_location_type", "Use as sites:", 
                                          list("Sites" = "site",
                                               "Sectors" = "sector",
                                               "Regions" = "region",
                                               "Provinces/States" = "province.state",
                                               "Countries/Oceans" = "country.ocean",
                                               "Ecoregions" = "ecoregion",
                                               # "Biomes" = "biome",
                                               "Realms" = "realm"))),
                       hidden(numericInput("div_grid_size", "Grid unit size (km):",
                                           value = 500, min = 0.001, max = 9227, step = 1)),
                       hidden(selectInput("div_higher_tax", "Compare across:",
                                          list("Orders" = "order",
                                               "Families" = "family",
                                               "Subfamilies" = "subfamily",
                                               "Tribes" = "tribe",
                                               "Genera" = "genus"))),
                       checkboxInput("div_presence", "Compute as presence-absence matrix"),
                       selectizeInput("div_profile", "Profile:", 
                                      list("Rarefaction & extrapolation" = "rarefy",
                                           "Shannon diversity index" = "shannon",
                                           "Preston plot" = "preston",
                                           "Beta diversity index" = "beta"),
                                      selected = c("rarefy", "shannon", "preston", "beta"),
                                      multiple = TRUE,
                                      options = list(plugins = list("remove_button"))),
                       checkboxInput("div_rare_by_site", 
                                     div("Perform rarefaction for all sites",
                                         bslib::tooltip(
                                           icon("circle-question"),
                                           paste0("Sites with fewer than 10 observations or 2 species will be dropped for this analysis."))),
                                     value = FALSE),
                       selectInput("div_beta_type", "Beta diversity type:",
                                   list("Jaccard index" = "jaccard",
                                        "Sorensen index" = "sorensen"),
                                   selected = "jaccard"),
                       actionButton("div_compute_btn", "Run")),
                     hidden(div(id = "div-analysis-msg",
                                span(style = "opacity: 0.5;",
                                     "Analysis only available with ≥10 records and ≥2 species/BINs.")
                     ))),
          inputGroup(title = "Consensus BIN taxonomy",
                     groupId = "bin-cons-opts",
                     numericInput(
                       "bc_threshold",
                       div("Consensus threshold:",
                           bslib::tooltip(
                             icon("circle-question"),
                             paste0("Minimum proportion of records in a BIN that must have a concordant identification in order to establish consensus."))),
                       1, min = 0.5, max = 1.0, step = 0.05),
                     numericInput(
                       "bc_minids",
                       div("Minimum IDs:",
                           bslib::tooltip(
                             icon("circle-question"),
                             paste0("Minimum number of concordant identifications needed to establish a consensus identification. If a BIN contains fewer records than this number, then they must all have matching identifications to reach consensus."))),                2, min = 1, step = 1),
                     checkboxInput(
                       "bc_enforcesci",
                       "Limit to scientific names",
                       value = TRUE
                     ),
                     checkboxInput(
                       "bc_portal_stats",
                       div("Include BOLD Portal stats",
                           bslib::tooltip(
                             icon("circle-question"),
                             HTML("<strong>Please note</strong>: This can take <strong>several minutes</strong> for more than a few dozen BINs. Additional statistics include member counts (total, private, public), pairwise distances (average, maximum), and nearest-neighbour information (NN distance, NN BIN)."),
                             id = "portaltip"
                           ))),
                     div(actionButton(
                       "bin_consensus_btn",
                       "Run"))),
          inputGroup(title = "BIN representatives",
                     groupId = "bin-rep-opts",
                     numericInput(
                       "bin_rep_num",
                       "Number of representatives per BIN:",
                       value = 1,
                       min = 1, step = 1
                     ),
                     checkboxInput(
                       "bin_rep_tax",
                       "Add representatives per distinct taxon",
                       value = FALSE
                     ),
                     hidden(div(id = "bin_rep_tax_opts",
                                checkboxInput(
                                  "bin_rep_scientific",
                                  div("Enforce scientific names",
                                      bslib::tooltip(
                                        icon("circle-question"),
                                        HTML("Ignore interim / provisional names for the purposes of determining distinct taxa.")
                                      )),
                                  value = TRUE
                                ),
                                checkboxInput(
                                  "bin_rep_non_redundant",
                                  div("Lowest non-redundant taxa",
                                      bslib::tooltip(
                                        icon("circle-question"),
                                        HTML("Select representatives at the lowest available rank from each distinct taxonomic lineage. For example, in a BIN with the identifications Apidae, <em>Bombus</em>, and <em>Bombus terrestris</em>, records assigned to <em>Bombus terrestris</em> will be selected first."),
                                      )),
                                  value = TRUE
                                ))),
                     bslib::input_switch(
                       "bin_rep_default",
                       div("Use default selection criteria",
                           bslib::tooltip(
                             icon("circle-question"),
                             HTML("Default criteria, in order of priority, are: <strong>(1)</strong> vouchered (i.e. not mined from an external database); <strong>(2)</strong> COI-5P sequence length closest to mode for BIN; <strong>(3)</strong> identified by morphology, image, or sequence (in descending order of preference); <strong>(4)</strong> specimen stored at the CBG (i.e. may be available to loan); <strong>(5)</strong> latest collection date; <strong>(6)</strong> latest sequence upload date.")
                           )),
                       value = TRUE
                     ),
                     hidden(div(id = "bin_rep_criteria_wrapper",
                       div(id = "bin-rep-tooltip",
                           bslib::tooltip(icon("circle-question"),
                                          options = list(customClass = "grey_tooltip"),
                                          "Drag and drop to order by priority, or move unwanted criteria to 'ignored' box.")),
                       bucket_list(
                         header = "Select representative records by:",
                         group_name = "bucket_list_group",
                         orientation = "vertical",
                         add_rank_list(
                           input_id = "bin_rep_criteria",
                           text = "Selected criteria:",
                           labels = list(
                             "bin_rep_vouchered" = div("Sequence is vouchered",
                                                       bslib::tooltip(icon("circle-question"),
                                                                      options = list(customClass = "grey_tooltip"),
                                                                      "Prioritize records with known voucher repositories over those mined from databases like GenBank.")),
                             "bin_rep_seq" = tagList(selectInput("bin_rep_seq_opt",
                                                         "COI sequence length",
                                                         choices = list("nearest to 658bp" = 658,
                                                                        "nearest to ____bp (specify)" = "custom",
                                                                        "nearest to mode for BIN" = "COI_auto",
                                                                        "longest",
                                                                        "shortest"),
                                                         selected = "COI_auto"),
                                                     hidden(numericInput("bin_rep_seq_len", "", value = 658))),
                             "bin_rep_id" = selectizeInput("bin_rep_id_opt", "Preferred ID method(s)",
                                                           choices = list("BIN based", "BOLD ID Engine", "Tree based", "Morphology",
                                                                          "Morphology and sequence based", "Image based",
                                                                          "Image and sequence based", "Other sequence based approach", "Other"),
                                                           selected = "Morphology",
                                                           multiple = TRUE,
                                                           options = list(plugins = list("remove_button"))),
                             "bin_rep_inst" = selectizeInput("bin_rep_inst_opt", "Preferred institute(s)",
                                                             choices = list("Centre for Biodiversity Genomics"),
                                                             selected = "Centre for Biodiversity Genomics",
                                                             multiple = TRUE,
                                                             options = list(plugins = list("remove_button"))),
                             "bin_rep_date" = selectInput("bin_rep_date_opt", "Collection date", choices = list("latest", "oldest")),
                             "bin_rep_up" = selectInput("bin_rep_up_opt", "Sequence upload date", choices = list("latest", "oldest"))
                           )
                         ),
                         add_rank_list(
                           input_id = "bin_rep_unused",
                           text = "Ignored criteria:",
                           labels = NULL
                         )
                       ))),
                     div(actionButton(
                       "bin_rep_btn",
                       "Run"
                     ))))
      ), width = 3
    ),
    hidden(mainPanel(
      id = "main_panel",
      bslib::navset_underline(
        id="tabs",
        bslib::nav_panel(
          value="data",
          class = "testclass",
          "Data",
          hidden(div(
            id = 'table_area',
            actionLink("reset_tbl", "Reset filters"),
            DT::dataTableOutput("data_table") |> withSpinner(color="#aaaaaa", type=5, size=0.6)))
          )),
      # button row with copy buttons on the left, save buttons on the right; buttons for each tab listed in tab_data (server.R)
      hidden(div(id = "table_buttons",
                 class="testclass",
                 div(id = "copy_buttons",
                     actionButton("copy_fasta", "Copy FASTA", icon = icon("copy")),
                     # (a button rather than a download link: the server checks for sequences, then starts the
                     # hidden link below, so nothing is saved when there are none)
                     actionButton("save_fasta", "Save FASTA", icon = icon("download")),
                     hidden(downloadLink("save_fasta_dl", "")),
                     actionButton("copy_table", "Copy table", icon = icon("copy")),
                     # alignment viewer buttons (these call the viewer in www/alignment-viewer.js)
                     hidden(actionButton("aln_copy_sel", "Copy selected", icon = icon("copy"),
                                         onclick = "BRAlignmentViewer.call('copySelection')")),
                     hidden(actionButton("aln_copy_all", "Copy alignment", icon = icon("copy"),
                                         onclick = "BRAlignmentViewer.call('copyAll')"))),
                 div(id = "save_buttons",
                     actionButton("save_dtpkg", "", icon = bsicons::bs_icon("database-fill-add")),
                     # group labels used for tree tab
                     hidden(span(id = "btn_label_data", class = "bar-label", "Data:")),
                     downloadButton("save_tsv", "TSV"),
                     downloadButton("save_csv", "CSV"),
                     downloadButton("save_xlsx", "XLSX"),
                     actionButton("open_xlsx", "Open XLSX", icon = icon("file-excel")),
                     hidden(actionButton("aln_save", "Save FASTA", icon = icon("download"),
                                         onclick = "BRAlignmentViewer.call('saveFasta')")),
                     # tree PDF download creates the PDF first, then uses the always-hidden 'tree_pdf_dl' button to download the finished file
                     hidden(span(id = "btn_label_tree", class = "bar-label", "Tree:")),
                     hidden(actionButton("tree_pdf", "PDF", icon = icon("download"))),
                     hidden(downloadButton("tree_pdf_dl", "")),
                     hidden(downloadButton("tree_newick", "Newick")))
      )),
    width = 9))
  )
)