library(shiny)
library(bslib)
library(taxodist)
library(ggplot2)
library(ggdendro)
library(DT)

# ── Helpers ───────────────────────────────────────────────────────────────────

parse_taxa_input <- function(text) {
  taxa <- unlist(strsplit(text, "[,\n]"))
  taxa <- trimws(taxa)
  taxa <- taxa[nchar(taxa) > 0]
  unique(taxa)
}

# ── Theme / UI ────────────────────────────────────────────────────────────────

app_theme <- bs_theme(
  version = 5,
  bg            = "#F5F0E8",
  fg            = "#1C1812",
  primary       = "#2D5016",
  secondary     = "#8B6914",
  success       = "#3A7D2C",
  info          = "#5B4A2E",
  font_scale    = 0.95,
  base_font     = font_google("Lora"),
  heading_font  = font_google("Playfair Display"),
  code_font     = font_google("JetBrains Mono")
) |>
  bs_add_rules("
    :root {
      --parchment:   #F5F0E8;
      --ink:         #1C1812;
      --moss:        #2D5016;
      --amber:       #8B6914;
      --rust:        #8B3A1A;
      --cream:       #FAF7F0;
      --border:      #C8B99A;
      --muted:       #7A6B55;
    }

    body {
      background-color: var(--parchment);
      background-image:
        radial-gradient(ellipse at 20% 10%, rgba(45,80,22,0.06) 0%, transparent 60%),
        radial-gradient(ellipse at 80% 90%, rgba(139,105,20,0.07) 0%, transparent 55%);
      font-family: 'Lora', Georgia, serif;
    }

    /* ── Header ── */
    .app-header {
      background: var(--moss);
      background-image: linear-gradient(135deg, #1a3009 0%, #2D5016 50%, #3d6b1e 100%);
      color: #F5F0E8;
      padding: 2rem 2.5rem 1.6rem;
      border-bottom: 3px solid var(--amber);
      position: relative;
      overflow: hidden;
    }
    .app-header::before {
      content: '';
      position: absolute;
      inset: 0;
      background-image: repeating-linear-gradient(
        90deg, transparent, transparent 60px,
        rgba(255,255,255,0.015) 60px, rgba(255,255,255,0.015) 61px
      );
    }
    .app-header h1 {
      font-family: 'Playfair Display', Georgia, serif;
      font-size: 2.1rem;
      font-weight: 700;
      letter-spacing: 0.01em;
      margin: 0;
      color: #F5F0E8;
      position: relative;
    }
    .app-header .subtitle {
      font-size: 0.85rem;
      color: rgba(245,240,232,0.72);
      margin-top: 0.3rem;
      letter-spacing: 0.05em;
      font-style: italic;
      position: relative;
    }
    .app-header .badge-pkg {
      display: inline-block;
      background: var(--amber);
      color: #1C1812;
      font-size: 0.72rem;
      font-family: 'JetBrains Mono', monospace;
      font-style: normal;
      padding: 0.15rem 0.55rem;
      border-radius: 2px;
      margin-left: 0.5rem;
      vertical-align: middle;
      position: relative;
    }

    /* ── Nav tabs ── */
    .nav-tabs {
      border-bottom: 2px solid var(--border);
      background: var(--cream);
      padding: 0 1.5rem;
    }
    .nav-tabs .nav-link {
      font-family: 'Playfair Display', Georgia, serif;
      font-size: 0.92rem;
      color: var(--muted);
      border: none;
      border-bottom: 3px solid transparent;
      padding: 0.75rem 1.1rem;
      border-radius: 0;
      transition: all 0.2s;
    }
    .nav-tabs .nav-link:hover { color: var(--moss); }
    .nav-tabs .nav-link.active {
      color: var(--moss);
      border-bottom-color: var(--moss);
      background: transparent;
      font-weight: 600;
    }

    /* ── Cards ── */
    .card {
      border: 1px solid var(--border);
      border-radius: 4px;
      background: var(--cream);
      box-shadow: 0 1px 4px rgba(28,24,18,0.07);
    }
    .card-header {
      background: transparent;
      border-bottom: 1px solid var(--border);
      font-family: 'Playfair Display', serif;
      font-size: 0.95rem;
      font-weight: 600;
      color: var(--moss);
      padding: 0.7rem 1.1rem;
      letter-spacing: 0.02em;
    }

    /* ── Inputs ── */
    .form-control, .form-select {
      background: #FEFCF8;
      border: 1px solid var(--border);
      border-radius: 3px;
      font-family: 'Lora', Georgia, serif;
      font-size: 0.88rem;
      color: var(--ink);
    }
    .form-control:focus, .form-select:focus {
      border-color: var(--moss);
      box-shadow: 0 0 0 2px rgba(45,80,22,0.15);
    }
    .form-label {
      font-size: 0.82rem;
      font-weight: 600;
      letter-spacing: 0.06em;
      text-transform: uppercase;
      color: var(--muted);
      margin-bottom: 0.3rem;
    }

    /* ── Buttons ── */
    .btn-primary {
      background: var(--moss);
      border-color: var(--moss);
      font-family: 'Lora', serif;
      letter-spacing: 0.04em;
      border-radius: 3px;
    }
    .btn-primary:hover { background: #1a3009; border-color: #1a3009; }
    .btn-outline-secondary {
      border-color: var(--border);
      color: var(--muted);
      font-size: 0.82rem;
    }

    /* ── Result box ── */
    .result-box {
      background: #FEFCF8;
      border: 1px solid var(--border);
      border-left: 4px solid var(--moss);
      border-radius: 3px;
      padding: 1.1rem 1.3rem;
      font-family: 'Lora', Georgia, serif;
    }
    .result-distance {
      font-family: 'Playfair Display', serif;
      font-size: 2.4rem;
      color: var(--moss);
      font-weight: 700;
      line-height: 1;
    }
    .result-label {
      font-size: 0.72rem;
      text-transform: uppercase;
      letter-spacing: 0.1em;
      color: var(--muted);
    }
    .result-mrca {
      font-style: italic;
      color: var(--amber);
      font-size: 1.05rem;
    }
    .result-meta {
      font-size: 0.82rem;
      color: var(--muted);
    }

    /* ── Lineage display ── */
    .lineage-node {
      display: inline-block;
      background: var(--parchment);
      border: 1px solid var(--border);
      border-radius: 2px;
      padding: 0.1rem 0.5rem;
      margin: 0.15rem 0.1rem;
      font-size: 0.8rem;
      font-family: 'Lora', serif;
      font-style: italic;
      color: var(--ink);
    }
    .lineage-node.shared {
      background: #d4e8c2;
      border-color: var(--moss);
      color: var(--moss);
      font-weight: 600;
    }
    .lineage-node.mrca {
      background: var(--moss);
      color: #F5F0E8;
      border-color: var(--moss);
    }
    .lineage-arrow {
      color: var(--border);
      font-size: 0.75rem;
      margin: 0 0.05rem;
    }

    /* ── Coverage pills ── */
    .cov-found    { background:#d4e8c2; color:#1a3009; }
    .cov-notfound { background:#f5d0c8; color:#6b1a0a; }
    .cov-pill {
      display:inline-block;
      border-radius:2px;
      padding:0.2rem 0.6rem;
      font-size:0.8rem;
      font-family:'JetBrains Mono',monospace;
      margin:0.2rem;
    }

    /* ── Misc ── */
    .section-divider {
      border: none;
      border-top: 1px solid var(--border);
      margin: 1.2rem 0;
    }
    .taxon-tag {
      display:inline-block;
      background:var(--parchment);
      border:1px solid var(--border);
      border-radius:2px;
      padding:0.1rem 0.45rem;
      font-size:0.78rem;
      font-style:italic;
      margin:0.1rem;
    }
    .spinner-border { color: var(--moss) !important; }
    .shiny-notification { font-family: 'Lora', serif; }

    /* ── Loading overlay ── */
    #loading_overlay {
      display:none;
      position:fixed;
      inset:0;
      background:rgba(245,240,232,0.65);
      z-index:9999;
      align-items:center;
      justify-content:center;
      flex-direction:column;
      gap:0.8rem;
    }
    #loading_overlay.show { display:flex; }
    .loading-text {
      font-family:'Playfair Display',serif;
      color:var(--moss);
      font-size:1rem;
    }
  ")

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = app_theme,
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$style(HTML("
      textarea { resize: vertical; }
      .dataTables_wrapper { font-size:0.83rem; font-family:'Lora',serif; }
    "))
  ),

  # Loading overlay
  div(id = "loading_overlay",
      div(class = "spinner-border", role = "status"),
      div(class = "loading-text", "Querying The Taxonomicon…")
  ),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('show_loading', function(msg) {
      document.getElementById('loading_overlay').classList.add('show');
    });
    Shiny.addCustomMessageHandler('hide_loading', function(msg) {
      document.getElementById('loading_overlay').classList.remove('show');
    });
  ")),

  # Header
  div(class = "app-header mb-0",
      h1(HTML('taxodist <span class=\"badge-pkg\">v0.1.0</span>')),
      div(class = "subtitle",
          "Taxonomic Distance & Phylogenetic Lineage Explorer — powered by The Taxonomicon"
      )
  ),

  # Main tabs
  navset_tab(
    id = "main_tabs",

    # ── Tab 1: Pairwise Distance ──────────────────────────────────────────────
    nav_panel("Pairwise Distance",
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header("Taxa"),
                             div(class = "p-3",
                                 textInput("pd_taxon_a", "Taxon A", placeholder = "e.g. Tyrannosaurus"),
                                 textInput("pd_taxon_b", "Taxon B", placeholder = "e.g. Velociraptor"),
                                 div(class = "d-flex gap-2 mt-3",
                                     actionButton("pd_run", "Compute Distance",
                                                  class = "btn btn-primary flex-grow-1", icon = icon("ruler")),
                                     actionButton("pd_example", "Shuffle",
                                                  class = "btn btn-outline-secondary", icon = icon("shuffle"))
                                 ),
                                 hr(class = "section-divider"),
                                 div(class = "result-label mb-1", "Try these:"),
                                 div(
                                   tags$small(class="text-muted fst-italic",
                                              "Tyrannosaurus / Velociraptor • Homo / Quercus • Nomingia / Huanansaurus"
                                   )
                                 )
                             )
                           )
                    ),
                    column(8,
                           uiOutput("pd_result_ui")
                    )
                  ),
                  fluidRow(
                    class = "mt-3",
                    column(12,
                           uiOutput("pd_lineage_ui")
                    )
                  )
              )
    ),

    # ── Tab 2: Distance Matrix ────────────────────────────────────────────────
    nav_panel("Distance Matrix",
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header("Taxa List"),
                             div(class = "p-3",
                                 textAreaInput("dm_taxa", "Enter taxa (one per line or comma-separated)",
                                               rows = 8,
                                               placeholder = "Tyrannosaurus\nVelociraptor\nSpinosaurus\nAllosaurus\nCarnotaurus"
                                 ),
                                 div(class = "d-flex gap-2 mt-2",
                                     actionButton("dm_run", "Build Matrix",
                                                  class = "btn btn-primary flex-grow-1", icon = icon("table")),
                                     actionButton("dm_example", "Shuffle",
                                                  class = "btn btn-outline-secondary", icon = icon("shuffle"))
                                 ),
                                 hr(class = "section-divider"),
                                 fileInput("dm_upload", "Or upload CSV (one taxon per row)",
                                           accept = ".csv", width = "100%"),
                             )
                           )
                    ),
                    column(8,
                           uiOutput("dm_result_ui")
                    )
                  )
              )
    ),

    # ── Tab 3: Closest Relative ───────────────────────────────────────────────
    nav_panel("Closest Relative",
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header("Query"),
                             div(class = "p-3",
                                 textInput("cr_query", "Query Taxon", placeholder = "e.g. Tyrannosaurus"),
                                 textAreaInput("cr_candidates", "Candidate Taxa (one per line or comma-separated)",
                                               rows = 6,
                                               placeholder = "Velociraptor\nTriceratops\nBrachiosaurus\nAllosaurus"
                                 ),
                                 div(class = "d-flex gap-2 mt-2",
                                     actionButton("cr_run", "Find Closest",
                                                  class = "btn btn-primary flex-grow-1", icon = icon("crosshairs")),
                                     actionButton("cr_example", "Shuffle",
                                                  class = "btn btn-outline-secondary", icon = icon("shuffle"))
                                 )
                             )
                           )
                    ),
                    column(8,
                           uiOutput("cr_result_ui")
                    )
                  )
              )
    ),

    # ── Tab 4: Lineage Explorer ───────────────────────────────────────────────
    nav_panel("Lineage Explorer",
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header("Taxon"),
                             div(class = "p-3",
                                 textInput("le_taxon", "Taxon name", placeholder = "e.g. Homo sapiens"),
                                 actionButton("le_run", "Get Lineage",
                                              class = "btn btn-primary w-100 mt-2", icon = icon("sitemap")),
                                 hr(class = "section-divider"),
                                 textInput("le_clade_check", "Check clade membership",
                                           placeholder = "e.g. Amniota"),
                                 actionButton("le_member_run", "Check",
                                              class = "btn btn-outline-secondary w-100 mt-1")
                             )
                           )
                    ),
                    column(8,
                           uiOutput("le_result_ui")
                    )
                  )
              )
    ),

    # ── Tab 5: Coverage Check ─────────────────────────────────────────────────
    nav_panel("Coverage Check",
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header("Taxa to Check"),
                             div(class = "p-3",
                                 textAreaInput("cc_taxa", "Enter taxa (one per line or comma-separated)",
                                               rows = 8,
                                               placeholder = "Tyrannosaurus\nVelociraptor\nFakeosaurus\nHomo"
                                 ),
                                 div(class = "d-flex gap-2 mt-2",
                                     actionButton("cc_run", "Check Coverage",
                                                  class = "btn btn-primary flex-grow-1", icon = icon("check-circle")),
                                     actionButton("cc_example", "Shuffle",
                                                  class = "btn btn-outline-secondary", icon = icon("shuffle"))
                                 ),
                                 hr(class = "section-divider"),
                                 fileInput("cc_upload", "Or upload CSV",
                                           accept = ".csv", width = "100%")
                             )
                           )
                    ),
                    column(8,
                           uiOutput("cc_result_ui")
                    )
                  )
              )
    ),

    # ── Tab 6: Filter by Clade ────────────────────────────────────────────────
    nav_panel("Filter by Clade",
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header("Filter Settings"),
                             div(class = "p-3",
                                 textAreaInput("fc_taxa", "Taxa list (one per line or comma-separated)",
                                               rows = 6,
                                               placeholder = "Tyrannosaurus\nTriceratops\nVelociraptor\nBrachiosaurus\nHomo"
                                 ),
                                 textInput("fc_clade", "Clade to filter by", placeholder = "e.g. Theropoda"),
                                 div(class = "d-flex gap-2 mt-2",
                                     actionButton("fc_run", "Filter",
                                                  class = "btn btn-primary flex-grow-1", icon = icon("filter")),
                                     actionButton("fc_example", "Shuffle",
                                                  class = "btn btn-outline-secondary", icon = icon("shuffle"))
                                 )
                             )
                           )
                    ),
                    column(8,
                           uiOutput("fc_result_ui")
                    )
                  )
              )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Pairwise Distance ───────────────────────────────────────────────────────

  # Pool of diverse taxa for random shuffle
  shuffle_pool <- c(
    "Tyrannosaurus", "Velociraptor", "Triceratops", "Brachiosaurus",
    "Spinosaurus", "Allosaurus", "Carnotaurus", "Diplodocus",
    "Ankylosaurus", "Stegosaurus", "Iguanodon", "Parasaurolophus",
    "Homo", "Quercus", "Agaricus", "Escherichia",
    "Drosophila", "Canis", "Panthera", "Pycnonemosaurus",
    "Nomingia", "Huanansaurus", "Aucasaurus", "Coelophysis"
  )

  observeEvent(input$pd_example, {
    pair <- sample(shuffle_pool, 2)
    updateTextInput(session, "pd_taxon_a", value = pair[1])
    updateTextInput(session, "pd_taxon_b", value = pair[2])
  })

  pd_result <- eventReactive(input$pd_run, {
    req(nchar(trimws(input$pd_taxon_a)) > 0, nchar(trimws(input$pd_taxon_b)) > 0)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    withCallingHandlers(
      taxo_distance(trimws(input$pd_taxon_a), trimws(input$pd_taxon_b), verbose = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
  })

  output$pd_result_ui <- renderUI({
    res <- pd_result()
    if (is.null(res)) return(div(class="p-3 text-muted fst-italic", "Results will appear here."))

    dist_fmt <- if (is.infinite(res$distance)) "∞" else
      if (res$distance == 0) "0 (ancestor)" else
        round(res$distance, 6)
    interp <- if (is.infinite(res$distance)) "No common ancestor found."
    else if (res$distance == 0) "One taxon is an ancestor of the other."
    else sprintf("MRCA at depth %d; distance = 1 / %d.", res$mrca_depth, res$mrca_depth)

    tagList(
      div(class = "result-box mb-3",
          fluidRow(
            column(4,
                   div(class = "result-label", "Distance"),
                   div(class = "result-distance", dist_fmt)
            ),
            column(8,
                   div(class = "result-label", "Most Recent Common Ancestor"),
                   div(class = "result-mrca", res$mrca),
                   div(class = "result-meta mt-1",
                       sprintf("MRCA depth: %d | Depth %s: %d | Depth %s: %d",
                               res$mrca_depth, res$taxon_a, res$depth_a, res$taxon_b, res$depth_b)
                   ),
                   div(class = "result-meta mt-1 fst-italic", interp)
            )
          )
      )
    )
  })

  output$pd_lineage_ui <- renderUI({
    res <- pd_result()
    if (is.null(res)) return(NULL)
    ta <- trimws(input$pd_taxon_a)
    tb <- trimws(input$pd_taxon_b)
    lin_a <- get_lineage(ta)
    lin_b <- get_lineage(tb)
    if (is.null(lin_a) || is.null(lin_b)) return(NULL)

    mrca_d <- res$mrca_depth
    shared  <- lin_a[seq_len(mrca_d)]

    render_lineage <- function(lin, label, shared_depth) {
      nodes <- lapply(seq_along(lin), function(i) {
        cls <- if (i == shared_depth) "lineage-node mrca"
        else if (i <= shared_depth) "lineage-node shared"
        else "lineage-node"
        tagList(
          if (i > 1) span(class = "lineage-arrow", "›") else NULL,
          span(class = cls, lin[i])
        )
      })
      tagList(
        div(class = "result-label mb-1", label),
        div(style = "line-height: 2.2;", nodes)
      )
    }

    card(
      card_header("Lineage Comparison"),
      div(class = "p-3",
          fluidRow(
            column(12,
                   div(class = "mb-2",
                       span(class = "lineage-node shared", "■"), " Shared trunk  ",
                       span(class = "lineage-node mrca", "■"), " MRCA  ",
                       span(class = "lineage-node", "■"), " Unique"
                   )
            )
          ),
          fluidRow(
            column(6, render_lineage(lin_a, ta, mrca_d)),
            column(6, render_lineage(lin_b, tb, mrca_d))
          )
      )
    )
  })

  # ── Distance Matrix ─────────────────────────────────────────────────────────

  observeEvent(input$dm_example, {
    n <- sample(5:7, 1)
    taxa <- sample(shuffle_pool, n)
    updateTextAreaInput(session, "dm_taxa", value = paste(taxa, collapse = "\n"))
  })

  observeEvent(input$dm_upload, {
    req(input$dm_upload)
    df <- tryCatch(read.csv(input$dm_upload$datapath, header = FALSE, stringsAsFactors = FALSE),
                   error = function(e) NULL)
    if (!is.null(df)) {
      taxa_str <- paste(as.character(df[[1]]), collapse = "\n")
      updateTextAreaInput(session, "dm_taxa", value = taxa_str)
    }
  })

  dm_result <- eventReactive(input$dm_run, {
    taxa <- parse_taxa_input(input$dm_taxa)
    req(length(taxa) >= 2)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    withCallingHandlers(
      distance_matrix(taxa, verbose = FALSE, progress = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
  })


  # ── Distance Matrix UI + Dendrogram ─────────────────────────────────────────

  output$dm_result_ui <- renderUI({
    dm <- dm_result()
    if (is.null(dm)) return(div(class="p-3 text-muted fst-italic", "Results will appear here."))
    n  <- length(attr(dm, "Labels"))
    ht <- max(350, n * 58)
    tagList(
      card(
        card_header("Distance Matrix"),
        div(class = "p-2", DTOutput("dm_table"))
      ),
      card(class = "mt-3",
           card_header("Dendrogram"),
           div(class = "p-2",
               plotOutput("dm_dendro", height = paste0(ht, "px"))
           )
      )
    )
  })

  output$dm_dendro <- renderPlot({
    dm <- dm_result()
    req(dm)

    hc <- hclust(dm, method = "average")
    n  <- length(hc$labels)

    # ── 1. Leaf y-positions: follow hc$order (the correct display order) ──────
    leaf_y        <- numeric(n)           # y for each original label index
    leaf_y[hc$order] <- seq_len(n)       # ordered 1..n top to bottom

    # ── 2. Internal node x (= merge height) and y (= mean of children) ────────
    node_x <- hc$height                  # x = distance at merge
    node_y <- numeric(n - 1)             # y = average position of children

    get_node_y <- function(k) {
      if (k < 0) return(leaf_y[-k])
      node_y[k]
    }

    for (i in seq_len(n - 1)) {
      node_y[i] <- mean(c(get_node_y(hc$merge[i, 1]),
                          get_node_y(hc$merge[i, 2])))
    }

    # ── 3. Build segment data ─────────────────────────────────────────────────
    #   (a) vertical connector between the two children's y positions, at height px
    #   (b) horizontal drop from left  child y to its own merge height (or 0 if leaf)
    #   (c) horizontal drop from right child y to its own merge height (or 0 if leaf)
    seg_list <- do.call(rbind, lapply(seq_len(n - 1), function(i) {
      px   <- node_x[i]          # this node's height (x in flipped plot)
      left <- hc$merge[i, 1]
      right<- hc$merge[i, 2]
      cy_l <- get_node_y(left)   # y position of left  child
      cy_r <- get_node_y(right)  # y position of right child
      ch_l <- if (left  < 0) 0 else node_x[left]   # height of left  child
      ch_r <- if (right < 0) 0 else node_x[right]  # height of right child
      data.frame(
        x    = c(cy_l, cy_l, cy_r),   # (a) vert, (b) left drop, (c) right drop
        xend = c(cy_r, cy_l, cy_r),
        y    = c(px,   ch_l, ch_r),
        yend = c(px,   px,   px),
        stringsAsFactors = FALSE
      )
    }))

    # ── 4. Tip label data ──────────────────────────────────────────────────────
    tip_df <- data.frame(
      x     = leaf_y,
      label = hc$labels,
      stringsAsFactors = FALSE
    )

    # ── 5. MRCA per internal node ──────────────────────────────────────────────
    get_leaves <- function(k) {
      if (k < 0) return(hc$labels[-k])
      c(get_leaves(hc$merge[k, 1]), get_leaves(hc$merge[k, 2]))
    }
    node_mrca <- sapply(seq_len(n - 1), function(i) {
      lvs <- get_leaves(i)
      tryCatch({
        r <- mrca(lvs[1], lvs[length(lvs)])
        if (is.null(r)) NA_character_ else r
      }, error = function(e) NA_character_)
    })

    node_df <- data.frame(
      x    = node_y,
      y    = node_x,
      mrca = node_mrca,
      stringsAsFactors = FALSE
    )
    node_df <- node_df[!is.na(node_df$mrca), ]

    # ── 6. Plot ────────────────────────────────────────────────────────────────
    max_dist  <- max(hc$height)
    max_chars <- max(nchar(hc$labels))
    label_gap   <- max_dist * 0.000001
    label_width <- max_chars * max_dist * 0.018

    ggplot() +
      geom_segment(data = seg_list,
                   aes(x = x, xend = xend, y = y, yend = yend),
                   colour = "#2D5016", linewidth = 1.1) +
      geom_text(data = tip_df,
                aes(x = x, label = label),
                y      = -label_gap,
                hjust  = 0, vjust = 0.5, size = 5.5,
                family = "serif", fontface = "italic", colour = "#1C1812") +
      geom_text(data = node_df,
                aes(x = x, y = y, label = mrca),
                hjust = 0.5, vjust = -0.65, size = 6,
                family = "serif", fontface = "italic", colour = "#8B6914") +
      geom_point(data = node_df,
                 aes(x = x, y = y),
                 colour = "#8B6914", fill = "#FAF7F0",
                 shape = 21, size = 3.5, stroke = 1.4) +
      coord_flip(clip = "off") +
      scale_y_reverse(
        limits = c(max_dist * 1.04, -(label_gap + label_width)),
        expand = expansion(0)
      ) +
      scale_x_continuous(expand = expansion(mult = 0.05)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_family = "serif") +
      theme(
        axis.text       = element_blank(),
        axis.ticks      = element_blank(),
        panel.grid      = element_blank(),
        plot.background = element_rect(fill = "#FAF7F0", colour = NA),
        plot.margin     = margin(10, 10, 10, 10)
      )
  }, bg = "#FAF7F0")

  output$dm_table <- renderDT({
    dm <- dm_result()
    req(dm)
    mat     <- as.matrix(dm)
    mat_fmt <- round(mat, 5)
    datatable(mat_fmt,
              options = list(pageLength = 20, dom = "tip", scrollX = TRUE),
              class = "compact hover"
    ) |>
      formatStyle(columns = colnames(mat_fmt),
                  background = styleInterval(
                    c(0.05, 0.15, 0.3),
                    c("#d4e8c2", "#eaf3d9", "#fdf6e3", "#f8e8d8")
                  )
      )
  })

  # ── Closest Relative ────────────────────────────────────────────────────────

  observeEvent(input$cr_example, {
    picked <- sample(shuffle_pool, sample(5:6, 1))
    updateTextInput(session, "cr_query", value = picked[1])
    updateTextAreaInput(session, "cr_candidates", value = paste(picked[-1], collapse = "\n"))
  })

  cr_result <- eventReactive(input$cr_run, {
    req(nchar(trimws(input$cr_query)) > 0)
    candidates <- parse_taxa_input(input$cr_candidates)
    req(length(candidates) >= 1)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))

    query <- trimws(input$cr_query)
    dist_df <- withCallingHandlers(
      closest_relative(query, candidates, verbose = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (is.null(dist_df)) return(NULL)

    # fetch lineage depths for query and all candidates
    query_depth <- lineage_depth(query)
    cand_depths <- sapply(candidates, function(t) {
      d <- lineage_depth(t)
      if (is.null(d)) NA_integer_ else as.integer(d)
    })
    dist_df$depth <- cand_depths[dist_df$taxon]

    list(df = dist_df, query = query, query_depth = query_depth)
  })

  # shallow lineage threshold — taxa with fewer nodes than this get flagged
  SHALLOW_THRESHOLD <- 25L

  output$cr_result_ui <- renderUI({
    res <- cr_result()
    if (is.null(res)) return(div(class="p-3 text-muted fst-italic", "Results will appear here."))

    df          <- res$df
    query       <- res$query
    query_depth <- res$query_depth
    closest     <- df[1, ]

    max_dist <- max(df$distance, na.rm = TRUE)
    if (is.na(max_dist) || max_dist == 0) max_dist <- 1

    any_shallow <- any(!is.na(df$depth) & df$depth < SHALLOW_THRESHOLD)

    rows <- lapply(seq_len(nrow(df)), function(i) {
      row      <- df[i, ]
      bar_pct  <- if (is.na(row$distance)) 0L else round(100 * row$distance / max_dist)
      bar_col  <- if (i == 1L) "#2D5016" else "#8B6914"
      is_shallow <- !is.na(row$depth) && row$depth < SHALLOW_THRESHOLD
      depth_lbl  <- if (is.na(row$depth)) "?" else as.character(row$depth)
      warn_icon  <- if (is_shallow) " ⚠" else ""

      tags$tr(
        style = if (is_shallow) "background:#fffbf0;" else "",
        tags$td(
          style = "font-style:italic; padding:0.4rem 0.6rem;",
          row$taxon, tags$span(style="color:#8B6914; font-style:normal;", warn_icon)
        ),
        tags$td(
          style = "padding:0.4rem 0.6rem; font-family:'JetBrains Mono',monospace; font-size:0.82rem;",
          if (is.na(row$distance)) "NA" else round(row$distance, 6)
        ),
        tags$td(
          style = "padding:0.4rem 0.6rem; font-family:'JetBrains Mono',monospace; font-size:0.78rem; color:#7A6B55;",
          depth_lbl
        ),
        tags$td(style = "padding:0.4rem 0.6rem; width:35%;",
                div(style = sprintf(
                  "height:10px; width:%d%%; background:%s; border-radius:2px;",
                  bar_pct, bar_col), "")
        )
      )
    })

    tagList(
      div(class = "result-box mb-3",
          div(class = "result-label", "Closest relative to"),
          div(style = "font-family:'Playfair Display',serif; font-size:1.3rem; font-style:italic;", query),
          div(class = "result-meta", if (!is.null(query_depth)) sprintf("lineage depth: %d", query_depth) else ""),
          div(class = "result-label mt-2", "is"),
          div(class = "result-mrca", closest$taxon),
          div(class = "result-meta",
              sprintf("distance = %s", round(closest$distance, 6)),
              if (!is.na(closest$depth)) sprintf(" | lineage depth: %d", closest$depth) else ""
          )
      ),
      if (any_shallow)
        div(
          style = "background:#fffbf0; border:1px solid #e0c97a; border-left:4px solid #8B6914; border-radius:3px; padding:0.7rem 1rem; margin-bottom:0.8rem; font-size:0.83rem; color:#5B4A2E;",
          tags$b("⚠ Data quality notice: "),
          "One or more taxa have a shallow lineage depth (< 25 nodes) in The Taxonomicon, ",
          "meaning they are poorly resolved in the database. Their distances may be ",
          "artificially large and rankings unreliable. Check the Lineage Explorer tab for details."
        ),
      card(
        card_header("All candidates ranked"),
        div(class = "p-0",
            tags$table(
              class = "table table-sm mb-0",
              style = "font-size:0.85rem;",
              tags$thead(tags$tr(
                tags$th(style="padding:0.4rem 0.6rem;", "Taxon"),
                tags$th(style="padding:0.4rem 0.6rem;", "Distance"),
                tags$th(style="padding:0.4rem 0.6rem;", "Depth"),
                tags$th(style="padding:0.4rem 0.6rem;", "")
              )),
              tags$tbody(rows)
            )
        )
      )
    )
  })

  # ── Lineage Explorer ────────────────────────────────────────────────────────

  le_lineage <- eventReactive(input$le_run, {
    req(nchar(trimws(input$le_taxon)) > 0)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    withCallingHandlers(
      get_lineage(trimws(input$le_taxon), verbose = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
  })

  le_member <- eventReactive(input$le_member_run, {
    req(le_lineage(), nchar(trimws(input$le_clade_check)) > 0)
    is_member(trimws(input$le_taxon), trimws(input$le_clade_check))
  })

  output$le_result_ui <- renderUI({
    lin <- le_lineage()
    if (is.null(lin)) return(div(class="p-3 text-muted fst-italic", "Results will appear here."))

    nodes <- lapply(seq_along(lin), function(i) {
      tagList(
        if (i > 1) span(class = "lineage-arrow", "›") else NULL,
        span(class = "lineage-node", lin[i])
      )
    })

    mem <- tryCatch(le_member(), error = function(e) NULL)
    mem_ui <- if (!is.null(mem) && nchar(trimws(input$le_clade_check)) > 0) {
      col <- if (isTRUE(mem)) "#d4e8c2" else "#f5d0c8"
      txt <- if (isTRUE(mem)) sprintf("%s IS a member of %s", trimws(input$le_taxon), trimws(input$le_clade_check))
      else sprintf("%s is NOT a member of %s", trimws(input$le_taxon), trimws(input$le_clade_check))
      div(style = sprintf("background:%s; border-radius:3px; padding:0.6rem 1rem; font-size:0.88rem; margin-top:0.8rem; font-style:italic;", col), txt)
    } else NULL

    tagList(
      card(
        card_header(sprintf("Lineage of %s (%d nodes)", trimws(input$le_taxon), length(lin))),
        div(class = "p-3",
            div(style = "line-height:2.4;", nodes),
            mem_ui
        )
      )
    )
  })

  # ── Coverage Check ──────────────────────────────────────────────────────────

  observeEvent(input$cc_example, {
    taxa <- c(sample(shuffle_pool, 4), "Fakeosaurus", "Imaginarius")
    updateTextAreaInput(session, "cc_taxa", value = paste(sample(taxa), collapse = "\n"))
  })

  observeEvent(input$cc_upload, {
    req(input$cc_upload)
    df <- tryCatch(read.csv(input$cc_upload$datapath, header = FALSE, stringsAsFactors = FALSE),
                   error = function(e) NULL)
    if (!is.null(df)) {
      taxa_str <- paste(as.character(df[[1]]), collapse = "\n")
      updateTextAreaInput(session, "cc_taxa", value = taxa_str)
    }
  })

  cc_result <- eventReactive(input$cc_run, {
    taxa <- parse_taxa_input(input$cc_taxa)
    req(length(taxa) >= 1)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    withCallingHandlers(
      check_coverage(taxa, verbose = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
  })

  output$cc_result_ui <- renderUI({
    res <- cc_result()
    if (is.null(res)) return(div(class="p-3 text-muted fst-italic", "Results will appear here."))

    n_found    <- sum(res, na.rm = TRUE)
    n_notfound <- sum(!res, na.rm = TRUE)

    pills <- lapply(names(res), function(nm) {
      cls <- if (isTRUE(res[nm])) "cov-pill cov-found" else "cov-pill cov-notfound"
      span(class = cls, nm)
    })

    tagList(
      div(class = "result-box mb-3",
          fluidRow(
            column(6,
                   div(class = "result-label", "Found in Taxonomicon"),
                   div(class = "result-distance", style = "color:#2D5016;", n_found)
            ),
            column(6,
                   div(class = "result-label", "Not found"),
                   div(class = "result-distance", style = "color:#8B3A1A;", n_notfound)
            )
          )
      ),
      card(
        card_header("Coverage by taxon"),
        div(class = "p-3", pills)
      )
    )
  })

  # ── Filter by Clade ─────────────────────────────────────────────────────────

  observeEvent(input$fc_example, {
    clades <- list(
      list(clade = "Theropoda",  taxa = c("Tyrannosaurus","Velociraptor","Spinosaurus","Carnotaurus","Triceratops","Brachiosaurus")),
      list(clade = "Dinosauria", taxa = c("Tyrannosaurus","Triceratops","Brachiosaurus","Velociraptor","Homo","Quercus")),
      list(clade = "Amniota",    taxa = c("Homo","Panthera","Canis","Tyrannosaurus","Drosophila","Quercus"))
    )
    pick <- clades[[sample(length(clades), 1)]]
    updateTextAreaInput(session, "fc_taxa",  value = paste(pick$taxa,  collapse = "\n"))
    updateTextInput(session,    "fc_clade", value = pick$clade)
  })

  fc_result <- eventReactive(input$fc_run, {
    taxa  <- parse_taxa_input(input$fc_taxa)
    clade <- trimws(input$fc_clade)
    req(length(taxa) >= 1, nchar(clade) > 0)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    list(
      all   = taxa,
      kept  = withCallingHandlers(
        filter_clade(taxa, clade, verbose = FALSE),
        error = function(e) { showNotification(conditionMessage(e), type = "error"); character(0) }
      ),
      clade = clade
    )
  })

  output$fc_result_ui <- renderUI({
    res <- fc_result()
    if (is.null(res)) return(div(class="p-3 text-muted fst-italic", "Results will appear here."))

    excluded <- setdiff(res$all, res$kept)

    make_tags <- function(taxa, cls) {
      lapply(taxa, function(t) span(class = cls, t))
    }

    tagList(
      div(class = "result-box mb-3",
          div(class = "result-label", "Clade filter"),
          div(style = "font-family:'Playfair Display',serif; font-size:1.2rem; font-style:italic;",
              res$clade),
          div(class = "result-meta mt-1",
              sprintf("%d of %d taxa retained", length(res$kept), length(res$all)))
      ),
      card(
        card_header("Results"),
        div(class = "p-3",
            div(class = "result-label mb-1", sprintf("In %s (%d)", res$clade, length(res$kept))),
            if (length(res$kept) > 0) div(make_tags(res$kept, "taxon-tag")) else div(class="text-muted fst-italic small", "none"),
            hr(class = "section-divider"),
            div(class = "result-label mb-1", sprintf("Not in %s (%d)", res$clade, length(excluded))),
            if (length(excluded) > 0) div(make_tags(excluded, "taxon-tag")) else div(class="text-muted fst-italic small", "none")
        )
      )
    )
  })
}

shinyApp(ui, server)
