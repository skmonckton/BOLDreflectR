# Helpers for neighbour-joining trees

# replace tip labels in Newick text: tips must be labelled <prefix><index>, and are replaced by new[index]
# (ape::write.tree and read.tree change some characters in labels)
replace_newick_tips <- function(newick, prefix, new) {
  pattern <- paste0(prefix, "[0-9]+(?=:)")
  hits <- gregexpr(pattern, newick, perl = TRUE)
  regmatches(newick, hits) <- list(new[as.integer(substring(regmatches(newick, hits)[[1]], nchar(prefix) + 1))])
  newick
}

# children of each node as a list indexed by node number, in postorder (children before parents)
node_children <- function(tree) {
  post <- ape::reorder.phylo(tree, "postorder")$edge
  list(order = unique(post[, 1]),
       kids = split(post[, 2], factor(post[, 1], levels = seq_len(ape::Ntip(tree) + tree$Nnode))))
}

# aligned sequences as an ape DNAbin matrix
alignment_dnabin <- function(seqs, labels) {
  m <- do.call(rbind, strsplit(tolower(seqs), "", fixed = TRUE))
  rownames(m) <- labels
  ape::as.DNAbin(m)
}

# neighbour-joining tree from aligned sequences
# collapse_identical builds the tree from unique sequences only, then adds identical copies back as zero-length tips
#   NJ time grows with the cube of the number of sequences, so this is much faster (5782 COI barcodes, 3113 unique: ~1 min instead of ~9 min)
#   but it is an approximation, as NJ joining criterion depends on every sequence's distances to all others, which means copies carry weight
#   (BIN-level structure agreed for 39 of 40 BINs in a test)
# distances use pairwise deletion of gaps/ambiguities
# pairs with no overlap or saturated under the chosen model are handled according to `incomparable`:
#   "drop" removes the sequences with the most such pairs (and their copies) until none are left
#   "njs" keeps them and uses ape::njs, which allows missing distances (~90 s instead of <1 s for 1000 barcodes)
#   if too many are missing for ape::njs, they're dropped instead
# tree attributes:
#  unique: # unique sequences in the tree
#  dropped: # sequences dropped
#  njs: whether ape::njs was used,
#  njs_failed: whether ape::njs failed to build a tree, and so sequences were dropped
build_nj_tree <- function(seqs, labels, model = "K80", midpoint = TRUE, collapse_identical = TRUE, incomparable = "drop") {
  unique_seqs <- if (collapse_identical) unique(seqs) else seqs
  group <- if (collapse_identical) match(seqs, unique_seqs) else seq_along(seqs)
  keep <- seq_along(unique_seqs)
  d <- if (length(keep) > 1) as.matrix(ape::dist.dna(alignment_dnabin(unique_seqs, paste0("U", keep)), model = model, pairwise.deletion = TRUE))
  missing <- if (!is.null(d)) !is.finite(d) else matrix(FALSE, 1, 1)
  # sequences to keep so that no pair is missing - the one with the most missing pairs is dropped until none are left
  comparable <- function() {
    k <- seq_along(unique_seqs)
    n_missing <- rowSums(missing)
    while (any(n_missing[k] > 0)) {
      worst <- k[which.max(n_missing[k])]
      k <- setdiff(k, worst)
      n_missing <- n_missing - missing[, worst]
    }
    k
  }
  # neighbour-joining on the kept sequences (ape::njs if some distances are missing)
  join <- function(keep) {
    if (length(keep) == 1) return(ape::read.tree(text = sprintf("(U%d:0);", keep)))
    if (length(keep) == 2) {
      half <- if (is.finite(d[keep[1], keep[2]])) d[keep[1], keep[2]] / 2 else 0
      return(ape::read.tree(text = sprintf("(U%d:%f,U%d:%f);", keep[1], half, keep[2], half)))
    }
    dk <- stats::as.dist(d[keep, keep])
    if (any(!is.finite(dk))) {
      dk[!is.finite(dk)] <- NA
      tree <- ape::njs(dk)
    } else {
      tree <- ape::nj(dk)
    }
    # NJ can produce small negative branch lengths, which have no meaning for display; these are rounded up to zero
    tree$edge.length <- pmax(tree$edge.length, 0)
    if (midpoint) tree <- phangorn::midpoint(tree)
    tree
  }
  use_njs <- incomparable == "njs" && any(missing)
  njs_failed <- FALSE
  tree <- NULL
  if (use_njs) {
    tree <- tryCatch(join(keep), error = function(e) NULL)
    njs_failed <- is.null(tree)
  }
  if (is.null(tree)) {
    keep <- comparable()
    tree <- join(keep)
  }
  # add the copies back, if applicable: each unique tip becomes a group of sequences joined by zero-length branches
  # this is done in Newick text with placeholder labels T<index>, replaced by the real labels afterwards
  groups <- vapply(split(seq_along(seqs), group), function(m) {
    if (length(m) == 1) paste0("T", m) else paste0("(", paste0("T", m, ":0", collapse = ","), ")")
  }, character(1))
  tree <- ape::read.tree(text = replace_newick_tips(ape::write.tree(tree), "U", groups))
  tree$tip.label <- labels[as.integer(sub("^T", "", tree$tip.label))]
  tree <- ape::ladderize(tree)
  attr(tree, "unique") <- length(keep)
  attr(tree, "dropped") <- sum(!group %in% keep)
  attr(tree, "njs") <- use_njs && !njs_failed
  attr(tree, "njs_failed") <- njs_failed
  tree
}

# colours for trees were picked greedily in CIELAB so that each is as different as possible from earlier ones
# Glasbey-style palette selection, lightness 30-72 for readability on white, away from black and the grey used for missing values
# values get colours in order of first appearance from the top of the tree and colours are recycled after 64
tree_palette <- c("#1E78B4", "#0000FF", "#FF0000", "#00C300", "#FF1EB4", "#D2A500", "#96002D", "#5A2DA5", "#0F5A00",
                  "#00C396", "#E196E1", "#E100FF", "#FF875A", "#694B00", "#A5B469", "#871E69", "#5A87FF", "#FF7896",
                  "#1EB4C3", "#87B40F", "#FF005A", "#0F695A", "#965A5A", "#D269F0", "#784BFF", "#B42D00", "#785A87",
                  "#FF8700", "#2DA53C", "#C39669", "#004BA5", "#697800", "#B46900", "#C30069", "#4B5A2D", "#A5A5E1",
                  "#A50096", "#965AB4", "#78B496", "#9600C3", "#873C1E", "#C387A5", "#E14B4B", "#00B4F0", "#005AF0",
                  "#E15AB4", "#E11ED2", "#00874B", "#9600FF", "#A5872D", "#5AA5FF", "#F09687", "#B4B43C", "#FF5A2D",
                  "#B496FF", "#782D4B", "#005A78", "#C35A87", "#F0002D", "#69B469", "#87875A", "#C35A5A", "#8769F0",
                  "#0F1EE1")
tree_missing_colour <- "#9E9E9E"

# position of each tip from the top of the drawn tree (as in the viewer and PDF) in the order of tree$tip.label
tip_numbers <- function(tree) {
  n_tip <- ape::Ntip(tree)
  num <- integer(n_tip)
  num[tree$edge[tree$edge[, 2] <= n_tip, 2]] <- seq_len(n_tip)
  num
}

# values of a record field for each tip; meta has one row per tip in the order of tree$tip.label
# "tip_number" is the tip's position from the top in square brackets; missing values are ""
tip_field_values <- function(meta, field, tip_number) {
  if (field == "tip_number") return(sprintf("[%d]", tip_number))
  if (!field %in% names(meta)) return(rep("", nrow(meta)))
  x <- as.character(meta[[field]])
  x[is.na(x)] <- ""
  x
}

# pipe-delimited tip labels from the chosen fields
tree_tip_labels <- function(meta, fields, tip_number) {
  if (length(fields) == 0) fields <- "processid"
  do.call(paste, c(lapply(fields, function(f) tip_field_values(meta, f, tip_number)), sep = "|"))
}

# tooltip details (HTML) for each tip: process ID, sample ID, BIN, identification, sequence length, then other label fields
tree_tip_info <- function(meta, fields, tip_number) {
  fixed <- c("processid", "sampleid", "bin_uri", "identification", "nuc_basecount")
  lines <- lapply(c(fixed, setdiff(fields, c(fixed, "tip_number"))), function(f) {
    x <- tip_field_values(meta, f, tip_number)
    if (f == "nuc_basecount") x[x != ""] <- paste(x[x != ""], "bp")
    name <- if (is.na(tree_label_names[f])) f else tree_label_names[[f]]
    ifelse(x == "", "", paste0(htmltools::htmlEscape(name), ": ", htmltools::htmlEscape(x)))
  })
  apply(do.call(cbind, lines), 1, function(d) paste(d[d != ""], collapse = "<br>"))
}

# colours for tips and branches given one value per tip (in tree order)
# a branch takes the colour of its "clade" if all tips have the same value
# tips without a value are grey
# returns colours for all nodes (tips first, as in tree_layout(), with "" = default) and the colour of each value
tree_colours <- function(tree, values, tip_number) {
  values[is.na(values)] <- ""
  lev <- unique(values[order(tip_number)])
  lev <- lev[lev != ""]
  # value index of each node: 0 = no value; NA = the tips below have different values
  tip_key <- match(values, lev, nomatch = 0L)
  key <- c(tip_key, rep(NA_integer_, tree$Nnode))
  nc <- node_children(tree)
  for (p in nc$order) {
    k <- unique(key[nc$kids[[p]]])
    key[p] <- if (length(k) == 1) k else NA_integer_
  }
  pal <- rep_len(tree_palette, length(lev))
  node <- ifelse(is.na(key), "", ifelse(key == 0, tree_missing_colour, pal[pmax(key, 1L)]))
  list(node = node, values = stats::setNames(pal, lev))
}

# Newick text with provided tip labels (in tree order) quoted where needed (e.g. spaces, brackets)
# apostrophes are replaced with typographic apostrophes because ape::read.tree can't read doubled ones
tree_newick <- function(tree, labels) {
  labels <- gsub("'", "’", labels, fixed = TRUE)
  quoted <- ifelse(grepl("^[A-Za-z0-9_.|-]+$", labels), labels, paste0("'", labels, "'"))
  tree$tip.label <- paste0("T", seq_along(labels))
  replace_newick_tips(ape::write.tree(tree), "T", quoted)
}

# node coordinates for drawing a rectangular tree (tips are numbered from the top, as in the viewer and PDF)
# info = optional character vector of tooltip details for each tip in tree order
# labels = optional tip labels to show instead of tree$tip.label
# col = optional colours for all nodes, e.g. from tree_colours()
tree_layout <- function(tree, info = NULL, labels = NULL, col = NULL) {
  n_tip <- ape::Ntip(tree)
  n_all <- n_tip + tree$Nnode
  x <- ape::node.depth.edgelength(tree)
  y <- c(tip_numbers(tree), numeric(tree$Nnode))
  # internal nodes sit midway between their outermost children, tip counts are accumulated on the way up
  n_tips_below <- c(rep(1L, n_tip), integer(tree$Nnode))
  nc <- node_children(tree)
  for (p in nc$order) {
    children <- nc$kids[[p]]
    y[p] <- mean(range(y[children]))
    n_tips_below[p] <- sum(n_tips_below[children])
  }
  parent <- integer(n_all)
  parent[tree$edge[, 2]] <- tree$edge[, 1]
  branch <- numeric(n_all)
  branch[tree$edge[, 2]] <- tree$edge.length
  list(
    parent = parent - 1L,            # 0-based for JavaScript; the root has parent -1
    x = x, y = y, bl = branch,
    tip = seq_len(n_all) <= n_tip,
    ntips = n_tips_below,
    label = c(if (is.null(labels)) tree$tip.label else labels, rep("", tree$Nnode)),
    info = c(if (is.null(info)) rep("", n_tip) else info, rep("", tree$Nnode)),
    col = if (is.null(col)) rep("", n_all) else col,
    max_x = max(x), n_tip = n_tip
  )
}

# R's own pdf device always draws "-" as a minus sign (see ?pdf), replaced here with hyphen in the current device's encoding
# cairo_pdf is used where available as it draws "-" correctly and keeps the text searchable
pdf_text <- function(x) if (names(grDevices::dev.cur()) == "pdf") gsub("-", "­", x, fixed = TRUE) else x

# multi-page tree PDF (A4)
# labels are duplicated by `overlap` rows across page boundaries, so that cut labels appear in full on the next page
# only the branches and labels visible on a given page are drawn (rather than plotting in full and clipping)
write_tree_pdf <- function(tree, file, tips_per_page = 90, title = NULL, overlap = 1, cover = NULL, progress = NULL,
                           labels = NULL, col = NULL) {
  n_tip <- ape::Ntip(tree)
  pages <- max(1, ceiling(n_tip / tips_per_page))
  lay <- tree_layout(tree, labels = labels, col = col)
  colour <- ifelse(lay$col == "", "black", lay$col)
  node <- seq_along(lay$x)
  child <- node[lay$parent >= 0]
  parent <- lay$parent[child] + 1L
  # vertical connector for each internal node spans its children
  y_lo <- tapply(lay$y[child], parent, min)
  y_hi <- tapply(lay$y[child], parent, max)
  conn <- as.integer(names(y_lo))
  tips <- node[lay$tip]
  cex <- 0.6  # set from the row height on the first page
  scale_len <- signif(lay$max_x / 5, 1)

  if (isTRUE(capabilities("cairo"))) {
    grDevices::cairo_pdf(file, width = 8.27, height = 11.69, onefile = TRUE)
  } else {
    grDevices::pdf(file, width = 8.27, height = 11.69)
  }
  on.exit(grDevices::dev.off())
  if (!is.null(cover)) draw_cover_page(cover, title = "BOLDreflectR NJ Tree")
  op <- graphics::par(mar = c(1.5, 1, 2.5, 1))
  on.exit(graphics::par(op), add = TRUE, after = FALSE)

  for (p in seq_len(pages)) {
    top <- (p - 1) * tips_per_page + 0.5 - overlap
    bottom <- p * tips_per_page + 0.5 + overlap
    graphics::plot.new()
    if (p == 1) {
      # label size: 10 pt or smaller to fit the rows (down to 6 pt at about 100 tips per page)
      cex <- max(6, min(10, 0.78 * graphics::par("pin")[2] / (bottom - top) * 72)) / 12
      # horizontal space: tree depth plus room for the longest label (in inches, converted to tree units)
      label_in <- max(graphics::strwidth(pdf_text(lay$label[tips]), units = "inches", cex = cex)) + 0.08
      width_in <- graphics::par("pin")[1]
      x_max <- lay$max_x * width_in / max(width_in - label_in, width_in * 0.2)
      if (x_max <= 0) x_max <- 1
      offset <- 0.04 / width_in * x_max
    }
    # reversed y limits put tip 1 at the top of the page; anything outside the plot region is clipped
    graphics::plot.window(xlim = c(0, x_max), ylim = c(bottom, top), xaxs = "i")
    in_page <- function(lo, hi) hi >= top - 1 & lo <= bottom + 1
    h <- child[in_page(lay$y[child], lay$y[child])]
    graphics::segments(lay$x[lay$parent[h] + 1L], lay$y[h], lay$x[h], lay$y[h], lwd = 0.8, col = colour[h])
    v <- which(in_page(y_lo, y_hi))
    graphics::segments(lay$x[conn[v]], pmax(y_lo[v], top - 1), lay$x[conn[v]], pmin(y_hi[v], bottom + 1), lwd = 0.8,
                       col = colour[conn[v]])
    t <- tips[lay$y[tips] >= top & lay$y[tips] <= bottom]
    graphics::text(lay$x[t] + offset, lay$y[t], pdf_text(lay$label[t]), adj = c(0, 0.5), cex = cex, col = colour[t])
    graphics::mtext(pdf_text(sprintf("%sPage %d of %d", if (!is.null(title)) paste0(title, "  -  ") else "", p, pages)),
                    side = 3, adj = 0, cex = 0.7, line = 1)
    # scale bar at the top right, between the page header and the first row, on a white background in case of overlap
    if (scale_len > 0) {
      y <- mean(c(top + 0.5, graphics::par("usr")[4] + graphics::yinch(0.2)))
      x1 <- x_max * 0.98
      x0 <- x1 - scale_len
      label_w <- graphics::strwidth(format(scale_len), cex = cex)
      graphics::rect(x0 - label_w - offset * 2, y - 0.5, x1 + offset, y + 0.5, col = "white", border = NA, xpd = NA)
      graphics::segments(c(x0, x0, x1), c(y, y - 0.2, y - 0.2), c(x1, x0, x1), c(y, y + 0.2, y + 0.2), lwd = 0.8, xpd = NA)
      graphics::text(x0 - offset, y, format(scale_len), adj = c(1, 0.5), cex = cex, xpd = NA)
    }
    if (is.function(progress)) progress(p, pages)
  }
  invisible(file)
}

# text wrapping for cover page
wrap_to_width <- function(text, width, cex = 1, font = 1) {
  fits <- function(s) graphics::strwidth(pdf_text(s), cex = cex, font = font) <= width
  lines <- character(0)
  line <- ""
  for (word in strsplit(text, "\\s+")[[1]]) {
    if (word == "") next
    candidate <- if (line == "") word else paste(line, word)
    if (fits(candidate)) {
      line <- candidate
      next
    }
    if (line != "") lines <- c(lines, line)
    # a word that doesn't fit on a line by itself is broken into pieces that do
    while (!fits(word)) {
      n <- nchar(word)
      while (n > 1 && !fits(substr(word, 1, n))) n <- n - 1
      lines <- c(lines, substr(word, 1, n))
      word <- substr(word, n + 1, nchar(word))
    }
    line <- word
  }
  c(lines, line)
}

# cover page with title and analysis details
draw_cover_page <- function(details, title) {
  op <- graphics::par(mar = c(0, 0, 0, 0))
  on.exit(graphics::par(op))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  left <- 0.08; right <- 0.92; values_x <- 0.36; cex <- 0.88
  graphics::text(left, 0.9, pdf_text(title), adj = c(0, 0.5), cex = 1.9, font = 2)
  graphics::segments(left, 0.875, right, 0.875, col = "grey60")
  line_h <- 0.021
  y <- 0.83
  for (i in seq_along(details)) {
    label <- wrap_to_width(names(details)[i], values_x - left - 0.02, cex = cex, font = 2)
    value <- wrap_to_width(details[[i]], right - values_x, cex = cex)
    graphics::text(left, y - (seq_along(label) - 1) * line_h, pdf_text(label), adj = c(0, 1), cex = cex, font = 2)
    graphics::text(values_x, y - (seq_along(value) - 1) * line_h, pdf_text(value), adj = c(0, 1), cex = cex)
    y <- y - max(length(label), length(value)) * line_h - 0.012
  }
  invisible()
}
