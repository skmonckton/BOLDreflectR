# Helpers for sequence alignment

# sequences for selected marker from selected records (using FASTA header labels)
# returns NULL if there are none
alignment_input <- function(data, marker, header_fields = c("processid", "marker_code")) {
  d <- unique(data[as.character(marker_code) == marker & !empty(nuc)], by = "processid")
  if (nrow(d) == 0) return(NULL)
  header_fields <- intersect(header_fields, names(d))
  if (length(header_fields) == 0) header_fields <- "processid"
  labels <- fasta_header(d[, do.call(paste, c(lapply(.SD, as.character), sep = "|")), .SDcols = header_fields])
  # gaps and non-IUPAC nucleotides are removed before aligning; raw keeps the gaps (for method = "none")
  raw <- gsub("[^ACGTRYSWKMBDHVN-]", "", gsub(".", "-", toupper(as.character(d$nuc)), fixed = TRUE))
  seqs <- gsub("-", "", raw, fixed = TRUE)
  keep <- nchar(seqs) > 0
  list(ids = as.character(d$processid[keep]),
       labels = make.unique(labels[keep], sep = "_"),
       seqs = seqs[keep],
       raw = raw[keep])
}

# multiple sequence alignment with DECIPHER; returns aligned sequences as a character vector
# method = "seqs" (nucleotides / AlignSeqs) or "translation" (amino acids / AlignTranslation), plus optional "_refined" (for iterative refinement)
# or pass "none" to use unaligned sequences (padded to equal length with terminal gaps)
align_sequences <- function(seqs, labels, method = "seqs", gcode = 1) {
  if (method == "none") {
    width <- max(nchar(seqs))
    return(stats::setNames(paste0(seqs, strrep("-", width - nchar(seqs))), labels))
  }
  dna <- Biostrings::DNAStringSet(stats::setNames(seqs, labels))
  fast <- if (grepl("_refined$", method)) list() else list(iterations = 0, refinements = 0)
  aligned <- if (startsWith(method, "translation")) {
    do.call(DECIPHER::AlignTranslation, c(list(dna, geneticCode = Biostrings::getGeneticCode(as.character(gcode)),
                                               processors = NULL, verbose = FALSE), fast))
  } else {
    do.call(DECIPHER::AlignSeqs, c(list(dna, processors = NULL, verbose = FALSE), fast))
  }
  stats::setNames(as.character(aligned), names(aligned))
}

# description of an alignment method for the PDF cover page
alignment_method_text <- function(method, gcode = NULL) {
  if (method == "none") return("None (unaligned sequences, padded to equal length with terminal gaps)")
  paste0("DECIPHER ", if (startsWith(method, "translation")) "AlignTranslation " else "AlignSeqs ", packageVersion("DECIPHER"),
         if (grepl("_refined$", method)) " (with iterative refinement)" else " (no iterative refinement)",
         if (startsWith(method, "translation")) paste0("; translation table ", names(genetic_codes)[genetic_codes == gcode]) else "")
}
