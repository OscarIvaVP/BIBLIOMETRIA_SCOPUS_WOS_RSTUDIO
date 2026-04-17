#!/usr/bin/env Rscript
# ============================================================================
# Merge WoS and Scopus (converted to WoS format) files into a single file,
# detecting and removing duplicate records.
#
# Fixes applied for Tree of Science compatibility:
#   1. FN header = "Clarivate Analytics Web of Science"
#   2. Blank line after each ER (record separator)
#   3. Scopus CR references reformatted to WoS style:
#      WoS:    Author, Year, JOURNAL_ABBREV, Vvol, Ppage, DOI xxx
#      Scopus: Author, Year, TITLE..., JOURNAL..., Vvol, Ppage
#      -> Removes title, keeps only Author, Year, Journal, Vvol, Ppage
#
# Usage:
#   Rscript merge_wos_scopus.R
#
# Outputs:
#   - merged_wos_scopus.txt   -> Unified file without duplicates
#   - merge_report.txt        -> Summary report with duplicate details
# ============================================================================

# ============================================================
# CONFIGURATION - Edit these paths as needed
# ============================================================
WOS_FILE      <- "input/wos.txt"
SCOPUS_FILE   <- "output/scopus_formato_wos.txt"
OUTPUT_FOLDER <- "output"
OUTPUT_FILE   <- file.path(OUTPUT_FOLDER, "wos_scopus_consolidado.txt")
REPORT_FILE   <- file.path(OUTPUT_FOLDER, "reporte_consolidado.txt")

TITLE_SIMILARITY_THRESHOLD <- 0.85
PRIORITY <- "wos"   # "wos" or "scopus" - which to keep on duplicate
# ============================================================


# ---- String similarity (equivalent to SequenceMatcher.ratio) ----
# Uses the longest common subsequence approach similar to Python's difflib
string_similarity <- function(a, b) {
  if (a == "" && b == "") return(1.0)
  if (a == "" || b == "") return(0.0)
  
  # Use adist (Levenshtein) to approximate SequenceMatcher ratio
  
  # ratio = 1 - (edit_distance / max_length)
  # This is a close approximation to SequenceMatcher for dedup purposes
  d <- adist(a, b)[1, 1]
  max_len <- max(nchar(a), nchar(b))
  return(1 - d / max_len)
}


parse_wos_records <- function(filepath) {
  lines <- readLines(filepath, encoding = "UTF-8", warn = FALSE)
  # Remove BOM if present
  if (length(lines) > 0) {
    lines[1] <- sub("^\uFEFF", "", lines[1])
  }
  
  records <- list()
  current <- list()
  current_tag <- NULL
  
  for (line in lines) {
    line <- sub("\\r$", "", line)  # remove trailing CR if any
    
    if (startsWith(line, "FN ") || startsWith(line, "VR ") || startsWith(line, "EF")) {
      next
    }
    
    # Check for a WoS tag: two uppercase chars followed by a space at position 3
    if (nchar(line) >= 3 && substr(line, 3, 3) == " " &&
        grepl("^[A-Z][A-Z0-9]$", substr(line, 1, 2))) {
      
      tag   <- substr(line, 1, 2)
      value <- substr(line, 4, nchar(line))
      
      if (tag == "PT") {
        if (length(current) > 0) {
          records <- c(records, list(current))
        }
        current <- list(PT = value)
        current_tag <- "PT"
        
      } else if (tag == "ER") {
        if (length(current) > 0) {
          records <- c(records, list(current))
        }
        current <- list()
        current_tag <- NULL
        
      } else {
        if (!is.null(current[[tag]])) {
          current[[tag]] <- paste0(current[[tag]], "\n", value)
        } else {
          current[[tag]] <- value
        }
        current_tag <- tag
      }
      
    } else if (startsWith(line, "   ") && !is.null(current_tag) && length(current) > 0) {
      # Continuation line
      current[[current_tag]] <- paste0(current[[current_tag]], "\n", substr(line, 4, nchar(line)))
    }
  }
  
  if (length(current) > 0) {
    records <- c(records, list(current))
  }
  
  return(records)
}


normalize_doi <- function(doi) {
  if (is.null(doi) || doi == "") return(NA_character_)
  doi <- trimws(tolower(doi))
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi)
  return(doi)
}


normalize_title <- function(title) {
  if (is.null(title) || title == "") return("")
  t <- gsub("\\s+", " ", title)
  t <- tolower(t)
  # Remove accents
  t <- iconv(t, to = "ASCII//TRANSLIT")
  if (is.na(t)) t <- tolower(gsub("\\s+", " ", title))
  # Remove non-word/non-space chars
  t <- gsub("[^\\w\\s]", " ", t, perl = TRUE)
  t <- gsub("\\s+", " ", trimws(t))
  return(t)
}


reformat_scopus_cr <- function(cr_text) {
  if (is.null(cr_text) || cr_text == "") return(cr_text)
  
  lines <- strsplit(cr_text, "\n")[[1]]
  reformatted <- character(0)
  
  for (line in lines) {
    line <- trimws(line)
    if (line == "") next
    
    # Try to find the pattern: ..., Vdigits, ...
    v_match <- regexpr(",\\s*(V\\d+)", line, perl = TRUE)
    
    if (v_match > 0) {
      before_v <- substr(line, 1, v_match - 1)
      from_v   <- substr(line, v_match, nchar(line))
      
      parts <- trimws(strsplit(before_v, ",")[[1]])
      
      # Find year (4-digit number)
      year_idx <- NULL
      year_val <- NULL
      for (idx in seq_along(parts)) {
        if (grepl("^\\d{4}$", trimws(parts[idx]))) {
          year_val <- trimws(parts[idx])
          year_idx <- idx
          break
        }
      }
      
      if (!is.null(year_idx)) {
        # Author = everything before year
        author <- paste(parts[1:(year_idx - 1)], collapse = ", ")
        author <- trimws(author)
        
        # Journal = last segment after year (just before Vxx)
        after_year <- if (year_idx < length(parts)) parts[(year_idx + 1):length(parts)] else character(0)
        journal <- if (length(after_year) > 0) trimws(after_year[length(after_year)]) else NULL
        
        # Clean from_v
        from_v_clean <- trimws(sub("^,\\s*", "", from_v))
        
        if (author != "" && !is.null(journal) && journal != "") {
          new_cr <- paste0(author, ", ", year_val, ", ", journal, ", ", from_v_clean)
        } else if (author != "") {
          new_cr <- paste0(author, ", ", year_val, ", ", from_v_clean)
        } else {
          new_cr <- line
        }
        reformatted <- c(reformatted, new_cr)
      } else {
        reformatted <- c(reformatted, line)
      }
    } else {
      reformatted <- c(reformatted, line)
    }
  }
  
  return(paste(reformatted, collapse = "\n"))
}


is_scopus_record <- function(rec) {
  ut <- rec[["UT"]]
  if (is.null(ut)) return(FALSE)
  return(grepl("SCOPUS:", ut))
}


record_to_wos_text <- function(rec) {
  lines <- character(0)
  lines <- c(lines, paste0("PT ", ifelse(is.null(rec[["PT"]]), "J", rec[["PT"]])))
  
  field_order <- c(
    "AU", "AF", "TI", "SO", "LA", "DT", "DE", "ID", "AB",
    "C1", "C3", "RP", "EM", "RI", "OI",
    "FU", "FX", "CR", "NR", "TC", "Z9", "U1", "U2",
    "PU", "PI", "PA", "SN", "EI", "BN",
    "J9", "JI", "PD", "PY", "VL", "IS", "SI",
    "BP", "EP", "AR", "DI", "EA", "PG",
    "PM", "WC", "WE", "SC", "GA", "UT", "OA", "HC", "HP", "DA"
  )
  
  written <- c("PT")
  
  for (tag in field_order) {
    if (!is.null(rec[[tag]])) {
      value <- rec[[tag]]
      
      # Reformat CR for Scopus-origin records
      if (tag == "CR" && is_scopus_record(rec)) {
        value <- reformat_scopus_cr(value)
      }
      
      value_lines <- strsplit(value, "\n")[[1]]
      lines <- c(lines, paste0(tag, " ", value_lines[1]))
      if (length(value_lines) > 1) {
        for (vl in value_lines[2:length(value_lines)]) {
          lines <- c(lines, paste0("   ", vl))
        }
      }
      written <- c(written, tag)
    }
  }
  
  # Write any remaining tags not in field_order
  for (tag in names(rec)) {
    if (!(tag %in% written)) {
      value <- rec[[tag]]
      value_lines <- strsplit(value, "\n")[[1]]
      lines <- c(lines, paste0(tag, " ", value_lines[1]))
      if (length(value_lines) > 1) {
        for (vl in value_lines[2:length(value_lines)]) {
          lines <- c(lines, paste0("   ", vl))
        }
      }
    }
  }
  
  lines <- c(lines, "ER")
  return(paste(lines, collapse = "\n"))
}


merge_records <- function(wos_records, scopus_records, threshold = 0.85, priority = "wos") {
  # Build DOI index for WoS records
  wos_by_doi <- list()
  for (rec in wos_records) {
    doi <- normalize_doi(rec[["DI"]])
    if (!is.na(doi) && doi != "") {
      wos_by_doi[[doi]] <- rec
    }
  }
  
  # Build title list for WoS records
  wos_titles <- lapply(wos_records, function(rec) {
    list(title = normalize_title(rec[["TI"]]), rec = rec)
  })
  
  merged <- wos_records
  duplicates <- list()
  added_from_scopus <- 0
  skipped_doi <- 0
  skipped_title <- 0
  
  for (s_rec in scopus_records) {
    s_doi <- normalize_doi(s_rec[["DI"]])
    is_dup <- FALSE
    
    # Check DOI match
    if (!is.na(s_doi) && s_doi != "" && !is.null(wos_by_doi[[s_doi]])) {
      w_rec <- wos_by_doi[[s_doi]]
      duplicates <- c(duplicates, list(list(
        wos = w_rec, scopus = s_rec, match_type = "DOI", score = 1.0
      )))
      skipped_doi <- skipped_doi + 1
      is_dup <- TRUE
    }
    
    # Check title similarity
    if (!is_dup) {
      s_title <- normalize_title(s_rec[["TI"]])
      if (s_title != "") {
        best_score <- 0
        best_match <- NULL
        
        for (wt in wos_titles) {
          w_title <- wt$title
          if (w_title == "") next
          len_ratio <- nchar(s_title) / max(nchar(w_title), 1)
          if (len_ratio < 0.7 || len_ratio > 1.4) next
          score <- string_similarity(s_title, w_title)
          if (score > best_score) {
            best_score <- score
            best_match <- wt$rec
          }
        }
        
        if (best_score >= threshold) {
          duplicates <- c(duplicates, list(list(
            wos = best_match, scopus = s_rec, match_type = "TITLE", score = best_score
          )))
          skipped_title <- skipped_title + 1
          is_dup <- TRUE
        }
      }
    }
    
    if (!is_dup) {
      merged <- c(merged, list(s_rec))
      added_from_scopus <- added_from_scopus + 1
    }
  }
  
  stats <- list(
    wos_total         = length(wos_records),
    scopus_total      = length(scopus_records),
    duplicates_doi    = skipped_doi,
    duplicates_title  = skipped_title,
    added_from_scopus = added_from_scopus,
    merged_total      = length(merged),
    priority          = priority
  )
  
  return(list(merged = merged, duplicates = duplicates, stats = stats))
}


generate_report <- function(stats, duplicates, report_path) {
  lines <- character(0)
  
  lines <- c(lines, paste(rep("=", 70), collapse = ""))
  lines <- c(lines, "  MERGE & DEDUPLICATION REPORT")
  lines <- c(lines, paste(rep("=", 70), collapse = ""))
  lines <- c(lines, "")
  lines <- c(lines, sprintf("  WoS records:                    %d", stats$wos_total))
  lines <- c(lines, sprintf("  Scopus records:                 %d", stats$scopus_total))
  lines <- c(lines, sprintf("  Total before dedup:             %d", stats$wos_total + stats$scopus_total))
  lines <- c(lines, "")
  lines <- c(lines, sprintf("  Duplicates by DOI:              %d", stats$duplicates_doi))
  lines <- c(lines, sprintf("  Duplicates by title similarity: %d", stats$duplicates_title))
  lines <- c(lines, sprintf("  Total duplicates removed:       %d", stats$duplicates_doi + stats$duplicates_title))
  lines <- c(lines, "")
  lines <- c(lines, sprintf("  Unique from Scopus added:       %d", stats$added_from_scopus))
  lines <- c(lines, sprintf("  Priority on duplicate:          %s", toupper(stats$priority)))
  lines <- c(lines, "")
  lines <- c(lines, sprintf("  FINAL MERGED RECORDS:           %d", stats$merged_total))
  lines <- c(lines, paste(rep("=", 70), collapse = ""))
  lines <- c(lines, "")
  
  if (length(duplicates) > 0) {
    lines <- c(lines, paste(rep("-", 70), collapse = ""))
    lines <- c(lines, "  DUPLICATE DETAILS")
    lines <- c(lines, paste(rep("-", 70), collapse = ""))
    lines <- c(lines, "")
    
    for (i in seq_along(duplicates)) {
      dup <- duplicates[[i]]
      w_title <- substr(gsub("\\s+", " ", ifelse(is.null(dup$wos[["TI"]]), "N/A", dup$wos[["TI"]])), 1, 100)
      s_title <- substr(gsub("\\s+", " ", ifelse(is.null(dup$scopus[["TI"]]), "N/A", dup$scopus[["TI"]])), 1, 100)
      doi <- ifelse(!is.null(dup$wos[["DI"]]), dup$wos[["DI"]],
                    ifelse(!is.null(dup$scopus[["DI"]]), dup$scopus[["DI"]], "N/A"))
      
      match_info <- sprintf("  [%3d] Match: %s", i, dup$match_type)
      if (dup$match_type == "TITLE") {
        match_info <- paste0(match_info, sprintf(" (similarity: %.2f%%)", dup$score * 100))
      }
      lines <- c(lines, match_info)
      lines <- c(lines, sprintf("        DOI:   %s", doi))
      lines <- c(lines, sprintf("        WoS:   %s", w_title))
      lines <- c(lines, sprintf("        Scopus: %s", s_title))
      lines <- c(lines, "")
    }
  }
  
  # Write with BOM
  con <- file(report_path, open = "wb")
  writeBin(charToRaw("\xEF\xBB\xBF"), con)
  writeBin(charToRaw(paste(lines, collapse = "\n")), con)
  close(con)
  
  return(report_path)
}


# ============================================================
# MAIN
# ============================================================
main <- function() {
  dir.create(OUTPUT_FOLDER, showWarnings = FALSE, recursive = TRUE)
  
  cat(sprintf("Loading WoS file: %s\n", WOS_FILE))
  wos_records <- parse_wos_records(WOS_FILE)
  cat(sprintf("  -> %d records\n\n", length(wos_records)))
  
  cat(sprintf("Loading Scopus file: %s\n", SCOPUS_FILE))
  scopus_records <- parse_wos_records(SCOPUS_FILE)
  cat(sprintf("  -> %d records\n\n", length(scopus_records)))
  
  cat("Merging and deduplicating...\n")
  result <- merge_records(
    wos_records, scopus_records,
    threshold = TITLE_SIMILARITY_THRESHOLD,
    priority  = PRIORITY
  )
  
  merged     <- result$merged
  duplicates <- result$duplicates
  stats      <- result$stats
  
  # Write merged file with exact WoS format
  cat(sprintf("\nWriting merged file: %s\n", OUTPUT_FILE))
  con <- file(OUTPUT_FILE, open = "wb")
  writeBin(charToRaw("\xEF\xBB\xBF"), con)  # BOM
  
  write_line <- function(text) {
    writeBin(charToRaw(paste0(text, "\n")), con)
  }
  
  write_line("FN Clarivate Analytics Web of Science")
  write_line("VR 1.0")
  
  for (rec in merged) {
    wos_text <- record_to_wos_text(rec)
    writeBin(charToRaw(paste0(wos_text, "\n\n")), con)
  }
  
  write_line("EF")
  close(con)
  
  report_path <- generate_report(stats, duplicates, REPORT_FILE)
  
  # Validation
  content <- paste(readLines(OUTPUT_FILE, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  pt_count <- length(gregexpr("\nPT ", content)[[1]])
  er_count <- length(gregexpr("\nER\n", content)[[1]])
  
  cat("\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("  MERGE SUMMARY\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat(sprintf("  WoS records:              %d\n", stats$wos_total))
  cat(sprintf("  Scopus records:           %d\n", stats$scopus_total))
  cat(sprintf("  Duplicates (DOI):         %d\n", stats$duplicates_doi))
  cat(sprintf("  Duplicates (title):       %d\n", stats$duplicates_title))
  cat(sprintf("  Unique added from Scopus: %d\n", stats$added_from_scopus))
  cat(sprintf("  TOTAL MERGED:             %d\n", stats$merged_total))
  cat(sprintf("  Validation: PT=%d, ER=%d\n", pt_count, er_count))
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat(sprintf("\n  Output:  %s\n", OUTPUT_FILE))
  cat(sprintf("  Report:  %s\n", report_path))
}

main()
