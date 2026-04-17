#!/usr/bin/env Rscript
# ============================================================================
# Convert Scopus plain text export to Web of Science (WoS) tagged format.
#
# Mapping Scopus fields -> WoS 2-letter tags:
#   Authors (short)          -> AU
#   AUTHOR FULL NAMES        -> AF
#   Title                    -> TI
#   Journal (from ref line)  -> SO
#   ABSTRACT                 -> AB
#   AUTHOR KEYWORDS          -> DE
#   INDEX KEYWORDS           -> ID
#   AFFILIATIONS             -> C1
#   CORRESPONDENCE ADDRESS   -> RP
#   DOI                      -> DI
#   Year (from ref line)     -> PY
#   Volume (from ref line)   -> VL
#   Issue (from ref line)    -> IS
#   Start page (from ref)    -> BP
#   End page (from ref)      -> EP
#   REFERENCES               -> CR
#   FUNDING DETAILS/TEXT     -> FU / FX
#   PUBLISHER                -> PU
#   ISSN                     -> SN
#   ISBN                     -> BN
#   LANGUAGE OF ORIG DOC     -> LA
#   ABBREVIATED SOURCE TITLE -> J9/JI
#   DOCUMENT TYPE            -> DT
#   OPEN ACCESS              -> OA
#   PUBMED ID                -> PM
#   Scopus URL (EID)         -> UT (adapted)
#   Article number           -> AR
#
# FIX: "SOURCE: Scopus" is now used as the true end-of-record marker.
#      This handles exports with and without the OPEN ACCESS field,
#      which is the case when downloading without open access filters.
#
# Usage:
#   Rscript 1_convert_scopus_to_wos.R
# ============================================================================

# ============================================================
# CONFIGURATION - Edit these paths as needed
# ============================================================
INPUT_FILE  <- "input/scopus.txt"
OUTPUT_DIR  <- "output"
OUTPUT_FILE <- file.path(OUTPUT_DIR, "scopus_formato_wos.txt")
# ============================================================


parse_scopus_records <- function(filepath) {
  # Read file with BOM handling
  lines <- readLines(filepath, encoding = "UTF-8", warn = FALSE)
  # Remove BOM if present
  if (length(lines) > 0) {
    lines[1] <- sub("^\uFEFF", "", lines[1])
  }
  
  records <- list()
  current_record <- list()
  current_field <- NULL
  in_record <- FALSE
  header_passed <- FALSE
  
  # Labels that signal start of a known field
  known_labels <- c(
    "AUTHOR FULL NAMES:", "DOI:", "AFFILIATIONS:", "ABSTRACT:",
    "AUTHOR KEYWORDS:", "INDEX KEYWORDS:", "FUNDING DETAILS:",
    "REFERENCES:", "CORRESPONDENCE ADDRESS:", "PUBLISHER:",
    "ISSN:", "ISBN:", "CODEN:", "LANGUAGE OF ORIGINAL DOCUMENT:",
    "ABBREVIATED SOURCE TITLE:", "DOCUMENT TYPE:", "PUBLICATION STAGE:",
    "PUBMED ID:", "MANUFACTURERS:", "TRADENAMES:", "OPEN ACCESS:",
    "SOURCE:"   # <-- NUEVO: campo presente en descargas sin filtro
  )
  
  # Labels that interrupt ABSTRACT continuation
  interrupt_labels <- c(
    "AUTHOR KEYWORDS:", "INDEX KEYWORDS:", "FUNDING",
    "REFERENCES:", "CORRESPONDENCE", "PUBLISHER:",
    "ISSN:", "ISBN:", "CODEN:", "LANGUAGE OF",
    "ABBREVIATED", "DOCUMENT TYPE:", "PUBLICATION STAGE:",
    "OPEN ACCESS:", "PUBMED ID:", "MANUFACTURERS:", "TRADENAMES:",
    "SOURCE:"   # <-- NUEVO
  )
  
  # Labels that interrupt FUNDING_TEXT continuation
  funding_interrupt <- c(
    "REFERENCES:", "CORRESPONDENCE", "PUBLISHER:",
    "ISSN:", "ISBN:", "FUNDING DETAILS:", "FUNDING TEXT",
    "SOURCE:"   # <-- NUEVO
  )
  
  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]
    
    # Skip header lines
    if (!header_passed) {
      if (grepl("^Scopus", line) || grepl("^EXPORT DATE:", line) || trimws(line) == "") {
        i <- i + 1
        next
      } else {
        header_passed <- TRUE
      }
    }
    
    stripped <- trimws(line)
    
    if (!in_record) {
      if (stripped == "") {
        i <- i + 1
        next
      }
      in_record <- TRUE
      current_record <- list()
      current_field <- "AU_SHORT"
      current_record[["AU_SHORT"]] <- stripped
      i <- i + 1
      next
    }
    
    # --- Inside a record ---
    
    if (startsWith(line, "AUTHOR FULL NAMES:")) {
      current_field <- "AUTHOR_FULL_NAMES"
      current_record[[current_field]] <- trimws(sub("^AUTHOR FULL NAMES:", "", line))
      
    } else if (startsWith(line, "DOI:")) {
      current_field <- "DOI"
      current_record[[current_field]] <- trimws(sub("^DOI:", "", line))
      
    } else if (startsWith(line, "https://www.scopus.com/")) {
      current_record[["SCOPUS_URL"]] <- stripped
      current_field <- "SCOPUS_URL"
      
    } else if (startsWith(line, "AFFILIATIONS:")) {
      current_field <- "AFFILIATIONS"
      current_record[[current_field]] <- trimws(sub("^AFFILIATIONS:", "", line))
      
    } else if (startsWith(line, "ABSTRACT:")) {
      current_field <- "ABSTRACT"
      current_record[[current_field]] <- trimws(sub("^ABSTRACT:", "", line))
      
    } else if (startsWith(line, "AUTHOR KEYWORDS:")) {
      current_field <- "AUTHOR_KEYWORDS"
      current_record[[current_field]] <- trimws(sub("^AUTHOR KEYWORDS:", "", line))
      
    } else if (startsWith(line, "INDEX KEYWORDS:")) {
      current_field <- "INDEX_KEYWORDS"
      current_record[[current_field]] <- trimws(sub("^INDEX KEYWORDS:", "", line))
      
    } else if (startsWith(line, "FUNDING DETAILS:")) {
      current_field <- "FUNDING_DETAILS"
      val <- trimws(sub("^FUNDING DETAILS:", "", line))
      if (!is.null(current_record[["FUNDING_DETAILS"]])) {
        current_record[["FUNDING_DETAILS"]] <- paste0(current_record[["FUNDING_DETAILS"]], "; ", val)
      } else {
        current_record[["FUNDING_DETAILS"]] <- val
      }
      
    } else if (grepl("^FUNDING TEXT", line)) {
      current_field <- "FUNDING_TEXT"
      val <- trimws(sub("^FUNDING TEXT \\d+:\\s*", "", line))
      if (!is.null(current_record[["FUNDING_TEXT"]])) {
        current_record[["FUNDING_TEXT"]] <- paste0(current_record[["FUNDING_TEXT"]], " ", val)
      } else {
        current_record[["FUNDING_TEXT"]] <- val
      }
      
    } else if (startsWith(line, "REFERENCES:")) {
      current_field <- "REFERENCES"
      current_record[[current_field]] <- trimws(sub("^REFERENCES:", "", line))
      
    } else if (startsWith(line, "CORRESPONDENCE ADDRESS:")) {
      current_field <- "CORRESPONDENCE"
      current_record[[current_field]] <- trimws(sub("^CORRESPONDENCE ADDRESS:", "", line))
      
    } else if (startsWith(line, "PUBLISHER:")) {
      current_field <- "PUBLISHER"
      current_record[[current_field]] <- trimws(sub("^PUBLISHER:", "", line))
      
    } else if (startsWith(line, "ISSN:")) {
      current_field <- "ISSN"
      current_record[[current_field]] <- trimws(sub("^ISSN:", "", line))
      
    } else if (startsWith(line, "ISBN:")) {
      current_field <- "ISBN"
      current_record[[current_field]] <- trimws(sub("^ISBN:", "", line))
      
    } else if (startsWith(line, "CODEN:")) {
      current_field <- "CODEN"
      current_record[[current_field]] <- trimws(sub("^CODEN:", "", line))
      
    } else if (startsWith(line, "LANGUAGE OF ORIGINAL DOCUMENT:")) {
      current_field <- "LANGUAGE"
      current_record[[current_field]] <- trimws(sub("^LANGUAGE OF ORIGINAL DOCUMENT:", "", line))
      
    } else if (startsWith(line, "ABBREVIATED SOURCE TITLE:")) {
      current_field <- "ABBREV_SOURCE"
      current_record[[current_field]] <- trimws(sub("^ABBREVIATED SOURCE TITLE:", "", line))
      
    } else if (startsWith(line, "DOCUMENT TYPE:")) {
      current_field <- "DOCUMENT_TYPE"
      current_record[[current_field]] <- trimws(sub("^DOCUMENT TYPE:", "", line))
      
    } else if (startsWith(line, "PUBLICATION STAGE:")) {
      current_field <- "PUB_STAGE"
      current_record[[current_field]] <- trimws(sub("^PUBLICATION STAGE:", "", line))
      
    } else if (startsWith(line, "PUBMED ID:")) {
      current_field <- "PUBMED_ID"
      current_record[[current_field]] <- trimws(sub("^PUBMED ID:", "", line))
      
    } else if (startsWith(line, "MANUFACTURERS:")) {
      current_field <- "MANUFACTURERS"
      current_record[[current_field]] <- trimws(sub("^MANUFACTURERS:", "", line))
      
    } else if (startsWith(line, "TRADENAMES:")) {
      current_field <- "TRADENAMES"
      current_record[[current_field]] <- trimws(sub("^TRADENAMES:", "", line))
      
    } else if (startsWith(line, "OPEN ACCESS:")) {
      # FIX: OPEN ACCESS ya NO cierra el registro; solo guarda el valor.
      # El cierre real ocurre en SOURCE: Scopus (ver abajo).
      current_field <- "OPEN_ACCESS"
      current_record[[current_field]] <- trimws(sub("^OPEN ACCESS:", "", line))
      
    } else if (startsWith(line, "SOURCE:")) {
      # FIX: SOURCE: Scopus es el verdadero marcador de fin de registro.
      # Aparece al final de TODOS los registros (con y sin OPEN ACCESS).
      current_record[["SOURCE"]] <- trimws(sub("^SOURCE:", "", line))
      records <- c(records, list(current_record))
      current_record <- list()
      in_record <- FALSE
      current_field <- NULL
      
    } else if (grepl("^\\d{8,}(;\\s*\\d{8,})*\\s*$", stripped)) {
      # Scopus Author IDs line
      current_record[["AUTHOR_IDS"]] <- stripped
      current_field <- "AUTHOR_IDS"
      
    } else if (grepl("^\\(\\d{4}\\)\\s", stripped)) {
      # Journal reference line
      current_record[["JOURNAL_REF"]] <- stripped
      current_field <- "JOURNAL_REF"
      
    } else if (current_field == "AU_SHORT" && is.null(current_record[["TITLE"]])) {
      if (!grepl("^\\(", stripped) && stripped != "" &&
          current_field %in% c("AU_SHORT", "AUTHOR_IDS", "AUTHOR_FULL_NAMES")) {
        current_record[["TITLE"]] <- stripped
        current_field <- "TITLE"
      }
      
    } else if (current_field == "AUTHOR_FULL_NAMES" && is.null(current_record[["TITLE"]])) {
      if (grepl("^\\d{8,}", stripped)) {
        current_record[["AUTHOR_IDS"]] <- stripped
        current_field <- "AUTHOR_IDS"
      } else if (stripped != "" && !grepl("^\\(", stripped)) {
        current_record[["TITLE"]] <- stripped
        current_field <- "TITLE"
      }
      
    } else if (current_field == "AUTHOR_IDS" && is.null(current_record[["TITLE"]])) {
      if (stripped != "" && !grepl("^\\(", stripped)) {
        current_record[["TITLE"]] <- stripped
        current_field <- "TITLE"
      }
      
    } else if (current_field == "TITLE" && is.null(current_record[["JOURNAL_REF"]])) {
      if (grepl("^\\(\\d{4}\\)\\s", stripped)) {
        current_record[["JOURNAL_REF"]] <- stripped
        current_field <- "JOURNAL_REF"
      } else if (stripped != "") {
        current_record[["TITLE"]] <- paste(current_record[["TITLE"]], stripped)
      }
      
    } else if (current_field == "REFERENCES" && stripped != "") {
      current_record[["REFERENCES"]] <- paste(current_record[["REFERENCES"]], stripped)
      
    } else if (current_field == "ABSTRACT" &&
               !any(sapply(interrupt_labels, function(lbl) startsWith(stripped, lbl))) &&
               stripped != "") {
      current_record[["ABSTRACT"]] <- paste(current_record[["ABSTRACT"]], stripped)
      
    } else if (current_field == "FUNDING_TEXT" &&
               !any(sapply(funding_interrupt, function(lbl) startsWith(stripped, lbl))) &&
               stripped != "") {
      current_record[["FUNDING_TEXT"]] <- paste(current_record[["FUNDING_TEXT"]], stripped)
    }
    # else: blank lines within records - skip
    
    i <- i + 1
  }
  
  # Guardar el ultimo registro si el archivo no termina con SOURCE:
  if (in_record && length(current_record) > 0) {
    records <- c(records, list(current_record))
  }
  
  return(records)
}


parse_journal_ref <- function(ref_line) {
  result <- list()
  
  # Extract year
  m <- regmatches(ref_line, regexec("^\\((\\d{4})\\)\\s*(.*)", ref_line))[[1]]
  if (length(m) < 3) return(result)
  
  result[["PY"]] <- m[2]
  rest <- m[3]
  
  # Remove "Cited X times" at end
  rest <- sub(",?\\s*Cited \\d+ times\\.?\\s*$", "", rest)
  rest <- sub(",\\s*$", "", trimws(rest))
  
  # Pattern 1: Journal Name, VOL (ISSUE), pp. START - END  or  art. no. XXX
  m1 <- regmatches(rest, regexec(
    "^(.+?),\\s*(\\d+)\\s*\\(([^)]+)\\)\\s*,\\s*(?:pp\\.\\s*(\\d+)\\s*-\\s*(\\d+)|art\\.\\s*no\\.\\s*(\\S+))",
    rest, perl = TRUE
  ))[[1]]
  if (length(m1) >= 4 && m1[1] != "") {
    result[["SO"]] <- trimws(m1[2])
    result[["VL"]] <- m1[3]
    result[["IS"]] <- m1[4]
    if (!is.na(m1[5]) && m1[5] != "" && !is.na(m1[6]) && m1[6] != "") {
      result[["BP"]] <- m1[5]
      result[["EP"]] <- m1[6]
    } else if (length(m1) >= 7 && !is.na(m1[7]) && m1[7] != "") {
      result[["AR"]] <- m1[7]
    }
    return(result)
  }
  
  # Pattern 2: Journal Name, VOL, pp. START - END  or  art. no. XXX (no issue)
  m2 <- regmatches(rest, regexec(
    "^(.+?),\\s*(\\d+)\\s*,\\s*(?:pp\\.\\s*(\\d+)\\s*-\\s*(\\d+)|art\\.\\s*no\\.\\s*(\\S+))",
    rest, perl = TRUE
  ))[[1]]
  if (length(m2) >= 3 && m2[1] != "") {
    result[["SO"]] <- trimws(m2[2])
    result[["VL"]] <- m2[3]
    if (!is.na(m2[4]) && m2[4] != "" && !is.na(m2[5]) && m2[5] != "") {
      result[["BP"]] <- m2[4]
      result[["EP"]] <- m2[5]
    } else if (length(m2) >= 6 && !is.na(m2[6]) && m2[6] != "") {
      result[["AR"]] <- m2[6]
    }
    return(result)
  }
  
  # Pattern 3: Journal Name, VOL (ISSUE), no pages
  m3 <- regmatches(rest, regexec("^(.+?),\\s*(\\d+)\\s*\\(([^)]+)\\)\\s*$", rest))[[1]]
  if (length(m3) >= 4 && m3[1] != "") {
    result[["SO"]] <- trimws(m3[2])
    result[["VL"]] <- m3[3]
    result[["IS"]] <- m3[4]
    return(result)
  }
  
  # Pattern 4: Journal Name, VOL only
  m4 <- regmatches(rest, regexec("^(.+?),\\s*(\\d+)\\s*$", rest))[[1]]
  if (length(m4) >= 3 && m4[1] != "") {
    result[["SO"]] <- trimws(m4[2])
    result[["VL"]] <- m4[3]
    return(result)
  }
  
  # Fallback: just journal name
  result[["SO"]] <- trimws(strsplit(rest, ",")[[1]][1])
  return(result)
}


parse_authors_short <- function(au_line) {
  # Split by semicolons if present
  parts <- strsplit(au_line, ";\\s*")[[1]]
  if (length(parts) > 1) {
    return(trimws(parts[parts != ""]))
  }
  
  # Parse comma-separated: each author is "Surname, Initials"
  tokens <- trimws(strsplit(au_line, ",")[[1]])
  authors <- character(0)
  i <- 1
  while (i <= length(tokens)) {
    if (i + 1 <= length(tokens)) {
      next_t <- trimws(tokens[i + 1])
      if (grepl("^[A-Z]\\.?[A-Z]?\\.?[A-Z]?\\.?\\s*$", next_t)) {
        authors <- c(authors, paste0(trimws(tokens[i]), ", ", next_t))
        i <- i + 2
        next
      }
    }
    authors <- c(authors, trimws(tokens[i]))
    i <- i + 1
  }
  return(authors)
}


parse_authors_full <- function(af_line) {
  parts <- strsplit(af_line, ";\\s*")[[1]]
  authors <- character(0)
  for (p in parts) {
    name <- sub("\\s*\\(\\d+\\)\\s*$", "", trimws(p))
    if (name != "") {
      authors <- c(authors, name)
    }
  }
  return(authors)
}


convert_references <- function(ref_text) {
  # Converts Scopus references to WoS CR format.
  #
  # Scopus uses this format for each reference:
  #   Author1 I., Author2 J., ..., AuthorN X., Title, Journal, VOL, (ISSUE), pp. PAG1-PAG2, (YEAR)
  #
  # WoS CR format is much more compact:
  #   FIRST_AUTHOR, YEAR, JOURNAL_ABBREV, Vvolume, Ppage_start
  #
  # Strategy:
  #   1. Anchor on the year (YYYY) at the end.
  #   2. Take the first "author-token" that matches 'Lastname I.' as first author.
  #   3. Look for Vvol and Ppag backwards from (YYYY).
  #   4. Journal is the token immediately before the volume.
  
  if (is.null(ref_text) || ref_text == "") return(character(0))
  
  refs <- strsplit(ref_text, ";\\s*\\n?")[[1]]
  cr_lines <- character(0)
  
  # Regex to detect a Scopus author-token: "Lastname I." or "Lastname I.J." etc
  AUTHOR_TOKEN_RE <- "^[A-Z][a-zA-Z\\'\\-]+(?:\\s+[A-Z][a-zA-Z\\'\\-]+)*\\s+[A-Z]\\.(?:[A-Z]\\.)*(?:[A-Z]\\.)?$"
  INITIAL_RE <- "^[A-Z]\\.(?:[A-Z]\\.)*(?:[A-Z]\\.)?$"
  
  for (ref in refs) {
    ref <- trimws(ref)
    if (ref == "") next
    
    # Extract year from the end (last occurrence of 4-digit parens)
    year_matches <- gregexpr("\\((\\d{4})\\)", ref, perl = TRUE)[[1]]
    year <- ""
    ref_no_year <- ref
    if (year_matches[1] > 0) {
      # Last year match
      last_pos <- year_matches[length(year_matches)]
      last_len <- attr(year_matches, "match.length")[length(year_matches)]
      year <- substr(ref, last_pos + 1, last_pos + 4)
      # Remove the year from the end to parse the rest
      ref_no_year <- trimws(substr(ref, 1, last_pos - 1))
      ref_no_year <- sub(",\\s*$", "", ref_no_year)
    }
    
    # Extract pages: "pp. 99-118" or "pp. 99"
    page <- ""
    ref_no_pp <- ref_no_year
    pp_m <- regmatches(ref_no_year,
                        regexec("pp\\.\\s*(\\d+)\\s*(?:-\\s*(\\d+))?",
                                ref_no_year, perl = TRUE))[[1]]
    if (length(pp_m) >= 2 && pp_m[1] != "") {
      page <- pp_m[2]
      # Remove pp.XX-YY block
      pp_start <- regexpr("pp\\.\\s*\\d+(?:\\s*-\\s*\\d+)?",
                          ref_no_year, perl = TRUE)
      if (pp_start > 0) {
        ref_no_pp <- trimws(substr(ref_no_year, 1, pp_start - 1))
        ref_no_pp <- sub(",\\s*$", "", ref_no_pp)
      }
    }
    
    # Remove "art. no. XXX" if present
    art_start <- regexpr("art\\.\\s*no\\.\\s*\\S+", ref_no_pp, perl = TRUE)
    if (art_start > 0) {
      ref_no_pp <- trimws(substr(ref_no_pp, 1, art_start - 1))
      ref_no_pp <- sub(",\\s*$", "", ref_no_pp)
    }
    
    # Split into tokens by comma
    tokens <- trimws(strsplit(ref_no_pp, ",\\s*")[[1]])
    tokens <- tokens[tokens != ""]
    
    # Find the last token that is a pure number (that is the volume)
    volume <- ""
    issue <- ""
    vol_idx <- NA
    if (length(tokens) > 0) {
      for (idx in length(tokens):1) {
        if (grepl("^\\d+$", tokens[idx])) {
          if (idx > 1 && grepl("^\\d+$", tokens[idx - 1])) {
            # token[idx-1] is volume, token[idx] is issue
            volume <- tokens[idx - 1]
            issue <- tokens[idx]
            vol_idx <- idx - 1
          } else {
            volume <- tokens[idx]
            vol_idx <- idx
          }
          break
        }
      }
    }
    
    # Journal is the token just before the volume
    journal <- ""
    if (!is.na(vol_idx) && vol_idx > 1) {
      journal <- tokens[vol_idx - 1]
    }
    
    # First author: first token
    first_author <- if (length(tokens) > 0) tokens[1] else ""
    # If the first token doesn't match an author pattern but the second is an
    # initial, join them: "Lastname" + "I."
    if (first_author != "" && !grepl(AUTHOR_TOKEN_RE, first_author, perl = TRUE)) {
      if (length(tokens) >= 2 && grepl(INITIAL_RE, tokens[2], perl = TRUE)) {
        first_author <- paste0(tokens[1], ", ", tokens[2])
      }
    }
    
    # Build CR in WoS format
    if (first_author != "" && year != "") {
      cr <- paste0(first_author, ", ", year)
      if (journal != "") cr <- paste0(cr, ", ", toupper(journal))
      if (volume != "")  cr <- paste0(cr, ", V", volume)
      if (page != "")    cr <- paste0(cr, ", P", page)
      cr_lines <- c(cr_lines, cr)
    } else if (first_author != "") {
      cr_lines <- c(cr_lines, first_author)
    } else if (ref != "") {
      cr_lines <- c(cr_lines, substr(ref, 1, 100))
    }
  }
  return(cr_lines)
}


format_wos_field <- function(tag, value) {
  if (is.null(value) || (length(value) == 1 && value == "")) return("")
  
  lines <- character(0)
  if (length(value) > 1) {
    # List of values (e.g., authors)
    for (j in seq_along(value)) {
      if (j == 1) {
        lines <- c(lines, paste0(tag, " ", value[j]))
      } else {
        lines <- c(lines, paste0("   ", value[j]))
      }
    }
  } else {
    lines <- c(lines, paste0(tag, " ", value))
  }
  return(paste(lines, collapse = "\n"))
}


record_to_wos <- function(rec) {
  output_lines <- character(0)
  
  # PT
  output_lines <- c(output_lines, "PT J")
  
  # AU
  if (!is.null(rec[["AU_SHORT"]])) {
    authors_short <- parse_authors_short(rec[["AU_SHORT"]])
    if (length(authors_short) > 0) {
      output_lines <- c(output_lines, format_wos_field("AU", authors_short))
    }
  }
  
  # AF
  if (!is.null(rec[["AUTHOR_FULL_NAMES"]])) {
    authors_full <- parse_authors_full(rec[["AUTHOR_FULL_NAMES"]])
    if (length(authors_full) > 0) {
      output_lines <- c(output_lines, format_wos_field("AF", authors_full))
    }
  }
  
  # TI
  if (!is.null(rec[["TITLE"]])) {
    output_lines <- c(output_lines, format_wos_field("TI", rec[["TITLE"]]))
  }
  
  # SO - from journal reference
  journal_info <- list()
  if (!is.null(rec[["JOURNAL_REF"]])) {
    journal_info <- parse_journal_ref(rec[["JOURNAL_REF"]])
    if (!is.null(journal_info[["SO"]])) {
      output_lines <- c(output_lines, format_wos_field("SO", journal_info[["SO"]]))
    }
  }
  
  # LA
  if (!is.null(rec[["LANGUAGE"]])) {
    output_lines <- c(output_lines, format_wos_field("LA", rec[["LANGUAGE"]]))
  }
  
  # DT
  if (!is.null(rec[["DOCUMENT_TYPE"]])) {
    output_lines <- c(output_lines, format_wos_field("DT", rec[["DOCUMENT_TYPE"]]))
  }
  
  # DE
  if (!is.null(rec[["AUTHOR_KEYWORDS"]])) {
    output_lines <- c(output_lines, format_wos_field("DE", rec[["AUTHOR_KEYWORDS"]]))
  }
  
  # ID
  if (!is.null(rec[["INDEX_KEYWORDS"]])) {
    output_lines <- c(output_lines, format_wos_field("ID", rec[["INDEX_KEYWORDS"]]))
  }
  
  # AB
  if (!is.null(rec[["ABSTRACT"]])) {
    output_lines <- c(output_lines, format_wos_field("AB", rec[["ABSTRACT"]]))
  }
  
  # C1
  if (!is.null(rec[["AFFILIATIONS"]])) {
    output_lines <- c(output_lines, format_wos_field("C1", rec[["AFFILIATIONS"]]))
  }
  
  # RP
  if (!is.null(rec[["CORRESPONDENCE"]])) {
    output_lines <- c(output_lines, format_wos_field("RP", rec[["CORRESPONDENCE"]]))
  }
  
  # EM
  if (!is.null(rec[["CORRESPONDENCE"]])) {
    email_m <- regmatches(rec[["CORRESPONDENCE"]], regexec("email:\\s*(\\S+)", rec[["CORRESPONDENCE"]]))[[1]]
    if (length(email_m) >= 2) {
      output_lines <- c(output_lines, format_wos_field("EM", email_m[2]))
    }
  }
  
  # FU
  if (!is.null(rec[["FUNDING_DETAILS"]])) {
    output_lines <- c(output_lines, format_wos_field("FU", rec[["FUNDING_DETAILS"]]))
  }
  
  # FX
  if (!is.null(rec[["FUNDING_TEXT"]])) {
    output_lines <- c(output_lines, format_wos_field("FX", rec[["FUNDING_TEXT"]]))
  }
  
  # CR
  if (!is.null(rec[["REFERENCES"]])) {
    cr_lines <- convert_references(rec[["REFERENCES"]])
    if (length(cr_lines) > 0) {
      output_lines <- c(output_lines, format_wos_field("CR", cr_lines))
    }
  }
  
  # NR
  if (!is.null(rec[["REFERENCES"]])) {
    cr_lines <- convert_references(rec[["REFERENCES"]])
    output_lines <- c(output_lines, format_wos_field("NR", as.character(length(cr_lines))))
  }
  
  # PU
  if (!is.null(rec[["PUBLISHER"]])) {
    output_lines <- c(output_lines, format_wos_field("PU", rec[["PUBLISHER"]]))
  }
  
  # SN
  if (!is.null(rec[["ISSN"]])) {
    output_lines <- c(output_lines, format_wos_field("SN", rec[["ISSN"]]))
  }
  
  # BN
  if (!is.null(rec[["ISBN"]])) {
    output_lines <- c(output_lines, format_wos_field("BN", rec[["ISBN"]]))
  }
  
  # J9 / JI
  if (!is.null(rec[["ABBREV_SOURCE"]])) {
    output_lines <- c(output_lines, format_wos_field("J9", rec[["ABBREV_SOURCE"]]))
    output_lines <- c(output_lines, format_wos_field("JI", rec[["ABBREV_SOURCE"]]))
  }
  
  # PY
  if (!is.null(journal_info[["PY"]])) {
    output_lines <- c(output_lines, format_wos_field("PY", journal_info[["PY"]]))
  }
  
  # VL
  if (!is.null(journal_info[["VL"]])) {
    output_lines <- c(output_lines, format_wos_field("VL", journal_info[["VL"]]))
  }
  
  # IS
  if (!is.null(journal_info[["IS"]])) {
    output_lines <- c(output_lines, format_wos_field("IS", journal_info[["IS"]]))
  }
  
  # BP
  if (!is.null(journal_info[["BP"]])) {
    output_lines <- c(output_lines, format_wos_field("BP", journal_info[["BP"]]))
  }
  
  # EP
  if (!is.null(journal_info[["EP"]])) {
    output_lines <- c(output_lines, format_wos_field("EP", journal_info[["EP"]]))
  }
  
  # AR
  if (!is.null(journal_info[["AR"]])) {
    output_lines <- c(output_lines, format_wos_field("AR", journal_info[["AR"]]))
  }
  
  # DI
  if (!is.null(rec[["DOI"]])) {
    output_lines <- c(output_lines, format_wos_field("DI", rec[["DOI"]]))
  }
  
  # PG - page count
  if (!is.null(journal_info[["BP"]]) && !is.null(journal_info[["EP"]])) {
    bp <- suppressWarnings(as.integer(journal_info[["BP"]]))
    ep <- suppressWarnings(as.integer(journal_info[["EP"]]))
    if (!is.na(bp) && !is.na(ep)) {
      output_lines <- c(output_lines, format_wos_field("PG", as.character(ep - bp + 1)))
    }
  }
  
  # PM
  if (!is.null(rec[["PUBMED_ID"]])) {
    output_lines <- c(output_lines, format_wos_field("PM", rec[["PUBMED_ID"]]))
  }
  
  # OA
  if (!is.null(rec[["OPEN_ACCESS"]])) {
    output_lines <- c(output_lines, format_wos_field("OA", tolower(rec[["OPEN_ACCESS"]])))
  }
  
  # UT - extract Scopus ID from URL (old and new formats)
  # Old format: https://www.scopus.com/inward/record.uri?eid=2-s2.0-85123456789
  # New format: https://www.scopus.com/pages/publications/85123456789?origin=resultslist
  scopus_id <- NULL
  if (!is.null(rec[["SCOPUS_URL"]])) {
    url <- rec[["SCOPUS_URL"]]
    # Try old format first with eid=2-s2.0-
    eid_m <- regmatches(url, regexec("eid=(2-s2\\.0-\\d+)", url))[[1]]
    if (length(eid_m) >= 2) {
      scopus_id <- eid_m[2]
    } else {
      # New format: /pages/publications/DIGITS
      new_m <- regmatches(url, regexec("/pages/publications/(\\d+)", url))[[1]]
      if (length(new_m) >= 2) {
        # Rebuild EID in canonical format 2-s2.0-XXXXX
        scopus_id <- paste0("2-s2.0-", new_m[2])
      }
    }
  }
  
  if (!is.null(scopus_id)) {
    output_lines <- c(output_lines, format_wos_field("UT", paste0("SCOPUS:", scopus_id)))
  } else if (!is.null(rec[["DOI"]]) && rec[["DOI"]] != "") {
    # Fallback: generate unique UT from DOI so bibliometrix doesn't collapse
    # records with empty database ID.
    doi_id <- substr(gsub("[^A-Za-z0-9]", "", rec[["DOI"]]), 1, 30)
    output_lines <- c(output_lines, format_wos_field("UT", paste0("SCOPUS:DOI-", doi_id)))
  } else if (!is.null(rec[["TITLE"]]) && rec[["TITLE"]] != "") {
    # Last fallback: UT derived from title + year for uniqueness
    title_hash <- substr(gsub("[^A-Za-z0-9]", "", rec[["TITLE"]]), 1, 40)
    year_str <- ""
    if (!is.null(rec[["JOURNAL_REF"]])) {
      y_m <- regmatches(rec[["JOURNAL_REF"]],
                        regexec("^\\((\\d{4})\\)", rec[["JOURNAL_REF"]]))[[1]]
      if (length(y_m) >= 2) year_str <- y_m[2]
    }
    output_lines <- c(output_lines,
                      format_wos_field("UT", paste0("SCOPUS:REC-", year_str, "-", title_hash)))
  }
  
  # ER
  output_lines <- c(output_lines, "ER")
  
  # Remove empty strings from format_wos_field returning ""
  output_lines <- output_lines[output_lines != ""]
  
  return(paste(output_lines, collapse = "\n"))
}


# ============================================================
# MAIN
# ============================================================
main <- function() {
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
  
  cat(sprintf("Parsing Scopus file: %s\n", INPUT_FILE))
  records <- parse_scopus_records(INPUT_FILE)
  cat(sprintf("Found %d records\n", length(records)))
  
  # Write WoS format with UTF-8 BOM
  con <- file(OUTPUT_FILE, open = "wb")
  # Write BOM
  writeBin(charToRaw("\xEF\xBB\xBF"), con)
  
  writeLines_raw <- function(text) {
    writeBin(charToRaw(paste0(text, "\n")), con)
  }
  
  writeLines_raw("FN Converted from Scopus to WoS Format")
  writeLines_raw("VR 1.0")
  
  for (i in seq_along(records)) {
    wos_text <- record_to_wos(records[[i]])
    writeBin(charToRaw(paste0(wos_text, "\n")), con)
    if (i %% 50 == 0) {
      cat(sprintf("  Converted %d/%d records...\n", i, length(records)))
    }
  }
  
  writeLines_raw("EF")
  close(con)
  
  cat(sprintf("\nConversion complete! Output: %s\n", OUTPUT_FILE))
  cat(sprintf("Total records converted: %d\n", length(records)))
  
  # Quick validation
  content <- readLines(OUTPUT_FILE, encoding = "UTF-8", warn = FALSE)
  content_text <- paste(content, collapse = "\n")
  pt_count <- length(gregexpr("\\nPT J", content_text)[[1]])
  er_count <- length(gregexpr("\\nER\\n", content_text)[[1]])
  cat(sprintf("Validation: PT count=%d, ER count=%d\n", pt_count, er_count))
}

main()