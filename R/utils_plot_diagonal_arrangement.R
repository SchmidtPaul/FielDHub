#' @noRd
plot_diagonal_arrangement <- function(x, l) {
    fieldbook <- x$fieldBook
    
    sites <- factor(fieldbook$LOCATION, levels = unique(fieldbook$LOCATION))
    
    site_levels <- levels(sites)
    
    loc_field_book <- fieldbook |> 
        dplyr::filter(LOCATION == site_levels[l]) |> 
        as.data.frame()
    
    cols <- max(as.numeric(loc_field_book$COLUMN))
    rows <- max(as.numeric(loc_field_book$ROW))

    loc_field_book$ENTRY <- as.numeric(loc_field_book$ENTRY)
    
    main <- paste0("Un-replicated Diagonal Arrangement ", rows, " x ", cols)
    p1 <- desplot::ggdesplot(
        loc_field_book, 
        EXPT ~ COLUMN + ROW,  
        text = ENTRY, 
        col = CHECKS, 
        cex = 1, 
        shorten = "no",
        out1 = EXPT,
        out2 = CHECKS, 
        xlab = "COLUMNS", 
        ylab = "ROWS",
        main = main,
        show.key = FALSE, 
        gg = TRUE,
        out2.gpar=list(col = "gray50", lwd = 1, lty = 1)
    )
    
    return(list(p1 = p1, allSitesFieldbook = fieldbook))
}

#' @noRd
plot_prep <- function(x, l) {

    fieldbook <- x$fieldBook
    
    sites <- factor(fieldbook$LOCATION, levels = unique(fieldbook$LOCATION))
    
    site_levels <- levels(sites)
    
    loc_field_book <- fieldbook |> 
        dplyr::filter(LOCATION == site_levels[l]) |> 
        as.data.frame()
    
    cols <- max(as.numeric(loc_field_book$COLUMN))
    rows <- max(as.numeric(loc_field_book$ROW))

    loc_field_book$ENTRY <- as.character(loc_field_book$ENTRY)
    
    loc_field_book$binay_checks <- ifelse(loc_field_book$CHECKS != 0, 1, 0)
    
    main <- paste0("Partially Replicated Design ", rows, " x ", cols)
    p1 <- desplot::ggdesplot(
        data = loc_field_book, 
        binay_checks ~ COLUMN + ROW,  
        text = ENTRY,  
        xlab = "COLUMNS", 
        ylab = "ROWS",
        main = main,
        cex = 1,
        shorten = "no",
        show.key = FALSE, 
        gg = TRUE,
        col.regions = c("gray", "seagreen")
    )
    
    return(list(p1 = p1, allSitesFieldbook = fieldbook))
}

#' @noRd
plot_optim <- function(x, l) {
  
    fieldbook <- x$fieldBook
    
    sites <- factor(fieldbook$LOCATION, levels = unique(fieldbook$LOCATION))
    
    site_levels <- levels(sites)
    
    loc_field_book <- fieldbook |> 
        dplyr::filter(LOCATION == site_levels[l]) |> 
        as.data.frame()
    
    cols <- max(as.numeric(loc_field_book$COLUMN))
    rows <- max(as.numeric(loc_field_book$ROW))
    
    loc_field_book$ENTRY <- as.character(loc_field_book$ENTRY)
    loc_field_book$CHECKS <- as.character(loc_field_book$CHECKS)
    
    main <- paste0("Un-replicated Optimized Arrangement ", rows, " x ", cols)
    
    p1 <- desplot::ggdesplot(
        loc_field_book,
        CHECKS ~ COLUMN + ROW,
        text= ENTRY,
        cex=1,
        shorten = "no",
        main = main,
        show.key=FALSE,
        xlab = "COLUMNS",
        ylab = "ROWS",
        gg = TRUE)
    
    return(list(p1 = p1, allSitesFieldbook = fieldbook))
}


#' @noRd
plot_augmented_RCBD <- function(x, l) {
  
  fieldbook <- x$fieldBook
  
  sites <- factor(fieldbook$LOCATION, levels = unique(fieldbook$LOCATION))
  site_levels <- levels(sites)
  
  loc_field_book <- fieldbook |>
    dplyr::filter(LOCATION == site_levels[l]) |>
    as.data.frame()
  
  cols <- max(as.numeric(loc_field_book$COLUMN))
  rows <- max(as.numeric(loc_field_book$ROW))
  
  loc_field_book$ENTRY <- as.character(loc_field_book$ENTRY)
  loc_field_book$CHECKS <- as.character(loc_field_book$CHECKS)
  loc_field_book$BLOCK <- as.character(loc_field_book$BLOCK)
  
  # plot numbers as plain integers, whatever PLOT is stored as
  loc_field_book$PLOT_TXT <- sprintf("%d", as.integer(loc_field_book$PLOT))
  
  # text color groups for p1: checks are highlighted, test lines are not
  loc_field_book$CHECK_TEXT <- ifelse(loc_field_book$CHECKS == "1",
                                      "check", "test")
  check_text_cols <- c(check = "red3", test = "gray10")
  
  # -----------------------------
  # Muted palette for BLOCK bg
  # -----------------------------
  # This is added as a scale_fill_manual() layer instead of being passed to
  # desplot as 'col.regions': in desplot 1.10 that argument is dropped for
  # factor fills in the ggplot2 branch. Once a desplot release carrying the fix
  # is on CRAN, both layers below can be replaced by 'col.regions = fill_vals'
  # in the ggdesplot() calls.
  block_levels <- sort(unique(loc_field_book$BLOCK))
  muted6 <- c("#F2F2F2", "#E6EEF5", "#E9F2EC", "#F3EEE6", "#EDE7F2", "#F1E9E9")
  if (length(block_levels) > length(muted6)) {
    muted6 <- grDevices::colorRampPalette(muted6)(length(block_levels))
  } else {
    muted6 <- muted6[seq_along(block_levels)]
  }
  fill_vals <- stats::setNames(muted6, block_levels)
  
  # -----------------------------
  # p1: layout (ENTRY labels)
  # -----------------------------
  main <- paste0("Augmented RCBD Layout ", rows, " x ", cols)
  
  p1 <- desplot::ggdesplot(
    BLOCK ~ COLUMN + ROW,
    text = ENTRY,
    cex = 0.8,
    shorten = "no",
    col = CHECK_TEXT,
    col.text = check_text_cols,
    out1 = EXPT,
    out2 = BLOCK,
    data = loc_field_book,
    xlab = "COLUMNS",
    ylab = "ROWS",
    main = main,
    show.key = FALSE,
    gg = TRUE,
    out2.gpar = list(col = "gray50", lwd = 1, lty = 1)
  ) +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none")
  
  p1 <- add_gg_features(p1)
  
  # -----------------------------
  # p2: plot numbers (NO check highlighting)
  # -----------------------------
  main_plot <- paste0("Augmented RCBD Plot Number Layout ", rows, " x ", cols)
  
  p2 <- desplot::ggdesplot(
    BLOCK ~ COLUMN + ROW,
    text = PLOT_TXT,
    cex = 0.8,
    shorten = "no",
    col.text = "gray10",
    out1 = EXPT,
    out2 = BLOCK,
    data = loc_field_book,
    xlab = "COLUMNS",
    ylab = "ROWS",
    main = main_plot,
    show.key = FALSE,
    gg = TRUE,
    out2.gpar = list(col = "gray50", lwd = 1, lty = 1)
  ) +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none")
  
  p2 <- add_gg_features(p2)
  
  return(list(p1 = p1, p2 = p2, allSitesFieldbook = fieldbook))
}
