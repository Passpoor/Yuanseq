# =====================================================
# 数据输入模块
# =====================================================

data_input_server <- function(input, output, session) {

  # --- 数据读取 ---
  raw_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = TRUE)
    if (ncol(df) >= 2) {
      rownames(df) <- make.names(df[, 1], unique = TRUE)
      df <- df[, -1, drop = FALSE]
    }
    df
  })

  # --- 差异基因结果读取 ---
  deg_file_data <- reactive({
    req(input$deg_file)
    df <- read.csv(input$deg_file$datapath, header = TRUE)
    return(df)
  })

  # --- 芯片差异结果读取 ---
  chip_file_data <- reactive({
    req(input$chip_file)
    df <- read.csv(input$chip_file$datapath, header = TRUE)
    return(df)
  })

  output$group_selector <- renderUI({
    req(raw_data())
    cols <- colnames(raw_data())
    tagList(
      selectInput("control_group", "Control组", choices = cols, multiple = TRUE),
      selectInput("treat_group", "Treatment组", choices = cols, multiple = TRUE)
    )
  })

  # --- 增强的注释函数 ---
  annotate_genes <- function(gene_ids, species_code) {
    db_pkg <- if (species_code == "Mm") "org.Mm.eg.db" else "org.Hs.eg.db"
    if (!require(db_pkg, character.only = TRUE, quietly = TRUE)) {
      warning("数据库包 ", db_pkg, " 未安装")
      return(NULL)
    }

    db_obj <- get(db_pkg)

    clean_ids <- trimws(gene_ids)
    clean_ids <- gsub("[\t\n\r]", "", clean_ids)

    is_ensembl <- grepl("^ENS", clean_ids, ignore.case = TRUE)
    clean_ids[!is_ensembl] <- gsub("[^[:alnum:]]", "", clean_ids[!is_ensembl])

    if (species_code == "Mm") {
      clean_ids <- sapply(clean_ids, function(x) {
        if (grepl("^ENS", x, ignore.case = TRUE)) {
          x
        } else if (grepl("^[A-Za-z]", x)) {
          paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
        } else {
          x
        }
      }, USE.NAMES = FALSE)
    } else {
      clean_ids <- sapply(clean_ids, function(x) {
        if (grepl("^ENS", x, ignore.case = TRUE)) {
          x
        } else {
          toupper(x)
        }
      }, USE.NAMES = FALSE)
    }

    clean_ids <- gsub("[^[:alnum:]]", "", clean_ids)

    cat("基因注释: 清理后基因数量 =", length(clean_ids), "\n")
    cat("前5个清理后的基因:", paste(head(clean_ids, 5), collapse = ", "), "\n")

    all_anno <- data.frame()

    tryCatch({
      valid_symbols <- clean_ids[clean_ids %in% keys(db_obj, keytype = "SYMBOL")]
      if (length(valid_symbols) > 0) {
        cat("找到", length(valid_symbols), "个有效的SYMBOL\n")
        anno <- AnnotationDbi::select(
          db_obj,
          keys = valid_symbols,
          columns = c("SYMBOL", "ENTREZID"),
          keytype = "SYMBOL"
        )
        if (nrow(anno) > 0) {
          anno <- anno[!duplicated(anno$SYMBOL), ]
          all_anno <- rbind(all_anno, anno)
          cat("SYMBOL注释成功:", nrow(anno), "个基因\n")
        }
      } else {
        cat("没有有效的SYMBOL\n")
      }
    }, error = function(e) {
      cat("SYMBOL注释错误:", e$message, "\n")
    })

    tryCatch({
      ensembl_ids <- clean_ids[grepl("^ENS", clean_ids, ignore.case = TRUE)]
      if (length(ensembl_ids) > 0) {
        valid_ensembl <- ensembl_ids[ensembl_ids %in% keys(db_obj, keytype = "ENSEMBL")]
        if (length(valid_ensembl) > 0) {
          cat("找到", length(valid_ensembl), "个有效的ENSEMBL ID (带版本号)\n")
          anno <- AnnotationDbi::select(
            db_obj,
            keys = valid_ensembl,
            columns = c("ENSEMBL", "SYMBOL", "ENTREZID"),
            keytype = "ENSEMBL"
          )
          if (nrow(anno) > 0) {
            anno <- anno[!duplicated(anno$ENSEMBL), ]
            all_anno <- rbind(all_anno, anno)
            cat("ENSEMBL注释成功:", nrow(anno), "个基因\n")
          }
        }

        unmatched_ensembl <- ensembl_ids[!ensembl_ids %in% valid_ensembl]
        if (length(unmatched_ensembl) > 0) {
          ensembl_no_version <- gsub("\\..*", "", unmatched_ensembl)
          valid_no_version <- ensembl_no_version[ensembl_no_version %in% keys(db_obj, keytype = "ENSEMBL")]

          if (length(valid_no_version) > 0) {
            cat("找到", length(valid_no_version), "个有效的ENSEMBL ID (不带版本号)\n")
            anno <- AnnotationDbi::select(
              db_obj,
              keys = valid_no_version,
              columns = c("ENSEMBL", "SYMBOL", "ENTREZID"),
              keytype = "ENSEMBL"
            )
            if (nrow(anno) > 0) {
              anno$ORIGINAL_ENSEMBL <- unmatched_ensembl[match(valid_no_version, ensembl_no_version)]
              anno <- anno[!duplicated(anno$ENSEMBL), ]
              all_anno <- rbind(all_anno, anno)
              cat("ENSEMBL注释成功 (无版本号):", nrow(anno), "个基因\n")
            }
          }
        }
      }
    }, error = function(e) {
      cat("ENSEMBL注释错误:", e$message, "\n")
    })

    tryCatch({
      numeric_ids <- clean_ids[grepl("^[0-9]+$", clean_ids)]
      if (length(numeric_ids) > 0) {
        valid_entrez <- numeric_ids[numeric_ids %in% keys(db_obj, keytype = "ENTREZID")]
        if (length(valid_entrez) > 0) {
          cat("找到", length(valid_entrez), "个有效的ENTREZID\n")
          anno <- AnnotationDbi::select(
            db_obj,
            keys = valid_entrez,
            columns = c("ENTREZID", "SYMBOL"),
            keytype = "ENTREZID"
          )
          if (nrow(anno) > 0) {
            anno <- anno[!duplicated(anno$ENTREZID), ]
            all_anno <- rbind(all_anno, anno)
            cat("ENTREZID注释成功:", nrow(anno), "个基因\n")
          }
        }
      }
    }, error = function(e) {
      cat("ENTREZID注释错误:", e$message, "\n")
    })

    if (nrow(all_anno) > 0) {
      all_anno <- all_anno[!duplicated(all_anno), ]
      cat("总注释成功:", nrow(all_anno), "个基因\n")

      if (!"SYMBOL" %in% colnames(all_anno)) {
        all_anno$SYMBOL <- NA
      }

      return(all_anno)
    } else {
      cat("所有注释尝试都失败\n")
      return(NULL)
    }
  }

  # --- 可选过滤假基因函数 ---
  # 默认不删除 Gm/Rik/-ps 基因，避免在探索性小鼠转录组分析中误删候选基因。
  filter_pseudo_genes <- function(df, enable = FALSE) {
    if (!isTRUE(enable)) {
      return(df)
    }

    symbol_col <- if ("SYMBOL" %in% colnames(df)) df$SYMBOL else rep(NA_character_, nrow(df))
    geneid_col <- if ("GeneID" %in% colnames(df)) df$GeneID else rep(NA_character_, nrow(df))

    is_pseudo_symbol <- !is.na(symbol_col) & symbol_col != "" &
      (grepl("^Gm", symbol_col, ignore.case = TRUE) |
         grepl("Rik$", symbol_col, ignore.case = TRUE) |
         grepl("-ps$", symbol_col, ignore.case = TRUE))

    is_pseudo_geneid <- !is.na(geneid_col) & geneid_col != "" &
      (grepl("^Gm", geneid_col, ignore.case = TRUE) |
         grepl("Rik$", geneid_col, ignore.case = TRUE) |
         grepl("-ps$", geneid_col, ignore.case = TRUE))

    df_filtered <- df[!(is_pseudo_symbol | is_pseudo_geneid), , drop = FALSE]
    removed_count <- nrow(df) - nrow(df_filtered)

    if (removed_count > 0) {
      showNotification(
        paste("过滤了", removed_count, "个 Gm/Rik/-ps 基因"),
        type = "message"
      )
    }

    return(df_filtered)
  }

  list(
    raw_data = raw_data,
    deg_file_data = deg_file_data,
    chip_file_data = chip_file_data,
    annotate_genes = annotate_genes,
    filter_pseudo_genes = filter_pseudo_genes
  )
}
