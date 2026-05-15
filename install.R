# =====================================================
# YuanSeq 一键安装脚本 | One-click Installation Script
# =====================================================
# 运行方式 | Run: source("install.R")
# =====================================================

cat("╔═══════════════════════════════════════════════════════╗\n")
cat("║        YuanSeq 安装脚本 | Installation Script         ║\n")
cat("╚═══════════════════════════════════════════════════════╝\n\n")

# =====================================================
# 基础设置 | Basic setup
# =====================================================

options(timeout = max(600, getOption("timeout")))

# 检查 R 版本 | Check R version
if (getRversion() < "4.0.0") {
  stop("R 版本过低，请升级到 R >= 4.0 | R version too old, please upgrade to R >= 4.0")
}
cat("R 版本检查通过 | R version check passed\n\n")

# =====================================================
# 自动代理设置 | Auto proxy setup
# =====================================================

normalize_proxy <- function(proxy) {
  proxy <- trimws(proxy %||% "")
  if (identical(proxy, "") || grepl("直接访问|Direct access|none", proxy, ignore.case = TRUE)) {
    return("")
  }
  if (!grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", proxy)) {
    proxy <- paste0("http://", proxy)
  }
  proxy
}

is_port_open <- function(host = "127.0.0.1", port, timeout = 0.5) {
  ok <- FALSE
  con <- NULL
  try({
    con <- socketConnection(host = host, port = port, open = "r+", timeout = timeout)
    ok <- TRUE
  }, silent = TRUE)
  if (!is.null(con)) {
    try(close(con), silent = TRUE)
  }
  ok
}

detect_windows_winhttp_proxy <- function() {
  if (.Platform$OS.type != "windows") return("")
  out <- tryCatch(
    system2("netsh", c("winhttp", "show", "proxy"), stdout = TRUE, stderr = TRUE),
    error = function(e) character()
  )
  if (length(out) == 0) return("")
  line <- grep("Proxy Server|代理服务器", out, value = TRUE)
  if (length(line) == 0) return("")
  proxy <- sub(".*:\\s*", "", line[1])
  proxy <- gsub("https=|http=", "", proxy, ignore.case = TRUE)
  proxy <- strsplit(proxy, ";")[[1]][1]
  normalize_proxy(proxy)
}

auto_configure_proxy <- function() {
  cat("检测网络代理 | Detecting proxy settings...\n")

  # 用户可显式设置 YUANSEQ_PROXY，优先级最高
  yuanseq_proxy <- normalize_proxy(Sys.getenv("YUANSEQ_PROXY", unset = ""))
  if (yuanseq_proxy != "") {
    Sys.setenv(
      http_proxy = yuanseq_proxy,
      https_proxy = yuanseq_proxy,
      HTTP_PROXY = yuanseq_proxy,
      HTTPS_PROXY = yuanseq_proxy
    )
    cat("   使用 YUANSEQ_PROXY:", yuanseq_proxy, "\n\n")
    return(invisible(yuanseq_proxy))
  }

  # 如果系统已有代理变量，直接沿用
  existing <- Sys.getenv(c("https_proxy", "HTTPS_PROXY", "http_proxy", "HTTP_PROXY", "all_proxy", "ALL_PROXY"), unset = "")
  existing <- existing[existing != ""]
  if (length(existing) > 0) {
    proxy <- normalize_proxy(existing[1])
    if (proxy != "") {
      Sys.setenv(
        http_proxy = proxy,
        https_proxy = proxy,
        HTTP_PROXY = proxy,
        HTTPS_PROXY = proxy
      )
      cat("   检测到系统环境代理:", proxy, "\n\n")
      return(invisible(proxy))
    }
  }

  # Windows WinHTTP 代理
  winhttp_proxy <- detect_windows_winhttp_proxy()
  if (winhttp_proxy != "") {
    Sys.setenv(
      http_proxy = winhttp_proxy,
      https_proxy = winhttp_proxy,
      HTTP_PROXY = winhttp_proxy,
      HTTPS_PROXY = winhttp_proxy
    )
    cat("   检测到 Windows WinHTTP 代理:", winhttp_proxy, "\n\n")
    return(invisible(winhttp_proxy))
  }

  # 常见本地代理端口。仅当端口开放时才设置，避免误配。
  common_ports <- c(7890, 7897, 10809, 1080, 17890, 20171, 33210)
  for (p in common_ports) {
    if (is_port_open("127.0.0.1", p)) {
      proxy <- paste0("http://127.0.0.1:", p)
      Sys.setenv(
        http_proxy = proxy,
        https_proxy = proxy,
        HTTP_PROXY = proxy,
        HTTPS_PROXY = proxy
      )
      cat("   检测到本地代理端口:", proxy, "\n\n")
      return(invisible(proxy))
    }
  }

  cat("   未检测到代理，将直接连接。\n")
  cat("   如网络受限，可在运行前设置：Sys.setenv(YUANSEQ_PROXY='http://127.0.0.1:7890')\n\n")
  invisible("")
}

auto_configure_proxy()

# Windows 推荐 libcurl；老版本 R 可回退 wininet
if (.Platform$OS.type == "windows") {
  if (capabilities("libcurl")) {
    options(download.file.method = "libcurl")
  } else {
    options(download.file.method = "wininet")
  }
} else {
  options(download.file.method = "libcurl")
}

# =====================================================
# CRAN 安装 | CRAN install
# =====================================================

cran_repos <- c(CRAN = "https://cloud.r-project.org")
options(repos = cran_repos)

cat("安装 CRAN 包 | Installing CRAN packages...\n")
cran_packages <- c(
  "shiny", "shinyjs", "bslib", "ggplot2", "dplyr", "DT",
  "pheatmap", "plotly", "colourpicker", "shinyWidgets", "rlang",
  "tibble", "tidyr", "ggrepel", "RColorBrewer", "VennDiagram",
  "grid", "gridExtra", "httr", "jsonlite", "base64enc", "remotes",
  "later", "sourcetools", "htmltools", "tinytex"
)

missing_cran <- cran_packages[!sapply(cran_packages, requireNamespace, quietly = TRUE)]
if (length(missing_cran) > 0) {
  cat("   安装 | Installing:", paste(missing_cran, collapse = ", "), "\n")
  install.packages(missing_cran, repos = cran_repos)
}
cat("CRAN 包安装完成 | CRAN packages installed\n\n")

# =====================================================
# Bioconductor 安装 | Bioconductor install
# =====================================================

cat("安装 Bioconductor 包 | Installing Bioconductor packages...\n")
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", repos = cran_repos)
}

bioc_packages <- c(
  "edgeR", "limma", "AnnotationDbi", "clusterProfiler",
  "org.Mm.eg.db", "org.Hs.eg.db", "GseaVis", "enrichplot",
  "decoupleR", "sva", "DOSE", "msigdb", "AUCell", "GSVA"
)

install_bioc_with_mirrors <- function(pkgs) {
  if (length(pkgs) == 0) return(invisible(TRUE))

  bioc_mirrors <- c(
    "https://bioconductor.org",
    "https://mirrors.tuna.tsinghua.edu.cn/bioconductor",
    "https://mirrors.ustc.edu.cn/bioc",
    "https://mirrors.nju.edu.cn/bioconductor"
  )

  for (mirror in bioc_mirrors) {
    cat("   尝试 Bioconductor 镜像 | Trying Bioconductor mirror:", mirror, "\n")
    ok <- tryCatch({
      options(BioC_mirror = mirror)
      # 关键：恢复 Bioconductor 标准仓库，避免 getOption('repos') 覆盖 Bioconductor 源
      options(repos = BiocManager::repositories())
      BiocManager::install(pkgs, ask = FALSE, update = FALSE, site_repository = character())
      TRUE
    }, error = function(e) {
      cat("   镜像失败 | Mirror failed:", conditionMessage(e), "\n")
      FALSE
    })

    still_missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
    if (ok && length(still_missing) == 0) {
      cat("   Bioconductor 包安装成功 | Bioconductor packages installed successfully.\n")
      return(invisible(TRUE))
    }
    if (length(still_missing) > 0) {
      cat("   仍缺失 | Still missing:", paste(still_missing, collapse = ", "), "\n")
      pkgs <- still_missing
    }
  }

  warning("以下 Bioconductor 包未安装成功: ", paste(pkgs, collapse = ", "))
  invisible(FALSE)
}

missing_bioc <- bioc_packages[!sapply(bioc_packages, requireNamespace, quietly = TRUE)]
if (length(missing_bioc) > 0) {
  cat("   安装 | Installing:", paste(missing_bioc, collapse = ", "), "\n")
  install_bioc_with_mirrors(missing_bioc)
}
cat("Bioconductor 包安装步骤完成 | Bioconductor installation step finished\n\n")

# 安装完成后恢复 CRAN 源，避免影响用户后续 install.packages()
options(repos = cran_repos)

# =====================================================
# GitHub 包安装 | GitHub packages
# =====================================================

cat("安装 GitHub 包（可选）| Installing GitHub packages (optional)...\n")
tryCatch({
  if (!requireNamespace("biofree.qyKEGGtools", quietly = TRUE)) {
    remotes::install_github("Passpoor/biofree.qyKEGGtools", upgrade = "never")
    cat("biofree.qyKEGGtools 安装完成 | biofree.qyKEGGtools installed\n")
  } else {
    cat("biofree.qyKEGGtools 已存在 | biofree.qyKEGGtools already installed\n")
  }
}, error = function(e) {
  cat("biofree.qyKEGGtools 安装失败，KEGG 本地富集将不可用\n")
  cat("biofree.qyKEGGtools installation failed, local KEGG enrichment unavailable\n")
  cat("错误信息 | Error:", conditionMessage(e), "\n")
})

cat("\n")
cat("╔═══════════════════════════════════════════════════════╗\n")
cat("║              安装完成 | Installation Complete          ║\n")
cat("╚═══════════════════════════════════════════════════════╝\n\n")

cat("启动应用 | Launch app:\n")
cat("   source('run.R')\n\n")
