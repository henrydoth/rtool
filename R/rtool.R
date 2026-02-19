#' rtool: Project helpers for navigation and file ops
#'
#' A small set of helpers for navigating project roots (via here::here()),
#' listing, searching, opening, creating, and renaming files.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# =========================================================
# Internal: HTML viewer helper (Viewer pane bên phải)
# =========================================================
.rt_view_html <- function(html, title = "R TOOL – Help") {
  f <- tempfile(paste0(gsub("\\s+", "_", title), "_"), fileext = ".html")

  page <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>", title, "</title>",
    "<style>",
    "body{font-family:Segoe UI,Arial,sans-serif;line-height:1.5;padding:16px;}",
    "h2{margin:0 0 10px 0;} h3{margin:18px 0 8px 0;}",
    "pre{background:#f6f8fa;border:1px solid #e1e4e8;border-radius:10px;",
    "padding:10px;overflow:auto;white-space:pre;}",
    ".ok{background:#eaffea;border:1px solid #b7f0b7;border-radius:10px;padding:10px;}",
    "code{font-family:Consolas,Menlo,Monaco,monospace;}",
    "hr{border:none;border-top:1px solid #eee;margin:16px 0;}",
    "</style></head><body>",
    html,
    "</body></html>"
  )

  # ✅ Ghi UTF-8 chuẩn (Windows + emoji/tiếng Việt)
  page_utf8 <- enc2utf8(page)
  con <- file(f, open = "wb")
  writeBin(charToRaw(page_utf8), con)
  close(con)

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    tryCatch(rstudioapi::viewer(f), error = function(e) browseURL(f))
  } else {
    browseURL(f)
  }

  invisible(f)
}

# =========================================================
# Helpers (internal)
# =========================================================
.norm_path <- function(x) {
  x <- path.expand(x)
  tryCatch(normalizePath(x, winslash = "/", mustWork = FALSE),
           error = function(e) x)
}

.clean_input <- function(x) {
  x <- gsub("`", "", x)
  x <- gsub("\\\\", "/", x)
  x <- sub("/+$", "", x)
  x
}

.as_text <- function(x) {
  if (is.character(x)) return(.clean_input(x))
  .clean_input(deparse(substitute(x), width.cutoff = 500))
}

.is_path_like <- function(x) {
  grepl("/", x) || grepl("^\\./", x) || grepl("^\\.\\\\", x) || grepl("^~", x)
}

.is_glob <- function(x) grepl("[*?]", x)

.split_glob_path <- function(x) {
  x <- .clean_input(x)
  x <- gsub("\\\\", "/", x)

  if (!grepl("/", x)) return(list(dir = ".", glob = x))

  dir  <- sub("/[^/]*$", "", x)
  glob <- sub("^.*/", "", x)
  if (dir == "") dir <- "."
  list(dir = dir, glob = glob)
}

.glob_to_regex <- function(glob) {
  esc <- function(s) gsub("([][{}()+^$.|\\\\])", "\\\\\\1", s)
  g2 <- esc(glob)
  g2 <- gsub("\\*", ".*", g2)
  g2 <- gsub("\\?", ".",  g2)
  paste0("^", g2, "$")
}

.has_here <- function() requireNamespace("here", quietly = TRUE)

.root_dir <- function() {
  if (.has_here()) return(.norm_path(here::here()))
  .norm_path(getwd())
}

.resolve_dir <- function(p) {
  p <- .as_text(p)
  if (identical(p, "") || is.na(p)) p <- "."

  p2 <- .norm_path(p)
  if (dir.exists(p2)) return(p2)

  cand_root <- .norm_path(file.path(.root_dir(), p))
  if (dir.exists(cand_root)) return(cand_root)

  cand_wd <- .norm_path(file.path(getwd(), p))
  if (dir.exists(cand_wd)) return(cand_wd)

  p2
}

.resolve_files <- function(x, path = ".", ignore.case = TRUE, full.names = TRUE) {
  x  <- .as_text(x)
  p0 <- .resolve_dir(path)

  x2 <- .norm_path(x)
  if (file.exists(x2)) return(x2)

  if (.is_path_like(x)) {
    cand_root <- .norm_path(file.path(.root_dir(), x))
    if (file.exists(cand_root)) return(cand_root)

    cand_wd <- .norm_path(file.path(getwd(), x))
    if (file.exists(cand_wd)) return(cand_wd)
  }

  files <- list.files(
    path        = p0,
    pattern     = x,
    recursive   = TRUE,
    ignore.case = ignore.case,
    full.names  = full.names
  )
  if (length(files) == 0) return(NULL)
  files
}

.is_abs_path <- function(x) {
  grepl("^[A-Za-z]:[\\/]", x) || grepl("^/", x) || grepl("^~", x)
}

.pick_dir_by_glob <- function(glob, base_dir) {
  rx <- .glob_to_regex(glob)
  dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  if (length(dirs) == 0) return(character())
  dirs[grepl(rx, basename(dirs), ignore.case = TRUE)]
}

# =========================================================
# ✅ Exported functions
# =========================================================

#' Show current working directory and project root
#' @export
g_w <- function() {
  wd <- getwd()
  root <- .root_dir()
  message("📁 Thư mục dự án (wd):   ", wd)
  message("🏠 Project root (here): ", root)
  invisible(wd)
}

#' Change working directory using project root (here::here()) with glob support
#' @param folder Folder name, glob, relative path, or absolute path. NULL -> go to root.
#' @param ls If TRUE, list directory after changing.
#' @param show If TRUE, print message.
#' @export
c_d <- function(folder = NULL, ls = FALSE, show = TRUE) {

  if (is.null(folder) || identical(.as_text(folder), "")) {
    target <- .root_dir()

  } else {

    folder <- .as_text(folder)

    if (.is_abs_path(folder)) {
      target <- .norm_path(folder)

    } else if (folder %in% c("..", ".", "./", "../") ||
               grepl("^\\.+[\\/]", folder)) {
      target <- .norm_path(file.path(getwd(), folder))

    } else if (.is_glob(folder)) {

      cand <- .pick_dir_by_glob(folder, getwd())
      if (length(cand) == 0) cand <- .pick_dir_by_glob(folder, .root_dir())

      if (length(cand) == 0) {
        message("❌ Không tìm thấy thư mục match: ", folder)
        return(invisible(NULL))
      }

      target <- .norm_path(cand[1])

    } else {
      target <- .norm_path(file.path(.root_dir(), folder))
    }
  }

  if (!dir.exists(target)) {
    message("❌ Thư mục không tồn tại: ", target)
    return(invisible(NULL))
  }

  setwd(target)
  if (show) message("📂 Đã chuyển đến: ", target)

  if (isTRUE(ls)) {
    cat("\n📄 Nội dung thư mục:\n")
    d_ir()
  }

  invisible(target)
}

#' List files/dirs with terminal-like glob support
#' @export
d_ir <- function(path = ".", pattern = NULL,
                 full = FALSE, recursive = FALSE, all.files = FALSE,
                 type = c("all", "file", "dir")) {

  type <- match.arg(type)
  x <- .as_text(path)

  .list_full <- function(p0, patt) {
    list.files(
      path       = p0,
      pattern    = patt,
      full.names = TRUE,
      recursive  = recursive,
      all.files  = all.files
    )
  }

  if (.is_glob(x)) {
    sp <- .split_glob_path(x)
    p0 <- .resolve_dir(sp$dir)
    rx <- .glob_to_regex(sp$glob)
    out_full <- .list_full(p0, rx)
  } else {
    p0 <- .resolve_dir(x)
    out_full <- .list_full(p0, pattern)
  }

  if (type != "all" && length(out_full) > 0) {
    is_dir <- dir.exists(out_full)
    if (type == "dir")  out_full <- out_full[is_dir]
    if (type == "file") out_full <- out_full[!is_dir]
  }

  out <- if (full) out_full else {
    if (length(out_full) == 0) character() else {
      sub(paste0("^", .norm_path(p0), "/?"), "", out_full)
    }
  }

  print(out)
  invisible(out)
}

#' Quick list (alias of d_ir)
#' @export
l_s <- function(path = ".", pattern = NULL,
                full = FALSE, recursive = FALSE, all.files = FALSE,
                type = c("all", "file", "dir")) {
  d_ir(path = path, pattern = pattern,
       full = full, recursive = recursive, all.files = all.files,
       type = type)
}

#' Alias of l_s
#' @export
ls_ <- l_s

#' Find files by regex pattern (recursive)
#' @export
f_ind <- function(pattern, path = ".", ignore.case = TRUE, full.names = TRUE) {
  files <- .resolve_files(pattern, path = path,
                          ignore.case = ignore.case,
                          full.names  = full.names)
  if (is.null(files)) {
    message("❌ Không tìm thấy file chứa: ", .as_text(pattern))
    return(invisible(NULL))
  }
  print(files)
  invisible(files)
}

#' Find files by glob (recursive)
#' @export
g_ind <- function(glob, path = ".", ignore.case = TRUE, full.names = TRUE) {
  g  <- .as_text(glob)
  p0 <- .resolve_dir(path)

  rx <- .glob_to_regex(g)

  files <- list.files(
    path        = p0,
    pattern     = rx,
    recursive   = TRUE,
    ignore.case = ignore.case,
    full.names  = full.names
  )

  if (length(files) == 0) {
    message("❌ Không tìm thấy file theo glob: ", g)
    return(invisible(NULL))
  }

  print(files)
  invisible(files)
}

#' Alias of g_ind
#' @export
g_find <- g_ind

#' Open file(s) in default application
#' @export
o_pen <- function(x, path = ".", ignore.case = TRUE, full.names = TRUE) {
  files <- .resolve_files(x, path = path,
                          ignore.case = ignore.case,
                          full.names  = full.names)
  if (is.null(files)) {
    message("❌ Không tìm thấy file chứa: ", .as_text(x))
    return(invisible(NULL))
  }

  if (.Platform$OS.type == "windows") {
    for (f in files) shell.exec(f)
  } else {
    if (Sys.info()[["sysname"]] == "Darwin") {
      system2("open", files)
    } else {
      for (f in files) system2("xdg-open", f)
    }
  }

  message("📂 Đã mở file.")
  invisible(files)
}

#' Open file(s) in RStudio editor
#' @export
e_dit <- function(x, file = NULL, path = ".", ignore.case = TRUE, full.names = TRUE) {

  if (!is.null(file)) {
    folder <- .as_text(x)
    file   <- .as_text(file)
    x      <- file
    path   <- folder
  } else {
    x <- .as_text(x)
  }

  files <- .resolve_files(x, path = path,
                          ignore.case = ignore.case,
                          full.names  = full.names)

  if (is.null(files)) {
    message("❌ Không tìm thấy file chứa: ", .as_text(x), " (trong: ", .as_text(path), ")")
    return(invisible(NULL))
  }

  files <- files[!dir.exists(files)]
  if (length(files) == 0) {
    message("❌ Bạn đang trỏ tới thư mục, không phải file (check lại folder/file).")
    return(invisible(NULL))
  }

  file.edit(files)
  message("✏️ Đã mở file trong RStudio.")
  invisible(files)
}

#' Show newest files by mtime
#' @export
l_ast <- function(path = ".", n = 10) {
  if (!missing(path) && missing(n) && is.numeric(path)) {
    n <- path
    path <- "."
  }
  p <- .resolve_dir(path)

  files <- list.files(
    path       = p,
    recursive  = TRUE,
    full.names = TRUE
  )

  if (length(files) == 0) {
    message("❌ Thư mục rỗng: ", p)
    return(invisible(character()))
  }

  info <- file.info(files)
  files_sorted <- files[order(info$mtime, decreasing = TRUE)]

  out <- head(files_sorted, n)
  print(out)
  invisible(out)
}

#' Create folder and/or file relative to project root
#' @export
c_dir <- function(folder, file = NULL, recursive = TRUE, overwrite = FALSE, show = TRUE) {
  folder <- .as_text(folder)
  folder_path <- .norm_path(file.path(.root_dir(), folder))

  if (!dir.exists(folder_path)) {
    ok <- dir.create(folder_path, recursive = recursive, showWarnings = FALSE)
    if (ok) {
      if (show) message("📁 Đã tạo folder: ", folder_path)
    } else {
      message("❌ Không tạo được folder: ", folder_path)
      return(invisible(NULL))
    }
  } else {
    if (show) message("📁 Folder đã tồn tại: ", folder_path)
  }

  if (!is.null(file)) {
    file <- .as_text(file)
    file_path <- .norm_path(file.path(folder_path, file))

    if (file.exists(file_path) && !overwrite) {
      message("⚠️ File đã tồn tại (không ghi đè): ", file_path)
      return(invisible(file_path))
    }

    dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
    okf <- file.create(file_path, showWarnings = FALSE)

    if (okf) {
      if (show) message("📄 Đã tạo file: ", file_path)
    } else {
      message("❌ Không tạo được file: ", file_path)
      return(invisible(NULL))
    }

    return(invisible(file_path))
  }

  invisible(folder_path)
}

#' Alias of c_dir (safe name for package docs)
#' @export
dir_ <- function(folder, file = NULL, overwrite = FALSE, show = TRUE) {
  c_dir(folder, file = file, overwrite = overwrite, show = show)
}

# Keep old habit, but DO NOT export/document (_ prefix breaks Rd filename)
`_dir` <- dir_

#' Alias of c_dir (create folder)
#' @export
mkdir <- function(folder, recursive = TRUE, show = TRUE) {
  c_dir(folder, file = NULL, recursive = recursive, show = show)
}

#' Rename file inside a folder relative to project root
#' @export
r_name <- function(folder, old, new, overwrite = FALSE, show = TRUE) {

  folder <- .as_text(folder)
  old    <- .as_text(old)
  new    <- .as_text(new)

  folder_path <- .norm_path(file.path(.root_dir(), folder))
  old_path    <- .norm_path(file.path(folder_path, old))
  new_path    <- .norm_path(file.path(folder_path, new))

  if (!file.exists(old_path)) {
    message("❌ File không tồn tại: ", old_path)
    return(invisible(NULL))
  }

  if (file.exists(new_path) && !overwrite) {
    message("⚠️ File đích đã tồn tại (không ghi đè): ", new_path)
    return(invisible(new_path))
  }

  if (file.exists(new_path) && overwrite) {
    file.remove(new_path)
  }

  ok <- file.rename(old_path, new_path)

  if (ok) {
    if (show) message("🔁 Đã đổi tên: ", old, " → ", new)
    return(invisible(new_path))
  } else {
    message("❌ Không đổi tên được.")
    return(invisible(NULL))
  }
}

#' Show HTML help in RStudio Viewer
#' @export
h_elp <- function() {

  html <- "
  <h2>📌 R TOOL – Quick help + ví dụ</h2>

  <div class='ok'>
    ✅ <b>Lưu ý</b><br>
    - Tool ưu tiên project root theo <code>here::here()</code> (nếu có), fallback <code>getwd()</code><br>
    - Glob có dấu <code>*</code> thì trong R phải dùng dấu nháy:<br>
    <pre>d_ir(\"*.docx\")\ng_ind(\"*bai_bao*\")</pre>
    - <code>d_ir()</code> hỗ trợ glob kiểu terminal:
    <pre>d_ir(\"R/*.R\")\nd_ir(\"source/*.qmd\")\nd_ir(\"*.docx\")</pre>
    - Lọc chỉ folder / chỉ file:
    <pre>d_ir(\".\", type=\"dir\")\nd_ir(\".\", type=\"file\")</pre>
  </div>

  <h3>1) Thư mục làm việc</h3>
  <pre>g_w()</pre>

  <h3>2) Chuyển thư mục</h3>
  <pre>
c_d()                  # về project root
c_d(\"R\")               # vào thư mục R
c_d(\"*hop*\")           # vào folder match glob
c_d(\"..\")              # lên 1 cấp theo wd hiện tại
  </pre>

  <h3>3) Liệt kê / tìm / mở</h3>
  <pre>
d_ir(\"R/*.R\")
g_ind(\"*.docx\")
e_dit(\"R/00_setup.R\")
o_pen(\"*.docx\")
  </pre>

  <h3>4) Tạo / đổi tên</h3>
  <pre>
c_dir(\"r_md\", \"a.txt\")
dir_(\"r_md\", \"a.txt\")     # alias safe
r_name(\"r_md\", \"a.txt\", \"b.txt\")
  </pre>
  "

  .rt_view_html(html, title = "R TOOL – Quick help")
  invisible(NULL)
}
