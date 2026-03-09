# 请在 YuanSeq 项目根目录运行，或修改为你的项目路径
if (file.exists("app.R")) {
  setwd(getwd())
} else if (file.exists("../app.R")) {
  setwd("..")
} else {
  stop("请在 YuanSeq 项目根目录运行，或修改本脚本中的路径")
}
if (file.exists("archive/tools/organize_files_safe.R")) {
  source("archive/tools/organize_files_safe.R")
} else {
  source("organize_files_safe.R")
}
