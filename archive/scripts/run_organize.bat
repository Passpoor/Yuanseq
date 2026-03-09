@echo off
cd /d "%~dp0"
cd ..\..
if not exist app.R (echo 请在 YuanSeq 项目根目录运行 & pause & exit /b 1)
Rscript.exe archive\tools\organize_files_safe.R
pause
