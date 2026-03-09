@echo off
echo ========================================
echo YuanSeq 应用启动器
echo ========================================
echo.

REM 检查R是否安装
where R >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo 错误: 未找到R语言环境
    echo 请安装R并添加到系统PATH
    pause
    exit /b 1
)

echo 正在启动 YuanSeq 应用...
echo.

REM 切换到脚本所在目录（项目根目录）
cd /d "%~dp0"

REM 启动R并运行启动脚本
Rscript.exe launch_app.R

pause
