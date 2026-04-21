# YuanSeq TF 模块 v2.0 升级指南

## 新增功能概览

| 功能 | 说明 |
|------|------|
| **DoRothEA 数据库** | 支持 A/B/C/D 四级置信度筛选，提升结果可靠性 |
| **数据库切换** | CollecTRI ↔ DoRothEA 自由切换 |
| **TF 家族富集分析** | 基于 Lambert et al. 2018 的 30+ 家族分类 |
| **家族可视化** | 柱状图、气泡图、Lollipop 图 |
| **保留全部原有功能** | 靶基因网络、散点图、SVG 导出等 |

---

## 安装步骤

### 1. 备份原文件

```bash
# 进入你的 YuanSeq 项目目录
cd /path/to/YuanSeq

# 备份原模块
cp modules/tf_activity.R modules/tf_activity.R.bak

# 备份 UI 文件
cp modules/ui_theme.R modules/ui_theme.R.bak
```

### 2. 替换 Server 模块

将 `modules/tf_activity_enhanced.R` 中的全部内容复制到 `modules/tf_activity.R`，**替换原有内容**。

```bash
cp /path/to/tf_activity_enhanced.R modules/tf_activity.R
```

### 3. 替换 UI 定义

打开 `modules/ui_theme.R`，找到**转录因子活性**的 `tabPanel` 定义：

- **查找起点**：`tabPanel("🔬 转录因子活性", icon = icon("dna"),`
- **查找终点**：紧接着的 `tabPanel("🔷 韦恩图"` 之前的 `),`

将 `tf_ui_enhanced.R` 中 `=== 替换为下面的内容：===` 之后的所有内容粘贴进去，替换原有 UI 定义。

> 提示：原 UI 大约从第 1013 行开始到第 1106 行结束（韦恩图 tabPanel 之前）。

### 4. 确保依赖包已安装

启动 R 并运行：

```r
# decoupleR 必须已安装（原有依赖）
if (!require("decoupleR")) install.packages("decoupleR")

# 以下包应该都已存在（YuanSeq 原有依赖）
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(tidyr)
```

### 5. 启动应用

```bash
Rscript app.R
# 或
R -e "shiny::runApp('.', launch.browser=TRUE)"
```

---

## 新增功能详细说明

### DoRothEA 数据库 + 置信度分级

#### 数据库对比

| 特性 | CollecTRI | DoRothEA |
|------|-----------|----------|
| 数据来源 | ColTRI + DoRothEA + 文献 | 多种实验 + 计算预测 |
| 置信度分级 | 无 | A/B/C/D 四级 |
| 覆盖范围 | ~2,500 TFs | ~1,500 TFs |
| 适用场景 | 全面探索 | 高置信度验证 |

#### DoRothEA 置信度等级

| 等级 | 颜色 | 证据类型 | 推荐使用场景 |
|------|------|----------|-------------|
| **A** | 红色 | 正交实验验证 (ChIP + KO) | 发表级高置信度分析 |
| **B** | 橙色 | 多种高通量实验支持 | 日常分析推荐 |
| **C** | 黄色 | 专家审阅的计算预测 | 探索性分析 |
| **D** | 灰色 | 单一来源证据 | 仅供参考 |

> **建议**：日常使用选择 A+B+C；发表时选择 A+B 进行高置信度验证。

---

### TF 家族富集分析

#### 分析原理

1. **前景集**：从 TF 活性推断结果中，选择 Top N 活性 TF（按 |score| 排序）
2. **背景集**：所有被成功推断的 TF
3. **统计方法**：Fisher 精确检验（超几何检验的精确版本）
4. **校正**：Benjamini-Hochberg (BH) 多重检验校正

#### 包含的 TF 家族（基于 Lambert et al. 2018 Cell）

| 家族类别 | 代表成员 | 生物学功能 |
|----------|----------|-----------|
| **bZIP** | JUN, FOS, ATF, CREB | 应激响应、细胞增殖 |
| **bHLH** | MYC, MYOD, NEUROD | 细胞分化、神经发育 |
| **Homeobox** | HOX, PAX, LHX | 胚胎发育、模式形成 |
| **Nuclear_Receptor** | ESR, PPARG, RAR | 激素响应、代谢调控 |
| **ETS** | ETS1, SPI1, ERG | 造血、血管生成 |
| **Forkhead** | FOXA, FOXO, FOXP | 代谢、免疫、发育 |
| **C2H2_ZF** | SP, KLF, WT1 | 广泛转录调控 |
| **HMG** | SOX, TCF, LEF | 干细胞、WNT 信号 |
| **STAT** | STAT1-6 | JAK-STAT 信号通路 |
| **GATA** | GATA1-6 | 造血、心血管发育 |
| **SMAD** | SMAD1-9 | TGF-beta 信号 |
| **IRF** | IRF1-9 | 干扰素响应、免疫 |
| **NFKB** | RELA, NFKB1 | 炎症、免疫响应 |
| **E2F** | E2F1-8 | 细胞周期调控 |
| **P53** | TP53, TP63, TP73 | 肿瘤抑制、DNA 损伤响应 |
| **T_Box** | TBX1-22 | 心脏发育、肢体形成 |
| **MADS** | MEF2, SRF | 肌细胞分化 |

#### 可视化图表

| 图表 | 说明 |
|------|------|
| **家族富集柱状图** | 显示显著富集家族的富集倍数 |
| **家族富集气泡图** | 气泡大小 = 家族 TF 数量，颜色 = 显著性 |
| **Lollipop 图** | 各家族平均 TF 活性分数，直观展示激活/抑制趋势 |

---

## 使用流程示例

### 场景1：高置信度 TF 分析（推荐发表）

1. 数据输入 → 差异分析（完成）
2. 进入 **转录因子活性** 模块
3. **数据库选择** → `DoRothEA (A-D 置信度分级)`
4. **置信度等级** → 仅勾选 `A` 和 `B`
5. **推断算法** → `ULM (推荐)`
6. 点击 **运行 TF 活性分析**
7. 切换到 **TF 家族分析** 页签
8. 设置 **前景 TF 数量** = 20，点击 **运行家族富集分析**

### 场景2：全面探索性分析

1. 数据输入 → 差异分析（完成）
2. **数据库选择** → `CollecTRI`
3. **推断算法** → `ULM`
4. 点击 **运行 TF 活性分析**
5. 查看活性柱状图 + 靶基因网络
6. 切换到 **TF 家族分析** 进行家族层面解读

---

## 常见问题 (FAQ)

**Q: DoRothEA 数据库第一次使用需要多久下载？**
A: 取决于网络状况，通常 10-30 秒。下载后会自动缓存到本地 `.rds` 文件，后续直接使用。

**Q: 为什么 DoRothEA 的 TF 数量比 CollecTRI 少？**
A: DoRothEA 主要覆盖经过实验验证的调控关系（~1,500 TFs），而 CollecTRI 整合了更多来源（~2,500 TFs）。建议先用 CollecTRI 全面探索，再用 DoRothEA A+B 验证。

**Q: TF 家族分析中 "前景 TF 数量" 应该如何设置？**
A: 默认 20 个适合大多数场景。如果差异表达基因较多 (>1000 DEGs)，可增加到 30-50；如果较少 (<100 DEGs)，可减少到 10-15。

**Q: 小鼠数据也支持 TF 家族分析吗？**
A: 支持！家族映射表已内置小鼠 ortholog，会自动根据选择的物种切换。

**Q: 升级后原有功能会受影响吗？**
A: 不会。所有原有功能（靶基因网络、散点图、SVG 导出、一致性统计等）均完整保留。

---

## 文件清单

| 文件 | 说明 | 操作 |
|------|------|------|
| `modules/tf_activity_enhanced.R` | 增强版 server 模块 | **替换** `modules/tf_activity.R` |
| `modules/tf_ui_enhanced.R` | 增强版 UI 定义 | **替换** `modules/ui_theme.R` 中对应部分 |
| `INSTALL_GUIDE.md` | 本安装说明 | 参考 |

---

## 技术参考

1. **Lambert et al. (2018)** The Human Transcription Factors. *Cell* 172(4):650-665.
   - 人类 ~1,600 个 TF 的分类体系

2. **Garcia-Alonso et al. (2019)** Benchmark and integration of resources for the estimation of human transcription factor activities. *Genome Biology* 20:240.
   - DoRothEA 数据库及置信度分级

3. **Badia-i-Mompel et al. (2022)** decoupleR: Ensemble of computational methods to infer biological activities from omics data. *Bioinformatics* 38(22):5174-5176.
   - decoupleR 算法框架

4. **CollecTRI**: https://saezlab.github.io/CollecTRI/
   - 整合型 TF 调控网络数据库
