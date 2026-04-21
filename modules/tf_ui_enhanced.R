# =====================================================
# TF 模块 UI 增强版（替换 ui_theme.R 中的对应部分）
# 使用方法：
#   将 ui_theme.R 中第 1013-1106 行替换为以下内容
# =====================================================

# 找到 ui_theme.R 中这一行开头：
#   tabPanel("🔬 转录因子活性", icon = icon("dna"),
# 到这一行结束：
#   ),
# （韦恩图 tabPanel 之前）

# 替换为下面的内容：


                   tabPanel("🔬 转录因子活性", icon = icon("dna"),
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                class = "sidebar-panel",

                                # ===== 数据库选择 =====
                                h4("🗄️ 调控网络数据库"),
                                selectInput("tf_database", "选择数据库",
                                            choices = c(
                                              "CollecTRI (文献 curated)" = "collectri",
                                              "DoRothEA (A-D 置信度分级)" = "dorothea"
                                            ),
                                            selected = "collectri"),
                                helpText(class="text-muted small",
                                         "CollecTRI: 整合 ColTRI + DoRothEA + 文献，覆盖较全面 | ",
                                         "DoRothEA: 基于实验证据分级，可筛选高置信度调控关系"),

                                # ===== DoRothEA 置信度筛选（条件显示） =====
                                conditionalPanel(
                                  condition = "input.tf_database == 'dorothea'",
                                  tags$div(
                                    style = "background: rgba(0,122,255,0.05); border-radius: 8px; padding: 10px; margin: 10px 0;",
                                    h5("🛡️ DoRothEA 置信度分级", style="color: #007AFF;"),
                                    checkboxGroupInput("dorothea_confidence", "选择证据等级 (多选)",
                                                       choices = c(
                                                         "A (最高 - 实验验证)" = "A",
                                                         "B (高 - 多种实验支持)" = "B",
                                                         "C (中 - 计算预测)" = "C",
                                                         "D (低 - 单一证据)" = "D"
                                                       ),
                                                       selected = c("A", "B", "C"),
                                                       inline = FALSE),
                                    helpText(class="text-muted small",
                                             "A: 经过正交实验验证 (如 ChIP + KO) | ",
                                             "B: 多种高通量实验支持 | ",
                                             "C: 经过专家审阅的计算预测 | ",
                                             "D: 单一来源证据"),
                                    tags$p("💡 建议: A+B 用于高置信度分析，A-D 用于全面探索",
                                           style="font-size: 11px; color: #6E6E73; font-style: italic;")
                                  )
                                ),

                                tags$hr(),

                                # ===== 算法选择 =====
                                h4("🧬 TF 活性推断"),
                                helpText(class="text-muted", "使用 decoupleR 推断转录因子活性。"),

                                selectInput("tf_method", "推断算法",
                                            choices = c(
                                              "ULM (单变量线性模型)" = "ulm",
                                              "MLM (多变量线性模型)" = "mlm",
                                              "WMEAN (加权平均)" = "wmean",
                                              "WSUM (加权求和)" = "wsum"
                                            ),
                                            selected = "ulm"),
                                helpText(class="text-muted small", "ULM: 快速准确 | MLM: 考虑共调控 | WMEAN/WSUM: 简单加权"),

                                numericInput("tf_min_size", "TF靶基因最小数量", 5, min = 1, step = 1),

                                tags$hr(),
                                actionButton("run_tf_activity", "运行 TF 活性分析", class = "btn-info"),
                                tags$hr(),

                                # ===== 绘图样式 =====
                                h4("🎨 绘图样式"),
                                sliderInput("tf_top_n", "显示 Top N 个TF", 10, 50, 20),
                                colourInput("tf_active_col", "激活颜色", "#e74c3c"),
                                colourInput("tf_inactive_col", "失活颜色", "#3498db"),
                                sliderInput("tf_bar_width", "柱状图宽度", 0.3, 1.0, 0.7, step = 0.05),
                                sliderInput("tf_bar_angle", "X轴标签角度", 0, 90, 45, step = 5),
                                sliderInput("tf_bar_font_size", "柱状图字体大小", 8, 18, 11, step = 0.5),

                                tags$hr(),
                                h5("🎯 网络图自定义"),
                                sliderInput("tf_network_node_size", "节点大小倍数", 0.5, 3, 1, step = 0.1),
                                sliderInput("tf_network_label_size", "标签大小", 2, 6, 3.5, step = 0.5),
                                colourInput("tf_tf_node_col", "TF节点颜色", "#2ecc71"),
                                colourInput("tf_consistent_act_col", "一致-激活节点", "#27ae60"),
                                colourInput("tf_consistent_rep_col", "一致-抑制节点", "#2980b9"),
                                colourInput("tf_inconsistent_act_col", "不一致-激活节点", "#c0392b"),
                                colourInput("tf_inconsistent_rep_col", "不一致-抑制节点", "#2c3e50"),
                                colourInput("tf_neutral_col", "未知节点", "#95a5a6"),

                                tags$hr(),
                                h5("🎯 散点图自定义"),
                                sliderInput("tf_scatter_point_size", "散点大小", 1, 8, 3, step = 0.5),
                                sliderInput("tf_scatter_alpha", "散点透明度", 0.1, 1, 0.7, step = 0.05),
                                selectInput("tf_scatter_point_shape", "散点形状",
                                            choices = c("圆形 (默认)" = 19,
                                                       "实心圆" = 16,
                                                       "实心三角" = 17,
                                                       "实心菱形" = 18,
                                                       "实心方块" = 15,
                                                       "空心圆" = 1,
                                                       "空心三角" = 2),
                                            selected = 19),
                                checkboxInput("tf_scatter_label_repel", "标签防重叠 (ggrepel)", TRUE),
                                sliderInput("tf_scatter_label_size", "标签大小", 1.5, 6, 3, step = 0.25),
                                sliderInput("tf_scatter_n_labels", "显示标签数量", 0, 50, 15, step = 1),
                                selectInput("tf_scatter_legend_pos", "图例位置",
                                            choices = c("右侧 (默认)" = "right",
                                                       "底部" = "bottom",
                                                       "顶部" = "top",
                                                       "左侧" = "left",
                                                       "不显示" = "none"),
                                            selected = "right"),
                                sliderInput("tf_scatter_title_size", "标题大小", 10, 20, 14, step = 1),
                                sliderInput("tf_scatter_axis_size", "坐标轴文字大小", 8, 16, 10, step = 0.5),
                                colourInput("tf_scatter_consis_col", "一致点颜色", "#2ecc71"),
                                colourInput("tf_scatter_incon_col", "不一致点颜色", "#e74c3c"),
                                colourInput("tf_scatter_neutral_col", "未知点颜色", "#95a5a6"),

                                tags$hr(),
                                downloadButton("download_tf_results", "下载 TF 结果表", class="btn-sm btn-light")
                              ),

                              mainPanel(
                                class = "main-panel",

                                # ===== 主结果页签 =====
                                tabsetPanel(
                                  id = "tf_main_tabs",
                                  type = "tabs",

                                  # --- 页签1: TF 活性结果 ---
                                  tabPanel("📊 TF 活性结果",
                                           br(),
                                           fluidRow(
                                             column(12,
                                                    h4("TF 活性变化柱状图 (Treatment vs Control)"),
                                                    plotOutput("tf_activity_bar_plot", height = "600px")
                                             )
                                           ),
                                           tags$hr(),
                                           h4("转录因子活性结果表"),
                                           DT::dataTableOutput("tf_activity_table"),

                                           tags$hr(),
                                           h4("所选 TF 靶基因调控一致性散点图"),
                                           p(class="text-muted", "图中的颜色显示了靶基因的实际差异表达方向与 TF 调控网络预设方向的匹配程度。"),
                                           uiOutput("tf_consistency_summary"),
                                           tags$br(),
                                           plotOutput("tf_target_plot", height = "600px"),

                                           tags$hr(),
                                           h4("所选 TF 靶基因调控网络"),
                                           p(class="text-muted", "显示TF与其靶基因的调控关系。红色=激活，蓝色=抑制，实线=一致，虚线=不一致。"),
                                           plotlyOutput("tf_network_plot_interactive", height = "700px"),
                                           hidden(plotOutput("tf_network_plot_static", height = "700px")),
                                           tags$br(),
                                           checkboxInput("use_static_network", "使用静态图（用于SVG导出）", FALSE),
                                           downloadButton("download_tf_network_svg", "📥 下载网络图 (SVG)", class = "btn-sm btn-primary"),
                                           downloadButton("download_tf_scatter_svg", "📥 下载散点图 (SVG)", class = "btn-sm btn-primary"),
                                           downloadButton("download_tf_scatter_data", "💾 导出靶基因数据 (CSV)", class = "btn-sm btn-success"),

                                           tags$hr(),
                                           h4("所选 TF 的靶基因详细信息"),
                                           p(class="text-muted", "请在上方 '转录因子活性结果表' 中点击一行，查看其靶基因。"),
                                           DT::dataTableOutput("tf_target_table")
                                  ),

                                  # --- 页签2: TF 家族富集分析 🆕 ---
                                  tabPanel("🏠 TF 家族分析",
                                           br(),
                                           fluidRow(
                                             column(12,
                                                    tags$div(
                                                      class = "alert alert-info",
                                                      style = "margin-bottom: 20px;",
                                                      tags$h4("🏠 TF 家族富集分析", style="margin-top: 0;"),
                                                      tags$p("基于 Lambert et al. 2018 (Cell) 的 TF 分类体系，分析哪些 TF 家族在你的实验条件下整体富集或激活。"),
                                                      tags$p("使用 Fisher 精确检验评估每个家族的富集显著性。")
                                                    )
                                             )
                                           ),

                                           # 家族分析参数
                                           fluidRow(
                                             column(3,
                                                    wellPanel(
                                                      h4("🔧 分析参数"),
                                                      numericInput("tf_family_top_n", "前景 TF 数量 (Top N)",
                                                                   value = 20, min = 5, max = 100, step = 5),
                                                      helpText(class="text-muted small",
                                                               "选择活性排序靠前的 N 个 TF 作为前景集"),

                                                      numericInput("tf_family_pvalue", "P 值阈值",
                                                                   value = 0.05, min = 0.001, max = 1, step = 0.01),
                                                      helpText(class="text-muted small",
                                                               "家族富集的显著性阈值"),

                                                      tags$hr(),
                                                      h5("🎨 家族图自定义"),
                                                      sliderInput("tf_family_font_size", "家族图字体大小",
                                                                   value = 11, min = 8, max = 18, step = 0.5),
                                                      sliderInput("tf_family_bar_width", "家族柱状图宽度",
                                                                   value = 0.7, min = 0.3, max = 1.0, step = 0.05),

                                                      tags$hr(),
                                                      actionButton("run_tf_family_enrichment", "🚀 运行家族富集分析",
                                                                   class = "btn-primary", width = "100%"),

                                                      tags$hr(),
                                                      downloadButton("download_tf_family_results",
                                                                     "📥 下载家族富集结果",
                                                                     class = "btn-success btn-sm",
                                                                     style = "width: 100%;")
                                                    )
                                             ),
                                             column(9,
                                                    h4("TF 家族富集结果表"),
                                                    DT::dataTableOutput("tf_family_table"),

                                                    tags$hr(),
                                                    h4("家族富集柱状图"),
                                                    p(class="text-muted", "显示显著富集的 TF 家族及其富集倍数"),
                                                    plotOutput("tf_family_bar_plot", height = "500px"),

                                                    tags$hr(),
                                                    h4("家族富集气泡图"),
                                                    p(class="text-muted", "气泡大小 = 家族 TF 数量，颜色 = 显著性"),
                                                    plotOutput("tf_family_dot_plot", height = "500px"),

                                                    tags$hr(),
                                                    h4("TF 家族平均活性 (Lollipop 图)"),
                                                    p(class="text-muted", "展示各家族的平均 TF 活性分数，揭示哪些家族整体激活或抑制"),
                                                    plotOutput("tf_family_lollipop_plot", height = "600px")
                                             )
                                           )
                                  ),

                                  # --- 页签3: 使用说明 ---
                                  tabPanel("📖 使用说明",
                                           br(),
                                           fluidRow(
                                             column(12,
                                                    tags$div(
                                                      style = "max-width: 900px;",

                                                      tags$h3("🔬 转录因子活性分析使用指南", style="color: #007AFF; margin-bottom: 20px;"),

                                                      tags$h4("1. 数据库选择", style="color: #333; margin-top: 25px;"),
                                                      tags$ul(
                                                        tags$li(tags$strong("CollecTRI"), ": 整合了 ColTRI、DoRothEA 和文献 curated 数据，是目前最全面的 TF-靶基因调控网络。适合全面探索。"),
                                                        tags$li(tags$strong("DoRothEA"), ": 基于实验证据分为 A/B/C/D 四个置信度等级。", tags$strong("推荐 A+B 等级用于高置信度分析"), "。")
                                                      ),

                                                      tags$h4("2. 推断算法", style="color: #333; margin-top: 25px;"),
                                                      tags$ul(
                                                        tags$li(tags$strong("ULM (推荐)"), ": 单变量线性模型，速度快，对大多数数据表现良好。"),
                                                        tags$li(tags$strong("MLM"), ": 多变量线性模型，考虑 TF 间的共调控关系，但容易出现共线性错误。"),
                                                        tags$li(tags$strong("WMEAN"), ": 加权平均，简单直观。"),
                                                        tags$li(tags$strong("WSUM"), ": 加权求和，与 WMEAN 类似。")
                                                      ),

                                                      tags$h4("3. DoRothEA 置信度分级说明", style="color: #333; margin-top: 25px;"),
                                                      tags$table(
                                                        class = "table table-bordered",
                                                        style = "max-width: 700px;",
                                                        tags$thead(
                                                          tags$tr(
                                                            tags$th("等级"), tags$th("证据类型"), tags$th("适用场景")
                                                          )
                                                        ),
                                                        tags$tbody(
                                                          tags$tr(tags$td(tags$span("A", style="color: #e74c3c; font-weight: bold;")), 
                                                                  tags$td("经过正交实验验证（如 ChIP-seq + 基因敲除）"),
                                                                  tags$td("高置信度验证，适合发表")),
                                                          tags$tr(tags$td(tags$span("B", style="color: #e67e22; font-weight: bold;")), 
                                                                  tags$td("多种高通量实验支持"),
                                                                  tags$td("可靠预测，推荐日常使用")),
                                                          tags$tr(tags$td(tags$span("C", style="color: #f39c12; font-weight: bold;")), 
                                                                  tags$td("经过专家审阅的计算预测"),
                                                                  tags$td("探索性分析")),
                                                          tags$tr(tags$td(tags$span("D", style="color: #95a5a6; font-weight: bold;")), 
                                                                  tags$td("单一来源或低通量证据"),
                                                                  tags$td("仅供参考，需谨慎解读"))
                                                        )
                                                      ),

                                                      tags$h4("4. TF 家族富集分析", style="color: #333; margin-top: 25px;"),
                                                      tags$p("TF 家族分析基于 Lambert et al. 2018 (Cell) 的转录因子分类体系，将 ~1,600 个人类 TF 分为 30+ 个家族（如 bZIP、bHLH、Homeobox、Nuclear Receptor 等）。"),
                                                      tags$p("分析流程："),
                                                      tags$ol(
                                                        tags$li("选择 Top N 活性 TF 作为前景集"),
                                                        tags$li("使用 Fisher 精确检验评估每个家族的富集显著性"),
                                                        tags$li("计算富集倍数 = 观察值 / 期望值")
                                                      ),

                                                      tags$h4("5. 结果解读", style="color: #333; margin-top: 25px;"),
                                                      tags$ul(
                                                        tags$li(tags$strong("活性分数 > 0"), ": TF 被预测为激活状态（其靶基因整体上调）"),
                                                        tags$li(tags$strong("活性分数 < 0"), ": TF 被预测为抑制状态（其靶基因整体下调）"),
                                                        tags$li(tags$strong("P 值"), ": 活性分数的统计显著性"),
                                                        tags$li(tags$strong("靶基因一致性"), ": 靶基因的实际变化方向与 TF 调控方向的一致程度")
                                                      ),

                                                      tags$hr(),
                                                      tags$p("📚 参考文献:", style="font-weight: bold; margin-top: 20px;"),
                                                      tags$ul(
                                                        tags$li("Lambert et al. (2018) The Human Transcription Factors. Cell."),
                                                        tags$li("Garcia-Alonso et al. (2019) Benchmark and integration of resources for the estimation of human transcription factor activities. Genome Biology. (DoRothEA)"),
                                                        tags$li("Badia-i-Mompel et al. (2022) decoupleR: Ensemble of computational methods to infer biological activities from omics data. Bioinformatics.")
                                                      )
                                                    )
                                             )
                                           )
                                  )
                                )
                              )
                            )
                   ),
