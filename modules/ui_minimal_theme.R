# =====================================================
# YuanSeq Minimal UI Overlay
# Purpose: keep all existing Shiny functions while making the interface cleaner,
# lighter, and more publication/product oriented.
# =====================================================

minimal_css <- tags$style(HTML("

/* =====================================================
   YuanSeq Minimal Theme Overlay
   ===================================================== */

:root {
  --ys-bg: #f7f8fa;
  --ys-surface: #ffffff;
  --ys-surface-soft: #f3f4f6;
  --ys-border: #e5e7eb;
  --ys-text: #111827;
  --ys-muted: #6b7280;
  --ys-primary: #2563eb;
  --ys-primary-hover: #1d4ed8;
  --ys-success: #059669;
  --ys-warning: #d97706;
  --ys-danger: #dc2626;
  --ys-radius: 10px;
}

body {
  background: var(--ys-bg) !important;
  color: var(--ys-text) !important;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  font-size: 14px !important;
  line-height: 1.5 !important;
}

/* Remove decorative gradients and heavy shadows */
.navbar,
.sidebar-panel,
.main-panel,
.well,
.panel,
.card,
.alert,
.tab-content,
.shiny-output-error {
  box-shadow: none !important;
}

.navbar {
  background: var(--ys-surface) !important;
  border-bottom: 1px solid var(--ys-border) !important;
  min-height: 48px !important;
}

.navbar-brand,
.nav-link {
  color: var(--ys-text) !important;
  font-weight: 500 !important;
}

.nav-tabs {
  border-bottom: 1px solid var(--ys-border) !important;
}

.nav-tabs .nav-link,
.nav-tabs > li > a {
  color: var(--ys-muted) !important;
  border: none !important;
  border-bottom: 2px solid transparent !important;
  background: transparent !important;
  border-radius: 0 !important;
  padding: 10px 12px !important;
  font-weight: 500 !important;
}

.nav-tabs .nav-link.active,
.nav-tabs > li.active > a,
.nav-tabs > li > a.active {
  color: var(--ys-primary) !important;
  border-bottom-color: var(--ys-primary) !important;
  background: transparent !important;
}

.sidebar-panel,
.main-panel {
  background: var(--ys-surface) !important;
  border: 1px solid var(--ys-border) !important;
  border-radius: var(--ys-radius) !important;
  padding: 18px !important;
}

.well,
.well-panel {
  background: var(--ys-surface-soft) !important;
  border: 1px solid var(--ys-border) !important;
  border-radius: 8px !important;
  padding: 14px !important;
}

.tab-pane {
  background: transparent !important;
  border: none !important;
  box-shadow: none !important;
}

h1, h2, h3, h4, h5, h6 {
  color: var(--ys-text) !important;
  font-weight: 600 !important;
  letter-spacing: 0 !important;
}

h4 {
  font-size: 16px !important;
  margin-top: 12px !important;
  margin-bottom: 10px !important;
}

h5 {
  font-size: 14px !important;
  margin-top: 10px !important;
  margin-bottom: 8px !important;
}

.help-block,
.text-muted,
small,
.small {
  color: var(--ys-muted) !important;
}

hr {
  border-top: 1px solid var(--ys-border) !important;
  margin: 14px 0 !important;
}

/* Inputs */
.form-control,
.selectize-input,
input,
select,
textarea {
  background: #ffffff !important;
  border: 1px solid var(--ys-border) !important;
  border-radius: 8px !important;
  color: var(--ys-text) !important;
  box-shadow: none !important;
}

.form-control:focus,
.selectize-input.focus,
input:focus,
select:focus,
textarea:focus {
  border-color: var(--ys-primary) !important;
  box-shadow: 0 0 0 2px rgba(37, 99, 235, 0.12) !important;
}

.control-label,
label {
  color: var(--ys-text) !important;
  font-weight: 500 !important;
  margin-bottom: 4px !important;
}

/* Buttons: reduce color noise and remove decorative lift */
.btn {
  border-radius: 8px !important;
  box-shadow: none !important;
  transform: none !important;
  font-weight: 500 !important;
  transition: background-color 0.12s ease, border-color 0.12s ease, color 0.12s ease !important;
}

.btn:hover {
  transform: none !important;
  box-shadow: none !important;
}

.btn-primary,
.btn-info,
.btn-warning {
  background: var(--ys-primary) !important;
  border: 1px solid var(--ys-primary) !important;
  color: #ffffff !important;
}

.btn-primary:hover,
.btn-info:hover,
.btn-warning:hover {
  background: var(--ys-primary-hover) !important;
  border-color: var(--ys-primary-hover) !important;
}

.btn-success {
  background: var(--ys-success) !important;
  border: 1px solid var(--ys-success) !important;
  color: #ffffff !important;
}

.btn-danger {
  background: var(--ys-danger) !important;
  border: 1px solid var(--ys-danger) !important;
  color: #ffffff !important;
}

.btn-light,
.btn-default,
.btn-outline-primary,
.btn-outline-success,
.btn-outline-info,
.btn-outline-warning,
.btn-outline-danger {
  background: #ffffff !important;
  color: var(--ys-text) !important;
  border: 1px solid var(--ys-border) !important;
}

/* Alerts: lighter, flatter */
.alert {
  border-radius: 8px !important;
  border: 1px solid var(--ys-border) !important;
  background: #ffffff !important;
  color: var(--ys-text) !important;
}

.alert-info,
.alert-secondary {
  background: #f8fafc !important;
}

.alert-warning {
  background: #fffbeb !important;
  border-color: #fde68a !important;
}

.alert-success {
  background: #ecfdf5 !important;
  border-color: #a7f3d0 !important;
}

.alert-danger {
  background: #fef2f2 !important;
  border-color: #fecaca !important;
}

/* Tables */
table.dataTable,
.dataTables_wrapper,
.table {
  color: var(--ys-text) !important;
}

table.dataTable thead th,
.table thead th {
  background: #f9fafb !important;
  border-bottom: 1px solid var(--ys-border) !important;
  color: var(--ys-text) !important;
  font-weight: 600 !important;
}

table.dataTable tbody td,
.table td {
  border-top: 1px solid var(--ys-border) !important;
}

/* Plot and output containers */
.shiny-plot-output,
.plotly,
.html-widget,
.shiny-html-output {
  border-radius: 8px !important;
}

/* Footer */
.xseq-developer-footer {
  background: transparent !important;
  border-top: 1px solid var(--ys-border) !important;
  color: var(--ys-muted) !important;
}

/* Dark mode: flatter and less contrast-heavy */
.dark-mode {
  --ys-bg: #111827;
  --ys-surface: #1f2937;
  --ys-surface-soft: #111827;
  --ys-border: #374151;
  --ys-text: #f9fafb;
  --ys-muted: #d1d5db;
  --ys-primary: #60a5fa;
  --ys-primary-hover: #3b82f6;
  background: var(--ys-bg) !important;
  color: var(--ys-text) !important;
}

.dark-mode .navbar,
.dark-mode .sidebar-panel,
.dark-mode .main-panel,
.dark-mode .well,
.dark-mode .well-panel,
.dark-mode .tab-pane,
.dark-mode .container-fluid,
.dark-mode .form-group,
.dark-mode .panel,
.dark-mode .panel-body {
  background: var(--ys-surface) !important;
  color: var(--ys-text) !important;
  border-color: var(--ys-border) !important;
  box-shadow: none !important;
}

.dark-mode .form-control,
.dark-mode .selectize-input,
.dark-mode input,
.dark-mode select,
.dark-mode textarea {
  background: #111827 !important;
  color: var(--ys-text) !important;
  border-color: var(--ys-border) !important;
}

.dark-mode .alert {
  background: var(--ys-surface) !important;
  color: var(--ys-text) !important;
  border-color: var(--ys-border) !important;
}

.dark-mode table,
.dark-mode th,
.dark-mode td,
.dark-mode table.dataTable tbody td,
.dark-mode table.dataTable thead th {
  background: var(--ys-surface) !important;
  color: var(--ys-text) !important;
  border-color: var(--ys-border) !important;
}

"))

# Add the minimal CSS after the existing theme so it overrides decorative rules.
sci_fi_css <- tagList(sci_fi_css, minimal_css)
