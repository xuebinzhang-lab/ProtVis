# globalVariables ---------------------------------------------------------
utils::globalVariables(
  c(
    # MaxQuant related column names
    "Only identified by site",
    "Reverse",
    "Potential contaminant",
    "Protein IDs",
    "ID",

    # Data processing related column names
    "sample_id",
    "sample_group",
    "value_fix",
    "intensity",
    "group",
    "maxquant_id",
    "Value",

    # Special symbols
    ".",

    # Other variables that may need to be declared
    "rowname"
  )
)

# icon --------------------------------------------------------------------

enrichment_bubble_icon <- HTML('
<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
  <rect x="2" y="2" width="20" height="20" rx="1" stroke="#333333" stroke-width="1.5" fill="none"/>
  <path d="M6 10H18M6 14H18M6 18H18M10 6V18M14 6V18" stroke="#cccccc" stroke-width="0.5"/>
  <circle cx="10" cy="10" r="2.5" fill="#666666" fill-opacity="0.3" stroke="#333333" stroke-width="1"/>
  <circle cx="16" cy="14" r="3.5" fill="#666666" fill-opacity="0.5" stroke="#333333" stroke-width="1"/>
  <circle cx="12" cy="16" r="2" fill="#666666" fill-opacity="0.4" stroke="#333333" stroke-width="1"/>
  <circle cx="8" cy="14" r="1.5" fill="#666666" fill-opacity="0.2" stroke="#333333" stroke-width="1"/>
</svg>
')

correlation_icon <- HTML('<svg viewBox="0 0 16 16" width="20" height="20" fill="currentColor">
        <rect x="2" y="2" width="12" height="12" stroke="currentColor" fill="none"/>
        <!-- Points below the line -->
        <circle cx="4" cy="12" r="1"/>
        <circle cx="5" cy="10.5" r="1"/>
        <circle cx="7" cy="9" r="1"/>
        <circle cx="9" cy="7.5" r="1"/>
        <circle cx="11" cy="6" r="1"/>
        <!-- Points above the line -->
        <circle cx="4.5" cy="9" r="1"/>
        <circle cx="6" cy="7.5" r="1"/>
        <circle cx="8" cy="5.5" r="1"/>
        <circle cx="10" cy="4" r="1"/>
        <circle cx="12" cy="2.5" r="1"/>
        <!-- Diagonal line -->
        <line x1="3" y1="13" x2="13" y2="3" stroke="currentColor" stroke-width="1.5"/>
    </svg>')

expression_pattern_icon <- HTML('<svg viewBox="0 0 16 16" width="20" height="20">
        <!-- 4x4 heatmap grid with varying grayscale -->
        <rect x="1" y="1" width="3" height="3" fill="#000000"/>
        <rect x="5" y="1" width="3" height="3" fill="#333333"/>
        <rect x="9" y="1" width="3" height="3" fill="#666666"/>
        <rect x="13" y="1" width="3" height="3" fill="#999999"/>

        <rect x="1" y="5" width="3" height="3" fill="#333333"/>
        <rect x="5" y="5" width="3" height="3" fill="#666666"/>
        <rect x="9" y="5" width="3" height="3" fill="#999999"/>
        <rect x="13" y="5" width="3" height="3" fill="#cccccc"/>

        <rect x="1" y="9" width="3" height="3" fill="#666666"/>
        <rect x="5" y="9" width="3" height="3" fill="#999999"/>
        <rect x="9" y="9" width="3" height="3" fill="#cccccc"/>
        <rect x="13" y="9" width="3" height="3" fill="#eeeeee"/>

        <rect x="1" y="13" width="3" height="3" fill="#999999"/>
        <rect x="5" y="13" width="3" height="3" fill="#cccccc"/>
        <rect x="9" y="13" width="3" height="3" fill="#eeeeee"/>
        <rect x="13" y="13" width="3" height="3" fill="#ffffff"/>
    </svg>')

dimensionality_reduction_icon <- HTML('<svg viewBox="0 0 16 16" width="20" height="20" fill="currentColor">
        <!-- Square outline -->
        <rect x="2" y="2" width="12" height="12" stroke="currentColor" fill="none"/>

        <!-- Left vertical ellipse with 3 points (taller than wide) -->
        <ellipse cx="4.5" cy="8" rx="1.5" ry="3" stroke="currentColor" fill="none"/>
        <circle cx="4" cy="6.5" r="0.8"/>  <!-- Top point -->
        <circle cx="4.5" cy="8" r="0.8"/>   <!-- Center point -->
        <circle cx="5" cy="9.5" r="0.8"/>   <!-- Bottom point -->

        <!-- Right vertical ellipse with 3 points (spaced farther) -->
        <ellipse cx="11.5" cy="8" rx="1.5" ry="3" stroke="currentColor" fill="none"/>
        <circle cx="11" cy="6.5" r="0.8"/>  <!-- Top point -->
        <circle cx="11.5" cy="8" r="0.8"/>  <!-- Center point -->
        <circle cx="12" cy="9.5" r="0.8"/>   <!-- Bottom point -->
    </svg>')

imputation_settings_icon <- HTML('<svg viewBox="0 0 24 24" width="24" height="24" fill="currentColor">
        <!-- Left square with NA -->
        <rect x="3" y="8" width="8" height="8" stroke="currentColor" stroke-width="1.2" fill="none"/>
        <text x="7" y="12" font-size="5" font-weight="bold" text-anchor="middle" dominant-baseline="middle">NA</text>

        <!-- Right square with num -->
        <rect x="13" y="8" width="8" height="8" stroke="currentColor" stroke-width="1.2" fill="none"/>
        <text x="17" y="12" font-size="4" font-weight="bold" text-anchor="middle" dominant-baseline="middle">num</text>
    </svg>')

gear_icon <- HTML('
<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
  <circle cx="12" cy="12" r="8" stroke="currentColor" stroke-width="1.5" fill="none"/>
  <path d="M12 4V2M12 22v-2M20 12h2M4 12h2" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
  <path d="M17.5 6.5L19 5M5 19l1.5-1.5M17.5 17.5L19 19M5 5l1.5 1.5" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
  <path d="M6.5 17.5L5 19M19 5l-1.5 1.5M6.5 6.5L5 5M19 19l-1.5-1.5" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
  <circle cx="12" cy="12" r="5" stroke="currentColor" stroke-width="1.5" stroke-dasharray="1.5,1.5" fill="none"/>
  <path d="M12 8L12 6M12 18l0 2M8 12H6M18 12h2" stroke="currentColor" stroke-width="1" stroke-linecap="round"/>
</svg>
')

extract_icon <- HTML('
<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
  <rect x="4" y="4" width="12" height="16" rx="1" stroke="currentColor" stroke-width="1.5"/>
  <path d="M16 12H22M22 12L19 9M22 12L19 15" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
  <path d="M15 5H12V8" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
</svg>
')

toolbox <- HTML('
<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
  <rect x="5" y="7" width="14" height="12" rx="1.5" stroke="currentColor" stroke-width="1.5"/>
  <path d="M5 9H19" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
  <path d="M11 7V5M13 7V5" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
  <path d="M11 5H13" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
  <circle cx="16" cy="8" r="0.75" fill="currentColor"/>
</svg>
')

download_icon <- HTML('
<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
  <rect x="6" y="4" width="12" height="16" rx="1" stroke="currentColor" stroke-width="1.5"/>
  <path d="M12 14V8M12 14L9 11M12 14L15 11" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
  <path d="M9 8H15" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
</svg>
')
volcano_icon <- HTML('
<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
  <!-- 坐标轴 -->
  <path d="M5 19L19 19" stroke="#333333" stroke-width="1.5" stroke-linecap="round"/>
  <path d="M5 19L5 5" stroke="#333333" stroke-width="1.5" stroke-linecap="round"/>

  <!-- 坐标轴箭头 -->
  <path d="M20 19L18 17M20 19L18 21" stroke="#333333" stroke-width="1.5" stroke-linecap="round"/>
  <path d="M5 4L3 6M5 4L7 6" stroke="#333333" stroke-width="1.5" stroke-linecap="round"/>

  <!-- 坐标轴标签 -->
  <text x="22" y="20" font-size="3" text-anchor="start" fill="#333333">x</text>
  <text x="4" y="3" font-size="3" text-anchor="start" fill="#333333">y</text>

  <!-- 火山图散点 -->
  <!-- 左侧：下调点（蓝色） -->
  <circle cx="8" cy="12" r="1" fill="#3498db" fill-opacity="0.8" stroke="#2980b9" stroke-width="0.5"/>
  <circle cx="6" cy="14" r="0.8" fill="#3498db" fill-opacity="0.8" stroke="#2980b9" stroke-width="0.5"/>
  <circle cx="7" cy="10" r="0.6" fill="#3498db" fill-opacity="0.6" stroke="#2980b9" stroke-width="0.5"/>
  <circle cx="5" cy="16" r="1.2" fill="#3498db" fill-opacity="0.9" stroke="#2980b9" stroke-width="0.5"/>
  <circle cx="9" cy="8" r="0.5" fill="#3498db" fill-opacity="0.5" stroke="#2980b9" stroke-width="0.5"/>

  <!-- 中间：不显著点（灰色） -->
  <circle cx="12" cy="10" r="0.4" fill="#95a5a6" fill-opacity="0.4" stroke="#7f8c8d" stroke-width="0.5"/>
  <circle cx="10" cy="12" r="0.5" fill="#95a5a6" fill-opacity="0.4" stroke="#7f8c8d" stroke-width="0.5"/>
  <circle cx="14" cy="8" r="0.3" fill="#95a5a6" fill-opacity="0.4" stroke="#7f8c8d" stroke-width="0.5"/>
  <circle cx="13" cy="14" r="0.6" fill="#95a5a6" fill-opacity="0.4" stroke="#7f8c8d" stroke-width="0.5"/>
  <circle cx="11" cy="16" r="0.4" fill="#95a5a6" fill-opacity="0.4" stroke="#7f8c8d" stroke-width="0.5"/>

  <!-- 右侧：上调点（红色） -->
  <circle cx="16" cy="6" r="1.5" fill="#e74c3c" fill-opacity="0.9" stroke="#c0392b" stroke-width="0.5"/>
  <circle cx="18" cy="8" r="1.3" fill="#e74c3c" fill-opacity="0.9" stroke="#c0392b" stroke-width="0.5"/>
  <circle cx="14" cy="4" r="1" fill="#e74c3c" fill-opacity="0.8" stroke="#c0392b" stroke-width="0.5"/>
  <circle cx="20" cy="10" r="0.8" fill="#e74c3c" fill-opacity="0.7" stroke="#c0392b" stroke-width="0.5"/>
  <circle cx="16" cy="3" r="1.8" fill="#e74c3c" fill-opacity="0.9" stroke="#c0392b" stroke-width="0.5"/>

  <!-- 阈值线 -->
  <path d="M5 9L19 9" stroke="#2ecc71" stroke-width="0.8" stroke-dasharray="2 2"/>
  <path d="M5 15L19 15" stroke="#2ecc71" stroke-width="0.8" stroke-dasharray="2 2"/>
</svg>
')
