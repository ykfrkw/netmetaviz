# =============================================================================
# netmetaviz - Sample script using W2I data
#
# netmetaviz.Rproj を RStudio で開いた状態で実行する。
# devtools::load_all() でパッケージを読み込んでから各関数を実行する。
#
# 出力ファイルはプロジェクトのルートディレクトリに保存される。
#
# アウトカム一覧:
#   remission_lt  : 長期フォローアップ時の寛解 (OR; 高いほど良)
#   dropout_lt    : 長期フォローアップ時の脱落 (OR; 低いほど良)
#   remission_pt  : post-treatment 時の寛解    (OR; 高いほど良)
#   dropout_pt    : post-treatment 時の脱落    (OR; 低いほど良)
#
# CINeMA データは remission_lt のみ利用可能。
# =============================================================================

# 0. Setup --------------------------------------------------------------------
devtools::load_all(reset = TRUE)

# 1. Load sample data and build netmeta objects --------------------------------

## 1a. Raw trial data and CINeMA report
w2i <- load_w2i()

head(w2i$trials)
#>            id               t   n  r n_dropout r_pt n_dropout_pt rob indirectness
#> 1   Gross2011           CBT-I  20  9         3   12            2   L            1
#> 2   Gross2011 Pharmacotherapy  10  4         2    5            1   L            1
#> ...

w2i$cinema   # CINeMA ratings (remission_lt のみ)

## 1b. Build netmeta objects for all 4 outcomes
net_lt  <- build_w2i_netmeta("remission_lt")   # 長期 remission
net_dlt <- build_w2i_netmeta("dropout_lt")     # 長期 dropout
net_pt  <- build_w2i_netmeta("remission_pt")   # PT remission
net_dpt <- build_w2i_netmeta("dropout_pt")     # PT dropout

# CINeMA CSV パス (remission_lt のみ)
cinema_path <- system.file("extdata", "w2i_cinema.csv", package = "netmetaviz")


# 2. color_league() — League table with CINeMA coloring ----------------------
#
#   remission_lt のみ CINeMA あり。
#   セルの背景色 = CINeMA 信頼度評価。
#
#   ★ デュアルアウトカム（x2 引数）
#   x2 を指定すると 1 枚の表に 2 アウトカムを収録できる:
#     下三角（左下）= x  のアウトカム（label1 で注記）
#     上三角（右上）= x2 のアウトカム（label2 で注記）
#   cinema / cinema2 で各アウトカムの CINeMA を個別指定。

## アルファベット順（デフォルト・pastel パレット）
color_league(
  x       = net_lt,
  cinema  = cinema_path,
  sort_by = "alphabet",
  file    = "color_league_alphabet.xlsx"
)

## P-score 順
color_league(
  x       = net_lt,
  cinema  = cinema_path,
  sort_by = "pscore",
  file    = "color_league_pscore.xlsx"
)

## 並び順を手動指定 (sort_by = "custom", sort_order = c(...))
color_league(
  x          = net_lt,
  cinema     = cinema_path,
  sort_by    = "custom",
  sort_order = c("CBT-I", "Combination", "Pharmacotherapy"),
  file       = "color_league_custom.xlsx"
)

## Classic パレット（濃い背景＋白文字、論文でよく使われる配色）
color_league(
  x            = net_lt,
  cinema       = cinema_path,
  palette_type = "classic",
  file         = "color_league_classic.xlsx"
)

## Colorblind-safe パレット（Okabe-Ito ベース）
color_league(
  x            = net_lt,
  cinema       = cinema_path,
  palette_type = "colorblind",
  file         = "color_league_colorblind.xlsx"
)

## 直接リスト渡し（後方互換）
color_league(
  x       = net_lt,
  cinema  = cinema_path,
  palette = cinema_palette("classic"),
  file    = "color_league_compat.xlsx"
)


## 2b. デュアルアウトカム（下三角 = remission_lt、上三角 = dropout_lt）
#
#   下三角: net_lt （長期寛解、CINeMA あり → pastel 色）
#   上三角: net_dlt（長期脱落、CINeMA なし → 色なし）
#   表の下に "↙ Lower-left: ..." / "↗ Upper-right: ..." の注記が書かれる。

color_league(
  x      = net_lt,
  cinema = cinema_path,
  x2     = net_dlt,
  label1 = "Remission (long-term)",
  label2 = "Dropout (long-term)",
  sort_by = "pscore",
  file   = "color_league_dual_lt.xlsx"
)

## post-treatment 版（下三角 = remission_pt、上三角 = dropout_pt）
color_league(
  x      = net_pt,
  x2     = net_dpt,
  label1 = "Remission (post-tx)",
  label2 = "Dropout (post-tx)",
  sort_by = "pscore",
  file   = "color_league_dual_pt.xlsx"
)


## 2c. クアッドアウトカム — 1シートに4アウトカムを上下二分割で収録
#
#   各セルが上下 2 サブ行に分割される（Excel 行数 = 2n）。
#     下三角・上半 (top)    : x  = net_lt （長期寛解、CINeMA あり → pastel 色）
#     下三角・下半 (bottom) : x3 = net_dlt（長期脱落、CINeMA なし → 色なし）
#     上三角・上半 (top)    : x2 = net_pt （短期寛解、CINeMA なし → 色なし）
#     上三角・下半 (bottom) : x4 = net_dpt（短期脱落、CINeMA なし → 色なし）
#   対角セルは 2 行を結合して治療名を表示。
#   表の下に 4 行の注記が書かれる。

color_league(
  x      = net_lt,
  cinema = cinema_path,
  x2     = net_pt,
  x3     = net_dlt,
  x4     = net_dpt,
  label1 = "Remission (long-term)",
  label2 = "Remission (post-tx)",
  label3 = "Dropout (long-term)",
  label4 = "Dropout (post-tx)",
  sort_by = "pscore",
  file   = "color_league_quad.xlsx"
)


## 2d. color_league_multi() — 4アウトカムを1ファイルにまとめる
#
#   outcomes: 名前付きリスト（名前 → Excelシート名になる）
#   cinema:   名前付きリスト（cinema あるアウトカムのみパスを指定、ない場合は NULL）
#             スカラー NULL を渡すとすべてのシートで CINeMA なし。
#
#   ★ remission_lt のみ CINeMA あり。他3アウトカムは cinema = NULL → 色なし。

## 基本（pastel / p-score 順）
color_league_multi(
  outcomes = list(
    "Remission (LT)"  = net_lt,
    "Dropout (LT)"    = net_dlt,
    "Remission (PT)"  = net_pt,
    "Dropout (PT)"    = net_dpt
  ),
  cinema  = list(
    "Remission (LT)"  = cinema_path,
    "Dropout (LT)"    = NULL,
    "Remission (PT)"  = NULL,
    "Dropout (PT)"    = NULL
  ),
  sort_by = "pscore",
  file    = "color_league_4outcomes.xlsx"
)

## Classic パレット
color_league_multi(
  outcomes = list(
    "Remission (LT)"  = net_lt,
    "Dropout (LT)"    = net_dlt,
    "Remission (PT)"  = net_pt,
    "Dropout (PT)"    = net_dpt
  ),
  cinema       = list(
    "Remission (LT)"  = cinema_path,
    "Dropout (LT)"    = NULL,
    "Remission (PT)"  = NULL,
    "Dropout (PT)"    = NULL
  ),
  sort_by      = "pscore",
  palette_type = "classic",
  file         = "color_league_4outcomes_classic.xlsx"
)


## 2e. SchneiderThoma2026 パレット（CINeMA 不要・trivial_range 必須）
#
#   各セルを CI と trivial_range の位置関係で4色に分類する。
#     Blue   : CI 全体が trivial 範囲内（臨床的に trivial）
#     Yellow : te が trivial を超えるが CI が trivial と重複
#     Orange : te と CI が trivial を完全に超える（有益または有害方向に有意）
#     White  : null 付近（CI が trivial 範囲内に収まらず、上記にも該当しない）
#   cinema = は不要（指定しても無視される）。
#   trivial_range は log スケールで指定（OR の場合）。

color_league(
  x             = net_lt,
  sort_by       = "pscore",
  palette_type  = "SchneiderThoma2026",
  trivial_range = log(c(1/1.1, 1.1)),
  file          = "color_league_st2026.xlsx"
)

## デュアルアウトカム × SchneiderThoma2026
color_league(
  x             = net_lt,
  x2            = net_dlt,
  label1        = "Remission (long-term)",
  label2        = "Dropout (long-term)",
  sort_by       = "pscore",
  palette_type  = "SchneiderThoma2026",
  trivial_range = log(c(1/1.1, 1.1)),
  file          = "color_league_dual_st2026.xlsx"
)


## 2f. Solid fill — 全セルを1色で塗りつぶす（CINeMA なし）
#
#   CINeMA もエビデンス方向も関係なく、すべてのオフ対角セルを同じ背景色にする。
#   使用例: 複数のサブグループ表で色を統一したい場合、
#           または表の「枠」として色を使いたい場合。
#   fill_color   : 下三角（outcome 1）のセル色
#   fill_color2  : 上三角（outcome 2）のセル色（デュアルモード時）

color_league(
  x            = net_lt,
  sort_by      = "pscore",
  palette_type = "solid",
  fill_color   = "#E2EFDA",     # 薄いグリーン
  file         = "color_league_solid.xlsx"
)

## デュアルアウトカム × solid fill（下三角と上三角を別色）
color_league(
  x            = net_lt,
  x2           = net_dlt,
  label1       = "Remission (long-term)",
  label2       = "Dropout (long-term)",
  sort_by      = "pscore",
  palette_type = "solid",
  fill_color   = "#E2EFDA",     # 下三角: 薄いグリーン（寛解）
  fill_color2  = "#FCE4D6",     # 上三角: 薄いオレンジ（脱落）
  file         = "color_league_dual_solid.xlsx"
)


# 3. color_netgraph() — Network graph with CINeMA edge coloring ---------------
#
#   エッジ（直接比較）を CINeMA 信頼度評価の色で描画する。
#   デフォルト: ノードサイズ = 参加者数（x$data の n1/n2 列から自動計算）。
#   remission_lt のみ CINeMA あり。CINeMA のない比較は col_no_cinema (grey60) で表示。
#
#   その他のデフォルト設定:
#     plastic = FALSE, pch = 21, col.points = "black", bg.points = "gray"
#     thickness = "number.of.studies"
#     number.of.studies = TRUE, pos.number.of.studies = 0.45

## デフォルト（pastel パレット、ノードサイズ = N）
color_netgraph(
  x      = net_lt,
  cinema = cinema_path
)

## Classic パレット（ノードサイズを固定値に上書き）
color_netgraph(
  x            = net_lt,
  cinema       = cinema_path,
  palette_type = "classic"
)

## CINeMA なし（全エッジを同一色、ノードサイズは N のまま）
color_netgraph(
  x             = net_lt,
  col_no_cinema = "steelblue"
)


# 4. kilim() — Kilim plot: 4アウトカム同時表示 --------------------------------
#
#   各行 = 治療、各列 = アウトカム。
#   palette = "GrRd" (default) / "GrYlRd"  : signed p-value による連続グラデーション
#     セル色 = 有効方向の signed p-value (緑 = 有益、赤 = 有害、青 = trivial)
#   palette = "SchneiderThoma2026": CI と trivial_range の関係による4値カテゴリ色
#     Blue   : CI 全体が trivial 範囲内（臨床的に trivial）
#     Yellow : te が trivial を超えるが CI が trivial と重複
#     Orange : te と CI が trivial を完全に超える（有益または有害）
#     White  : null（OR = 1 / MD = 0）付近
#     ★ SchneiderThoma2026 では trivial_range 必須
#   dropout は small_values = "desirable"（脱落は少ないほど良い）。

## 4a. 基本（trivial_range なし）
kilim(
  outcomes = list(
    list(
      x            = net_lt,
      name         = "remission_lt",
      reference    = "Pharmacotherapy",
      small_values = "undesirable",
      label        = "Remission\n(long-term)",
      digits       = 2
    ),
    list(
      x            = net_dlt,
      name         = "dropout_lt",
      reference    = "Pharmacotherapy",
      small_values = "desirable",
      label        = "Dropout\n(long-term)",
      digits       = 2
    ),
    list(
      x            = net_pt,
      name         = "remission_pt",
      reference    = "Pharmacotherapy",
      small_values = "undesirable",
      label        = "Remission\n(post-tx)",
      digits       = 2
    ),
    list(
      x            = net_dpt,
      name         = "dropout_pt",
      reference    = "Pharmacotherapy",
      small_values = "desirable",
      label        = "Dropout\n(post-tx)",
      digits       = 2
    )
  ),
  sort_by = "pscore",
  file    = "kilim_4outcomes.xlsx"
)

## 4b. trivial_range 指定（OR 0.91–1.10 → 青）
#
#   trivial_range は log スケールで指定（OR/RR/HR の場合）。
#   log(1/1.1) ≈ -0.095, log(1.1) ≈ 0.095
kilim(
  outcomes = list(
    list(
      x            = net_lt,
      name         = "remission_lt",
      reference    = "Pharmacotherapy",
      small_values = "undesirable",
      label        = "Remission\n(long-term)",
      digits       = 2
    ),
    list(
      x            = net_dlt,
      name         = "dropout_lt",
      reference    = "Pharmacotherapy",
      small_values = "desirable",
      label        = "Dropout\n(long-term)",
      digits       = 2
    ),
    list(
      x            = net_pt,
      name         = "remission_pt",
      reference    = "Pharmacotherapy",
      small_values = "undesirable",
      label        = "Remission\n(post-tx)",
      digits       = 2
    ),
    list(
      x            = net_dpt,
      name         = "dropout_pt",
      reference    = "Pharmacotherapy",
      small_values = "desirable",
      label        = "Dropout\n(post-tx)",
      digits       = 2
    )
  ),
  trivial_range = log(c(1/5, 5)),   # OR 0.91–1.10 を trivial とみなす
  sort_by = "pscore",
  file    = "kilim_4outcomes_trivial.xlsx"
)

## 4c. SchneiderThoma2026 パレット（trivial_range 必須）
#
#   CI と trivial_range の関係に基づいてセルを4色に分類する。
#   trivial_range は log スケールで指定（OR の場合）。
kilim(
  outcomes = list(
    list(
      x            = net_lt,
      name         = "remission_lt",
      reference    = "Pharmacotherapy",
      small_values = "undesirable",
      label        = "Remission\n(long-term)",
      digits       = 2
    ),
    list(
      x            = net_dlt,
      name         = "dropout_lt",
      reference    = "Pharmacotherapy",
      small_values = "desirable",
      label        = "Dropout\n(long-term)",
      digits       = 2
    ),
    list(
      x            = net_pt,
      name         = "remission_pt",
      reference    = "Pharmacotherapy",
      small_values = "undesirable",
      label        = "Remission\n(post-tx)",
      digits       = 2
    ),
    list(
      x            = net_dpt,
      name         = "dropout_pt",
      reference    = "Pharmacotherapy",
      small_values = "desirable",
      label        = "Dropout\n(post-tx)",
      digits       = 2
    )
  ),
  trivial_range = log(c(1/1.1, 1.1)),
  palette = "SchneiderThoma2026",
  sort_by = "pscore",
  file    = "kilim_4outcomes_st2026.xlsx"
)

## 4d. アウトカムごとに異なる trivial_range を設定（per-outcome 上書き）
kilim(
  outcomes = list(
    list(
      x             = net_lt,
      name          = "remission_lt",
      reference     = "Pharmacotherapy",
      small_values  = "undesirable",
      label         = "Remission\n(long-term)",
      digits        = 2,
      trivial_range = log(c(1/2, 2))   # このアウトカムのみ OR 0.83–1.20
    ),
    list(
      x            = net_dlt,
      name         = "dropout_lt",
      reference    = "Pharmacotherapy",
      small_values = "desirable",
      label        = "Dropout\n(long-term)",
      digits       = 2
      # trivial_range 未指定 → トップレベルの trivial_range を使用
    )
  ),
  trivial_range = log(c(1/1.1, 1.1)),
  sort_by = "alphabet",
  file    = "kilim_trivial_peroutcome.xlsx"
)


# 5. vitruvian() — Vitruvian plot (per-treatment, multi-outcome spider plot) ------
#
#   治療ごとに1枚のチャート。各スポーク = 1アウトカム。
#   バー高さ = 絶対リスク（%）、バー色 = p値による連続グラデーション。
#   Reference治療は灰青色で表示。青丸 = reference治療の値をマーク。
#
#   引数構造は kilim() と同様（outcomes リスト）。
#
#   ★ 連続アウトカム（SMD / MD）の対応
#   sm = "SMD" または "MD" の netmeta オブジェクトに cer（数値）を指定すると、
#   SMD 近似式 lnOR = π/√3 × SMD（Cox & Snell 1989）でORに変換し、
#   バイナリアウトカムと同じ絶対リスクスケールで表示する。
#     SM = "SMD" : te を直接 SMD として使用
#     SM = "MD"  : SMD = MD / pooled_sd に換算してから変換
#                  pooled_sd は seTE + n1/n2 から自動推定（未指定時）
#   cer を指定しない場合はMD/SMDの生値（変換なし）で表示する。
#
#   ★ 表示サイズの固定について
#   file = NULL かつ interactive() の場合、width/height で指定したサイズの
#   PNG を tempfile に保存して RStudio Viewer（またはシステムビューア）に
#   表示する。これにより Zoom ウィンドウのリサイズでレイアウトが変わる問題を
#   回避できる。ファイルに保存する場合は file = "xxx.png" を指定する。
#
#   ★ ラベル背景・グループ色分けについて
#   各スポーク（アウトカム）の外側リングは group ごとに色分けされる。
#   group を指定すると同じ group のアウトカムが同色になり、グループ名
#   （arc ラベル）が外側リングの中央付近に表示される。
#   内側のグラフ領域は透明（背景色なし）。

## 5a. 基本（4アウトカム × 3治療）— file = NULL → Viewer に固定サイズで表示
#
# cer: Pharmacotherapy の論文値（Pharmacotherapy reference arm event rate）
#   remission_lt  28%  long-term remission
#   dropout_lt    39%  long-term dropout
#   remission_pt  28%  post-treatment remission
#   dropout_pt    16%  post-treatment dropout
#
# cer を指定しない場合（または cer = "metaprop"）は meta::metaprop()（GLMM）で
# reference arm 事象率を自動推定。単純平均は cer = "simple"。
# 論文値と一致させる場合は数値を明示的に指定する（以下の例）。
#
# width / height は省略すると ncol と治療数から自動算出（1パネル = 4×4 インチ）。
# 例: 3治療 × ncol=3 → 1行 → width = 3*4+1.8 = 13.8, height = 1*4 = 4

vitruvian(
  outcomes = list(
    list(
      x            = net_lt,
      name         = "remission_lt",
      label        = "Remission",
      small_values = "undesirable",
      cer          = 0.28,
      group        = "Long-term"
    ),
    list(
      x            = net_dlt,
      name         = "dropout_lt",
      label        = "Dropout",
      small_values = "desirable",
      cer          = 0.39,
      group        = "Long-term"
    ),
    list(
      x            = net_pt,
      name         = "remission_pt",
      label        = "Remission",
      small_values = "undesirable",
      cer          = 0.28,
      group        = "Post-treatment"
    ),
    list(
      x            = net_dpt,
      name         = "dropout_pt",
      label        = "Dropout",
      small_values = "desirable",
      cer          = 0.16,
      group        = "Post-treatment"
    )
  ),
  reference = "Pharmacotherapy",
  digits    = 1,
  ncol      = 3
  # width / height 省略 → 自動算出
)

## 5b. trivial_range 付き（OR 0.91–1.10 → 青）— PNG ファイルに保存
vitruvian(
  outcomes = list(
    list(x = net_lt,  name = "remission_lt", label = "Remission\n(long-term)",
         small_values = "undesirable", cer = 0.28, group = "Long-term"),
    list(x = net_dlt, name = "dropout_lt",   label = "Dropout\n(long-term)",
         small_values = "desirable",  cer = 0.39, group = "Long-term"),
    list(x = net_pt,  name = "remission_pt", label = "Remission\n(post-tx)",
         small_values = "undesirable", cer = 0.28, group = "Post-treatment"),
    list(x = net_dpt, name = "dropout_pt",   label = "Dropout\n(post-tx)",
         small_values = "desirable",  cer = 0.16, group = "Post-treatment")
  ),
  reference     = "Pharmacotherapy",
  trivial_range = log(c(1/1.1, 1.1)),
  digits        = 1,
  ncol          = 3,
  file          = "vitruvian_4outcomes_trivial.png"
  # width / height 省略 → 自動算出
)

## 5c. PNG 保存（group なし・trivial なし）
vitruvian(
  outcomes = list(
    list(x = net_lt,  name = "remission_lt", label = "Remission\n(long-term)",
         small_values = "undesirable", cer = 0.28),
    list(x = net_dlt, name = "dropout_lt",   label = "Dropout\n(long-term)",
         small_values = "desirable",  cer = 0.39),
    list(x = net_pt,  name = "remission_pt", label = "Remission\n(post-tx)",
         small_values = "undesirable", cer = 0.28),
    list(x = net_dpt, name = "dropout_pt",   label = "Dropout\n(post-tx)",
         small_values = "desirable",  cer = 0.16)
  ),
  reference = "Pharmacotherapy",
  digits    = 1,
  ncol      = 3,
  file      = "vitruvian_4outcomes.png"
  # width / height 省略 → 自動算出
)


## 5d. 連続アウトカム（SMD / MD）を含む混合例（インターフェース例示 — 実行不可）
#
#   バイナリアウトカムと SMD / MD アウトカムを同一チャートに配置できる。
#   SMD : cer を数値で指定 → lnOR = π/√3 × SMD で絶対リスクに変換
#   MD  : cer + pooled_sd（省略可・自動推定）で変換
#         pooled_sd を省略すると netmeta$data の seTE / n1 / n2 から自動推定する。
#
#   ★★ W2I データには連続アウトカムが含まれないため以下はそのまま実行できない。
#      net_smd / net_md を実際の sm = "SMD" / "MD" の netmeta オブジェクトに
#      差し替えてから使用すること。

## SMD アウトカム — pooled_sd 不要（te が既に SMD スケール）
# vitruvian(
#   outcomes = list(
#     list(
#       x            = net_lt,     # バイナリアウトカム（OR）
#       name         = "remission_lt",
#       label        = "Remission",
#       small_values = "undesirable",
#       cer          = 0.28,
#       group        = "Binary"
#     ),
#     list(
#       x            = net_smd,    # ★ 実際の SMD netmeta オブジェクトに差し替え
#       name         = "sleep_quality_smd",
#       label        = "Sleep quality\n(SMD)",
#       small_values = "desirable",
#       cer          = 0.30,       # 参照群のベースライン睡眠障害率
#       group        = "Continuous"
#     )
#   ),
#   reference = "Pharmacotherapy",
#   digits    = 1,
#   ncol      = 2,
#   file      = "vitruvian_mixed_smd.png",
#   width     = 10,
#   height    = 5
# )

## MD アウトカム — pooled_sd を手動指定
# vitruvian(
#   outcomes = list(
#     list(
#       x            = net_md,     # ★ 実際の MD netmeta オブジェクトに差し替え
#       name         = "isi_md",
#       label        = "ISI (MD)",
#       small_values = "desirable",
#       cer          = 0.30,       # 参照群のベースライン率
#       pooled_sd    = 5.2         # プールSD（省略すると seTE/n1/n2 から自動推定）
#     )
#   ),
#   reference = "Pharmacotherapy",
#   digits    = 1,
#   ncol      = 1,
#   file      = "vitruvian_md.png",
#   width     = 5,
#   height    = 5
# )

## MD アウトカム — pooled_sd を自動推定（netmeta$data に seTE/n1/n2 が必要）
# vitruvian(
#   outcomes = list(
#     list(
#       x            = net_md,     # ★ 実際の MD netmeta オブジェクトに差し替え
#       name         = "isi_md",
#       label        = "ISI (MD)",
#       small_values = "desirable",
#       cer          = 0.30        # pooled_sd 省略 → 自動推定
#     )
#   ),
#   reference = "Pharmacotherapy",
#   digits    = 1,
#   ncol      = 1,
#   file      = "vitruvian_md_auto.png",
#   width     = 5,
#   height    = 5
# )


# 6. min_context() — Minimally contextualised evidence framework --------------
#
#   治療をエビデンスグループに分類:
#     Group  0 : reference と有意差なし
#     Group +1 : reference より有意に良好
#     Group +2 : Group +1 内の全治療より有意に良好
#     Group -1 : reference より有意に不良
#
#   n_thresholds = c(100, 400) により n_quality 列も自動計算（デフォルト有効）。
#   CINeMA は remission_lt のみ利用可能。

## 6a. Long-term remission: CINeMA + N 品質
mc_lt <- min_context(
  x            = net_lt,
  cinema       = cinema_path,
  reference    = "Pharmacotherapy",
  small_values = "undesirable"
)
print(mc_lt)
#>         treatment group cinema n_total n_quality
#> 1           CBT-I     1   High     ...      High
#> 2     Combination     0   ...      ...      ...
#> 3 Pharmacotherapy     0   <NA>     ...      ...

## 6b. Dropout (long-term): n_quality のみ（CINeMA なし）
mc_dlt <- min_context(
  x            = net_dlt,
  reference    = "Pharmacotherapy",
  small_values = "desirable"
)
print(mc_dlt)

## 6c. n_thresholds を変更（例: 50 / 200）
mc_lt_custom <- min_context(
  x            = net_lt,
  cinema       = cinema_path,
  n_thresholds = c(50, 200),
  reference    = "Pharmacotherapy",
  small_values = "undesirable"
)
print(mc_lt_custom)

## 6d. n_quality 列を省略（n_thresholds = NULL）
mc_lt_noN <- min_context(
  x            = net_lt,
  cinema       = cinema_path,
  n_thresholds = NULL,
  reference    = "Pharmacotherapy",
  small_values = "undesirable"
)
print(mc_lt_noN)

## 6e. Post-treatment outcomes
mc_pt <- min_context(
  x            = net_pt,
  reference    = "Pharmacotherapy",
  small_values = "undesirable"
)
mc_dpt <- min_context(
  x            = net_dpt,
  reference    = "Pharmacotherapy",
  small_values = "desirable"
)

## 6f. table_min_context(): Group × CINeMA（デフォルト）
tbl_cinema <- table_min_context(mc_lt)
print(tbl_cinema)

table_min_context(mc_lt, file = "min_context_remission_lt.docx")
table_min_context(mc_lt, file = "min_context_remission_lt.xlsx")

## 6g. table_min_context(): Group × N 品質
tbl_n <- table_min_context(mc_lt, quality_col = "n_quality")
print(tbl_n)

table_min_context(mc_lt, quality_col = "n_quality",
                  file = "min_context_remission_lt_nquality.xlsx")


## 6h. table_min_context_multi(): 複数アウトカムを横断した一覧表
#
#   行 = アウトカム、列 = Group +2, +1, 0, -1, -2, ...
#   cinema・n_quality の区別は行わない。
#   セル内の治療名は sep で区切られる（デフォルト: 改行 "\n"）。
#   mc_lt, mc_dlt, mc_pt, mc_dpt はすべて上の 6b/6e で作成済み。

# data.frame として確認
tbl_multi <- table_min_context_multi(
  outcome_list = list(
    "Remission (long-term)"  = mc_lt,
    "Dropout (long-term)"    = mc_dlt,
    "Remission (post-tx)"    = mc_pt,
    "Dropout (post-tx)"      = mc_dpt
  )
)
print(tbl_multi)
#>                  Outcome Group +1   Group 0
#> 1  Remission (long-term)   CBT-I  Combination\nPharmacotherapy
#> ...

# カンマ区切りで出力（Excel 向け）
table_min_context_multi(
  outcome_list = list(
    "Remission (long-term)"  = mc_lt,
    "Dropout (long-term)"    = mc_dlt,
    "Remission (post-tx)"    = mc_pt,
    "Dropout (post-tx)"      = mc_dpt
  ),
  sep  = ", ",
  file = "min_context_multi.xlsx"
)

# docx 出力（縦置き横長レイアウト）
table_min_context_multi(
  outcome_list = list(
    "Remission (long-term)"  = mc_lt,
    "Dropout (long-term)"    = mc_dlt,
    "Remission (post-tx)"    = mc_pt,
    "Dropout (post-tx)"      = mc_dpt
  ),
  sep  = "\n",
  file = "min_context_multi.docx"
)


# 7. part_context() — Partially contextualised evidence framework -------------
#
#   効果量を絶対スケールに変換し、ユーザー定義の thresholds（切点）で分類する。
#   min_context() との違い: グループ境界が「統計的有意差」ではなく「臨床的重要差」。
#
#   thresholds の仕組み（例: thresholds = c(lo, hi)）:
#     Group -1 : abs_effect < lo
#     Group  0 : lo ≤ abs_effect < hi  ← findInterval(0, thresholds) のビンが Group 0
#     Group +1 : abs_effect ≥ hi
#
#   abs_effect の単位:
#     binary (OR/RR) : ARD（absolute risk difference、参照群との絶対リスク差）
#     continuous (MD/SMD) : raw MD または SMD
#
#   small_values = "desirable" の場合、direction が -1 になるため
#   abs_effect が負（参照より少ない）ほど高いグループ番号になる。

## 7a. Long-term remission: SWD = ARD 10%（2段階分類）
#   thresholds = c(0.10)
#     Group  0 : ARD < 0.10（SWD 未達）
#     Group +1 : ARD ≥ 0.10（SWD 達成）
pc_lt <- part_context(
  x            = net_lt,
  reference    = "Pharmacotherapy",
  thresholds   = c(0.12),         # ARD 10% をカットオフに
  cer          = 0.28,            # Pharmacotherapy の寛解率 28%
  small_values = "undesirable"
)
print(pc_lt)
#>         treatment abs_effect group cinema n_total n_quality
#> 1 Pharmacotherapy 0.00000000     0   <NA>     ...      ...
#> 2           CBT-I 0.12...        1   High     ...      High
#> 3     Combination 0.07...        0   ...      ...      ...
#
# グループラベルの確認:
attr(pc_lt, "threshold_labels")

## 7b. 3段階分類（有害 / 同等 / 有益）
#   thresholds = c(-0.05, 0.10)
#     Group -1 : ARD < -0.05   （有害方向に >5%）
#     Group  0 : -0.05 ≤ ARD < 0.10（同等〜軽微）
#     Group +1 : ARD ≥ 0.10   （SWD 達成）
pc_lt_3cat <- part_context(
  x            = net_lt,
  reference    = "Pharmacotherapy",
  thresholds   = c(-0.05, 0.10),
  cer          = 0.28,
  small_values = "undesirable"
)
print(pc_lt_3cat)

## 7c. Near SWD バンド（SWD 直前を中間グループに）
#   thresholds = c(0.07, 0.10)
#     Group  0 : ARD < 0.07 （SWD 未達）
#     Group +1 : 0.07 ≤ ARD < 0.10（SWD 付近）
#     Group +2 : ARD ≥ 0.10 （SWD 達成）
pc_lt_near <- part_context(
  x            = net_lt,
  reference    = "Pharmacotherapy",
  thresholds   = c(0.07, 0.10),
  cer          = 0.28,
  small_values = "undesirable"
)
print(pc_lt_near)

## 7d. Long-term dropout: small_values = "desirable"（脱落は少ないほど良い）
#   thresholds = c(-0.10, 0.10)
#   abs_effect = 参照群との ARD（正 = 脱落増加、負 = 脱落減少）
#   direction = -1（desirable）→ abs_effect が負ほど有益
#     Group +1 : ARD < -0.10  （脱落が 10%以上低減 = 有益）
#     Group  0 : -0.10 ≤ ARD < 0.10（差なし）
#     Group -1 : ARD ≥ 0.10  （脱落が 10%以上増加 = 有害）
pc_dlt <- part_context(
  x            = net_dlt,
  reference    = "Pharmacotherapy",
  thresholds   = c(-0.10, 0.10),
  cer          = 0.39,
  small_values = "desirable"
)
print(pc_dlt)

## 7e. CINeMA + N 品質を同時に付与
pc_lt_full <- part_context(
  x            = net_lt,
  reference    = "Pharmacotherapy",
  thresholds   = c(0.10),
  cer          = 0.28,
  small_values = "undesirable",
  cinema       = cinema_path,
  n_thresholds = c(100, 400)
)
print(pc_lt_full)

## 7f. N 品質なし（n_thresholds = NULL）
pc_lt_noN <- part_context(
  x            = net_lt,
  reference    = "Pharmacotherapy",
  thresholds   = c(0.10),
  cer          = 0.28,
  small_values = "undesirable",
  n_thresholds = NULL
)
print(pc_lt_noN)

# =============================================================================
# End of sample.R
# =============================================================================
