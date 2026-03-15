=============================================================================
# generate_hex.R — parallax hex sticker generator
#
# Prerequisites:
#   install.packages(c("hexSticker", "showtext", "sysfonts", "ggplot2"))
#
# Usage:
#   source("generate_hex.R")
#
# Output:
#   man/figures/logo.png  — web (72 DPI)
#   man/figures/logo.svg  — scalable source
#   branding/sticker.png  — print (300 DPI)
# =============================================================================

library(hexSticker)
library(ggplot2)
library(showtext)
# 
# ── Load fonts ───────────────────────────────────────────────────────────────
# JetBrains Mono for the package name (matches occulus typography)
font_add_google("JetBrains Mono", "jetbrains")
showtext_auto()

# ── Colors ───────────────────────────────────────────────────────────────────
bg_color      <- "#1B2631"   # dark navy-charcoal
border_color  <- "#1ABC9C"   # teal
text_color    <- "#FFFFFF"   # white

# ── Build the inner plot ─────────────────────────────────────────────────────
# Two offset point cloud terrain profiles (teal + coral)

set.seed(42)

# Generate terrain profile as sine superposition
terrain_y <- function(x) {
  80 * sin(x * 0.012 + 1.2) +
  40 * sin(x * 0.025 + 0.8) +
  20 * sin(x * 0.05 + 2.5) +
  10 * sin(x * 0.11 + 0.3)
}

# Generate dots along the terrain surface
generate_dots <- function(x_offset = 0, n_surface = 3, n_sub = 1, n_above = 0.3) {
  xs <- seq(40, 560, by = 2)
  dots <- data.frame(x = numeric(), y = numeric())

  for (xval in xs) {
    base_y <- terrain_y(xval + x_offset)

    # Surface dots
    for (i in seq_len(sample(2:n_surface, 1))) {
      dots <- rbind(dots, data.frame(
        x = xval + rnorm(1, 0, 2.5),
        y = base_y + rnorm(1, 0, 4)
      ))
    }

    # Subsurface
    for (i in seq_len(sample(0:n_sub, 1))) {
      dots <- rbind(dots, data.frame(
        x = xval + rnorm(1, 0, 3),
        y = base_y - abs(rnorm(1, 8, 6))
      ))
    }

    # Above
    if (runif(1) < n_above) {
      dots <- rbind(dots, data.frame(
        x = xval + rnorm(1, 0, 4),
        y = base_y + runif(1, 5, 25)
      ))
    }
  }

  dots
}

# Generate both profiles
offset <- 40  # parallax shift in x units

dots_teal  <- generate_dots(x_offset = 0)
dots_coral <- generate_dots(x_offset = 0)
dots_coral$x <- dots_coral$x + offset  # apply parallax shift

dots_teal$group  <- "teal"
dots_coral$group <- "coral"

# Detect overlaps for blend coloring
dots_coral$blend <- FALSE
for (i in seq_len(nrow(dots_coral))) {
  dists <- sqrt((dots_teal$x - dots_coral$x[i])^2 +
                (dots_teal$y - dots_coral$y[i])^2)
  if (any(dists < 8)) {
    dots_coral$blend[i] <- TRUE
  }
}

# Build ggplot (no axes, no background — just the dots)
p <- ggplot() +
  # Teal profile
  geom_point(
    data = dots_teal,
    aes(x = x, y = y),
    color = "#1ABC9C",
    alpha = 0.7,
    size = 0.4,
    shape = 16
  ) +
  # Coral profile (non-overlap)
  geom_point(
    data = dots_coral[!dots_coral$blend, ],
    aes(x = x, y = y),
    color = "#E67E22",
    alpha = 0.6,
    size = 0.4,
    shape = 16
  ) +
  # Blend dots (overlap region)
  geom_point(
    data = dots_coral[dots_coral$blend, ],
    aes(x = x, y = y),
    color = "#F0E6D2",
    alpha = 0.9,
    size = 0.5,
    shape = 16
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  coord_cartesian(xlim = c(0, 600), ylim = c(-60, 120))

# ── Generate hex sticker ─────────────────────────────────────────────────────

sticker(
  # Inner plot
  subplot = p,
  s_x = 1.0,          # subplot x position (centered)
  s_y = 1.05,         # subplot y position (slightly above center for text room)
  s_width = 1.6,      # subplot width relative to hex
  s_height = 1.0,     # subplot height relative to hex


  # Package name
  package = "parallax",
  p_x = 1.0,
  p_y = 0.45,         # below the icon
  p_size = 16,
  p_color = text_color,
  p_family = "jetbrains",
  p_fontface = "bold",

  # Hex shape
  h_fill = bg_color,
  h_color = border_color,
  h_size = 1.5,       # border thickness

  # Output — web resolution
  filename = "man/figures/logo.png",
  dpi = 300
)

message("Created: man/figures/logo.png")

# ── Also save print version ──────────────────────────────────────────────────
sticker(
  subplot = p,
  s_x = 1.0, s_y = 1.05, s_width = 1.6, s_height = 1.0,
  package = "parallax",
  p_x = 1.0, p_y = 0.45, p_size = 17,
  p_color = text_color, p_family = "jetbrains", p_fontface = "bold",
  h_fill = bg_color, h_color = border_color, h_size = 1.5,
  filename = "branding/sticker.png",
  dpi = 600  # print quality for die-cut stickers
)

message("Created: branding/sticker.png")
message("Done! Place logo.png in man/figures/ for pkgdown and README.")
