# Render a hex-style logo_2.png with diagonal line, asterisk, dot, and wordmark
# Output: config/style/logo_2.png and config/style/logo.svg

# Parameters
outfile_png <- "config/style/logo_2.png"
outfile_svg <- "config/style/logo.svg"
width_px <- 1024
height_px <- 1024

# Colors
bg_fill <- "#0f1f3a"     # dark blue background
bg_border <- "#1f2a44"   # border stroke
accent <- "#22d3ee"      # cyan accent
accent2 <- "#38bdf8"
wordmark <- "#e5e7eb"    # near-white
star_col <- "#eab308"    # amber
core_col <- "#fbbf24"    # small core
core_border <- "#f59e0b"

# Reusable draw routine
draw_hex_logo <- function() {
	par(mar = c(0, 0, 0, 0))
	plot.new()
	plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)

	# Compute pointy-top hexagon vertices
	r <- 0.95
	angles_deg <- seq(90, 450, by = 60)  # 90° = top point
	ang <- angles_deg * pi/180
	hx <- r * cos(ang)
	hy <- r * sin(ang)

	# Draw filled hex background
	polygon(hx, hy, col = bg_fill, border = bg_border, lwd = 8, xpd = NA)

	# Optional subtle core
	points(0, 0, pch = 21, cex = 3, bg = core_col, col = core_border, lwd = 3)

	# Diagonal dividing line (slightly tilted)
	segments(x0 = -0.85, y0 = -0.15, x1 = 0.85, y1 = -0.35, col = accent, lwd = 12, lend = "round")

	# Asterisk above the line
	text(x = -0.20, y = 0.20, labels = "*", col = star_col, cex = 6, font = 2)

	# Dot below the line
	points(0.38, -0.62, pch = 16, cex = 2.8, col = accent)

	# Wordmark inside hex (lowercase 'qml')
	text(0, 0.10, labels = "qml", col = wordmark, cex = 6, font = 2)
}

# Render PNG
png(outfile_png, width = width_px, height = height_px, bg = "transparent")
draw_hex_logo()
invisible(dev.off())

# Render SVG (size in inches)
svg(outfile_svg, width = 6, height = 6, bg = "transparent")
draw_hex_logo()
invisible(dev.off())
