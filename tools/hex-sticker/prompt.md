# Scans sticker artwork

Generated with the built-in image generation tool, then cropped and typeset
with Pillow to match the tempest, graft, and rill stickers. Original square
artwork is preserved in artwork.png. The final PNG has transparent outer corners.

## Generation prompt

Create a polished original illustration for an R package sticker, inspired by the friendly illustrative craft of tidyverse package mascots. SQUARE full-bleed illustration, NOT a hexagon, no border, NO TEXT or letters. Flat vector-like shapes, confident dark outlines, charming expressive animal character, tightly controlled vivid palette, subtle hand-drawn character, excellent clarity at 2 inches. No photorealism, no 3D, no gradients, no drop shadows. Central composition suitable for subsequent point-up regular hexagonal cropping: keep important features within the central 65% width and central 60% height; corners contain only background. Leave lower 22% fairly quiet for a package wordmark to be added later. Character is sophisticated and endearing, not generic clip art.

A charming curious raccoon detective inspecting a short curving trail of exactly three simple dark pawprints with a single large handheld magnifying glass. The raccoon has blue-gray fur, a distinctive dark navy eye mask, warm cream cheeks, round ears, a beautiful curved striped tail and a focused friendly expression. Kneeling, looking down through the magnifying glass at one pawprint, holding the glass with one paw. Clear readable lens with pale aqua glass and a golden-yellow handle. No costume, no hat, no clothes. The trail represents inspecting the steps an AI agent took, rather than merely its final result. Warm soft coral-peach solid background, dark plum-navy outlines, slate blue fur, warm cream, golden yellow accent. Main raccoon face, magnifying glass, and pawprint trail in upper-middle, clean generous shapes. The image should belong beside an illustrated storm petrel in blue swirling wind, a bright tree frog tending a grafted branch, and an otter reading in a teal stream. Keep the background opaque and uniform all the way to all four edges.

## Render

Requires Python, Pillow, and Avenir Next Bold on macOS. From the package root:

`python3 tools/hex-sticker/render.py tools/hex-sticker/artwork.png scans man/figures/hex-sticker.png --ink '#20354b'`

Outputs a 1600 by 1848 pixel sticker and a 400 by 462 pixel README logo.

## Website icons

Run `python3 tools/hex-sticker/export-favicons.py` to regenerate pkgdown icons from `man/figures/logo.png`. The SVG favicon embeds raster artwork; it is not a vector master.
