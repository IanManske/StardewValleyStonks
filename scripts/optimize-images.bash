#!/bin/env bash

find public/static/img/Crops public/static/img/Items -name '*.png' | xargs -I {} magick mogrify -type PaletteAlpha -sample 400% {}
find public/static/img/Crops public/static/img/Items -name '*.png' | xargs -I {} bash -c 'magick -define webp:lossless=true "$0" "${0%.png}.webp"' {}
rm public/static/img/**/*.png
