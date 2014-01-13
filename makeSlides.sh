#!/bin/bash
pandoc --slide-level 1 -f markdown+lhs -t slidy+lhs \
	--self-contained  --indented-code-classes=haskell \
	-s Slides.lhs \
	-o Slides.html
