@echo off
htlatex index_en.tex "html,2,sections+,fonts+,imgdir:images/"
rem htlatex index_en.tex "html,2,fonts+,imgdir:images/"
move /Y *.png images
