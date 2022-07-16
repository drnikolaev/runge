./tex.sh
rm ~/Library/Application\ Support/QtProject/Assistant/help_en.qhc
qcollectiongenerator help_en.qhcp
~/Qt5.2.0/5.2.0-beta1/clang_64/bin/Assistant.app/Contents/MacOS/Assistant -collectionFile help_en.qhc

