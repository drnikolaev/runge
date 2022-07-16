./tex.sh
rm ~/Library/Application\ Support/QtProject/Assistant/help_ru.qhc
qcollectiongenerator help_ru.qhcp
~/Qt5.2.0/5.2.0-beta1/clang_64/bin/Assistant.app/Contents/MacOS/Assistant -collectionFile help_ru.qhc

