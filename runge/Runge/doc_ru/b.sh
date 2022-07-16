./tex.sh
rm -R  ~/.local/share/data/cvmlib/runge/*
rm -rf  ~/.local/share/QtProject/Assistant
rm -rf  ~/.local/share/data/Trolltech/Assistant
qcollectiongenerator help_ru.qhcp
assistant -collectionFile help_ru.qhc

