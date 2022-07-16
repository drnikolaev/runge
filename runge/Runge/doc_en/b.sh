./tex.sh
rm -R  ~/.local/share/data/cvmlib/runge/*
rm -rf  ~/.local/share/QtProject/Assistant
qcollectiongenerator help_en.qhcp
assistant -collectionFile help_en.qhc

