cp Runge.proMac Runge.pro
#cp runge_mac.ui runge.ui
#cp drawer_mac.ui drawer.ui
#cp drawersettings_mac.ui drawersettings.ui
#cp exporttoimage_mac.ui exporttoimage.ui
#cp printto_mac.ui printto.ui


pushd debug
rm -rf Runge.app
popd

make clean
qmake -spec macx-g++
make

pushd debug/Runge.app/Contents/MacOS

cp ../../../../../../lib64/libcvm_em64t_ilp64.dylib .
cp ../../../../../../lib64/libdes_engl_em64t_ilp64.dylib .
cp ../../../../../../lib64/libdes_lou_em64t_ilp64.dylib .
cp ../../../../../../lib64/libdes_ros_em64t_ilp64.dylib .

cp ../../../../../../_distr/1.0/configs/macos64/Runge.xml .
cp ../../../../../../_distr/1.0/help_en/help_en.qch .
cp ../../../../../../_distr/1.0/help_en/help_en.qhc .
cp ../../../../../../_distr/1.0/help_ru/help_ru.qch .
cp ../../../../../../_distr/1.0/help_ru/help_ru.qhc .
cp ../../../../../../runge/Runge/runge_ru.qm .

cp -rf /Developer/Tools/Qt/Assistant.app .

#install_name_tool -change ../../lib64/libcvm_em64t_ilp64.dylib @executable_path/libcvm_em64t_ilp64.dylib libcfun_em64t_ilp64.dylib 
install_name_tool -change ../../lib64/libcvm_em64t_ilp64.dylib @executable_path/libcvm_em64t_ilp64.dylib libdes_engl_em64t_ilp64.dylib 
install_name_tool -change ../../lib64/libcvm_em64t_ilp64.dylib @executable_path/libcvm_em64t_ilp64.dylib libdes_lou_em64t_ilp64.dylib 
install_name_tool -change ../../lib64/libcvm_em64t_ilp64.dylib @executable_path/libcvm_em64t_ilp64.dylib libdes_ros_em64t_ilp64.dylib 
install_name_tool -change ../../lib64/libcvm_em64t_ilp64.dylib @executable_path/libcvm_em64t_ilp64.dylib Runge
#install_name_tool -change ../../lib64/libcfun_em64t_ilp64.dylib @executable_path/libcfun_em64t_ilp64.dylib Runge

install_name_tool -change QtWebKit.framework/Versions/4/QtWebKit @executable_path/../Frameworks/QtWebKit.framework/Versions/4/QtWebKit Assistant.app/Contents/MacOS/Assistant 
install_name_tool -change QtNetwork.framework/Versions/4/QtNetwork @executable_path/../Frameworks/QtNetwork.framework/Versions/4/QtNetwork Assistant.app/Contents/MacOS/Assistant
install_name_tool -change QtGui.framework/Versions/4/QtGui @executable_path/../Frameworks/QtGui.framework/Versions/4/QtGui Assistant.app/Contents/MacOS/Assistant 
install_name_tool -change QtCore.framework/Versions/4/QtCore @executable_path/../Frameworks/QtCore.framework/Versions/4/QtCore Assistant.app/Contents/MacOS/Assistant 
install_name_tool -change QtSql.framework/Versions/4/QtSql @executable_path/../Frameworks/QtSql.framework/Versions/4/QtSql Assistant.app/Contents/MacOS/Assistant 
install_name_tool -change QtHelp.framework/Versions/4/QtHelp @executable_path/../Frameworks/QtHelp.framework/Versions/4/QtHelp Assistant.app/Contents/MacOS/Assistant

popd
pushd debug/Runge.app/Contents/MacOS/Assistant.app/Contents/
ln -Fs ../../../PlugIns/ Plugins
popd

pushd debug
/Developer/Tools/Qt/macdeployqt Runge.app/ 
/Developer/Tools/Qt/macdeployqt Runge.app/ 
rm -rf Runge-1.0.1.dmg 

# main icon:
cp ../Resources/Runge.icns  Runge.app/Contents/Resources
# then in Info.plist file do this:
#<key>CFBundleIconFile</key>
#<string>Runge</string>
# then do
#hdiutil create -format UDBZ -srcfolder Runge.app/ Runge-1.0.1.dmg
popd
