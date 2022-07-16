cp Runge.proMac Runge.pro

pushd debug
rm -rf Runge.app
popd

make clean
qmake -spec macx-g++481
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

#cp -rf /Developer/Tools/Qt/Assistant.app .
cp -rf /Users/sam/Qt5.2.0/5.2.0-beta1/clang_64/bin/Assistant.app .
popd

pushd debug
#macdeployqt Runge.app/ -verbose=2 -executable=Runge.app/Contents/MacOS/Runge
macdeployqt Runge.app/ -executable=Runge.app/Contents/MacOS/Runge
rm -rf Runge-1.1.dmg 
popd

pushd debug/Runge.app/Contents/MacOS/Assistant.app/Contents/
ln -Fs ../../../PlugIns/ Plugins
popd

cp debug/Runge.app/Contents/Resources/qt.conf debug/Runge.app/Contents/MacOS/Assistant.app/Contents/Resources

pushd debug/Runge.app/Contents/MacOS
#install_name_tool -change ../../lib64/libcvm_em64t_ilp64.dylib @executable_path/libcvm_em64t_ilp64.dylib libcfun_em64t_ilp64.dylib 
install_name_tool -change ../../lib64/libcvm_em64t_ilp64.dylib @executable_path/libcvm_em64t_ilp64.dylib libdes_engl_em64t_ilp64.dylib 
install_name_tool -change ../../lib64/libcvm_em64t_ilp64.dylib @executable_path/libcvm_em64t_ilp64.dylib libdes_lou_em64t_ilp64.dylib 
install_name_tool -change ../../lib64/libcvm_em64t_ilp64.dylib @executable_path/libcvm_em64t_ilp64.dylib libdes_ros_em64t_ilp64.dylib 
install_name_tool -change ../../lib64/libcvm_em64t_ilp64.dylib @executable_path/libcvm_em64t_ilp64.dylib Runge
popd

pushd debug/Runge.app/Contents/MacOS/Assistant.app/Contents/MacOS/
CVM_QT_LIB=/Users/sam/Qt5.2.0/5.2.0-beta1/clang_64/lib
install_name_tool -change ${CVM_QT_LIB}/QtWebKit.framework/Versions/5/QtWebKit @executable_path/../Frameworks/QtWebKit.framework/Versions/5/QtWebKit Assistant 
install_name_tool -change ${CVM_QT_LIB}/QtWebKitWidgets.framework/Versions/5/QtWebKitWidgets @executable_path/../Frameworks/QtWebKitWidgets.framework/Versions/5/QtWebKitWidgets Assistant 
install_name_tool -change ${CVM_QT_LIB}/QtNetwork.framework/Versions/5/QtNetwork @executable_path/../Frameworks/QtNetwork.framework/Versions/5/QtNetwork Assistant
install_name_tool -change ${CVM_QT_LIB}/QtGui.framework/Versions/5/QtGui @executable_path/../Frameworks/QtGui.framework/Versions/5/QtGui Assistant 
install_name_tool -change ${CVM_QT_LIB}/QtCore.framework/Versions/5/QtCore @executable_path/../Frameworks/QtCore.framework/Versions/5/QtCore Assistant 
install_name_tool -change ${CVM_QT_LIB}/QtSql.framework/Versions/5/QtSql @executable_path/../Frameworks/QtSql.framework/Versions/5/QtSql Assistant 
install_name_tool -change ${CVM_QT_LIB}/QtHelp.framework/Versions/5/QtHelp @executable_path/../Frameworks/QtHelp.framework/Versions/5/QtHelp Assistant
install_name_tool -change ${CVM_QT_LIB}/QtQuick.framework/Versions/5/QtQuick @executable_path/../Frameworks/QtQuick.framework/Versions/5/QtQuick Assistant
install_name_tool -change ${CVM_QT_LIB}/QtQml.framework/Versions/5/QtQml @executable_path/../Frameworks/QtQml.framework/Versions/5/QtQml Assistant
install_name_tool -change ${CVM_QT_LIB}/QtOpenGL.framework/Versions/5/QtOpenGL @executable_path/../Frameworks/QtOpenGL.framework/Versions/5/QtOpenGL Assistant
install_name_tool -change ${CVM_QT_LIB}/QtWidgets.framework/Versions/5/QtWidgets @executable_path/../Frameworks/QtWidgets.framework/Versions/5/QtWidgets Assistant
install_name_tool -change ${CVM_QT_LIB}/QtPrintSupport.framework/Versions/5/QtPrintSupport @executable_path/../Frameworks/QtPrintSupport.framework/Versions/5/QtPrintSupport Assistant
popd


# main icon:
pushd debug
cp ../Resources/Runge.icns  Runge.app/Contents/Resources
# then in Info.plist file do this:
#<key>CFBundleIconFile</key>
#<string>Runge</string>
# then do
#hdiutil create -format UDBZ -srcfolder Runge.app/ Runge-1.1.dmg
popd

#      export DYLD_PRINT_LIBRARIES=1
#Sergeis-MacBook-Pro:~/cvmlib/runge/Runge/debug sam$ echo $DYLD_FRAMEWORK_PATH 
#/Users/sam/cvmlib/runge/Runge/debug/Runge.app/Contents/Frameworks/
