#include <QtDebug>
#include <QMessageBox>
#include <QStyleFactory>
#include <QTranslator>
#include <QtPlugin>
#include <QSettings>
#include <QtOpenGL>

#include <stdio.h>
#include <fstream>

#if defined(_MSC_VER)
    #include <windows.h>
    #include <tchar.h>
    #include <dbghelp.h>
    #include <stdio.h>
    #include <crtdbg.h>
    #pragma comment (lib, "dbghelp.lib")
	
	static LONG __stdcall CrashHandlerExceptionFilter(EXCEPTION_POINTERS* pExPtrs);
#endif

#include "runge.h"
#include "utils.h"
#include "../loader.h"

// http://www.qtcentre.org/wiki/index.php?title=Memory_Leak_Detection_in_VS
#if defined(WIN32) && defined(_DEBUG)
     #define _CRTDBG_MAP_ALLOC
     #include <stdlib.h>
     #include <crtdbg.h>
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif


#if defined(QT_NO_DEBUG) && defined(__APPLE__)
void noMessageOutput(QtMsgType, const QMessageLogContext&, const QString&)
{
}    
#endif
    

int main(int argc, char *argv[])
{
#if defined(_MSC_VER) && defined(_DEBUG)
    _CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
#endif

#if defined(_MSC_VER)
    SetUnhandledExceptionFilter(CrashHandlerExceptionFilter);
#endif

#if defined(__APPLE__)
//#elif defined(_WIN32)
#else
    std::string exeDir = safe_get_exe_dir("./");
    QCoreApplication::addLibraryPath(QLatin1String(exeDir.c_str()) + QLatin1String("plugins/"));
    std::cout << exeDir << std::endl;
#endif


    QApplication app(argc, argv);

//	QGLFormat glf = QGLFormat::defaultFormat(); 
//	glf.setVersion(2,1);
	//glf.setSampleBuffers(true); 
 //   glf.setProfile(QGLFormat::CoreProfile);
 // 	glf.setSamples(4); 
//	QGLFormat::setDefaultFormat(glf); 



//QGLFormat f = QGLFormat::defaultFormat();
//f.setSampleBuffers(true);
//QGLFormat::setDefaultFormat(f);
//if (!QGLFormat::hasOpenGL()) {
//    QMessageBox::information(0, "OpenGL samplebuffers",
//                            "This system does not support OpenGL.");
//    return 0;
//}



    bool loadRu = false;
    QString languageSetting = Runge::getLanguageSetting();
    if (languageSetting.length() > 0) { // explicitly set by user
        if (languageSetting.contains("ru", Qt::CaseInsensitive)) {
            loadRu = true;
        }
    } else {
        loadRu = hasRuLocale();
    }

    QTranslator translator;
    const std::string ruFile = safe_get_exe_dir("./") + "runge_ru.qm";
    translator.load(ruFile.c_str());
    if (loadRu) {
        app.installTranslator(&translator);
    }

//  http://doc.trolltech.com/qq/qq10-windows-deployment.html
//	app.addLibraryPath("C:\\Runge");

	Runge rungeMainWindow;
    if (loadRu) {
		rungeMainWindow.setAssistantRU();
	}

	// QApplication::setStyle(QStyleFactory::create("Cleanlooks"));
	//"Windows" "WindowsXP" "WindowsVista" "Motif" "CDE" "Plastique" "Cleanlooks" 
	// Qt5 win: "Windows" "WindowsXP" "WindowsVista" "Fusion" 
	// sl[6]("Windows","WindowsXP","Motif","CDE","Plastique","Cleanlooks")
    // Ubuntu: "Windows","GTK+","Fusion",
    // MACOS: "Motif" "CDE" "Plastique" "Cleanlooks" "Macintosh (aqua)"
    //QStringList sl = QStyleFactory::keys();
    //QStringList::const_iterator it = sl.begin();
    //while (it != sl.end()) {
    //    qDebug() << *it << "\n";
    //    ++it;
    //}

    QApplication::setStyle(QStyleFactory::create("Fusion"));

	rungeMainWindow.show();
	app.connect(&app, SIGNAL(lastWindowClosed()), &app, SLOT(quit()));
    


	//glf = QGLFormat::defaultFormat(); 
	//qDebug("Have %d buffers and %d samples", glf.sampleBuffers(), glf.samples());


#if defined(QT_NO_DEBUG) && defined(__APPLE__)
    qInstallMessageHandler(noMessageOutput);
#endif

	if (argc > 1) {
		QString openFileName(argv[1]);
		rungeMainWindow.openFile(openFileName);
	}
	return app.exec();
}

#if defined(_MSC_VER)
static LONG __stdcall CrashHandlerExceptionFilter(EXCEPTION_POINTERS* pExPtrs)
{
    TCHAR lStringMessage[400];
    TCHAR lStringExInfo[128];
    _stprintf_s(lStringExInfo,
    _T("Code: 0x%8.8X Flags: %d Address: 0x%8.8X"),
    pExPtrs->ExceptionRecord->ExceptionCode,
    pExPtrs->ExceptionRecord->ExceptionFlags,
    pExPtrs->ExceptionRecord->ExceptionAddress);

#if defined(_DEBUG)
    bool miniDumpCreated = false;
    DWORD lastError = 0U;
    HANDLE hFile = CreateFile(L"RungeDump.dmp", GENERIC_READ | GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	if((hFile != NULL) && (hFile != INVALID_HANDLE_VALUE)) {
		MINIDUMP_EXCEPTION_INFORMATION mdei; 
		mdei.ThreadId = GetCurrentThreadId(); 
		mdei.ExceptionPointers = pExPtrs; 
		mdei.ClientPointers = FALSE; 
		MINIDUMP_TYPE mdt = (MINIDUMP_TYPE) (MiniDumpWithFullMemory | 
		                                     MiniDumpWithFullMemoryInfo | 
		                                     MiniDumpWithHandleData | 
		                                     MiniDumpWithThreadInfo | 
		                                     MiniDumpWithUnloadedModules);
		if (!MiniDumpWriteDump(GetCurrentProcess(), GetCurrentProcessId(), hFile, mdt, (pExPtrs != 0) ? &mdei : 0, 0, 0)) {
            lastError = GetLastError();
        } else {
            miniDumpCreated = true;
        }
        CloseHandle(hFile);
	}
	else {
        lastError = GetLastError();
	}

    if (miniDumpCreated) {
        _stprintf_s(lStringMessage,
        _T("Runge just crashed. Please submit generated RungeDump.dmp file on cvmlib.com/runge web site.\r\n%s"),
        lStringExInfo);
    } else {
        _stprintf_s(lStringMessage,
        _T("Runge just crashed. It then tried to generate dump file but failed with error %u. Please contact developer.\r\n%s"), lastError,
        lStringExInfo);
    }
#else
    _stprintf_s(lStringMessage,
        _T("Runge just crashed. Please install debug version, run it to reproduce the crash and to generate a dump file. Please submit that dump file to the developer.\r\n%s"),
        lStringExInfo);
#endif
    FatalAppExit(-1, lStringMessage);
    return EXCEPTION_CONTINUE_SEARCH;
}
#endif

