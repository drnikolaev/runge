#include <QObject>
#include <QWidget>
#include <QString>
#include <QDir>
#include <QFile>
#include <QMessageBox>
#include <QLocale>
#include <float.h>
#include <stdexcept>
#include <cmath>

#if defined(_MSC_VER)
#   include <Windows.h>
#endif

#include "utils.h"
#include "colordialog.h"

void focusEditBox(QLineEdit* editBox)
{
    editBox->setFocus();
    editBox->selectAll();
}

void inputErrorBox(QWidget* widget, const QString& msg)
{
	static const QString caption = QObject::tr("Input Error");
	QMessageBox::critical(widget, caption, msg);
}

bool enforcePositiveValue(QWidget* widget, QLineEdit* editBox)
{
    bool ret = true;
    const double value = editBox->text().toDouble();
    if (value <= 0.0) {
        ret = false;
    	static const QString message = QObject::tr("Please enter positive value (currently entered value is %1)");
        inputErrorBox(widget, message.arg(value));
        focusEditBox(editBox);
    }
    return ret;
}

bool enforceNonNegativeValue(QWidget* widget, QLineEdit* editBox)
{
    bool ret = true;
    const double value = editBox->text().toDouble();
    if (value < 0.0) {
        ret = false;
    	static const QString message = QObject::tr("Please enter nonnegative value (currently entered value is %1)");
        inputErrorBox(widget, message.arg(value));
        focusEditBox(editBox);
    }
    return ret;
}

void validate(const cvm::rvector& x) throw(std::exception)
{
	static const std::string component = QObject::tr("Component ").toStdString();
    for (int i = 1; i <= x.size(); ++i) {
#if defined(_MSC_VER)
        int cls = _fpclass(x[i]);
        if (cls == _FPCLASS_SNAN || cls == _FPCLASS_QNAN) {
            std::ostringstream os;
			os << component << i << QObject::tr(" is not a number").toStdString() << std::ends;
            throw std::runtime_error(os.str());
        }
        if (cls == _FPCLASS_NINF) {
            std::ostringstream os;
            os << component << i << QObject::tr(" is negative infinity").toStdString() << std::ends;
            throw std::runtime_error(os.str());
        }
        if (cls == _FPCLASS_PINF) {
            std::ostringstream os;
            os << component << i << QObject::tr(" is positive infinity").toStdString() << std::ends;
            throw std::runtime_error(os.str());
        }
#else 
        int cls = std::fpclassify(x[i]);
        if (cls == FP_NAN) {
            std::ostringstream os;
            os << component << i << QObject::tr(" is not a number").toStdString() << std::ends;
            throw std::runtime_error(os.str());
        }
        if (cls == FP_INFINITE) {
            std::ostringstream os;
            os << component << i << QObject::tr(" is infinity").toStdString() << std::ends;
            throw std::runtime_error(os.str());
        }
#endif
    }
}

bool hasRuLocale()
{
    bool hasRu = false;
#if defined(_MSC_VER)
    typedef int (WINAPI* LOCALEFUNC)(LPWSTR,int);
    HMODULE hKernel = GetModuleHandle(TEXT("KERNEL32.DLL"));
    if (hKernel != NULL) {
        const WCHAR wcsRu[] = L"ru";
        WCHAR lpLocaleName[LOCALE_NAME_MAX_LENGTH];
        // to let it go on Windows XP we use indirect calls here:
        LOCALEFUNC pGetUserDefaultLocaleName = (LOCALEFUNC)GetProcAddress(hKernel, "GetUserDefaultLocaleName");
        if (pGetUserDefaultLocaleName != NULL) {
            pGetUserDefaultLocaleName(lpLocaleName, LOCALE_NAME_MAX_LENGTH);
            if (0 == _wcsnicmp(wcsRu, lpLocaleName, 2)) {
                hasRu = true;
            } else {
                //"Language for non-Unicode programs" lives here:
                LOCALEFUNC pGetSystemDefaultLocaleName = (LOCALEFUNC)GetProcAddress(hKernel, "GetSystemDefaultLocaleName");
                if (pGetSystemDefaultLocaleName != NULL) {
                    pGetSystemDefaultLocaleName(lpLocaleName, LOCALE_NAME_MAX_LENGTH);
                    if (0 == _wcsnicmp(wcsRu, lpLocaleName, 2)) {
                        hasRu = true;
                    }
                }
            }
        }
    }
#else 
    QString locale = QLocale::system().name();
    hasRu = locale.contains("ru", Qt::CaseInsensitive);
#endif
    return hasRu;
}

bool removeDirectory(const QString& dirPath)
{
    QDir dir(dirPath);
    bool ok = dir.exists();
    if (ok) {
        QFileInfoList entries = dir.entryInfoList(QDir::NoDotAndDotDot | QDir::Dirs | QDir::Files | QDir::Hidden);
        foreach (QFileInfo entryInfo, entries) {
            QString path = entryInfo.absoluteFilePath();
            if (entryInfo.isDir())
            {
                if (!removeDirectory(path)) {
                    ok = false;
                    break;
                }
            } else {
                QFile file(path);
                if (! file.remove()) {
                    ok = false;
                    break;
                }
            }
        }
    }
    return ok && dir.rmdir(dir.absolutePath());
}


QColor pickColor(ColorDialog& dlg, const QColor& initial, const QString& title, QColorDialog::ColorDialogOptions options)
{
	dlg.setCurrentColor(initial);
	dlg.setWindowTitle(title);
	dlg.setOptions(options);
	dlg.exec();
	return dlg.color();
}
