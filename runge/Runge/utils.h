#ifndef _RUNGE_UTILS_H
#define _RUNGE_UTILS_H

#include <QApplication>
#include <QLineEdit>
#include <QColorDialog>
#include <cvm.h>

inline int iround(double val) {
	double f = std::floor(val);
	if (val >= 0.) {
		f = val - f < 0.5 ? f : f + 1.;
	} else {
		f = val - f > 0.5 ? f + 1. : f;
	}
	return static_cast<int>(f);
}

void focusEditBox(QLineEdit* editBox);
void inputErrorBox(QWidget* widget, const QString& msg);
bool enforcePositiveValue(QWidget* widget, QLineEdit* editBox);
bool enforceNonNegativeValue(QWidget* widget, QLineEdit* editBox);
void validate(const cvm::rvector& x) throw(std::exception);
bool hasRuLocale();
bool removeDirectory(const QString& dirPath);

class ColorDialog;
QColor pickColor(ColorDialog& dlg, const QColor& initial = Qt::white, const QString& title = QString(), QColorDialog::ColorDialogOptions options = 0);

class AutoWaitCursor {
public:
	AutoWaitCursor() {
		QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
	}
	~AutoWaitCursor() {
		QApplication::restoreOverrideCursor();
	}
};

#endif //_RUNGE_UTILS_H
