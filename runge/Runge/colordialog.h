#ifndef _RUNGE_COLORDIALOG_H
#define _RUNGE_COLORDIALOG_H

#include <QColorDialog>

class ColorDialog : public QColorDialog
{
	QColor mcolor;
public:
	ColorDialog(QWidget* parent) : 
		QColorDialog(parent), mcolor(Qt::black) {
	}
	QColor color() const {
		return mcolor;
	}
	void setCurrentColor(const QColor& color) {
		mcolor = color;
		QColorDialog::setCurrentColor(color);
	}
protected:
    virtual void accept(); 
};

#endif	// _RUNGE_COLORDIALOG_H
