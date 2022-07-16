#ifndef _RUNGE_ABOUT_H
#define _RUNGE_ABOUT_H

#include <QDialog>
#include "ui_about.h"

class AboutRunge : public QDialog, public Ui::AboutRunge
{
    Q_OBJECT
public:
    AboutRunge(QWidget* parent);    
	~AboutRunge() {}
};

#endif // _RUNGE_ABOUT_H
