#ifndef _RUNGE_PRINTTO_H
#define _RUNGE_PRINTTO_H

#include <QDialog>
#include <QDoubleValidator>
#include "ui_printto.h"

class Drawer;

class PrintTo : public QDialog, public Ui::PrintTo
{
	Q_OBJECT

private:
    Drawer* drawer;
    QDoubleValidator doubleValidator;

public:
	PrintTo(Drawer* parent);
	~PrintTo();

private:
    bool applyChanges();

protected:
    virtual void showEvent(QShowEvent* event);
    virtual void accept(); 
    void keyReleaseEvent(QKeyEvent* event); 

protected slots:
//    void onButtonBox(QAbstractButton* button);
    void showHelp();
};

#endif // _RUNGE_PRINTTO_H

