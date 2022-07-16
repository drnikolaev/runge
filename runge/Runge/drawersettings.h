#ifndef _RUNGE_DRAWERSETTINGS_H
#define _RUNGE_DRAWERSETTINGS_H

#include <QDialog>
#include "colordialog.h"
#include "ui_drawersettings.h"

class Drawer;

class DrawerSettings : public QDialog, public Ui::DrawerSettings
{
    Q_OBJECT

private:
    Drawer* drawer;
    QDoubleValidator doubleValidator;
    QIntValidator intValidator;
    QColor axesColor;
	ColorDialog colorDialog;

public:
    DrawerSettings(Drawer* parent);
    ~DrawerSettings();

private:
    bool applyChanges();

protected:
    virtual void showEvent(QShowEvent* event);
    void keyReleaseEvent(QKeyEvent* event); 
    virtual void accept(); 
    virtual void reject(); 

protected slots:
    void onButtonBox(QAbstractButton* button);
    void onAxesColor();
    void onAdjustLengths();
    void showHelp();
};

#endif // _RUNGE_DRAWERSETTINGS_H
