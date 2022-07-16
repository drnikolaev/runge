#ifndef _RUNGE_DRAWER3DSETTINGS_H
#define _RUNGE_DRAWER3DSETTINGS_H

#include <QDialog>
#include "colordialog.h"
#include "ui_drawer3dsettings.h"

class Drawer3D;

class Drawer3DSettings : public QDialog, public Ui::Drawer3DSettings
{
    Q_OBJECT

private:
    Drawer3D* drawer3d;
    QDoubleValidator doubleValidator;
    QIntValidator intValidator;
	QColor xAxisColor, yAxisColor, zAxisColor;
	ColorDialog colorDialog;

public:
    Drawer3DSettings(Drawer3D* parent);
    ~Drawer3DSettings();

private:
    bool applyChanges();

protected:
    virtual void showEvent(QShowEvent* event);
    void keyReleaseEvent(QKeyEvent* event); 
    virtual void accept(); 
    virtual void reject(); 

protected slots:
    void onButtonBox(QAbstractButton* button);
    void onXAxisColor();
    void onYAxisColor();
    void onZAxisColor();
    void onAdjustLengths();
    void showHelp();
};

#endif // _RUNGE_DRAWER3DSETTINGS_H
