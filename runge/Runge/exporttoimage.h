#ifndef _RUNGE_EXPORTTOIMAGE_H
#define _RUNGE_EXPORTTOIMAGE_H

#include <QDialog>
#include <QIntValidator>
#include "ui_exporttoimage.h"

class Drawer;
class Drawer3D;

class ExportToImage : public QDialog, public Ui::ExportToImage
{
	Q_OBJECT

private:
    Drawer* drawer;
    Drawer3D* drawer3d;
    QIntValidator intValidator;

public:
	ExportToImage(Drawer* parent, Drawer3D* parent3D);
	~ExportToImage();

protected:
    virtual void accept(); 
    virtual void showEvent(QShowEvent* event);
    void keyReleaseEvent(QKeyEvent* event); 

protected slots:
    void onImageTypeChanged(int index);
    void showHelp();
};



#endif // _RUNGE_EXPORTTOIMAGE_H


