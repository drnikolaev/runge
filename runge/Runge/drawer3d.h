#ifndef _RUNGE_DRAWER3D_H
#define _RUNGE_DRAWER3D_H

#include <QWidget>
#include <QMainWindow>
#include <QMenu>

#include <QGraphicsView>
#include <QGraphicsLineItem>
#include <QMutex>
#include <QCursor>
#include <QCloseEvent>
#include <vector>

#include "solutions.h"
#include "runge.h"
#include "ui_drawer3d.h"
#include "drawer3dsettings.h"
#include "ptrvector.h"
#include "exporttoimage.h"
#include "glwidget.h"

#if defined(WIN32) && defined(_DEBUG)
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif

#define SLIDING_STEPS_PER_DEGREE 10
#define SLIDING_STEPS_PER_DEGREED 10.0


class Drawer3D : public QMainWindow, public Ui::Drawer3D
{
    Q_OBJECT

    friend class Drawer3DSettings;
	friend class GLWidget;

private:
    Runge* runge;
    Solutions& solutions;
	GLWidget* glWidget;

	bool panMode;
	bool panHoldMode;
	QCursor cursorPalm;
	QCursor cursorPalmHold;
	bool closedByUser;

	bool showAxes;
    double xAxisFrom, xAxisTo, yAxisFrom, yAxisTo, zAxisFrom, zAxisTo;
    QColor xAxisColor, yAxisColor, zAxisColor;
	double axesWidth, solutionsWidth;
    int handleDiameter;
	double minXBox, maxXBox, minYBox, maxYBox, minZBox, maxZBox;

    ExportToImage exportToImageDlg;
    Drawer3DSettings settingsDlg;

public:
    Drawer3D(Runge* parent);
    ~Drawer3D();

	void show();
    void redrawAllSolutions() const;
    void repaint() const;
	void pan(bool _panMode, bool _panHoldMode);

	Runge* getRunge() const {
        return runge;
    }
	bool isClosedByUser() const {
		return closedByUser;
	}
	int widgetWidth() const {
		return glWidget->width();
	}
	int widgetHeight() const {
		return glWidget->height();
	}

	QImage toImage(int w, int h);

protected:
	void closeEvent(QCloseEvent *event);
    void keyReleaseEvent(QKeyEvent* event); 
	void resizeEvent(QResizeEvent * event);

private:
	void tuneSlider(QSlider* slider);
    void warning(const QString& text);
    void setVarNames();
	bool drawSolution(int sol_id) const;
	void enableAllButtons(bool enable);

private slots:
    void xAxisVarChanged(int index);
    void yAxisVarChanged(int index);
    void zAxisVarChanged(int index);
	void xSpinBoxChanged(double);
	void ySpinBoxChanged(double);
	void zSpinBoxChanged(double);
	void zoomFactorChanged(double);
	void onPan();
	void onButtonExport();
	void onButtonSettings();
	void onButtonHelp();
};

#endif // _RUNGE_DRAWER_H
