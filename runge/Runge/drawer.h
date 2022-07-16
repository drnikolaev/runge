#ifndef _RUNGE_DRAWER_H
#define _RUNGE_DRAWER_H

#include <QWidget>
#include <QMainWindow>
#include <QMenu>

#include <QGraphicsView>
#include <QGraphicsLineItem>
#include <QMutex>
#include <QCursor>
#include <vector>

#include "view2d.h"
#include "scene2d.h"
#include "solutions.h"
#include "runge.h"
#include "ui_drawer.h"
#include "ptrvector.h"
#include "drawersettings.h"
#include "exporttoimage.h"
#include "printto.h"

#if defined(WIN32) && defined(_DEBUG)
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif

class Orbit;

class Drawer : public QMainWindow, public Ui::Drawer
{
    Q_OBJECT

    friend class DrawerSettings;
    friend class ExportToImage;
    friend class PrintTo;

private:
    Runge* runge;
    Solutions& solutions;

    double axisLengthLeft, axisLengthTop, axisLengthRight, axisLengthBottom;
    QColor axesColor;
    QGraphicsLineItem* lineXAxis;
	QGraphicsLineItem* lineYAxis;
	QList<QGraphicsLineItem*> xaxisMarks;
	QList<QGraphicsLineItem*> yaxisMarks;
    Scene2D scene;
    std::map<int, Orbit*> mapOrbits;
    int handleDiameter;
    double handleDiameterToPrint;
    int scrollbarWidth;
    bool rescaleAfterRunningFromPoint;
	bool drawMarks;
	double markStepX, markStepY;
	int markLength;
	double markLengthToPrint;
    double axesWidth;
    double axesWidthToPrint;
    double solutionsWidth;
    double solutionsWidthToPrint;

    QMenu menuZoom;
	QAction* pActionZoomIn;
	QAction* pActionZoomOut;
	QAction* pActionZoomRect;
	QAction* pActionZoomFit;

    QMenu menuColor;
	QAction* pActionColor1;
	QAction* pActionColor2;

	bool zoomToRectMode;
	bool runFromMode;
	bool panMode;
	bool panHoldMode;
	QCursor cursorZoomToRect;
	QCursor cursorRunFrom;
	QCursor cursorPalm;
	QCursor cursorPalmHold;
	QLabel xStatusBarLabel, yStatusBarLabel;
	QLabel xStatusBar, yStatusBar;
	double currentXPos, currentYPos;
	bool closedByUser;

    DrawerSettings settingsDlg;
    ExportToImage exportToImageDlg;
	PrintTo printToDlg;

public:
    Drawer(Runge* parent);
    ~Drawer();

    void show();
    void deleteAllOrbits();
    bool drawSolution(int sol_id, bool new_one);
    void redrawSolution(int sol_id);
    void selectSolution(int sol_id);
    void deleteOrbit(int sol_id);
    void rescaleHandlesAxesMarks(bool toScreen);
    double pixelsToScene(int pix);
	QRectF resetScreenRect();
    void scaleToBoundingRect();
    void zoomToRect(bool finishZoomToRect);
	QString currentXAxis() const;
	QString currentYAxis() const;
	void setCurrentPos(double xPos, double yPos);
	void runFromPos();
	void pan(bool _panMode, bool _panHoldMode);
    void checkDeleteAvailability();
	void checkPanningAvailability();
	void addMark(QList<QGraphicsLineItem*>& marks, QGraphicsLineItem* mark);
	void removeMarks(QList<QGraphicsLineItem*>& marks);
	void rescaleMarks(QList<QGraphicsLineItem*>& marks, bool x, bool toScreen);

	bool inZoomToRect() const {
		return zoomToRectMode;
	}
	bool inRunFrom() const {
		return runFromMode;
	}
	bool inPanMode() const {
		return panMode;
	}
	bool inPanHoldMode() const {
		return panHoldMode;
	}
	Runge* getRunge() const {
        return runge;
    }
	bool isClosedByUser() const {
		return closedByUser;
	}
    bool isRescaleAfterRunningFromPoint() const {
        return rescaleAfterRunningFromPoint;
    }
    double getSolutionsWidth(bool toPrint) {
        return toPrint ? solutionsWidthToPrint : solutionsWidth;
    }
	int viewWidth() const {
		return graphicsView->width() - scrollbarWidth;
	}
    int viewHeight() const {
		return graphicsView->height() - scrollbarWidth;
	}

protected:
	void closeEvent(QCloseEvent *event);
    void keyReleaseEvent(QKeyEvent* event); 
	void runFrom(bool _runFromMode);

private:
    void warning(const QString& text);
    void showAxes(bool show);
    bool axesShown() const;
    void setVarNames();
    void redrawAllSolutions(bool new_one);
	void enableAllButtons(bool enable);

private slots:
    void zoom(QAction*);
    void color(QAction*);
    void xAxisVarChanged(int index);
    void yAxisVarChanged(int index);
	void onRunFrom();
	void onButtonDelete();
	void onPan();
	void onButtonSettings();
	void onButtonExport();
	void onButtonPrint();
	void onButtonHelp();
};

#endif // _RUNGE_DRAWER_H
