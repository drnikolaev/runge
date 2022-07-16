#include <QMessageBox>
#include <QDebug>
#include <QScrollBar>
#include <QPixmap>
#include <QBitmap>
#include <QColorDialog>
#include <QFileDialog>
#include <QPrinter>
#include <QDesktopServices>
#include <QUrl>
#include <QImageWriter>

#include "utils.h"
#include "drawer.h"
#include "engine.h"
#include "orbit.h"
#include "orbithandle.h"

#define RUNGE_STATUS_BAR_AXIS_WIDTH 100

Drawer::Drawer(Runge* parent)
    : QMainWindow(NULL),    // we don't want them to overlap main window
        runge(parent),
        solutions(parent->get_engine()->get_solutions()),
        axisLengthLeft(5.), axisLengthTop(5.), axisLengthRight(5.), axisLengthBottom(5.),
        axesColor(Qt::gray),
        scene(QRectF(-axisLengthLeft, -axisLengthTop, 
            axisLengthLeft + axisLengthRight, axisLengthTop + axisLengthBottom), this),    // just initial, will be rescaled right away
        handleDiameter(8),
        handleDiameterToPrint(0.1),
        rescaleAfterRunningFromPoint(true),
		drawMarks(true),
		markStepX(1.0),
		markStepY(1.0),
		markLength(3),
        markLengthToPrint(0.05),
        axesWidth(0.),
        axesWidthToPrint(0.01),
        solutionsWidth(0.),
        solutionsWidthToPrint(0.02),
        zoomToRectMode(false),
		runFromMode(false),
		panMode(false),
		panHoldMode(false),
// until they fix that bug in qt5 about wrong hot spot (retina?)
#ifdef __APPLE__
		cursorZoomToRect(Qt::SizeAllCursor),
		cursorRunFrom(Qt::CrossCursor),
#else
		cursorZoomToRect(QPixmap(":/Runge/Resources/32x32/zoomrect.ico"), 13, 13),
		cursorRunFrom(QPixmap(":/Runge/Resources/32x32/runfrom.ico"), 12, 12),
#endif
// these ones look better:
		cursorPalm(Qt::OpenHandCursor),
		cursorPalmHold(Qt::ClosedHandCursor),
//		cursorPalm(QPixmap(":/Runge/Resources/32x32/palm.ico"), 12, 12),
//		cursorPalmHold(QPixmap(":/Runge/Resources/32x32/palm_hold.ico"), 12, 12),
		currentXPos(0.), currentYPos(0.),
		closedByUser(false),
        settingsDlg(this),
		exportToImageDlg(this, nullptr),
		printToDlg(this)
{
    setupUi(this);
    graphicsView->setScene(&scene);
	graphicsView->setDrawer(this);
    lineXAxis = scene.addLine(QLineF(), QPen(axesColor));
    lineYAxis = scene.addLine(QLineF(), QPen(axesColor));
    showAxes(true);
    setVarNames();

	pActionZoomIn = menuZoom.addAction(tr("Zoom &in"));
    pActionZoomIn->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_Plus));
	pActionZoomOut = menuZoom.addAction(tr("Zoom &out"));
    pActionZoomOut->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_Minus));
	pActionZoomRect = menuZoom.addAction(tr("Zoom &to rectangle"));
    pActionZoomRect->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_Z));
	pActionZoomRect->setCheckable(true);
	pActionZoomFit = menuZoom.addAction(tr("Fit &all"));
    pActionZoomFit->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_F));
	buttonZoom->setMenu(&menuZoom);
	
    QIcon iconZoomIn;
    iconZoomIn.addFile(QString::fromUtf8(":/Runge/Resources/16x16/zoom_in.png"), QSize(), QIcon::Normal, QIcon::Off);
    pActionZoomIn->setIcon(iconZoomIn);
    pActionZoomIn->setIconVisibleInMenu(true);
	pActionZoomIn->setStatusTip(tr("2X Zoom In"));
    QIcon iconZoomOut;
    iconZoomOut.addFile(QString::fromUtf8(":/Runge/Resources/16x16/zoom_out.png"), QSize(), QIcon::Normal, QIcon::Off);
    pActionZoomOut->setIcon(iconZoomOut);
    pActionZoomOut->setIconVisibleInMenu(true);
	pActionZoomOut->setStatusTip(tr("2X Zoom Out"));
    QIcon iconZoomRect;
    iconZoomRect.addFile(QString::fromUtf8(":/Runge/Resources/16x16/selection_view.png"), QSize(), QIcon::Normal, QIcon::Off);
    pActionZoomRect->setIcon(iconZoomRect);
    pActionZoomRect->setIconVisibleInMenu(true);
	pActionZoomRect->setStatusTip(tr("Enters to manual zooming mode"));
    QIcon iconZoomFit;
    iconZoomFit.addFile(QString::fromUtf8(":/Runge/Resources/16x16/fit_to_size.png"), QSize(), QIcon::Normal, QIcon::Off);
    pActionZoomFit->setIcon(iconZoomFit);
    pActionZoomFit->setIconVisibleInMenu(true);
	pActionZoomFit->setStatusTip(tr("Shows all solutions and axes"));

    QIcon iconPalette;
    iconPalette.addFile(QString::fromUtf8(":/Runge/Resources/16x16/palette.png"), QSize(), QIcon::Normal, QIcon::Off);
	pActionColor1 = menuColor.addAction(tr("&Main and odd steps color"));
    pActionColor1->setIcon(iconPalette);
    pActionColor1->setIconVisibleInMenu(true);
	pActionColor1->setStatusTip(tr("Sets main color and color for every odd step (when selected)"));
    pActionColor2 = menuColor.addAction(tr("&Even steps color"));
    pActionColor2->setIcon(iconPalette);
    pActionColor2->setIconVisibleInMenu(true);
	pActionColor2->setStatusTip(tr("Sets color for every even step (when selected)"));
	buttonPalette->setMenu(&menuColor);

	xStatusBarLabel.setAlignment(Qt::AlignRight | Qt::AlignVCenter);
	yStatusBarLabel.setAlignment(Qt::AlignRight | Qt::AlignVCenter);
	xStatusBar.setFrameStyle(QFrame::Panel | QFrame::Sunken);
	yStatusBar.setFrameStyle(QFrame::Panel | QFrame::Sunken);
	xStatusBar.setMinimumWidth(RUNGE_STATUS_BAR_AXIS_WIDTH);
	yStatusBar.setMinimumWidth(RUNGE_STATUS_BAR_AXIS_WIDTH);
	xStatusBar.setMaximumWidth(RUNGE_STATUS_BAR_AXIS_WIDTH);
	yStatusBar.setMaximumWidth(RUNGE_STATUS_BAR_AXIS_WIDTH);
	xStatusBar.setAlignment(Qt::AlignRight);
	yStatusBar.setAlignment(Qt::AlignRight);
	statusBar()->addPermanentWidget(&xStatusBarLabel);
	statusBar()->addPermanentWidget(&xStatusBar);
	statusBar()->addPermanentWidget(&yStatusBarLabel);
	statusBar()->addPermanentWidget(&yStatusBar);
	
    QStyle* appStype = QApplication::style();
    scrollbarWidth = appStype == NULL ? 0 : appStype->pixelMetric(QStyle::PM_ScrollBarExtent);

	checkPanningAvailability();
	checkDeleteAvailability();

    connect(&menuZoom, SIGNAL(triggered(QAction*)), this, SLOT(zoom(QAction*)));
    connect(&menuColor, SIGNAL(triggered(QAction*)), this, SLOT(color(QAction*)));
    connect(xAxisBox, SIGNAL(currentIndexChanged(int)), this, SLOT(xAxisVarChanged(int)));
    connect(yAxisBox, SIGNAL(currentIndexChanged(int)), this, SLOT(yAxisVarChanged(int)));
    connect(buttonRun, SIGNAL(clicked()), this, SLOT(onRunFrom()));
    connect(buttonDelete, SIGNAL(clicked()), this, SLOT(onButtonDelete()));
    connect(buttonPan, SIGNAL(clicked()), this, SLOT(onPan()));
    connect(buttonSettings, SIGNAL(clicked()), this, SLOT(onButtonSettings()));
    connect(buttonExport, SIGNAL(clicked()), this, SLOT(onButtonExport()));
    connect(buttonPrint, SIGNAL(clicked()), this, SLOT(onButtonPrint()));
    connect(buttonHelp, SIGNAL(clicked()), this, SLOT(onButtonHelp()));
}

Drawer::~Drawer()
{
    deleteAllOrbits();
	removeMarks(xaxisMarks);
	removeMarks(yaxisMarks);
}

void Drawer::closeEvent(QCloseEvent *event)
{
	closedByUser = true;
    settingsDlg.close();
	event->accept();
}

void Drawer::zoomToRect(bool finishZoomToRect)
{
	if (finishZoomToRect) {
		zoomToRectMode = false;
		pActionZoomRect->setChecked(false);
	} else {
		zoomToRectMode = pActionZoomRect->isChecked();
	}
	if (zoomToRectMode) {
		setCursor(cursorZoomToRect);
	} else {
		unsetCursor();
	}
    buttonPan->setEnabled(!zoomToRectMode);
}

QString Drawer::currentXAxis() const
{
	return xAxisBox->currentText();
}

QString Drawer::currentYAxis() const
{
	return yAxisBox->currentText();
}

void Drawer::setCurrentPos(double xPos, double yPos)
{
	static QString a1("%1");
	currentXPos = xPos;
	currentYPos = yPos;
	xStatusBar.setText(a1.arg(currentXPos, 0, 'E'));
	yStatusBar.setText(a1.arg(currentYPos, 0, 'E'));
}

void Drawer::runFromPos()
{
	runge->runFromPos(xAxisBox->currentIndex(), currentXPos, yAxisBox->currentIndex(), currentYPos);
}

double Drawer::pixelsToScene(int pix)
{
    return graphicsView->mapToScene(QRect(0,0,pix,pix)).boundingRect().width();
}

void Drawer::warning(const QString& text)
{
    QMessageBox::warning (this, tr("Warning"), text);
}

void Drawer::showAxes(bool show)
{
    if (show) {
        lineXAxis->show();
        lineYAxis->show();
        lineXAxis->setPen(QPen(axesColor));
        lineYAxis->setPen(QPen(axesColor));
        lineXAxis->setLine(-axisLengthLeft, 0., axisLengthRight, 0.);
        lineYAxis->setLine(0., -axisLengthTop, 0., axisLengthBottom);
    } else {
        lineXAxis->hide();
        lineYAxis->hide();
    }

	removeMarks(xaxisMarks);
	removeMarks(yaxisMarks);
	if (drawMarks) {
		double x, y;
		const double markLengthToScene = pixelsToScene(markLength);
		if (markStepX > 0.) {
			for (x = - markStepX; x > - axisLengthLeft; x -= markStepX) {
				addMark(xaxisMarks, new QGraphicsLineItem(x, 0, x, markLengthToScene));
			}
			for (x = markStepX; x < axisLengthRight; x += markStepX) {
				addMark(xaxisMarks, new QGraphicsLineItem(x, 0, x, markLengthToScene));
			}
		}
		if (markStepY > 0.) {
			for (y = - markStepY; y > - axisLengthTop; y -= markStepY) {
				addMark(yaxisMarks, new QGraphicsLineItem(0, y, - markLengthToScene, y));
			}
			for (y = markStepY; y < axisLengthBottom; y += markStepY) {
				addMark(yaxisMarks, new QGraphicsLineItem(0, y, - markLengthToScene, y));
			}
		}
	}
}

void Drawer::addMark(QList<QGraphicsLineItem*>& marks, QGraphicsLineItem* mark)
{
	mark->setPen(QPen(axesColor));
	scene.addItem(mark);
	marks.push_back(mark);
}

void Drawer::removeMarks(QList<QGraphicsLineItem*>& marks)
{
	QList<QGraphicsLineItem*>::iterator it = marks.begin();
	while (it != marks.end()) {
		QGraphicsLineItem* item = *it++;
		scene.removeItem(item);
		delete item;
	}
	marks.clear();
}

void Drawer::rescaleMarks(QList<QGraphicsLineItem*>& marks, bool x, bool toScreen)
{
	const double markLengthToScene = toScreen ? pixelsToScene(markLength) : markLengthToPrint;
    QPen axesPen(axesColor);
    axesPen.setWidthF(toScreen ? axesWidth : axesWidthToPrint);
	QList<QGraphicsLineItem*>::iterator it = marks.begin();
	while (it != marks.end()) {
		QGraphicsLineItem* item = *it++;
		QLineF line = item->line();
		if (x) {
			item->setLine(line.x1(), line.y1(), line.x2(), markLengthToScene);
		} else {
			item->setLine(line.x1(), line.y1(), - markLengthToScene, line.y2());
		}
        item->setPen(axesPen);
	}
}

bool Drawer::axesShown() const
{
	return lineXAxis->isVisible() && lineYAxis->isVisible();
}

void Drawer::setVarNames()
{
    xAxisBox->clear();
    yAxisBox->clear();
    std::vector<std::string> names;
    runge->get_names(names, true, true);
    std::vector<std::string>::const_iterator it = names.begin();
    while (it != names.end()) {
        xAxisBox->addItem(it->c_str());
        yAxisBox->addItem(it->c_str());
        ++it;
    }
    xAxisBox->addItem(RUNGE_STEP_NAME);
    yAxisBox->addItem(RUNGE_STEP_NAME);
    xAxisBox->setCurrentIndex(0);
    yAxisBox->setCurrentIndex(1);
	xStatusBarLabel.setText(currentXAxis());
	yStatusBarLabel.setText(currentYAxis());
}

void Drawer::show()
{
	QMainWindow::show();
    scaleToBoundingRect();
    graphicsView->show();
    redrawAllSolutions(true);
}

QRectF Drawer::resetScreenRect()
{
    QRectF rect = scene.itemsBoundingRect();
    scene.setSceneRect(rect);
	return rect;
}

void Drawer::scaleToBoundingRect()
{
    QRectF rect = resetScreenRect();
    double xscale = (double) this->viewWidth() / rect.width();
    double yscale = (double) this->viewHeight() / rect.height();
    double scale = std::min<double>(xscale, yscale);
    double oldscale = graphicsView->transform().m11();
    scale /= oldscale;
    graphicsView->scale(scale, scale);
    rescaleHandlesAxesMarks(true);
}

void Drawer::checkPanningAvailability()
{
	QScrollBar* hBar = graphicsView->horizontalScrollBar();
	QScrollBar* vBar = graphicsView->verticalScrollBar();
	buttonPan->setEnabled(!zoomToRectMode && (hBar->isVisible() || vBar->isVisible()));
}

void Drawer::zoom(QAction* action)
{
    if (action == pActionZoomIn) {
        graphicsView->zoomIn();
    }
    else if (action == pActionZoomOut) {
        graphicsView->zoomOut();
    }
    else if (action == pActionZoomFit) {
        scaleToBoundingRect();
	}
	else if (action == pActionZoomRect) {
        zoomToRect(false);
    }
}

void Drawer::xAxisVarChanged(int)
{
	xStatusBarLabel.setText(currentXAxis());
    redrawAllSolutions(false);
}

void Drawer::yAxisVarChanged(int)
{
	yStatusBarLabel.setText(currentYAxis());
    redrawAllSolutions(false);
}

bool Drawer::drawSolution(int sol_id, bool new_one)
{
    const std::string xvar = xAxisBox->currentText().toStdString();
    const std::string yvar = yAxisBox->currentText().toStdString();
    Solutions::iterator it = solutions.find(sol_id);
    if (it != solutions.end()) {
        Solution& sol = it->second;
        const int xindex = sol.get_var_index(xvar);
        const int yindex = sol.get_var_index(yvar);
        if (xindex < 0 || yindex < 0) {
            warning(tr("Some solution(s) were skipped because they have non-existent variable names"));
            return false;
        }
        if (! new_one) {
			deleteOrbit(sol_id);
        }

        SafeLock lock(Solution::mutex());

        Orbit* porbit = new Orbit(this, sol_id, sol, xindex, yindex);
        scene.addItem(porbit);
        mapOrbits.insert(std::pair<int, Orbit*>(sol_id, porbit));
		double xstart = 0., ystart = 0.;
		if (sol.get_start(xindex, xstart) && sol.get_start(yindex, ystart)) {
			OrbitHandle* phandle = new OrbitHandle(porbit, this, xstart, ystart);
            phandle->setDiameter(pixelsToScene(handleDiameter));
            phandle->setSelected(sol.is_selected());
		}
    }    
    return true;
}

void Drawer::redrawSolution(int sol_id)
{
    std::map<int, Orbit*>::const_iterator it = mapOrbits.find(sol_id);
    if (it != mapOrbits.end()) {
        scene.invalidate(it->second->boundingRect());
    }
}

void Drawer::selectSolution(int sol_id)
{
	runge->selectSolutionInTable(sol_id);
}

void Drawer::redrawAllSolutions(bool new_one)
{
    AutoWaitCursor waitCursor;
    Solutions::const_iterator its = solutions.begin();
    while (its != solutions.end()) {
        if (!drawSolution(its++->first, new_one)) {
            close();
            break;
        }
    }
	scaleToBoundingRect();
}

void Drawer::deleteAllOrbits()
{
    std::map<int, Orbit*>::iterator it = mapOrbits.begin();
    while (it != mapOrbits.end()) {
        scene.removeItem(it->second);
        delete it->second;
        ++it;
    }
	mapOrbits.clear();
}

void Drawer::deleteOrbit(int sol_id)
{
    std::map<int, Orbit*>::iterator it = mapOrbits.find(sol_id);
    if (it != mapOrbits.end()) {
        scene.removeItem(it->second);
		// use it with NoIndex only!
		// Qt tries to paint deleted items sometimes (don't ask me why)
		delete it->second;
        mapOrbits.erase(it);
    }
}

void Drawer::rescaleHandlesAxesMarks(bool toScreen)
{
    std::map<int, Orbit*>::iterator it = mapOrbits.begin();
    while (it != mapOrbits.end()) {
        QList<QGraphicsItem*> children = it++->second->childItems();
        QList<QGraphicsItem*>::const_iterator itc = children.begin();
        while (itc != children.end()) {
            QGraphicsItem* item = *itc++;
            if (typeid(OrbitHandle) == typeid(*item)) {
			    OrbitHandle* handle = dynamic_cast<OrbitHandle*>(item);
                handle->setDiameter(toScreen ? pixelsToScene(handleDiameter) : handleDiameterToPrint);
            }
        }
    }
	rescaleMarks(xaxisMarks, true, toScreen);
	rescaleMarks(yaxisMarks, false, toScreen);

    QPen axesPen(axesColor);
    axesPen.setWidthF(toScreen ? axesWidth : axesWidthToPrint);
    lineXAxis->setPen(axesPen);
    lineYAxis->setPen(axesPen);
}

void Drawer::onRunFrom()
{
    runFrom(buttonRun->isChecked());
}

void Drawer::checkDeleteAvailability()
{
	buttonDelete->setEnabled(runge->getSolutionSelected() >= 0);
}

void Drawer::onButtonDelete()
{
	runge->deleteSelectedSolutions(false);
	checkDeleteAvailability();
}

void Drawer::onPan()
{
	pan(buttonPan->isChecked(), false);
}

void Drawer::runFrom(bool _runFromMode)
{
	runFromMode = _runFromMode;
	enableAllButtons(!runFromMode);
	if (runFromMode) {
		buttonPan->setEnabled(false);
		setCursor(cursorRunFrom);
	} else {
		checkPanningAvailability();
        buttonRun->setChecked(false);
		unsetCursor();
	}
}

void Drawer::pan(bool _panMode, bool _panHoldMode)
{
	panMode = _panMode;
	panHoldMode = _panHoldMode;
	enableAllButtons(!panMode);
	buttonRun->setEnabled(!panMode);
	if (panMode) {
		setCursor(panHoldMode ? cursorPalmHold : cursorPalm);
	} else {
		buttonPan->setChecked(false);
		unsetCursor();
	}
}

void Drawer::enableAllButtons(bool enable)
{
	xAxisBox->setEnabled(enable);
	yAxisBox->setEnabled(enable);
	buttonZoom->setEnabled(enable);
	buttonPalette->setEnabled(enable);
	buttonSettings->setEnabled(enable);
	buttonExport->setEnabled(enable);
	buttonPrint->setEnabled(enable);
}

void Drawer::keyReleaseEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Escape) {
        if (runFromMode) {
            runFrom(false);
        }
        if (panMode || panHoldMode) {
            pan(false, false);
        }
        if (zoomToRectMode) {
            zoomToRect(true);
        }
    }
    else if (event->key() == Qt::Key_F1) {
        onButtonHelp();
    }
    else {
        QMainWindow::keyReleaseEvent(event);
    }
}

void Drawer::color(QAction* action)
{
	const bool odd = action == pActionColor1;
	int sol_id = runge->getSolutionSelected();
	if (sol_id < 0) {
		QColor color = QColorDialog::getColor(runge->getCurrentOrbitColor(odd), this, 
			"Select current color", QColorDialog::ShowAlphaChannel);
		runge->setCurrentOrbitColor(odd, color);
	} else {
		Solution& solution = solutions.get_solution(sol_id);
		unsigned int r, g, b, a;
		solution.get_color(odd, &r, &g, &b, &a);
		QColor oldColor(r, g, b, a);
		QColor color = QColorDialog::getColor(oldColor, this, 
			"Select selected solution color", QColorDialog::ShowAlphaChannel);
		if (color != oldColor) {
			solution.set_color(odd, color.red(), color.green(), color.blue(), color.alpha());
			runge->redrawSolution(sol_id);
		}
	}
}

void Drawer::onButtonSettings()
{
    settingsDlg.show();
}

void Drawer::onButtonExport()
{
    exportToImageDlg.show();
}

void Drawer::onButtonPrint()
{
	printToDlg.show();
}

void Drawer::onButtonHelp()
{
    getRunge()->showHelp("2D grahics");
}

