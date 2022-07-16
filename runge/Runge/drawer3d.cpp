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

#include <limits>

#include "utils.h"
#include "drawer3d.h"
#include "engine.h"

Drawer3D::Drawer3D(Runge* parent)
    : QMainWindow(NULL),    // we don't want them to overlap main window
        runge(parent),
        solutions(parent->get_engine()->get_solutions()),
		panMode(false),
		panHoldMode(false),
		cursorPalm(Qt::OpenHandCursor),
		cursorPalmHold(Qt::ClosedHandCursor),
		closedByUser(false),
		showAxes(true),
		xAxisFrom(0.), xAxisTo(1.), yAxisFrom(0.), yAxisTo(1.), zAxisFrom(0.), zAxisTo(1.),
		xAxisColor(Qt::red), yAxisColor(Qt::green), zAxisColor(Qt::blue),
		axesWidth(1.0), solutionsWidth(1.5),
		handleDiameter(8),
		minXBox((std::numeric_limits<double>::max)()), maxXBox((std::numeric_limits<double>::min)()),
		minYBox((std::numeric_limits<double>::max)()), maxYBox((std::numeric_limits<double>::min)()),
		minZBox((std::numeric_limits<double>::max)()), maxZBox((std::numeric_limits<double>::min)()),
	    exportToImageDlg(nullptr, this),
		settingsDlg(this)
{
    setupUi(this);
	glWidget = new GLWidget(widget, this);

    setVarNames();

	tuneSlider(xSlider);
	tuneSlider(ySlider);
	tuneSlider(zSlider);
	xSlider->setValue(0 * SLIDING_STEPS_PER_DEGREE);
    ySlider->setValue(0 * SLIDING_STEPS_PER_DEGREE);
    zSlider->setValue(0 * SLIDING_STEPS_PER_DEGREE);

    connect(xAxisBox, SIGNAL(currentIndexChanged(int)), this, SLOT(xAxisVarChanged(int)));
    connect(yAxisBox, SIGNAL(currentIndexChanged(int)), this, SLOT(yAxisVarChanged(int)));
    connect(zAxisBox, SIGNAL(currentIndexChanged(int)), this, SLOT(zAxisVarChanged(int)));

    connect(buttonPan, SIGNAL(clicked()), this, SLOT(onPan()));
    connect(buttonExport, SIGNAL(clicked()), this, SLOT(onButtonExport()));

    connect(xSlider, SIGNAL(valueChanged(int)), glWidget, SLOT(setXRotation(int)));
    connect(glWidget, SIGNAL(xRotationChanged(int)), xSlider, SLOT(setValue(int)));
    connect(xAngleSpinBox, SIGNAL(valueChanged(double)), this, SLOT(xSpinBoxChanged(double)));

    connect(ySlider, SIGNAL(valueChanged(int)), glWidget, SLOT(setYRotation(int)));
    connect(glWidget, SIGNAL(yRotationChanged(int)), ySlider, SLOT(setValue(int)));
    connect(yAngleSpinBox, SIGNAL(valueChanged(double)), this, SLOT(ySpinBoxChanged(double)));

	connect(zSlider, SIGNAL(valueChanged(int)), glWidget, SLOT(setZRotation(int)));
    connect(glWidget, SIGNAL(zRotationChanged(int)), zSlider, SLOT(setValue(int)));
    connect(zAngleSpinBox, SIGNAL(valueChanged(double)), this, SLOT(zSpinBoxChanged(double)));

    connect(zoomSpinBox, SIGNAL(valueChanged(double)), this, SLOT(zoomFactorChanged(double)));
    connect(buttonSettings, SIGNAL(clicked()), this, SLOT(onButtonSettings()));
    connect(buttonHelp, SIGNAL(clicked()), this, SLOT(onButtonHelp()));

	horizontalLayoutBottom->invalidate();
}

Drawer3D::~Drawer3D()
{
	delete glWidget;
}

void Drawer3D::closeEvent(QCloseEvent *event)
{
	closedByUser = true;
    settingsDlg.close();
	event->accept();
}

void Drawer3D::tuneSlider(QSlider* slider)
{
    slider->setRange(0, 360 * SLIDING_STEPS_PER_DEGREE);
    slider->setSingleStep(1);
    slider->setPageStep(SLIDING_STEPS_PER_DEGREE);
    slider->setTickInterval(SLIDING_STEPS_PER_DEGREE * SLIDING_STEPS_PER_DEGREE);
    slider->setTickPosition(QSlider::TicksRight);
}

void Drawer3D::resizeEvent(QResizeEvent*)
{
	glWidget->setGeometry(QRect(0, 0, widget->size().width(), widget->size().height()));
}

void Drawer3D::warning(const QString& text)
{
    QMessageBox::warning (this, tr("Warning"), text);
}

void Drawer3D::setVarNames()
{
    xAxisBox->clear();
    yAxisBox->clear();
    std::vector<std::string> names;
    runge->get_names(names, true, true);
    std::vector<std::string>::const_iterator it = names.begin();
    while (it != names.end()) {
        xAxisBox->addItem(it->c_str());
        yAxisBox->addItem(it->c_str());
        zAxisBox->addItem(it->c_str());
        ++it;
    }
    xAxisBox->setCurrentIndex(0);
    yAxisBox->setCurrentIndex(1);
    zAxisBox->setCurrentIndex(2);
}

void Drawer3D::xAxisVarChanged(int)
{
    repaint();
}

void Drawer3D::yAxisVarChanged(int)
{
    repaint();
}

void Drawer3D::zAxisVarChanged(int)
{
    repaint();
}

void Drawer3D::show()
{
	QMainWindow::show();
    repaint();
}

void Drawer3D::redrawAllSolutions() const
{
//    AutoWaitCursor waitCursor;
    Solutions::const_iterator its = solutions.begin();
    while (its != solutions.end()) {
        drawSolution(its++->first);
    }
}

void Drawer3D::repaint() const
{
	glWidget->updateGL();
}

bool Drawer3D::drawSolution(int sol_id) const
{
    const std::string xvar = xAxisBox->currentText().toStdString();
    const std::string yvar = yAxisBox->currentText().toStdString();
    const std::string zvar = zAxisBox->currentText().toStdString();
    Solutions::iterator it = solutions.find(sol_id);
    if (it != solutions.end()) {
        Solution& sol = it->second;
        const int xindex = sol.get_var_index(xvar);
        const int yindex = sol.get_var_index(yvar);
        const int zindex = sol.get_var_index(zvar);
        if (xindex < 0 || yindex < 0 || zindex < 0) {
            return false;
        }
		glWidget->draw(sol, xindex, yindex, zindex);
    }    
    return true;
}

void Drawer3D::keyReleaseEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Escape) {
        if (panMode || panHoldMode) {
            pan(false, false);
        }
    }
    else if (event->key() == Qt::Key_F1) {
        onButtonHelp();
    }
    else {
        QMainWindow::keyReleaseEvent(event);
    }
}

void Drawer3D::enableAllButtons(bool enable)
{
	xAxisBox->setEnabled(enable);
	yAxisBox->setEnabled(enable);
	zAxisBox->setEnabled(enable);
	buttonSettings->setEnabled(enable);
	buttonExport->setEnabled(enable);
}

void Drawer3D::onPan()
{
	pan(buttonPan->isChecked(), false);
}

void Drawer3D::pan(bool _panMode, bool _panHoldMode)
{
	panMode = _panMode;
	panHoldMode = _panHoldMode;
	enableAllButtons(!panMode);
	if (panMode) {
		setCursor(panHoldMode ? cursorPalmHold : cursorPalm);
	} else {
		buttonPan->setChecked(false);
		unsetCursor();
	}
}

QImage Drawer3D::toImage(int w, int h)
{
	// https://bugreports.qt-project.org/browse/QTBUG-33186
	//	return glWidget->renderPixmap(w, h).toImage();

	return glWidget->toImage(w, h);
}

void Drawer3D::xSpinBoxChanged(double realAngle)
{
	glWidget->setXRotation(iround(realAngle*SLIDING_STEPS_PER_DEGREED));
}

void Drawer3D::ySpinBoxChanged(double realAngle)
{
	glWidget->setYRotation(iround(realAngle*SLIDING_STEPS_PER_DEGREED));
}

void Drawer3D::zSpinBoxChanged(double realAngle)
{
	glWidget->setZRotation(iround(realAngle*SLIDING_STEPS_PER_DEGREED));
}

void Drawer3D::zoomFactorChanged(double zoomFactor)
{
	glWidget->setZoomFactor(zoomFactor);
}

void Drawer3D::onButtonExport()
{
    exportToImageDlg.show();
}

void Drawer3D::onButtonSettings()
{
    settingsDlg.show();
}

void Drawer3D::onButtonHelp()
{
    getRunge()->showHelp("3D grahics");
}
