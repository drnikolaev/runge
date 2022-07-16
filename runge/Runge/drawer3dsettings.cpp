#include <QColorDialog>
#include <QDebug>
#include <QMessageBox>

#include <algorithm>

#include "utils.h"
#include "drawer3dsettings.h"
#include "drawer3d.h"

Drawer3DSettings::Drawer3DSettings(Drawer3D* parent) :
    QDialog(parent, Qt::WindowSystemMenuHint | Qt::WindowTitleHint), 
    drawer3d(parent), 
    doubleValidator(this), 
    intValidator(1, 20, this), 
    xAxisColor(drawer3d->xAxisColor),
    yAxisColor(drawer3d->yAxisColor),
    zAxisColor(drawer3d->zAxisColor),
	colorDialog(this)
{
    setupUi(this);

    lineEditXFrom->setValidator(&doubleValidator);
    lineEditXTo->setValidator(&doubleValidator);
    lineEditYFrom->setValidator(&doubleValidator);
    lineEditYTo->setValidator(&doubleValidator);
	lineEditZFrom->setValidator(&doubleValidator);
	lineEditZTo->setValidator(&doubleValidator);

	lineEditAxesWidth->setValidator(&doubleValidator);
	lineEditSolutionsWidth->setValidator(&doubleValidator);
    lineEditHandleDiameter->setValidator(&intValidator);

    connect(buttonBox, SIGNAL(clicked(QAbstractButton*)), this, SLOT(onButtonBox(QAbstractButton*)));
    connect(pushButtonXAxisColor, SIGNAL(clicked()), this, SLOT(onXAxisColor()));
    connect(pushButtonYAxisColor, SIGNAL(clicked()), this, SLOT(onYAxisColor()));
    connect(pushButtonZAxisColor, SIGNAL(clicked()), this, SLOT(onZAxisColor()));
    connect(pushButtonAdjustLengths, SIGNAL(clicked()), this, SLOT(onAdjustLengths()));
    connect(buttonBox, SIGNAL(helpRequested()), this, SLOT(showHelp()));
}

Drawer3DSettings::~Drawer3DSettings()
{
}

void Drawer3DSettings::showEvent (QShowEvent*)
{
    static const QString a1("%1");
	groupBoxAxes->setChecked(drawer3d->showAxes);
	lineEditXFrom->setText(a1.arg(drawer3d->xAxisFrom));
	lineEditXTo->setText(a1.arg(drawer3d->xAxisTo));
	lineEditYFrom->setText(a1.arg(drawer3d->yAxisFrom));
	lineEditYTo->setText(a1.arg(drawer3d->yAxisTo));
	lineEditZFrom->setText(a1.arg(drawer3d->zAxisFrom));
	lineEditZTo->setText(a1.arg(drawer3d->zAxisTo));

    lineEditAxesWidth->setText(a1.arg(drawer3d->axesWidth));
    lineEditSolutionsWidth->setText(a1.arg(drawer3d->solutionsWidth));
    lineEditHandleDiameter->setText(a1.arg(drawer3d->handleDiameter));

	xAxisColor = drawer3d->xAxisColor;
	yAxisColor = drawer3d->yAxisColor;
	zAxisColor = drawer3d->zAxisColor;
}

bool Drawer3DSettings::applyChanges()
{
	if (!enforcePositiveValue(this, lineEditAxesWidth) ||
		!enforcePositiveValue(this, lineEditSolutionsWidth) ||
		!enforcePositiveValue(this, lineEditHandleDiameter)) {
			return false;
	}

	drawer3d->showAxes = groupBoxAxes->isChecked();

	drawer3d->xAxisFrom = lineEditXFrom->text().toDouble();
	drawer3d->xAxisTo = lineEditXTo->text().toDouble();
	drawer3d->yAxisFrom = lineEditYFrom->text().toDouble();
	drawer3d->yAxisTo = lineEditYTo->text().toDouble();
	drawer3d->zAxisFrom = lineEditZFrom->text().toDouble();
	drawer3d->zAxisTo = lineEditZTo->text().toDouble();

	drawer3d->xAxisColor = xAxisColor;
	drawer3d->yAxisColor = yAxisColor;
	drawer3d->zAxisColor = zAxisColor;

	drawer3d->axesWidth = lineEditAxesWidth->text().toDouble();
	drawer3d->solutionsWidth = lineEditSolutionsWidth->text().toDouble();
	drawer3d->handleDiameter = lineEditHandleDiameter->text().toInt();
	return true;
}

void Drawer3DSettings::accept()
{
    if (applyChanges()) {
		QDialog::accept();
	}
}

void Drawer3DSettings::reject()
{
    QDialog::reject();
}

void Drawer3DSettings::onButtonBox(QAbstractButton* button)
{
    QDialogButtonBox::StandardButton buttonClicked = buttonBox->standardButton(button);
    if (buttonClicked == QDialogButtonBox::Apply) {
        applyChanges();
    }
}

void Drawer3DSettings::onXAxisColor()
{
	xAxisColor = pickColor(colorDialog, xAxisColor, tr("Select X axis color"));
}

void Drawer3DSettings::onYAxisColor()
{
	yAxisColor = pickColor(colorDialog, yAxisColor, tr("Select Y axis color"));
}

void Drawer3DSettings::onZAxisColor()
{
	zAxisColor = pickColor(colorDialog, zAxisColor, tr("Select Z axis color"));
}

void Drawer3DSettings::onAdjustLengths()
{
	const static QString a1("%1");
	lineEditXFrom->setText(a1.arg((std::min)(0.0, drawer3d->minXBox)));
	lineEditYFrom->setText(a1.arg((std::min)(0.0, drawer3d->minYBox)));
	lineEditZFrom->setText(a1.arg((std::min)(0.0, drawer3d->minZBox)));
	lineEditXTo->setText(a1.arg((std::max)(0.0, drawer3d->maxXBox)));
	lineEditYTo->setText(a1.arg((std::max)(0.0, drawer3d->maxYBox)));
	lineEditZTo->setText(a1.arg((std::max)(0.0, drawer3d->maxZBox)));
}

void Drawer3DSettings::keyReleaseEvent(QKeyEvent* event)
{
    QDialog::keyReleaseEvent(event);
    if (event->key() == Qt::Key_F1) {
        showHelp();
    }
}

void Drawer3DSettings::showHelp()
{
    drawer3d->getRunge()->showHelp("3D grahics settings");
}
