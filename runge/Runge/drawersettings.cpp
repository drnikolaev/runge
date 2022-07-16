#include <QColorDialog>
#include <QDebug>
#include <QMessageBox>
#include <algorithm>

#include "utils.h"
#include "drawersettings.h"
#include "drawer.h"

DrawerSettings::DrawerSettings(Drawer* parent) :
    QDialog(parent, Qt::WindowSystemMenuHint | Qt::WindowTitleHint), 
    drawer(parent), 
    doubleValidator(this), 
    intValidator(1, 20, this), 
    axesColor(drawer->axesColor),
	colorDialog(this)
{
    setupUi(this);

    lineEditLeftLength->setValidator(&doubleValidator);
    lineEditTopLength->setValidator(&doubleValidator);
    lineEditRightLength->setValidator(&doubleValidator);
    lineEditBottomLength->setValidator(&doubleValidator);
	lineEditStepX->setValidator(&doubleValidator);
	lineEditStepY->setValidator(&doubleValidator);
    lineEditHandleDiameter->setValidator(&intValidator);
    lineEditMarkLength->setValidator(&intValidator);
	lineEditAxesWidth->setValidator(&doubleValidator);
	lineEditSolutionsWidth->setValidator(&doubleValidator);

    connect(buttonBox, SIGNAL(clicked(QAbstractButton*)), this, SLOT(onButtonBox(QAbstractButton*)));
    connect(pushButtonAxesColor, SIGNAL(clicked()), this, SLOT(onAxesColor()));
    connect(pushButtonAdjustLengths, SIGNAL(clicked()), this, SLOT(onAdjustLengths()));
    connect(buttonBox, SIGNAL(helpRequested()), this, SLOT(showHelp()));
}

DrawerSettings::~DrawerSettings()
{
}

void DrawerSettings::showEvent (QShowEvent*)
{
    const static QString a1("%1");
    lineEditLeftLength->setText(a1.arg(drawer->axisLengthLeft));
    lineEditTopLength->setText(a1.arg(drawer->axisLengthTop));
    lineEditRightLength->setText(a1.arg(drawer->axisLengthRight));
    lineEditBottomLength->setText(a1.arg(drawer->axisLengthBottom));
	groupBoxAxes->setChecked(drawer->axesShown());
    checkBoxRescaleAfterRunningFromPoint->setCheckState(drawer->rescaleAfterRunningFromPoint ? Qt::Checked : Qt::Unchecked);
	lineEditStepX->setText(a1.arg(drawer->markStepX));
	lineEditStepY->setText(a1.arg(drawer->markStepY));
	groupBoxAxesMarks->setChecked(drawer->drawMarks);
	pushButtonAdjustLengths->setEnabled(drawer->mapOrbits.size() > 0);
    lineEditAxesWidth->setText(a1.arg(drawer->axesWidth));
    lineEditSolutionsWidth->setText(a1.arg(drawer->solutionsWidth));
    lineEditHandleDiameter->setText(a1.arg(drawer->handleDiameter));
    lineEditMarkLength->setText(a1.arg(drawer->markLength));
    axesColor = drawer->axesColor;
}

bool DrawerSettings::applyChanges()
{
	if (!enforcePositiveValue(this, lineEditStepX) ||
		!enforcePositiveValue(this, lineEditStepY) ||
		!enforceNonNegativeValue(this, lineEditLeftLength) ||
		!enforceNonNegativeValue(this, lineEditTopLength) ||
		!enforceNonNegativeValue(this, lineEditRightLength) ||
		!enforceNonNegativeValue(this, lineEditBottomLength) ||
        !enforceNonNegativeValue(this, lineEditAxesWidth) ||
        !enforceNonNegativeValue(this, lineEditSolutionsWidth)) {
			return false;
	}

	drawer->axesWidth = groupBoxLineWidths->isChecked() ? lineEditAxesWidth->text().toDouble() : 0.;
	drawer->solutionsWidth = groupBoxLineWidths->isChecked() ? lineEditSolutionsWidth->text().toDouble() : 0.;
	drawer->handleDiameter = lineEditHandleDiameter->text().toInt();
    drawer->markLength = lineEditMarkLength->text().toInt();

	if (groupBoxAxesMarks->isChecked() && groupBoxAxes->isChecked()) {
		drawer->markStepX = lineEditStepX->text().toDouble();
		drawer->markStepY = lineEditStepY->text().toDouble();
		drawer->drawMarks = true;
	} else {
		drawer->drawMarks = false;
	}
    if (groupBoxAxes->isChecked()) {
        drawer->axesColor = axesColor;
        drawer->axisLengthLeft = lineEditLeftLength->text().toDouble();
        drawer->axisLengthTop = lineEditTopLength->text().toDouble();
        drawer->axisLengthRight = lineEditRightLength->text().toDouble();
        drawer->axisLengthBottom = lineEditBottomLength->text().toDouble();
        drawer->showAxes(true);
    } else {
        drawer->showAxes(false);
    }

    drawer->scaleToBoundingRect();
    drawer->rescaleAfterRunningFromPoint = checkBoxRescaleAfterRunningFromPoint->checkState() == Qt::Checked;
	return true;
}

void DrawerSettings::accept()
{
    if (applyChanges()) {
		QDialog::accept();
	}
}

void DrawerSettings::reject()
{
    QDialog::reject();
}

void DrawerSettings::onButtonBox(QAbstractButton* button)
{
    QDialogButtonBox::StandardButton buttonClicked = buttonBox->standardButton(button);
    if (buttonClicked == QDialogButtonBox::Apply) {
        applyChanges();
    }
}

void DrawerSettings::onAxesColor()
{
	axesColor = pickColor(colorDialog, axesColor, tr("Select axes color"), QColorDialog::ShowAlphaChannel);
}

void DrawerSettings::onAdjustLengths()
{
    QRectF rect = drawer->scene.getAllOrbitsBoundingRect();
	if (rect != QRectF()) {
		const static QString a1("%1");
        const double incr = std::max<double>(rect.width(), rect.height()) * 0.05;
		lineEditLeftLength->setText(a1.arg(std::max<double>(-rect.left(), 0.) + incr));
		lineEditTopLength->setText(a1.arg(std::max<double>(-rect.top(), 0.) + incr));
		lineEditRightLength->setText(a1.arg(std::max<double>(rect.right(), 0.) + incr));
		lineEditBottomLength->setText(a1.arg(std::max<double>(rect.bottom(), 0.) + incr));
	}
}

void DrawerSettings::keyReleaseEvent(QKeyEvent* event)
{
    QDialog::keyReleaseEvent(event);
    if (event->key() == Qt::Key_F1) {
        showHelp();
    }
}

void DrawerSettings::showHelp()
{
    drawer->getRunge()->showHelp("2D grahics settings");
}
