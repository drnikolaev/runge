#include <QFileDialog>
#include <QPrinter>
#include <QPrintDialog>
#include <QDesktopServices>
#include <QUrl>

#include "utils.h"
#include "printto.h"
#include "drawer.h"

PrintTo::PrintTo(Drawer* parent) :
    QDialog(parent, Qt::WindowSystemMenuHint | Qt::WindowTitleHint),
    drawer(parent),
    doubleValidator(this)
{
	setupUi(this);

    lineEditAxesWidth->setValidator(&doubleValidator);
    lineEditMarksLength->setValidator(&doubleValidator);
    lineEditSolutionsWidth->setValidator(&doubleValidator);
    lineEditHandleDiameter->setValidator(&doubleValidator);

    comboBoxDocumentSize->addItem("Letter (8.5 x 11 inches, 215.9 x 279.4 mm)", QPrinter::Letter);
    comboBoxDocumentSize->addItem("A4 (210 x 297 mm, 8.26 x 11.69 in)", QPrinter::A4);
    comboBoxDocumentSize->addItem("B5 (176 x 250 mm, 6.93 x 9.84 in)", QPrinter::B5);
    comboBoxDocumentSize->addItem("Legal (8.5 x 14 inches, 215.9 x 355.6 mm)", QPrinter::Legal);
    comboBoxDocumentSize->addItem("Executive (7.5 x 10 inches, 190.5 x 254 mm)", QPrinter::Executive);
    comboBoxDocumentSize->addItem("A0 (841 x 1189 mm)", QPrinter::A0);
    comboBoxDocumentSize->addItem("A1 (594 x 841 mm)", QPrinter::A1);
    comboBoxDocumentSize->addItem("A2 (420 x 594 mm)", QPrinter::A2);
    comboBoxDocumentSize->addItem("A3 (297 x 420 mm)", QPrinter::A3);
    comboBoxDocumentSize->addItem("A5 (148 x 210 mm)", QPrinter::A5);
    comboBoxDocumentSize->addItem("A6 (105 x 148 mm)", QPrinter::A6);
    comboBoxDocumentSize->addItem("A7 (74 x 105 mm)", QPrinter::A7);
    comboBoxDocumentSize->addItem("A8 (52 x 74 mm)", QPrinter::A8);
    comboBoxDocumentSize->addItem("A9 (37 x 52 mm)", QPrinter::A9);
    comboBoxDocumentSize->addItem("B0 (1000 x 1414 mm)", QPrinter::B0);
    comboBoxDocumentSize->addItem("B1 (707 x 1000 mm)", QPrinter::B1);
    comboBoxDocumentSize->addItem("B2 (500 x 707 mm)", QPrinter::B2);
    comboBoxDocumentSize->addItem("B3 (353 x 500 mm)", QPrinter::B3);
    comboBoxDocumentSize->addItem("B4 (250 x 353 mm)", QPrinter::B4);
    comboBoxDocumentSize->addItem("B6 (125 x 176 mm)", QPrinter::B6);
    comboBoxDocumentSize->addItem("B7 (88 x 125 mm)", QPrinter::B7);
    comboBoxDocumentSize->addItem("B8 (62 x 88 mm)", QPrinter::B8);
    comboBoxDocumentSize->addItem("B9 (33 x 62 mm)", QPrinter::B9);
    comboBoxDocumentSize->addItem("B10 (31 x 44 mm)", QPrinter::B10);
    comboBoxDocumentSize->addItem("C5E (163 x 229 mm)", QPrinter::C5E);
    comboBoxDocumentSize->addItem("Comm10E (105 x 241 mm, U.S. Common 10 Envelope)", QPrinter::Comm10E);
    comboBoxDocumentSize->addItem("DLE (110 x 220 mm)", QPrinter::DLE);
    comboBoxDocumentSize->addItem("Folio (210 x 330 mm)", QPrinter::Folio);
    comboBoxDocumentSize->addItem("Ledger (431.8 x 279.4 mm)", QPrinter::Ledger);
    comboBoxDocumentSize->addItem("Tabloid (279.4 x 431.8 mm)", QPrinter::Tabloid);

//    connect(buttonBox, SIGNAL(clicked(QAbstractButton*)), this, SLOT(onButtonBox(QAbstractButton*)));
    connect(buttonBox, SIGNAL(helpRequested()), this, SLOT(showHelp()));
}

PrintTo::~PrintTo()
{
}

void PrintTo::showEvent(QShowEvent*)
{
    const static QString a1("%1");
    lineEditAxesWidth->setText(a1.arg(drawer->axesWidthToPrint));
    lineEditMarksLength->setText(a1.arg(drawer->markLengthToPrint));
    lineEditSolutionsWidth->setText(a1.arg(drawer->solutionsWidthToPrint));
    lineEditHandleDiameter->setText(a1.arg(drawer->handleDiameterToPrint));
}

void PrintTo::accept()
{
    if (!applyChanges()) {
        return;
    }
	bool printToFile = true;
    QString fileName;
    QPrinter printer(QPrinter::HighResolution);

    if (radioButtonPrintToPDF->isChecked()) {
        fileName = QFileDialog::getSaveFileName(this, tr("Export to PDF"), "", tr("PDF files (*.pdf)"));
    	if (fileName.size() <= 0) {
            return;
        }
		printer.setOutputFormat(QPrinter::PdfFormat);
#if QT_VERSION >= 0x050000
//#error "5.0.0 ?"
#else
    } else if (radioButtonPrintToPS->isChecked()) {
        fileName = QFileDialog::getSaveFileName(this, tr("Export to PS"), "", tr("PostScript files (*.ps)"));
    	if (fileName.size() <= 0) {
            return;
        }
		printer.setOutputFormat(QPrinter::PostScriptFormat);
#endif
    } else {
		QPrintDialog printDialog(&printer, this);
		printDialog.setOption(QAbstractPrintDialog::PrintSelection, false);
		printDialog.setOption(QAbstractPrintDialog::PrintToFile, false);
		printDialog.setOption(QAbstractPrintDialog::PrintPageRange, false);
		if (printDialog.exec() != QDialog::Accepted) {
			return;
		}
		printToFile = false;
	}

	if (printToFile) {
		printer.setOutputFileName(fileName);
		int	pageSizeIndex = comboBoxDocumentSize->currentIndex();
		if (pageSizeIndex >= 0) {
			int pageSize = comboBoxDocumentSize->itemData (pageSizeIndex).toInt();
			printer.setPageSize((QPrinter::PageSize) pageSize);
		} else {
			printer.setPageSize(QPrinter::Letter);
		}
	}

	AutoWaitCursor waitCursor;
    drawer->rescaleHandlesAxesMarks(false);
	QPainter painter(&printer);
	painter.setRenderHint(QPainter::Antialiasing);
	drawer->graphicsView->render(&painter);
    drawer->rescaleHandlesAxesMarks(true);
    drawer->redrawAllSolutions(false);

    if (checkBoxOpenAfterSave->isChecked() && printToFile) {
        QDesktopServices::openUrl(QUrl(QString("file:///") + fileName, QUrl::TolerantMode));
    }
}

//void PrintTo::onButtonBox(QAbstractButton* button)
//{
//    QDialogButtonBox::StandardButton buttonClicked = buttonBox->standardButton(button);
//    if (buttonClicked == QDialogButtonBox::Close) {
//        if (applyChanges()) {
//		    QDialog::accept();
//        }
//    }
//}

bool PrintTo::applyChanges()
{
	if (!enforceNonNegativeValue(this, lineEditAxesWidth) ||
		!enforceNonNegativeValue(this, lineEditMarksLength) ||
		!enforceNonNegativeValue(this, lineEditSolutionsWidth) ||
		!enforceNonNegativeValue(this, lineEditHandleDiameter)) {
			return false;
	}
    
    drawer->axesWidthToPrint = lineEditAxesWidth->text().toDouble();
    drawer->markLengthToPrint = lineEditMarksLength->text().toDouble();
    drawer->solutionsWidthToPrint = lineEditSolutionsWidth->text().toDouble();
    drawer->handleDiameterToPrint = lineEditHandleDiameter->text().toDouble();
    return true;
}

void PrintTo::keyReleaseEvent(QKeyEvent* event)
{
    QDialog::keyReleaseEvent(event);
    if (event->key() == Qt::Key_F1) {
        showHelp();
    }
}

void PrintTo::showHelp()
{
    drawer->getRunge()->showHelp("2D grahics print");
}
