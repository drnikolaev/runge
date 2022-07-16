#include <QFileDialog>
#include <QDesktopServices>
#include <QUrl>
#include <QImageWriter>
#include <QPixmap>

#include <QMessageBox>

#include "exporttoimage.h"
#include "utils.h"
#include "drawer.h"
#include "drawer3d.h"

ExportToImage::ExportToImage(Drawer* parent, Drawer3D* parent3D) :
	QDialog(parent3D == nullptr ? (QWidget*) parent : (QWidget*) parent3D, Qt::WindowSystemMenuHint | Qt::WindowTitleHint),
    drawer(parent), drawer3d(parent3D),
    intValidator(1, 1000000000, this)
{
	setupUi(this);

    lineEditWidth->setValidator(&intValidator);
    lineEditHeight->setValidator(&intValidator);

    connect(comboBoxImageType, SIGNAL(currentIndexChanged(int)), this, SLOT(onImageTypeChanged(int)));
    connect(buttonBox, SIGNAL(helpRequested()), this, SLOT(showHelp()));
}

ExportToImage::~ExportToImage()
{
}

void ExportToImage::showEvent(QShowEvent*)
{
    const static QString a1("%1");
	if (drawer3d != nullptr) {   // for GLWidget it means current window size
		lineEditWidth->setText(a1.arg(drawer3d->widgetWidth()));
		lineEditHeight->setText(a1.arg(drawer3d->widgetHeight()));
	} else if (drawer != nullptr) {
		lineEditWidth->setText(a1.arg(drawer->viewWidth()));
		lineEditHeight->setText(a1.arg(drawer->viewHeight()));
	}

    QList<QByteArray> formats = QImageWriter::supportedImageFormats();
    QList<QByteArray>::iterator it = formats.begin();
    while (it != formats.end()) {
        QString format = *it++;
        if (format == "png") {
            comboBoxImageType->addItem("PNG - Portable Network Graphics", format);
        } else if (format == "ico") {
            comboBoxImageType->addItem("ICO - Windows Icon", format);
        } else if (format == "jpg") {
            comboBoxImageType->addItem("JPG - Joint Photographic Experts Group", format);
        } else if (format == "jpeg") {
            comboBoxImageType->addItem("JPEG - Joint Photographic Experts Group", format);
        } else if (format == "bmp") {
            comboBoxImageType->addItem("BMP - Windows Bitmap", format);
        } else if (format == "ppm") {
            comboBoxImageType->addItem("PPM - Portable Pixmap", format);
        } else if (format == "tif") {
            comboBoxImageType->addItem("TIF - Tagged Image File Format", format);
        } else if (format == "tiff") {
            comboBoxImageType->addItem("TIFF - Tagged Image File Format", format);
        } else if (format == "xbm") {
            comboBoxImageType->addItem("XBM - X11 Bitmap", format);
        } else if (format == "xpm") {
            comboBoxImageType->addItem("XPM - X11 Pixmap", format);
        } else {
            comboBoxImageType->addItem(format, format);
        }
    }
}

void ExportToImage::onImageTypeChanged(int index)
{
    if (index >= 0) {
        QString format = comboBoxImageType->itemData(index).toString();
        if (format == "jpg" || format == "jpeg" ) {
            labelQuality->setEnabled(true);
            sliderJPGQuality->setEnabled(true);
            labelJPEGQuality->setEnabled(true);
        } else {
            labelQuality->setEnabled(false);
            sliderJPGQuality->setEnabled(false);
            labelJPEGQuality->setEnabled(false);
        }
    }
}


void ExportToImage::accept()
{
	int	imageTypeIndex = comboBoxImageType->currentIndex();
	if (imageTypeIndex >= 0) {
        QString title = comboBoxImageType->itemText(imageTypeIndex);
        QString format = comboBoxImageType->itemData(imageTypeIndex).toString();

        QString fileName = QFileDialog::getSaveFileName(this, tr("Export to ") + title, "", format.toUpper() + tr(" files (*.") + format + ")");

	    if (fileName.size() > 0) {
            const int width = lineEditWidth->text().toInt();
            const int height = lineEditHeight->text().toInt();

		    AutoWaitCursor waitCursor;
			QImage outerImage;
			if (drawer != nullptr) {
				drawer->rescaleHandlesAxesMarks(false);
				QImage image (width, height,  QImage::Format_ARGB32_Premultiplied);
				image.fill((uint)-1);	// paint it white
				QPainter painter(&image);
				painter.setRenderHint(QPainter::Antialiasing);
				drawer->graphicsView->render(&painter);
				outerImage = image;	// copy-on-write
			} else {
				outerImage = drawer3d->toImage(width, height);
			}

			QImageWriter writer(fileName, format.toLatin1());            
			if (writer.supportsOption(QImageIOHandler::Description)) {
				writer.setText("Title", "Runge");
			}
			writer.write(outerImage);

			if (drawer != nullptr) {
				drawer->rescaleHandlesAxesMarks(true);
			}
			if (checkBoxOpenAfterSave->isChecked()) {
                QDesktopServices::openUrl(QUrl(QString("file:///") + fileName, QUrl::TolerantMode));
            }
        }
    }
}

void ExportToImage::keyReleaseEvent(QKeyEvent* event)
{
    QDialog::keyReleaseEvent(event);
    if (event->key() == Qt::Key_F1) {
        showHelp();
    }
}

void ExportToImage::showHelp()
{
	if (drawer != nullptr) {
		drawer->getRunge()->showHelp("2D grahics export to file");
	} else if (drawer3d != nullptr) {
		drawer3d->getRunge()->showHelp("3D grahics export to file");
	}
}
