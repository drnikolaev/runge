/********************************************************************************
** Form generated from reading UI file 'exporttoimage.ui'
**
** Created by: Qt User Interface Compiler version 5.2.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_EXPORTTOIMAGE_H
#define UI_EXPORTTOIMAGE_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QSlider>

QT_BEGIN_NAMESPACE

class Ui_ExportToImage
{
public:
    QLabel *label_3;
    QLabel *label_2;
    QLabel *labelQuality;
    QLabel *label;
    QSlider *sliderJPGQuality;
    QCheckBox *checkBoxOpenAfterSave;
    QLineEdit *lineEditHeight;
    QComboBox *comboBoxImageType;
    QDialogButtonBox *buttonBox;
    QLineEdit *lineEditWidth;
    QLabel *labelJPEGQuality;

    void setupUi(QDialog *ExportToImage)
    {
        if (ExportToImage->objectName().isEmpty())
            ExportToImage->setObjectName(QStringLiteral("ExportToImage"));
        ExportToImage->setWindowModality(Qt::WindowModal);
        ExportToImage->resize(698, 141);
        ExportToImage->setMinimumSize(QSize(698, 141));
        ExportToImage->setMaximumSize(QSize(698, 141));
        QIcon icon;
        icon.addFile(QStringLiteral(":/Runge/Resources/16x16/export1.png"), QSize(), QIcon::Normal, QIcon::Off);
        ExportToImage->setWindowIcon(icon);
        label_3 = new QLabel(ExportToImage);
        label_3->setObjectName(QStringLiteral("label_3"));
        label_3->setGeometry(QRect(10, 20, 121, 24));
        label_3->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        label_2 = new QLabel(ExportToImage);
        label_2->setObjectName(QStringLiteral("label_2"));
        label_2->setGeometry(QRect(440, 60, 61, 24));
        label_2->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        labelQuality = new QLabel(ExportToImage);
        labelQuality->setObjectName(QStringLiteral("labelQuality"));
        labelQuality->setEnabled(false);
        labelQuality->setGeometry(QRect(10, 60, 121, 24));
        labelQuality->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        label = new QLabel(ExportToImage);
        label->setObjectName(QStringLiteral("label"));
        label->setGeometry(QRect(440, 20, 61, 24));
        label->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        sliderJPGQuality = new QSlider(ExportToImage);
        sliderJPGQuality->setObjectName(QStringLiteral("sliderJPGQuality"));
        sliderJPGQuality->setEnabled(false);
        sliderJPGQuality->setGeometry(QRect(140, 62, 251, 24));
        sliderJPGQuality->setMaximum(100);
        sliderJPGQuality->setValue(100);
        sliderJPGQuality->setOrientation(Qt::Horizontal);
        checkBoxOpenAfterSave = new QCheckBox(ExportToImage);
        checkBoxOpenAfterSave->setObjectName(QStringLiteral("checkBoxOpenAfterSave"));
        checkBoxOpenAfterSave->setGeometry(QRect(140, 100, 291, 24));
        lineEditHeight = new QLineEdit(ExportToImage);
        lineEditHeight->setObjectName(QStringLiteral("lineEditHeight"));
        lineEditHeight->setGeometry(QRect(510, 60, 71, 24));
        comboBoxImageType = new QComboBox(ExportToImage);
        comboBoxImageType->setObjectName(QStringLiteral("comboBoxImageType"));
        comboBoxImageType->setGeometry(QRect(140, 20, 291, 24));
        buttonBox = new QDialogButtonBox(ExportToImage);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setGeometry(QRect(596, 20, 91, 101));
        buttonBox->setOrientation(Qt::Vertical);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Help|QDialogButtonBox::Ok);
        buttonBox->setCenterButtons(false);
        lineEditWidth = new QLineEdit(ExportToImage);
        lineEditWidth->setObjectName(QStringLiteral("lineEditWidth"));
        lineEditWidth->setGeometry(QRect(510, 20, 71, 24));
        labelJPEGQuality = new QLabel(ExportToImage);
        labelJPEGQuality->setObjectName(QStringLiteral("labelJPEGQuality"));
        labelJPEGQuality->setEnabled(false);
        labelJPEGQuality->setGeometry(QRect(400, 60, 31, 24));
        labelJPEGQuality->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
#ifndef QT_NO_SHORTCUT
        label_3->setBuddy(comboBoxImageType);
        label_2->setBuddy(lineEditHeight);
        labelQuality->setBuddy(sliderJPGQuality);
        label->setBuddy(lineEditWidth);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(comboBoxImageType, sliderJPGQuality);
        QWidget::setTabOrder(sliderJPGQuality, lineEditWidth);
        QWidget::setTabOrder(lineEditWidth, lineEditHeight);
        QWidget::setTabOrder(lineEditHeight, checkBoxOpenAfterSave);
        QWidget::setTabOrder(checkBoxOpenAfterSave, buttonBox);

        retranslateUi(ExportToImage);
        QObject::connect(buttonBox, SIGNAL(accepted()), ExportToImage, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), ExportToImage, SLOT(reject()));
        QObject::connect(sliderJPGQuality, SIGNAL(valueChanged(int)), labelJPEGQuality, SLOT(setNum(int)));

        QMetaObject::connectSlotsByName(ExportToImage);
    } // setupUi

    void retranslateUi(QDialog *ExportToImage)
    {
        ExportToImage->setWindowTitle(QApplication::translate("ExportToImage", "Export To Image", 0));
        label_3->setText(QApplication::translate("ExportToImage", "Image &Type", 0));
        label_2->setText(QApplication::translate("ExportToImage", "&Height", 0));
        labelQuality->setText(QApplication::translate("ExportToImage", "JPEG &Quality", 0));
        label->setText(QApplication::translate("ExportToImage", "&Width", 0));
#ifndef QT_NO_TOOLTIP
        sliderJPGQuality->setToolTip(QApplication::translate("ExportToImage", "Choose JPEG compression quality (more is better)", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        checkBoxOpenAfterSave->setToolTip(QApplication::translate("ExportToImage", "Open image created by default viewer", 0));
#endif // QT_NO_TOOLTIP
        checkBoxOpenAfterSave->setText(QApplication::translate("ExportToImage", "Open image a&fter save", 0));
#ifndef QT_NO_TOOLTIP
        lineEditHeight->setToolTip(QApplication::translate("ExportToImage", "Picture height (pixels)", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        comboBoxImageType->setToolTip(QApplication::translate("ExportToImage", "Choose image type to export to", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        lineEditWidth->setToolTip(QApplication::translate("ExportToImage", "Picture width (pixels)", 0));
#endif // QT_NO_TOOLTIP
        labelJPEGQuality->setText(QApplication::translate("ExportToImage", "100", 0));
    } // retranslateUi

};

namespace Ui {
    class ExportToImage: public Ui_ExportToImage {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_EXPORTTOIMAGE_H
