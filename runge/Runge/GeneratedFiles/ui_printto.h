/********************************************************************************
** Form generated from reading UI file 'printto.ui'
**
** Created by: Qt User Interface Compiler version 5.2.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_PRINTTO_H
#define UI_PRINTTO_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QGroupBox>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QRadioButton>

QT_BEGIN_NAMESPACE

class Ui_PrintTo
{
public:
    QDialogButtonBox *buttonBox;
    QCheckBox *checkBoxOpenAfterSave;
    QGroupBox *groupBox;
    QRadioButton *radioButtonPrintToPDF;
    QRadioButton *radioButtonPrintToPS;
    QRadioButton *radioButtonPrintToPaper;
    QComboBox *comboBoxDocumentSize;
    QLabel *label;
    QLineEdit *lineEditMarksLength;
    QLabel *label_8;
    QLineEdit *lineEditHandleDiameter;
    QLabel *label_9;
    QLabel *label_10;
    QLineEdit *lineEditAxesWidth;
    QLineEdit *lineEditSolutionsWidth;
    QLabel *label_11;

    void setupUi(QDialog *PrintTo)
    {
        if (PrintTo->objectName().isEmpty())
            PrintTo->setObjectName(QStringLiteral("PrintTo"));
        PrintTo->setWindowModality(Qt::WindowModal);
        PrintTo->resize(730, 187);
        PrintTo->setMinimumSize(QSize(730, 187));
        PrintTo->setMaximumSize(QSize(730, 187));
        QIcon icon;
        icon.addFile(QStringLiteral(":/Runge/Resources/16x16/printer.png"), QSize(), QIcon::Normal, QIcon::Off);
        PrintTo->setWindowIcon(icon);
        buttonBox = new QDialogButtonBox(PrintTo);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setGeometry(QRect(627, 20, 91, 101));
        buttonBox->setOrientation(Qt::Vertical);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Help|QDialogButtonBox::Ok);
        buttonBox->setCenterButtons(false);
        checkBoxOpenAfterSave = new QCheckBox(PrintTo);
        checkBoxOpenAfterSave->setObjectName(QStringLiteral("checkBoxOpenAfterSave"));
        checkBoxOpenAfterSave->setGeometry(QRect(30, 150, 411, 24));
        checkBoxOpenAfterSave->setChecked(true);
        groupBox = new QGroupBox(PrintTo);
        groupBox->setObjectName(QStringLiteral("groupBox"));
        groupBox->setGeometry(QRect(10, 10, 161, 131));
        radioButtonPrintToPDF = new QRadioButton(groupBox);
        radioButtonPrintToPDF->setObjectName(QStringLiteral("radioButtonPrintToPDF"));
        radioButtonPrintToPDF->setGeometry(QRect(20, 30, 131, 24));
        radioButtonPrintToPDF->setChecked(true);
        radioButtonPrintToPS = new QRadioButton(groupBox);
        radioButtonPrintToPS->setObjectName(QStringLiteral("radioButtonPrintToPS"));
        radioButtonPrintToPS->setEnabled(false);
        radioButtonPrintToPS->setGeometry(QRect(20, 60, 141, 24));
        radioButtonPrintToPaper = new QRadioButton(groupBox);
        radioButtonPrintToPaper->setObjectName(QStringLiteral("radioButtonPrintToPaper"));
        radioButtonPrintToPaper->setGeometry(QRect(20, 90, 82, 24));
        comboBoxDocumentSize = new QComboBox(PrintTo);
        comboBoxDocumentSize->setObjectName(QStringLiteral("comboBoxDocumentSize"));
        comboBoxDocumentSize->setGeometry(QRect(320, 20, 291, 24));
        label = new QLabel(PrintTo);
        label->setObjectName(QStringLiteral("label"));
        label->setGeometry(QRect(180, 20, 131, 24));
        label->setLayoutDirection(Qt::LeftToRight);
        label->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        lineEditMarksLength = new QLineEdit(PrintTo);
        lineEditMarksLength->setObjectName(QStringLiteral("lineEditMarksLength"));
        lineEditMarksLength->setGeometry(QRect(320, 100, 71, 24));
        label_8 = new QLabel(PrintTo);
        label_8->setObjectName(QStringLiteral("label_8"));
        label_8->setGeometry(QRect(180, 100, 131, 24));
        label_8->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        lineEditHandleDiameter = new QLineEdit(PrintTo);
        lineEditHandleDiameter->setObjectName(QStringLiteral("lineEditHandleDiameter"));
        lineEditHandleDiameter->setGeometry(QRect(540, 100, 71, 24));
        label_9 = new QLabel(PrintTo);
        label_9->setObjectName(QStringLiteral("label_9"));
        label_9->setGeometry(QRect(400, 100, 131, 24));
        label_9->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        label_10 = new QLabel(PrintTo);
        label_10->setObjectName(QStringLiteral("label_10"));
        label_10->setGeometry(QRect(190, 60, 121, 24));
        label_10->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        lineEditAxesWidth = new QLineEdit(PrintTo);
        lineEditAxesWidth->setObjectName(QStringLiteral("lineEditAxesWidth"));
        lineEditAxesWidth->setGeometry(QRect(320, 60, 71, 24));
        lineEditSolutionsWidth = new QLineEdit(PrintTo);
        lineEditSolutionsWidth->setObjectName(QStringLiteral("lineEditSolutionsWidth"));
        lineEditSolutionsWidth->setGeometry(QRect(540, 60, 71, 24));
        label_11 = new QLabel(PrintTo);
        label_11->setObjectName(QStringLiteral("label_11"));
        label_11->setGeometry(QRect(390, 60, 141, 24));
        label_11->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
#ifndef QT_NO_SHORTCUT
        label->setBuddy(comboBoxDocumentSize);
        label_8->setBuddy(lineEditMarksLength);
        label_9->setBuddy(lineEditHandleDiameter);
        label_10->setBuddy(lineEditAxesWidth);
        label_11->setBuddy(lineEditSolutionsWidth);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(radioButtonPrintToPDF, radioButtonPrintToPS);
        QWidget::setTabOrder(radioButtonPrintToPS, radioButtonPrintToPaper);
        QWidget::setTabOrder(radioButtonPrintToPaper, comboBoxDocumentSize);
        QWidget::setTabOrder(comboBoxDocumentSize, lineEditAxesWidth);
        QWidget::setTabOrder(lineEditAxesWidth, lineEditMarksLength);
        QWidget::setTabOrder(lineEditMarksLength, lineEditSolutionsWidth);
        QWidget::setTabOrder(lineEditSolutionsWidth, lineEditHandleDiameter);
        QWidget::setTabOrder(lineEditHandleDiameter, checkBoxOpenAfterSave);
        QWidget::setTabOrder(checkBoxOpenAfterSave, buttonBox);

        retranslateUi(PrintTo);
        QObject::connect(buttonBox, SIGNAL(accepted()), PrintTo, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), PrintTo, SLOT(reject()));
        QObject::connect(radioButtonPrintToPaper, SIGNAL(toggled(bool)), checkBoxOpenAfterSave, SLOT(setDisabled(bool)));
        QObject::connect(radioButtonPrintToPaper, SIGNAL(toggled(bool)), comboBoxDocumentSize, SLOT(setDisabled(bool)));

        QMetaObject::connectSlotsByName(PrintTo);
    } // setupUi

    void retranslateUi(QDialog *PrintTo)
    {
        PrintTo->setWindowTitle(QApplication::translate("PrintTo", "Print To Document/Paper", 0));
#ifndef QT_NO_TOOLTIP
        checkBoxOpenAfterSave->setToolTip(QApplication::translate("PrintTo", "Open exported document", 0));
#endif // QT_NO_TOOLTIP
        checkBoxOpenAfterSave->setText(QApplication::translate("PrintTo", "Open document a&fter save", 0));
        groupBox->setTitle(QApplication::translate("PrintTo", "Print To", 0));
#ifndef QT_NO_TOOLTIP
        radioButtonPrintToPDF->setToolTip(QApplication::translate("PrintTo", "Export current drawing to PDF file", 0));
#endif // QT_NO_TOOLTIP
        radioButtonPrintToPDF->setText(QApplication::translate("PrintTo", "P&DF file", 0));
#ifndef QT_NO_TOOLTIP
        radioButtonPrintToPS->setToolTip(QApplication::translate("PrintTo", "Export current drawing to PS file (Not available yet in Qt5)", 0));
#endif // QT_NO_TOOLTIP
        radioButtonPrintToPS->setText(QApplication::translate("PrintTo", "Post&Script file", 0));
#ifndef QT_NO_TOOLTIP
        radioButtonPrintToPaper->setToolTip(QApplication::translate("PrintTo", "Print current drawing to paper", 0));
#endif // QT_NO_TOOLTIP
        radioButtonPrintToPaper->setText(QApplication::translate("PrintTo", "&Paper", 0));
#ifndef QT_NO_TOOLTIP
        comboBoxDocumentSize->setToolTip(QApplication::translate("PrintTo", "PDF/PS document size", 0));
#endif // QT_NO_TOOLTIP
        label->setText(QApplication::translate("PrintTo", "Document Si&ze", 0));
#ifndef QT_NO_TOOLTIP
        lineEditMarksLength->setToolTip(QApplication::translate("PrintTo", "Axes marks length (in coordinate units)", 0));
#endif // QT_NO_TOOLTIP
        label_8->setText(QApplication::translate("PrintTo", "Axes &marks length", 0));
#ifndef QT_NO_TOOLTIP
        lineEditHandleDiameter->setToolTip(QApplication::translate("PrintTo", "Solution handles diameter (in coordinate units)", 0));
#endif // QT_NO_TOOLTIP
        label_9->setText(QApplication::translate("PrintTo", "&Handle diameter", 0));
        label_10->setText(QApplication::translate("PrintTo", "Axes &line width", 0));
#ifndef QT_NO_TOOLTIP
        lineEditAxesWidth->setToolTip(QApplication::translate("PrintTo", "Axes lime width (in coordinate units)", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        lineEditSolutionsWidth->setToolTip(QApplication::translate("PrintTo", "Solutions line width (in coordinate units)", 0));
#endif // QT_NO_TOOLTIP
        label_11->setText(QApplication::translate("PrintTo", "Sol&utions line width", 0));
    } // retranslateUi

};

namespace Ui {
    class PrintTo: public Ui_PrintTo {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PRINTTO_H
