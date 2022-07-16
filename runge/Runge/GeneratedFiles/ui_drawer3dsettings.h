/********************************************************************************
** Form generated from reading UI file 'drawer3dsettings.ui'
**
** Created by: Qt User Interface Compiler version 5.2.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_DRAWER3DSETTINGS_H
#define UI_DRAWER3DSETTINGS_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QGroupBox>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>

QT_BEGIN_NAMESPACE

class Ui_Drawer3DSettings
{
public:
    QGroupBox *groupBoxAxes;
    QPushButton *pushButtonAdjustLengths;
    QPushButton *pushButtonXAxisColor;
    QLineEdit *lineEditXFrom;
    QLineEdit *lineEditXTo;
    QLabel *label_11;
    QLabel *label_12;
    QLineEdit *lineEditYFrom;
    QLineEdit *lineEditYTo;
    QPushButton *pushButtonYAxisColor;
    QLabel *label_13;
    QLineEdit *lineEditZFrom;
    QLineEdit *lineEditZTo;
    QPushButton *pushButtonZAxisColor;
    QDialogButtonBox *buttonBox;
    QGroupBox *groupBoxHandles;
    QLineEdit *lineEditHandleDiameter;
    QLabel *label_10;
    QGroupBox *groupBoxLineWidths;
    QLineEdit *lineEditAxesWidth;
    QLabel *label_8;
    QLabel *label_9;
    QLineEdit *lineEditSolutionsWidth;

    void setupUi(QDialog *Drawer3DSettings)
    {
        if (Drawer3DSettings->objectName().isEmpty())
            Drawer3DSettings->setObjectName(QStringLiteral("Drawer3DSettings"));
        Drawer3DSettings->setWindowModality(Qt::WindowModal);
        Drawer3DSettings->resize(523, 375);
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(Drawer3DSettings->sizePolicy().hasHeightForWidth());
        Drawer3DSettings->setSizePolicy(sizePolicy);
        Drawer3DSettings->setMinimumSize(QSize(523, 375));
        Drawer3DSettings->setMaximumSize(QSize(523, 375));
        QIcon icon;
        icon.addFile(QStringLiteral(":/Runge/Resources/16x16/wrench.png"), QSize(), QIcon::Normal, QIcon::Off);
        Drawer3DSettings->setWindowIcon(icon);
        groupBoxAxes = new QGroupBox(Drawer3DSettings);
        groupBoxAxes->setObjectName(QStringLiteral("groupBoxAxes"));
        groupBoxAxes->setGeometry(QRect(10, 10, 391, 191));
        groupBoxAxes->setCheckable(true);
        groupBoxAxes->setChecked(true);
        pushButtonAdjustLengths = new QPushButton(groupBoxAxes);
        pushButtonAdjustLengths->setObjectName(QStringLiteral("pushButtonAdjustLengths"));
        pushButtonAdjustLengths->setGeometry(QRect(290, 150, 91, 24));
        pushButtonXAxisColor = new QPushButton(groupBoxAxes);
        pushButtonXAxisColor->setObjectName(QStringLiteral("pushButtonXAxisColor"));
        pushButtonXAxisColor->setGeometry(QRect(290, 30, 91, 24));
        lineEditXFrom = new QLineEdit(groupBoxAxes);
        lineEditXFrom->setObjectName(QStringLiteral("lineEditXFrom"));
        lineEditXFrom->setGeometry(QRect(100, 30, 71, 24));
        lineEditXTo = new QLineEdit(groupBoxAxes);
        lineEditXTo->setObjectName(QStringLiteral("lineEditXTo"));
        lineEditXTo->setGeometry(QRect(190, 30, 71, 24));
        label_11 = new QLabel(groupBoxAxes);
        label_11->setObjectName(QStringLiteral("label_11"));
        label_11->setGeometry(QRect(0, 30, 91, 24));
        label_11->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        label_12 = new QLabel(groupBoxAxes);
        label_12->setObjectName(QStringLiteral("label_12"));
        label_12->setGeometry(QRect(0, 70, 91, 24));
        label_12->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        lineEditYFrom = new QLineEdit(groupBoxAxes);
        lineEditYFrom->setObjectName(QStringLiteral("lineEditYFrom"));
        lineEditYFrom->setGeometry(QRect(100, 70, 71, 24));
        lineEditYTo = new QLineEdit(groupBoxAxes);
        lineEditYTo->setObjectName(QStringLiteral("lineEditYTo"));
        lineEditYTo->setGeometry(QRect(190, 70, 71, 24));
        pushButtonYAxisColor = new QPushButton(groupBoxAxes);
        pushButtonYAxisColor->setObjectName(QStringLiteral("pushButtonYAxisColor"));
        pushButtonYAxisColor->setGeometry(QRect(290, 70, 91, 24));
        label_13 = new QLabel(groupBoxAxes);
        label_13->setObjectName(QStringLiteral("label_13"));
        label_13->setGeometry(QRect(0, 110, 91, 24));
        label_13->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        lineEditZFrom = new QLineEdit(groupBoxAxes);
        lineEditZFrom->setObjectName(QStringLiteral("lineEditZFrom"));
        lineEditZFrom->setGeometry(QRect(100, 110, 71, 24));
        lineEditZTo = new QLineEdit(groupBoxAxes);
        lineEditZTo->setObjectName(QStringLiteral("lineEditZTo"));
        lineEditZTo->setGeometry(QRect(190, 110, 71, 24));
        pushButtonZAxisColor = new QPushButton(groupBoxAxes);
        pushButtonZAxisColor->setObjectName(QStringLiteral("pushButtonZAxisColor"));
        pushButtonZAxisColor->setGeometry(QRect(290, 110, 91, 24));
        buttonBox = new QDialogButtonBox(Drawer3DSettings);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setGeometry(QRect(417, 16, 91, 101));
        buttonBox->setOrientation(Qt::Vertical);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Help|QDialogButtonBox::Ok);
        buttonBox->setCenterButtons(false);
        groupBoxHandles = new QGroupBox(Drawer3DSettings);
        groupBoxHandles->setObjectName(QStringLiteral("groupBoxHandles"));
        groupBoxHandles->setGeometry(QRect(10, 290, 391, 71));
        lineEditHandleDiameter = new QLineEdit(groupBoxHandles);
        lineEditHandleDiameter->setObjectName(QStringLiteral("lineEditHandleDiameter"));
        lineEditHandleDiameter->setGeometry(QRect(100, 30, 71, 24));
        label_10 = new QLabel(groupBoxHandles);
        label_10->setObjectName(QStringLiteral("label_10"));
        label_10->setGeometry(QRect(0, 30, 91, 24));
        label_10->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        groupBoxLineWidths = new QGroupBox(Drawer3DSettings);
        groupBoxLineWidths->setObjectName(QStringLiteral("groupBoxLineWidths"));
        groupBoxLineWidths->setGeometry(QRect(10, 210, 391, 71));
        groupBoxLineWidths->setCheckable(false);
        groupBoxLineWidths->setChecked(false);
        lineEditAxesWidth = new QLineEdit(groupBoxLineWidths);
        lineEditAxesWidth->setObjectName(QStringLiteral("lineEditAxesWidth"));
        lineEditAxesWidth->setGeometry(QRect(100, 30, 71, 24));
        label_8 = new QLabel(groupBoxLineWidths);
        label_8->setObjectName(QStringLiteral("label_8"));
        label_8->setGeometry(QRect(20, 30, 71, 24));
        label_8->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        label_9 = new QLabel(groupBoxLineWidths);
        label_9->setObjectName(QStringLiteral("label_9"));
        label_9->setGeometry(QRect(180, 30, 101, 24));
        label_9->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        lineEditSolutionsWidth = new QLineEdit(groupBoxLineWidths);
        lineEditSolutionsWidth->setObjectName(QStringLiteral("lineEditSolutionsWidth"));
        lineEditSolutionsWidth->setGeometry(QRect(290, 30, 71, 24));
#ifndef QT_NO_SHORTCUT
        label_11->setBuddy(lineEditXFrom);
        label_12->setBuddy(lineEditYFrom);
        label_13->setBuddy(lineEditZFrom);
        label_10->setBuddy(lineEditHandleDiameter);
        label_8->setBuddy(lineEditAxesWidth);
        label_9->setBuddy(lineEditSolutionsWidth);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(groupBoxAxes, lineEditXFrom);
        QWidget::setTabOrder(lineEditXFrom, lineEditXTo);
        QWidget::setTabOrder(lineEditXTo, pushButtonXAxisColor);
        QWidget::setTabOrder(pushButtonXAxisColor, lineEditYFrom);
        QWidget::setTabOrder(lineEditYFrom, lineEditYTo);
        QWidget::setTabOrder(lineEditYTo, pushButtonYAxisColor);
        QWidget::setTabOrder(pushButtonYAxisColor, lineEditZFrom);
        QWidget::setTabOrder(lineEditZFrom, lineEditZTo);
        QWidget::setTabOrder(lineEditZTo, pushButtonZAxisColor);
        QWidget::setTabOrder(pushButtonZAxisColor, pushButtonAdjustLengths);
        QWidget::setTabOrder(pushButtonAdjustLengths, lineEditAxesWidth);
        QWidget::setTabOrder(lineEditAxesWidth, lineEditSolutionsWidth);
        QWidget::setTabOrder(lineEditSolutionsWidth, lineEditHandleDiameter);
        QWidget::setTabOrder(lineEditHandleDiameter, buttonBox);

        retranslateUi(Drawer3DSettings);
        QObject::connect(buttonBox, SIGNAL(accepted()), Drawer3DSettings, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), Drawer3DSettings, SLOT(reject()));

        QMetaObject::connectSlotsByName(Drawer3DSettings);
    } // setupUi

    void retranslateUi(QDialog *Drawer3DSettings)
    {
        Drawer3DSettings->setWindowTitle(QApplication::translate("Drawer3DSettings", "3D Drawer Settings", 0));
#ifndef QT_NO_TOOLTIP
        Drawer3DSettings->setToolTip(QString());
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        groupBoxAxes->setToolTip(QApplication::translate("Drawer3DSettings", "Show axes", 0));
#endif // QT_NO_TOOLTIP
        groupBoxAxes->setTitle(QApplication::translate("Drawer3DSettings", "&Axes", 0));
#ifndef QT_NO_TOOLTIP
        pushButtonAdjustLengths->setToolTip(QApplication::translate("Drawer3DSettings", "Reset all axes lengths to fit current graph", 0));
#endif // QT_NO_TOOLTIP
        pushButtonAdjustLengths->setText(QApplication::translate("Drawer3DSettings", "Ad&just", 0));
#ifndef QT_NO_TOOLTIP
        pushButtonXAxisColor->setToolTip(QApplication::translate("Drawer3DSettings", "Set X axis color", 0));
#endif // QT_NO_TOOLTIP
        pushButtonXAxisColor->setText(QApplication::translate("Drawer3DSettings", "&Color...", 0));
#ifndef QT_NO_TOOLTIP
        lineEditXFrom->setToolTip(QApplication::translate("Drawer3DSettings", "X axis goes from this point", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        lineEditXTo->setToolTip(QApplication::translate("Drawer3DSettings", "X axis goes to this point", 0));
#endif // QT_NO_TOOLTIP
        label_11->setText(QApplication::translate("Drawer3DSettings", "&X from/to", 0));
        label_12->setText(QApplication::translate("Drawer3DSettings", "&Y from/to", 0));
#ifndef QT_NO_TOOLTIP
        lineEditYFrom->setToolTip(QApplication::translate("Drawer3DSettings", "Y axis goes from this point", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        lineEditYTo->setToolTip(QApplication::translate("Drawer3DSettings", "Y axis goes to this point", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        pushButtonYAxisColor->setToolTip(QApplication::translate("Drawer3DSettings", "Set Y axis color", 0));
#endif // QT_NO_TOOLTIP
        pushButtonYAxisColor->setText(QApplication::translate("Drawer3DSettings", "&Color...", 0));
        label_13->setText(QApplication::translate("Drawer3DSettings", "&Z from/to", 0));
#ifndef QT_NO_TOOLTIP
        lineEditZFrom->setToolTip(QApplication::translate("Drawer3DSettings", "Z axis goes from this point", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        lineEditZTo->setToolTip(QApplication::translate("Drawer3DSettings", "Z axis goes to this point", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        pushButtonZAxisColor->setToolTip(QApplication::translate("Drawer3DSettings", "Set Z axis color", 0));
#endif // QT_NO_TOOLTIP
        pushButtonZAxisColor->setText(QApplication::translate("Drawer3DSettings", "&Color...", 0));
        groupBoxHandles->setTitle(QApplication::translate("Drawer3DSettings", "&Handles", 0));
#ifndef QT_NO_TOOLTIP
        lineEditHandleDiameter->setToolTip(QApplication::translate("Drawer3DSettings", "Solution handles diameter on screen and images (in pixels)", 0));
#endif // QT_NO_TOOLTIP
        label_10->setText(QApplication::translate("Drawer3DSettings", "&Diameter", 0));
#ifndef QT_NO_TOOLTIP
        groupBoxLineWidths->setToolTip(QApplication::translate("Drawer3DSettings", "Assign width to lines (in pixels)", 0));
#endif // QT_NO_TOOLTIP
        groupBoxLineWidths->setTitle(QApplication::translate("Drawer3DSettings", "Line &Widths", 0));
#ifndef QT_NO_TOOLTIP
        lineEditAxesWidth->setToolTip(QApplication::translate("Drawer3DSettings", "Axes width on screen and images (in pixels)", 0));
#endif // QT_NO_TOOLTIP
        label_8->setText(QApplication::translate("Drawer3DSettings", "Ax&es", 0));
        label_9->setText(QApplication::translate("Drawer3DSettings", "Sol&utions", 0));
#ifndef QT_NO_TOOLTIP
        lineEditSolutionsWidth->setToolTip(QApplication::translate("Drawer3DSettings", "Solutions width on screen and images (in pixels)", 0));
#endif // QT_NO_TOOLTIP
    } // retranslateUi

};

namespace Ui {
    class Drawer3DSettings: public Ui_Drawer3DSettings {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_DRAWER3DSETTINGS_H
