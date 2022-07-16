/********************************************************************************
** Form generated from reading UI file 'drawersettings.ui'
**
** Created by: Qt User Interface Compiler version 5.2.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_DRAWERSETTINGS_H
#define UI_DRAWERSETTINGS_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QGroupBox>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>

QT_BEGIN_NAMESPACE

class Ui_DrawerSettings
{
public:
    QGroupBox *groupBoxAxesMarks;
    QLabel *label_5;
    QLineEdit *lineEditStepX;
    QLabel *label_6;
    QLineEdit *lineEditStepY;
    QLineEdit *lineEditMarkLength;
    QLabel *label_7;
    QGroupBox *groupBoxAxes;
    QLabel *label;
    QLineEdit *lineEditTopLength;
    QLineEdit *lineEditRightLength;
    QLabel *label_2;
    QLabel *label_3;
    QLineEdit *lineEditBottomLength;
    QLabel *label_4;
    QLineEdit *lineEditLeftLength;
    QPushButton *pushButtonAdjustLengths;
    QPushButton *pushButtonAxesColor;
    QDialogButtonBox *buttonBox;
    QCheckBox *checkBoxRescaleAfterRunningFromPoint;
    QGroupBox *groupBoxHandles;
    QLineEdit *lineEditHandleDiameter;
    QLabel *label_10;
    QGroupBox *groupBoxLineWidths;
    QLineEdit *lineEditAxesWidth;
    QLabel *label_8;
    QLabel *label_9;
    QLineEdit *lineEditSolutionsWidth;

    void setupUi(QDialog *DrawerSettings)
    {
        if (DrawerSettings->objectName().isEmpty())
            DrawerSettings->setObjectName(QStringLiteral("DrawerSettings"));
        DrawerSettings->setWindowModality(Qt::WindowModal);
        DrawerSettings->resize(523, 370);
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(DrawerSettings->sizePolicy().hasHeightForWidth());
        DrawerSettings->setSizePolicy(sizePolicy);
        DrawerSettings->setMinimumSize(QSize(523, 370));
        DrawerSettings->setMaximumSize(QSize(523, 370));
        QIcon icon;
        icon.addFile(QStringLiteral(":/Runge/Resources/16x16/wrench.png"), QSize(), QIcon::Normal, QIcon::Off);
        DrawerSettings->setWindowIcon(icon);
        groupBoxAxesMarks = new QGroupBox(DrawerSettings);
        groupBoxAxesMarks->setObjectName(QStringLiteral("groupBoxAxesMarks"));
        groupBoxAxesMarks->setGeometry(QRect(10, 230, 181, 131));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(groupBoxAxesMarks->sizePolicy().hasHeightForWidth());
        groupBoxAxesMarks->setSizePolicy(sizePolicy1);
        groupBoxAxesMarks->setFlat(false);
        groupBoxAxesMarks->setCheckable(true);
        groupBoxAxesMarks->setChecked(true);
        label_5 = new QLabel(groupBoxAxesMarks);
        label_5->setObjectName(QStringLiteral("label_5"));
        label_5->setGeometry(QRect(10, 30, 71, 24));
        label_5->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        lineEditStepX = new QLineEdit(groupBoxAxesMarks);
        lineEditStepX->setObjectName(QStringLiteral("lineEditStepX"));
        lineEditStepX->setGeometry(QRect(90, 30, 71, 24));
        label_6 = new QLabel(groupBoxAxesMarks);
        label_6->setObjectName(QStringLiteral("label_6"));
        label_6->setGeometry(QRect(10, 60, 71, 24));
        label_6->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        lineEditStepY = new QLineEdit(groupBoxAxesMarks);
        lineEditStepY->setObjectName(QStringLiteral("lineEditStepY"));
        lineEditStepY->setGeometry(QRect(90, 60, 71, 24));
        lineEditMarkLength = new QLineEdit(groupBoxAxesMarks);
        lineEditMarkLength->setObjectName(QStringLiteral("lineEditMarkLength"));
        lineEditMarkLength->setGeometry(QRect(90, 90, 71, 24));
        label_7 = new QLabel(groupBoxAxesMarks);
        label_7->setObjectName(QStringLiteral("label_7"));
        label_7->setGeometry(QRect(10, 90, 71, 24));
        label_7->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        groupBoxAxes = new QGroupBox(DrawerSettings);
        groupBoxAxes->setObjectName(QStringLiteral("groupBoxAxes"));
        groupBoxAxes->setGeometry(QRect(10, 10, 391, 131));
        groupBoxAxes->setCheckable(true);
        groupBoxAxes->setChecked(true);
        label = new QLabel(groupBoxAxes);
        label->setObjectName(QStringLiteral("label"));
        label->setGeometry(QRect(150, 30, 71, 24));
        label->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        lineEditTopLength = new QLineEdit(groupBoxAxes);
        lineEditTopLength->setObjectName(QStringLiteral("lineEditTopLength"));
        lineEditTopLength->setGeometry(QRect(230, 30, 71, 24));
        lineEditRightLength = new QLineEdit(groupBoxAxes);
        lineEditRightLength->setObjectName(QStringLiteral("lineEditRightLength"));
        lineEditRightLength->setGeometry(QRect(300, 60, 71, 24));
        label_2 = new QLabel(groupBoxAxes);
        label_2->setObjectName(QStringLiteral("label_2"));
        label_2->setGeometry(QRect(230, 60, 61, 24));
        label_2->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        label_3 = new QLabel(groupBoxAxes);
        label_3->setObjectName(QStringLiteral("label_3"));
        label_3->setGeometry(QRect(140, 90, 81, 24));
        label_3->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        lineEditBottomLength = new QLineEdit(groupBoxAxes);
        lineEditBottomLength->setObjectName(QStringLiteral("lineEditBottomLength"));
        lineEditBottomLength->setGeometry(QRect(230, 90, 71, 24));
        label_4 = new QLabel(groupBoxAxes);
        label_4->setObjectName(QStringLiteral("label_4"));
        label_4->setGeometry(QRect(110, 60, 41, 24));
        label_4->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        lineEditLeftLength = new QLineEdit(groupBoxAxes);
        lineEditLeftLength->setObjectName(QStringLiteral("lineEditLeftLength"));
        lineEditLeftLength->setGeometry(QRect(160, 60, 71, 24));
        pushButtonAdjustLengths = new QPushButton(groupBoxAxes);
        pushButtonAdjustLengths->setObjectName(QStringLiteral("pushButtonAdjustLengths"));
        pushButtonAdjustLengths->setGeometry(QRect(20, 30, 91, 24));
        pushButtonAxesColor = new QPushButton(groupBoxAxes);
        pushButtonAxesColor->setObjectName(QStringLiteral("pushButtonAxesColor"));
        pushButtonAxesColor->setGeometry(QRect(20, 60, 91, 24));
        buttonBox = new QDialogButtonBox(DrawerSettings);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setGeometry(QRect(417, 16, 91, 101));
        buttonBox->setOrientation(Qt::Vertical);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Help|QDialogButtonBox::Ok);
        buttonBox->setCenterButtons(false);
        checkBoxRescaleAfterRunningFromPoint = new QCheckBox(DrawerSettings);
        checkBoxRescaleAfterRunningFromPoint->setObjectName(QStringLiteral("checkBoxRescaleAfterRunningFromPoint"));
        checkBoxRescaleAfterRunningFromPoint->setGeometry(QRect(220, 320, 291, 24));
        groupBoxHandles = new QGroupBox(DrawerSettings);
        groupBoxHandles->setObjectName(QStringLiteral("groupBoxHandles"));
        groupBoxHandles->setGeometry(QRect(210, 230, 191, 71));
        lineEditHandleDiameter = new QLineEdit(groupBoxHandles);
        lineEditHandleDiameter->setObjectName(QStringLiteral("lineEditHandleDiameter"));
        lineEditHandleDiameter->setGeometry(QRect(100, 30, 71, 24));
        label_10 = new QLabel(groupBoxHandles);
        label_10->setObjectName(QStringLiteral("label_10"));
        label_10->setGeometry(QRect(0, 30, 91, 24));
        label_10->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        groupBoxLineWidths = new QGroupBox(DrawerSettings);
        groupBoxLineWidths->setObjectName(QStringLiteral("groupBoxLineWidths"));
        groupBoxLineWidths->setGeometry(QRect(10, 150, 391, 71));
        groupBoxLineWidths->setCheckable(true);
        groupBoxLineWidths->setChecked(false);
        lineEditAxesWidth = new QLineEdit(groupBoxLineWidths);
        lineEditAxesWidth->setObjectName(QStringLiteral("lineEditAxesWidth"));
        lineEditAxesWidth->setGeometry(QRect(90, 30, 71, 24));
        label_8 = new QLabel(groupBoxLineWidths);
        label_8->setObjectName(QStringLiteral("label_8"));
        label_8->setGeometry(QRect(10, 30, 71, 24));
        label_8->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        label_9 = new QLabel(groupBoxLineWidths);
        label_9->setObjectName(QStringLiteral("label_9"));
        label_9->setGeometry(QRect(190, 30, 101, 24));
        label_9->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        lineEditSolutionsWidth = new QLineEdit(groupBoxLineWidths);
        lineEditSolutionsWidth->setObjectName(QStringLiteral("lineEditSolutionsWidth"));
        lineEditSolutionsWidth->setGeometry(QRect(300, 30, 71, 24));
#ifndef QT_NO_SHORTCUT
        label_5->setBuddy(lineEditStepX);
        label_6->setBuddy(lineEditStepY);
        label_7->setBuddy(lineEditMarkLength);
        label->setBuddy(lineEditTopLength);
        label_2->setBuddy(lineEditRightLength);
        label_3->setBuddy(lineEditBottomLength);
        label_4->setBuddy(lineEditLeftLength);
        label_10->setBuddy(lineEditHandleDiameter);
        label_8->setBuddy(lineEditAxesWidth);
        label_9->setBuddy(lineEditSolutionsWidth);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(groupBoxAxes, pushButtonAdjustLengths);
        QWidget::setTabOrder(pushButtonAdjustLengths, pushButtonAxesColor);
        QWidget::setTabOrder(pushButtonAxesColor, lineEditLeftLength);
        QWidget::setTabOrder(lineEditLeftLength, lineEditTopLength);
        QWidget::setTabOrder(lineEditTopLength, lineEditBottomLength);
        QWidget::setTabOrder(lineEditBottomLength, lineEditRightLength);
        QWidget::setTabOrder(lineEditRightLength, groupBoxLineWidths);
        QWidget::setTabOrder(groupBoxLineWidths, lineEditAxesWidth);
        QWidget::setTabOrder(lineEditAxesWidth, lineEditSolutionsWidth);
        QWidget::setTabOrder(lineEditSolutionsWidth, groupBoxAxesMarks);
        QWidget::setTabOrder(groupBoxAxesMarks, lineEditStepX);
        QWidget::setTabOrder(lineEditStepX, lineEditStepY);
        QWidget::setTabOrder(lineEditStepY, lineEditMarkLength);
        QWidget::setTabOrder(lineEditMarkLength, lineEditHandleDiameter);
        QWidget::setTabOrder(lineEditHandleDiameter, checkBoxRescaleAfterRunningFromPoint);
        QWidget::setTabOrder(checkBoxRescaleAfterRunningFromPoint, buttonBox);

        retranslateUi(DrawerSettings);
        QObject::connect(buttonBox, SIGNAL(accepted()), DrawerSettings, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), DrawerSettings, SLOT(reject()));

        QMetaObject::connectSlotsByName(DrawerSettings);
    } // setupUi

    void retranslateUi(QDialog *DrawerSettings)
    {
        DrawerSettings->setWindowTitle(QApplication::translate("DrawerSettings", "2D Drawer Settings", 0));
#ifndef QT_NO_TOOLTIP
        DrawerSettings->setToolTip(QString());
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        groupBoxAxesMarks->setToolTip(QApplication::translate("DrawerSettings", "Show axes marks", 0));
#endif // QT_NO_TOOLTIP
        groupBoxAxesMarks->setTitle(QApplication::translate("DrawerSettings", "Axes &Marks", 0));
        label_5->setText(QApplication::translate("DrawerSettings", "&X Step", 0));
#ifndef QT_NO_TOOLTIP
        lineEditStepX->setToolTip(QApplication::translate("DrawerSettings", "Distance between X axis marks", 0));
#endif // QT_NO_TOOLTIP
        label_6->setText(QApplication::translate("DrawerSettings", "&Y Step", 0));
#ifndef QT_NO_TOOLTIP
        lineEditStepY->setToolTip(QApplication::translate("DrawerSettings", "Distance between Y axis marks", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        lineEditMarkLength->setToolTip(QApplication::translate("DrawerSettings", "Axes marks length on screen and images (in pixels)", 0));
#endif // QT_NO_TOOLTIP
        label_7->setText(QApplication::translate("DrawerSettings", "Len&gth", 0));
#ifndef QT_NO_TOOLTIP
        groupBoxAxes->setToolTip(QApplication::translate("DrawerSettings", "Show axes", 0));
#endif // QT_NO_TOOLTIP
        groupBoxAxes->setTitle(QApplication::translate("DrawerSettings", "&Axes", 0));
        label->setText(QApplication::translate("DrawerSettings", "&Top", 0));
#ifndef QT_NO_TOOLTIP
        lineEditTopLength->setToolTip(QApplication::translate("DrawerSettings", "Top semi-axis length", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        lineEditRightLength->setToolTip(QApplication::translate("DrawerSettings", "Right semi-axis length", 0));
#endif // QT_NO_TOOLTIP
        label_2->setText(QApplication::translate("DrawerSettings", "&Right", 0));
        label_3->setText(QApplication::translate("DrawerSettings", "&Bottom", 0));
#ifndef QT_NO_TOOLTIP
        lineEditBottomLength->setToolTip(QApplication::translate("DrawerSettings", "Bottom semi-axis length", 0));
#endif // QT_NO_TOOLTIP
        label_4->setText(QApplication::translate("DrawerSettings", "&Left", 0));
#ifndef QT_NO_TOOLTIP
        lineEditLeftLength->setToolTip(QApplication::translate("DrawerSettings", "Left semi-axis length", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        pushButtonAdjustLengths->setToolTip(QApplication::translate("DrawerSettings", "Reset all axes lengths to fit current graph", 0));
#endif // QT_NO_TOOLTIP
        pushButtonAdjustLengths->setText(QApplication::translate("DrawerSettings", "Ad&just", 0));
#ifndef QT_NO_TOOLTIP
        pushButtonAxesColor->setToolTip(QApplication::translate("DrawerSettings", "Set axes color", 0));
#endif // QT_NO_TOOLTIP
        pushButtonAxesColor->setText(QApplication::translate("DrawerSettings", "&Color...", 0));
#ifndef QT_NO_TOOLTIP
        checkBoxRescaleAfterRunningFromPoint->setToolTip(QApplication::translate("DrawerSettings", "Rescale to fit everything after each new soluttion added", 0));
#endif // QT_NO_TOOLTIP
        checkBoxRescaleAfterRunningFromPoint->setText(QApplication::translate("DrawerSettings", "Rescale A&fter Running From Point", 0));
        groupBoxHandles->setTitle(QApplication::translate("DrawerSettings", "&Handles", 0));
#ifndef QT_NO_TOOLTIP
        lineEditHandleDiameter->setToolTip(QApplication::translate("DrawerSettings", "Solution handles diameter on screen and images (in pixels)", 0));
#endif // QT_NO_TOOLTIP
        label_10->setText(QApplication::translate("DrawerSettings", "&Diameter", 0));
#ifndef QT_NO_TOOLTIP
        groupBoxLineWidths->setToolTip(QApplication::translate("DrawerSettings", "Assign width to lines, 0 means no width at any scale", 0));
#endif // QT_NO_TOOLTIP
        groupBoxLineWidths->setTitle(QApplication::translate("DrawerSettings", "Line &Widths", 0));
#ifndef QT_NO_TOOLTIP
        lineEditAxesWidth->setToolTip(QApplication::translate("DrawerSettings", "Axes width on screen and images (in coordinate units)", 0));
#endif // QT_NO_TOOLTIP
        label_8->setText(QApplication::translate("DrawerSettings", "Ax&es", 0));
        label_9->setText(QApplication::translate("DrawerSettings", "Sol&utions", 0));
#ifndef QT_NO_TOOLTIP
        lineEditSolutionsWidth->setToolTip(QApplication::translate("DrawerSettings", "Solutions width on screen and images (in coordinate units)", 0));
#endif // QT_NO_TOOLTIP
    } // retranslateUi

};

namespace Ui {
    class DrawerSettings: public Ui_DrawerSettings {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_DRAWERSETTINGS_H
