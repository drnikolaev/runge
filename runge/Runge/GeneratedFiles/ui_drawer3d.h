/********************************************************************************
** Form generated from reading UI file 'drawer3d.ui'
**
** Created by: Qt User Interface Compiler version 5.2.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_DRAWER3D_H
#define UI_DRAWER3D_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDoubleSpinBox>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSlider>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_Drawer3D
{
public:
    QWidget *centralwidget;
    QVBoxLayout *verticalLayout;
    QVBoxLayout *verticalLayoutAll;
    QHBoxLayout *horizontalLayoutTop;
    QLabel *labelX;
    QComboBox *xAxisBox;
    QLabel *labelY;
    QComboBox *yAxisBox;
    QLabel *labelZ;
    QComboBox *zAxisBox;
    QSpacerItem *horizontalSpacer_2;
    QPushButton *buttonPan;
    QPushButton *buttonSettings;
    QPushButton *buttonExport;
    QPushButton *buttonHelp;
    QHBoxLayout *horizontalLayoutBottom;
    QVBoxLayout *verticalLayoutSlidersAngles;
    QHBoxLayout *horizontalLayoutSliders;
    QSlider *xSlider;
    QSlider *ySlider;
    QSlider *zSlider;
    QGridLayout *gridLayout;
    QLabel *label_3;
    QLabel *label_2;
    QLabel *label;
    QDoubleSpinBox *zoomSpinBox;
    QLabel *label_4;
    QDoubleSpinBox *zAngleSpinBox;
    QDoubleSpinBox *yAngleSpinBox;
    QDoubleSpinBox *xAngleSpinBox;
    QWidget *widget;
    QStatusBar *statusbar;

    void setupUi(QMainWindow *Drawer3D)
    {
        if (Drawer3D->objectName().isEmpty())
            Drawer3D->setObjectName(QStringLiteral("Drawer3D"));
        Drawer3D->resize(565, 520);
        Drawer3D->setMinimumSize(QSize(565, 520));
        Drawer3D->setMaximumSize(QSize(16777215, 16777215));
        QIcon icon;
        icon.addFile(QStringLiteral(":/Runge/Resources/16x16/view.png"), QSize(), QIcon::Normal, QIcon::Off);
        Drawer3D->setWindowIcon(icon);
        centralwidget = new QWidget(Drawer3D);
        centralwidget->setObjectName(QStringLiteral("centralwidget"));
        verticalLayout = new QVBoxLayout(centralwidget);
        verticalLayout->setObjectName(QStringLiteral("verticalLayout"));
        verticalLayoutAll = new QVBoxLayout();
        verticalLayoutAll->setObjectName(QStringLiteral("verticalLayoutAll"));
        horizontalLayoutTop = new QHBoxLayout();
        horizontalLayoutTop->setSpacing(3);
        horizontalLayoutTop->setObjectName(QStringLiteral("horizontalLayoutTop"));
        labelX = new QLabel(centralwidget);
        labelX->setObjectName(QStringLiteral("labelX"));

        horizontalLayoutTop->addWidget(labelX);

        xAxisBox = new QComboBox(centralwidget);
        xAxisBox->setObjectName(QStringLiteral("xAxisBox"));
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(xAxisBox->sizePolicy().hasHeightForWidth());
        xAxisBox->setSizePolicy(sizePolicy);
        xAxisBox->setMinimumSize(QSize(52, 0));
        xAxisBox->setMaximumSize(QSize(52, 16777215));
        QFont font;
        font.setBold(true);
        font.setWeight(75);
        xAxisBox->setFont(font);

        horizontalLayoutTop->addWidget(xAxisBox);

        labelY = new QLabel(centralwidget);
        labelY->setObjectName(QStringLiteral("labelY"));

        horizontalLayoutTop->addWidget(labelY);

        yAxisBox = new QComboBox(centralwidget);
        yAxisBox->setObjectName(QStringLiteral("yAxisBox"));
        sizePolicy.setHeightForWidth(yAxisBox->sizePolicy().hasHeightForWidth());
        yAxisBox->setSizePolicy(sizePolicy);
        yAxisBox->setMinimumSize(QSize(52, 0));
        yAxisBox->setMaximumSize(QSize(52, 16777215));
        yAxisBox->setFont(font);

        horizontalLayoutTop->addWidget(yAxisBox);

        labelZ = new QLabel(centralwidget);
        labelZ->setObjectName(QStringLiteral("labelZ"));

        horizontalLayoutTop->addWidget(labelZ);

        zAxisBox = new QComboBox(centralwidget);
        zAxisBox->setObjectName(QStringLiteral("zAxisBox"));
        sizePolicy.setHeightForWidth(zAxisBox->sizePolicy().hasHeightForWidth());
        zAxisBox->setSizePolicy(sizePolicy);
        zAxisBox->setMinimumSize(QSize(52, 0));
        zAxisBox->setMaximumSize(QSize(52, 16777215));
        zAxisBox->setFont(font);

        horizontalLayoutTop->addWidget(zAxisBox);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayoutTop->addItem(horizontalSpacer_2);

        buttonPan = new QPushButton(centralwidget);
        buttonPan->setObjectName(QStringLiteral("buttonPan"));
        QSizePolicy sizePolicy1(QSizePolicy::Minimum, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(buttonPan->sizePolicy().hasHeightForWidth());
        buttonPan->setSizePolicy(sizePolicy1);
        buttonPan->setMinimumSize(QSize(40, 24));
        buttonPan->setMaximumSize(QSize(40, 24));
        buttonPan->setBaseSize(QSize(0, 0));
        QIcon icon1;
        icon1.addFile(QStringLiteral(":/Runge/Resources/16x16/palm_hold.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonPan->setIcon(icon1);
        buttonPan->setCheckable(true);

        horizontalLayoutTop->addWidget(buttonPan);

        buttonSettings = new QPushButton(centralwidget);
        buttonSettings->setObjectName(QStringLiteral("buttonSettings"));
        sizePolicy1.setHeightForWidth(buttonSettings->sizePolicy().hasHeightForWidth());
        buttonSettings->setSizePolicy(sizePolicy1);
        buttonSettings->setMinimumSize(QSize(40, 24));
        buttonSettings->setMaximumSize(QSize(40, 24));
        QIcon icon2;
        icon2.addFile(QStringLiteral(":/Runge/Resources/16x16/wrench.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonSettings->setIcon(icon2);

        horizontalLayoutTop->addWidget(buttonSettings);

        buttonExport = new QPushButton(centralwidget);
        buttonExport->setObjectName(QStringLiteral("buttonExport"));
        sizePolicy1.setHeightForWidth(buttonExport->sizePolicy().hasHeightForWidth());
        buttonExport->setSizePolicy(sizePolicy1);
        buttonExport->setMinimumSize(QSize(40, 24));
        buttonExport->setMaximumSize(QSize(40, 24));
        QIcon icon3;
        icon3.addFile(QStringLiteral(":/Runge/Resources/16x16/export1.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonExport->setIcon(icon3);

        horizontalLayoutTop->addWidget(buttonExport);

        buttonHelp = new QPushButton(centralwidget);
        buttonHelp->setObjectName(QStringLiteral("buttonHelp"));
        sizePolicy1.setHeightForWidth(buttonHelp->sizePolicy().hasHeightForWidth());
        buttonHelp->setSizePolicy(sizePolicy1);
        buttonHelp->setMinimumSize(QSize(40, 24));
        buttonHelp->setMaximumSize(QSize(40, 24));
        QIcon icon4;
        icon4.addFile(QStringLiteral(":/Runge/Resources/16x16/help.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonHelp->setIcon(icon4);

        horizontalLayoutTop->addWidget(buttonHelp);


        verticalLayoutAll->addLayout(horizontalLayoutTop);

        horizontalLayoutBottom = new QHBoxLayout();
        horizontalLayoutBottom->setObjectName(QStringLiteral("horizontalLayoutBottom"));
        verticalLayoutSlidersAngles = new QVBoxLayout();
        verticalLayoutSlidersAngles->setObjectName(QStringLiteral("verticalLayoutSlidersAngles"));
        horizontalLayoutSliders = new QHBoxLayout();
        horizontalLayoutSliders->setObjectName(QStringLiteral("horizontalLayoutSliders"));
        xSlider = new QSlider(centralwidget);
        xSlider->setObjectName(QStringLiteral("xSlider"));
        QSizePolicy sizePolicy2(QSizePolicy::Fixed, QSizePolicy::Expanding);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(xSlider->sizePolicy().hasHeightForWidth());
        xSlider->setSizePolicy(sizePolicy2);
        xSlider->setOrientation(Qt::Vertical);

        horizontalLayoutSliders->addWidget(xSlider);

        ySlider = new QSlider(centralwidget);
        ySlider->setObjectName(QStringLiteral("ySlider"));
        sizePolicy2.setHeightForWidth(ySlider->sizePolicy().hasHeightForWidth());
        ySlider->setSizePolicy(sizePolicy2);
        ySlider->setOrientation(Qt::Vertical);

        horizontalLayoutSliders->addWidget(ySlider);

        zSlider = new QSlider(centralwidget);
        zSlider->setObjectName(QStringLiteral("zSlider"));
        sizePolicy2.setHeightForWidth(zSlider->sizePolicy().hasHeightForWidth());
        zSlider->setSizePolicy(sizePolicy2);
        zSlider->setOrientation(Qt::Vertical);

        horizontalLayoutSliders->addWidget(zSlider);


        verticalLayoutSlidersAngles->addLayout(horizontalLayoutSliders);

        gridLayout = new QGridLayout();
        gridLayout->setObjectName(QStringLiteral("gridLayout"));
        gridLayout->setSizeConstraint(QLayout::SetDefaultConstraint);
        gridLayout->setHorizontalSpacing(6);
        gridLayout->setVerticalSpacing(2);
        label_3 = new QLabel(centralwidget);
        label_3->setObjectName(QStringLiteral("label_3"));
        label_3->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        gridLayout->addWidget(label_3, 2, 0, 1, 1);

        label_2 = new QLabel(centralwidget);
        label_2->setObjectName(QStringLiteral("label_2"));
        label_2->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        gridLayout->addWidget(label_2, 1, 0, 1, 1);

        label = new QLabel(centralwidget);
        label->setObjectName(QStringLiteral("label"));
        label->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        gridLayout->addWidget(label, 0, 0, 1, 1);

        zoomSpinBox = new QDoubleSpinBox(centralwidget);
        zoomSpinBox->setObjectName(QStringLiteral("zoomSpinBox"));
        QSizePolicy sizePolicy3(QSizePolicy::Ignored, QSizePolicy::Fixed);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(zoomSpinBox->sizePolicy().hasHeightForWidth());
        zoomSpinBox->setSizePolicy(sizePolicy3);
        zoomSpinBox->setMinimumSize(QSize(0, 0));
        zoomSpinBox->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        zoomSpinBox->setDecimals(3);
        zoomSpinBox->setMinimum(0.001);
        zoomSpinBox->setMaximum(1000);
        zoomSpinBox->setSingleStep(0.001);
        zoomSpinBox->setValue(1);

        gridLayout->addWidget(zoomSpinBox, 3, 1, 1, 1);

        label_4 = new QLabel(centralwidget);
        label_4->setObjectName(QStringLiteral("label_4"));
        label_4->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        gridLayout->addWidget(label_4, 3, 0, 1, 1);

        zAngleSpinBox = new QDoubleSpinBox(centralwidget);
        zAngleSpinBox->setObjectName(QStringLiteral("zAngleSpinBox"));
        zAngleSpinBox->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        zAngleSpinBox->setDecimals(1);
        zAngleSpinBox->setMaximum(360);
        zAngleSpinBox->setSingleStep(0.1);
        zAngleSpinBox->setValue(0);

        gridLayout->addWidget(zAngleSpinBox, 2, 1, 1, 1);

        yAngleSpinBox = new QDoubleSpinBox(centralwidget);
        yAngleSpinBox->setObjectName(QStringLiteral("yAngleSpinBox"));
        yAngleSpinBox->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        yAngleSpinBox->setDecimals(1);
        yAngleSpinBox->setMaximum(360);
        yAngleSpinBox->setSingleStep(0.1);

        gridLayout->addWidget(yAngleSpinBox, 1, 1, 1, 1);

        xAngleSpinBox = new QDoubleSpinBox(centralwidget);
        xAngleSpinBox->setObjectName(QStringLiteral("xAngleSpinBox"));
        xAngleSpinBox->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        xAngleSpinBox->setDecimals(1);
        xAngleSpinBox->setMaximum(360);
        xAngleSpinBox->setSingleStep(0.1);

        gridLayout->addWidget(xAngleSpinBox, 0, 1, 1, 1);

        gridLayout->setColumnMinimumWidth(1, 50);

        verticalLayoutSlidersAngles->addLayout(gridLayout);


        horizontalLayoutBottom->addLayout(verticalLayoutSlidersAngles);

        widget = new QWidget(centralwidget);
        widget->setObjectName(QStringLiteral("widget"));
        QSizePolicy sizePolicy4(QSizePolicy::Expanding, QSizePolicy::Preferred);
        sizePolicy4.setHorizontalStretch(0);
        sizePolicy4.setVerticalStretch(0);
        sizePolicy4.setHeightForWidth(widget->sizePolicy().hasHeightForWidth());
        widget->setSizePolicy(sizePolicy4);

        horizontalLayoutBottom->addWidget(widget);


        verticalLayoutAll->addLayout(horizontalLayoutBottom);


        verticalLayout->addLayout(verticalLayoutAll);

        Drawer3D->setCentralWidget(centralwidget);
        statusbar = new QStatusBar(Drawer3D);
        statusbar->setObjectName(QStringLiteral("statusbar"));
        Drawer3D->setStatusBar(statusbar);
#ifndef QT_NO_SHORTCUT
        labelX->setBuddy(xAxisBox);
        labelY->setBuddy(yAxisBox);
        labelZ->setBuddy(zAxisBox);
        label_3->setBuddy(zAngleSpinBox);
        label_2->setBuddy(yAngleSpinBox);
        label->setBuddy(xAngleSpinBox);
        label_4->setBuddy(zoomSpinBox);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(xAxisBox, yAxisBox);
        QWidget::setTabOrder(yAxisBox, zAxisBox);
        QWidget::setTabOrder(zAxisBox, buttonPan);
        QWidget::setTabOrder(buttonPan, buttonSettings);
        QWidget::setTabOrder(buttonSettings, buttonExport);
        QWidget::setTabOrder(buttonExport, buttonHelp);
        QWidget::setTabOrder(buttonHelp, xSlider);
        QWidget::setTabOrder(xSlider, ySlider);
        QWidget::setTabOrder(ySlider, zSlider);
        QWidget::setTabOrder(zSlider, xAngleSpinBox);
        QWidget::setTabOrder(xAngleSpinBox, yAngleSpinBox);
        QWidget::setTabOrder(yAngleSpinBox, zAngleSpinBox);
        QWidget::setTabOrder(zAngleSpinBox, zoomSpinBox);

        retranslateUi(Drawer3D);

        QMetaObject::connectSlotsByName(Drawer3D);
    } // setupUi

    void retranslateUi(QMainWindow *Drawer3D)
    {
        Drawer3D->setWindowTitle(QApplication::translate("Drawer3D", "3D Drawer", 0));
        labelX->setText(QApplication::translate("Drawer3D", "&X", 0));
#ifndef QT_NO_TOOLTIP
        xAxisBox->setToolTip(QApplication::translate("Drawer3D", "X axis variable", 0));
#endif // QT_NO_TOOLTIP
        labelY->setText(QApplication::translate("Drawer3D", " &Y", 0));
#ifndef QT_NO_TOOLTIP
        yAxisBox->setToolTip(QApplication::translate("Drawer3D", "Y axis variable", 0));
#endif // QT_NO_TOOLTIP
        labelZ->setText(QApplication::translate("Drawer3D", " &Z", 0));
#ifndef QT_NO_TOOLTIP
        zAxisBox->setToolTip(QApplication::translate("Drawer3D", "Z axis variable", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        buttonPan->setToolTip(QApplication::translate("Drawer3D", "Pan", 0));
#endif // QT_NO_TOOLTIP
        buttonPan->setText(QString());
#ifndef QT_NO_TOOLTIP
        buttonSettings->setToolTip(QApplication::translate("Drawer3D", "Drawer Settings", 0));
#endif // QT_NO_TOOLTIP
        buttonSettings->setText(QString());
#ifndef QT_NO_TOOLTIP
        buttonExport->setToolTip(QApplication::translate("Drawer3D", "Export to File", 0));
#endif // QT_NO_TOOLTIP
        buttonExport->setText(QString());
#ifndef QT_NO_TOOLTIP
        buttonHelp->setToolTip(QApplication::translate("Drawer3D", "Help", 0));
#endif // QT_NO_TOOLTIP
        buttonHelp->setText(QString());
#ifndef QT_NO_TOOLTIP
        xSlider->setToolTip(QApplication::translate("Drawer3D", "X rotation angle", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        ySlider->setToolTip(QApplication::translate("Drawer3D", "Y rotation angle", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        zSlider->setToolTip(QApplication::translate("Drawer3D", "Z rotation angle", 0));
#endif // QT_NO_TOOLTIP
        label_3->setText(QApplication::translate("Drawer3D", "rot &Z", 0));
        label_2->setText(QApplication::translate("Drawer3D", "rot &Y", 0));
        label->setText(QApplication::translate("Drawer3D", "rot &X", 0));
#ifndef QT_NO_TOOLTIP
        zoomSpinBox->setToolTip(QApplication::translate("Drawer3D", "Zoom Factor", 0));
#endif // QT_NO_TOOLTIP
        label_4->setText(QApplication::translate("Drawer3D", "Zoo&m", 0));
#ifndef QT_NO_TOOLTIP
        zAngleSpinBox->setToolTip(QApplication::translate("Drawer3D", "Z rotation angle", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        yAngleSpinBox->setToolTip(QApplication::translate("Drawer3D", "Y rotation angle", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        xAngleSpinBox->setToolTip(QApplication::translate("Drawer3D", "X rotation angle", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        widget->setToolTip(QString());
#endif // QT_NO_TOOLTIP
    } // retranslateUi

};

namespace Ui {
    class Drawer3D: public Ui_Drawer3D {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_DRAWER3D_H
