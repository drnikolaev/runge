/********************************************************************************
** Form generated from reading UI file 'drawer.ui'
**
** Created by: Qt User Interface Compiler version 5.2.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_DRAWER_H
#define UI_DRAWER_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>
#include <view2d.h>

QT_BEGIN_NAMESPACE

class Ui_Drawer
{
public:
    QWidget *centralwidget;
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    QLabel *labelX;
    QComboBox *xAxisBox;
    QLabel *labelY;
    QComboBox *yAxisBox;
    QSpacerItem *horizontalSpacer_2;
    QPushButton *buttonRun;
    QPushButton *buttonDelete;
    QPushButton *buttonZoom;
    QPushButton *buttonPan;
    QPushButton *buttonPalette;
    QPushButton *buttonSettings;
    QPushButton *buttonExport;
    QPushButton *buttonPrint;
    QPushButton *buttonHelp;
    View2D *graphicsView;
    QStatusBar *statusbar;

    void setupUi(QMainWindow *Drawer)
    {
        if (Drawer->objectName().isEmpty())
            Drawer->setObjectName(QStringLiteral("Drawer"));
        Drawer->resize(577, 629);
        Drawer->setMinimumSize(QSize(577, 629));
        Drawer->setMaximumSize(QSize(16777215, 16777215));
        QIcon icon;
        icon.addFile(QStringLiteral(":/Runge/Resources/16x16/view.png"), QSize(), QIcon::Normal, QIcon::Off);
        Drawer->setWindowIcon(icon);
        centralwidget = new QWidget(Drawer);
        centralwidget->setObjectName(QStringLiteral("centralwidget"));
        verticalLayout = new QVBoxLayout(centralwidget);
        verticalLayout->setObjectName(QStringLiteral("verticalLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(3);
        horizontalLayout->setObjectName(QStringLiteral("horizontalLayout"));
        labelX = new QLabel(centralwidget);
        labelX->setObjectName(QStringLiteral("labelX"));
        labelX->setLayoutDirection(Qt::LeftToRight);
        labelX->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        horizontalLayout->addWidget(labelX);

        xAxisBox = new QComboBox(centralwidget);
        xAxisBox->setObjectName(QStringLiteral("xAxisBox"));
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(xAxisBox->sizePolicy().hasHeightForWidth());
        xAxisBox->setSizePolicy(sizePolicy);
        xAxisBox->setMinimumSize(QSize(65, 0));
        xAxisBox->setMaximumSize(QSize(65, 16777215));
        QFont font;
        font.setBold(true);
        font.setWeight(75);
        xAxisBox->setFont(font);

        horizontalLayout->addWidget(xAxisBox);

        labelY = new QLabel(centralwidget);
        labelY->setObjectName(QStringLiteral("labelY"));
        labelY->setLayoutDirection(Qt::LeftToRight);
        labelY->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        horizontalLayout->addWidget(labelY);

        yAxisBox = new QComboBox(centralwidget);
        yAxisBox->setObjectName(QStringLiteral("yAxisBox"));
        sizePolicy.setHeightForWidth(yAxisBox->sizePolicy().hasHeightForWidth());
        yAxisBox->setSizePolicy(sizePolicy);
        yAxisBox->setMinimumSize(QSize(65, 0));
        yAxisBox->setMaximumSize(QSize(65, 16777215));
        yAxisBox->setFont(font);

        horizontalLayout->addWidget(yAxisBox);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_2);

        buttonRun = new QPushButton(centralwidget);
        buttonRun->setObjectName(QStringLiteral("buttonRun"));
        buttonRun->setMinimumSize(QSize(40, 24));
        buttonRun->setMaximumSize(QSize(40, 24));
        QIcon icon1;
        icon1.addFile(QStringLiteral(":/Runge/Resources/16x16/media_play_green.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonRun->setIcon(icon1);
        buttonRun->setCheckable(true);

        horizontalLayout->addWidget(buttonRun);

        buttonDelete = new QPushButton(centralwidget);
        buttonDelete->setObjectName(QStringLiteral("buttonDelete"));
        buttonDelete->setMinimumSize(QSize(40, 24));
        buttonDelete->setMaximumSize(QSize(40, 24));
        QIcon icon2;
        icon2.addFile(QStringLiteral(":/Runge/Resources/16x16/delete.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonDelete->setIcon(icon2);
        buttonDelete->setCheckable(false);

        horizontalLayout->addWidget(buttonDelete);

        buttonZoom = new QPushButton(centralwidget);
        buttonZoom->setObjectName(QStringLiteral("buttonZoom"));
        QSizePolicy sizePolicy1(QSizePolicy::Minimum, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(buttonZoom->sizePolicy().hasHeightForWidth());
        buttonZoom->setSizePolicy(sizePolicy1);
        buttonZoom->setMinimumSize(QSize(40, 24));
        buttonZoom->setMaximumSize(QSize(40, 24));
        buttonZoom->setBaseSize(QSize(0, 0));
        buttonZoom->setIcon(icon);

        horizontalLayout->addWidget(buttonZoom);

        buttonPan = new QPushButton(centralwidget);
        buttonPan->setObjectName(QStringLiteral("buttonPan"));
        sizePolicy1.setHeightForWidth(buttonPan->sizePolicy().hasHeightForWidth());
        buttonPan->setSizePolicy(sizePolicy1);
        buttonPan->setMinimumSize(QSize(40, 24));
        buttonPan->setMaximumSize(QSize(40, 24));
        buttonPan->setBaseSize(QSize(0, 0));
        QIcon icon3;
        icon3.addFile(QStringLiteral(":/Runge/Resources/16x16/palm_hold.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonPan->setIcon(icon3);
        buttonPan->setCheckable(true);

        horizontalLayout->addWidget(buttonPan);

        buttonPalette = new QPushButton(centralwidget);
        buttonPalette->setObjectName(QStringLiteral("buttonPalette"));
        sizePolicy1.setHeightForWidth(buttonPalette->sizePolicy().hasHeightForWidth());
        buttonPalette->setSizePolicy(sizePolicy1);
        buttonPalette->setMinimumSize(QSize(40, 24));
        buttonPalette->setMaximumSize(QSize(40, 24));
        buttonPalette->setBaseSize(QSize(0, 0));
        QIcon icon4;
        icon4.addFile(QStringLiteral(":/Runge/Resources/16x16/palette.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonPalette->setIcon(icon4);

        horizontalLayout->addWidget(buttonPalette);

        buttonSettings = new QPushButton(centralwidget);
        buttonSettings->setObjectName(QStringLiteral("buttonSettings"));
        sizePolicy1.setHeightForWidth(buttonSettings->sizePolicy().hasHeightForWidth());
        buttonSettings->setSizePolicy(sizePolicy1);
        buttonSettings->setMinimumSize(QSize(40, 24));
        buttonSettings->setMaximumSize(QSize(40, 24));
        QIcon icon5;
        icon5.addFile(QStringLiteral(":/Runge/Resources/16x16/wrench.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonSettings->setIcon(icon5);

        horizontalLayout->addWidget(buttonSettings);

        buttonExport = new QPushButton(centralwidget);
        buttonExport->setObjectName(QStringLiteral("buttonExport"));
        sizePolicy1.setHeightForWidth(buttonExport->sizePolicy().hasHeightForWidth());
        buttonExport->setSizePolicy(sizePolicy1);
        buttonExport->setMinimumSize(QSize(40, 24));
        buttonExport->setMaximumSize(QSize(40, 24));
        QIcon icon6;
        icon6.addFile(QStringLiteral(":/Runge/Resources/16x16/export1.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonExport->setIcon(icon6);

        horizontalLayout->addWidget(buttonExport);

        buttonPrint = new QPushButton(centralwidget);
        buttonPrint->setObjectName(QStringLiteral("buttonPrint"));
        sizePolicy1.setHeightForWidth(buttonPrint->sizePolicy().hasHeightForWidth());
        buttonPrint->setSizePolicy(sizePolicy1);
        buttonPrint->setMinimumSize(QSize(40, 24));
        buttonPrint->setMaximumSize(QSize(40, 24));
        QIcon icon7;
        icon7.addFile(QStringLiteral(":/Runge/Resources/16x16/printer.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonPrint->setIcon(icon7);

        horizontalLayout->addWidget(buttonPrint);

        buttonHelp = new QPushButton(centralwidget);
        buttonHelp->setObjectName(QStringLiteral("buttonHelp"));
        sizePolicy1.setHeightForWidth(buttonHelp->sizePolicy().hasHeightForWidth());
        buttonHelp->setSizePolicy(sizePolicy1);
        buttonHelp->setMinimumSize(QSize(40, 24));
        buttonHelp->setMaximumSize(QSize(40, 24));
        QIcon icon8;
        icon8.addFile(QStringLiteral(":/Runge/Resources/16x16/help.png"), QSize(), QIcon::Normal, QIcon::Off);
        buttonHelp->setIcon(icon8);

        horizontalLayout->addWidget(buttonHelp);


        verticalLayout->addLayout(horizontalLayout);

        graphicsView = new View2D(centralwidget);
        graphicsView->setObjectName(QStringLiteral("graphicsView"));
        graphicsView->setMouseTracking(true);
        graphicsView->setAcceptDrops(false);
        graphicsView->setRenderHints(QPainter::Antialiasing|QPainter::HighQualityAntialiasing|QPainter::TextAntialiasing);

        verticalLayout->addWidget(graphicsView);

        Drawer->setCentralWidget(centralwidget);
        statusbar = new QStatusBar(Drawer);
        statusbar->setObjectName(QStringLiteral("statusbar"));
        Drawer->setStatusBar(statusbar);
#ifndef QT_NO_SHORTCUT
        labelX->setBuddy(xAxisBox);
        labelY->setBuddy(yAxisBox);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(xAxisBox, yAxisBox);
        QWidget::setTabOrder(yAxisBox, buttonRun);
        QWidget::setTabOrder(buttonRun, buttonDelete);
        QWidget::setTabOrder(buttonDelete, buttonZoom);
        QWidget::setTabOrder(buttonZoom, buttonPan);
        QWidget::setTabOrder(buttonPan, buttonPalette);
        QWidget::setTabOrder(buttonPalette, buttonSettings);
        QWidget::setTabOrder(buttonSettings, buttonExport);
        QWidget::setTabOrder(buttonExport, buttonPrint);
        QWidget::setTabOrder(buttonPrint, buttonHelp);
        QWidget::setTabOrder(buttonHelp, graphicsView);

        retranslateUi(Drawer);

        QMetaObject::connectSlotsByName(Drawer);
    } // setupUi

    void retranslateUi(QMainWindow *Drawer)
    {
        Drawer->setWindowTitle(QApplication::translate("Drawer", "2D Drawer", 0));
        labelX->setText(QApplication::translate("Drawer", "&X", 0));
#ifndef QT_NO_TOOLTIP
        xAxisBox->setToolTip(QApplication::translate("Drawer", "X axis variable", 0));
#endif // QT_NO_TOOLTIP
        labelY->setText(QApplication::translate("Drawer", " &Y", 0));
#ifndef QT_NO_TOOLTIP
        yAxisBox->setToolTip(QApplication::translate("Drawer", "Y axis variable", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        buttonRun->setToolTip(QApplication::translate("Drawer", "Run from point", 0));
#endif // QT_NO_TOOLTIP
        buttonRun->setText(QString());
#ifndef QT_NO_TOOLTIP
        buttonDelete->setToolTip(QApplication::translate("Drawer", "Delete selected solution", 0));
#endif // QT_NO_TOOLTIP
        buttonDelete->setText(QString());
#ifndef QT_NO_TOOLTIP
        buttonZoom->setToolTip(QApplication::translate("Drawer", "Zoom", 0));
#endif // QT_NO_TOOLTIP
        buttonZoom->setText(QString());
#ifndef QT_NO_TOOLTIP
        buttonPan->setToolTip(QApplication::translate("Drawer", "Pan", 0));
#endif // QT_NO_TOOLTIP
        buttonPan->setText(QString());
#ifndef QT_NO_TOOLTIP
        buttonPalette->setToolTip(QApplication::translate("Drawer", "Current/Selected Color", 0));
#endif // QT_NO_TOOLTIP
        buttonPalette->setText(QString());
#ifndef QT_NO_TOOLTIP
        buttonSettings->setToolTip(QApplication::translate("Drawer", "Drawer Settings", 0));
#endif // QT_NO_TOOLTIP
        buttonSettings->setText(QString());
#ifndef QT_NO_TOOLTIP
        buttonExport->setToolTip(QApplication::translate("Drawer", "Export to File", 0));
#endif // QT_NO_TOOLTIP
        buttonExport->setText(QString());
#ifndef QT_NO_TOOLTIP
        buttonPrint->setToolTip(QApplication::translate("Drawer", "Print", 0));
#endif // QT_NO_TOOLTIP
        buttonPrint->setText(QString());
#ifndef QT_NO_TOOLTIP
        buttonHelp->setToolTip(QApplication::translate("Drawer", "Help", 0));
#endif // QT_NO_TOOLTIP
        buttonHelp->setText(QString());
    } // retranslateUi

};

namespace Ui {
    class Drawer: public Ui_Drawer {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_DRAWER_H
