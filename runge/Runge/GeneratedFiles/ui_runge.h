/********************************************************************************
** Form generated from reading UI file 'runge.ui'
**
** Created by: Qt User Interface Compiler version 5.2.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_RUNGE_H
#define UI_RUNGE_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QGroupBox>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenu>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QRadioButton>
#include <QtWidgets/QSpinBox>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QTableWidget>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QToolBox>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_RungeClass
{
public:
    QAction *action_New;
    QAction *action_Open;
    QAction *action_Close;
    QAction *actionOpen_Recent;
    QAction *action_Start;
    QAction *action_Pause;
    QAction *actionS_top;
    QAction *actionE_xit;
    QAction *action_Save;
    QAction *actionSave_As;
    QAction *action_Font;
    QAction *actionResume;
    QAction *actionConfiguration;
    QAction *action_2D_Draw;
    QAction *action_English;
    QAction *action_Russian;
    QAction *action_Contents;
    QAction *action_About_Runge;
    QAction *actionMRU1;
    QAction *actionMRU2;
    QAction *actionMRU3;
    QAction *actionMRU4;
    QAction *actionClearAllMRUs;
    QAction *action_3D_Draw;
    QWidget *centralWidget;
    QHBoxLayout *hboxLayout;
    QVBoxLayout *leftLayout;
    QVBoxLayout *leftVerticalLayout;
    QToolBox *toolBox;
    QWidget *page_system;
    QSpinBox *spinBoxDimension;
    QGroupBox *groupODEType;
    QRadioButton *radioFTX;
    QRadioButton *radioFX;
    QRadioButton *radioAF;
    QRadioButton *radioBF;
    QLabel *labelDimension;
    QLabel *labelIndepVar;
    QLineEdit *editIndepVar;
    QWidget *page_method;
    QListWidget *listSolvers;
    QWidget *page_parameters;
    QLabel *labelEndT;
    QLineEdit *editEndT;
    QLineEdit *editStartT;
    QLabel *labelStartT;
    QTableWidget *tableMacros;
    QLabel *labelMacros;
    QPushButton *addMacro;
    QPushButton *removeMacro;
    QLabel *labelStep;
    QLineEdit *editStep;
    QLineEdit *editStepMin;
    QLabel *labelStepMin;
    QLineEdit *editStepMax;
    QLabel *labelStepMax;
    QLineEdit *editEps;
    QLabel *labelEps;
    QLineEdit *editP;
    QLabel *labelP;
    QWidget *page_solutions;
    QCheckBox *checkBoxShowAllPoints;
    QPushButton *pushButtonRemoveSolution;
    QPushButton *pushButtonExportSolution;
    QTableWidget *tableSolutions;
    QVBoxLayout *rightLayout;
    QTableWidget *tableFtx;
    QTableWidget *tableFi;
    QTableWidget *tableA;
    QTableWidget *tableFx;
    QTableWidget *tableB;
    QTableWidget *tableStartValues;
    QTableWidget *tableUtx;
    QTableWidget *tableSolutionPoints;
    QMenuBar *menuBar;
    QMenu *menuFile;
    QMenu *menuOpen_Recent;
    QMenu *menu_Run;
    QMenu *menu_Options;
    QMenu *menuLanguage;
    QMenu *menuView;
    QMenu *menu_Help;
    QToolBar *mainToolBar;
    QStatusBar *statusBar;

    void setupUi(QMainWindow *RungeClass)
    {
        if (RungeClass->objectName().isEmpty())
            RungeClass->setObjectName(QStringLiteral("RungeClass"));
        RungeClass->resize(742, 538);
        RungeClass->setMinimumSize(QSize(742, 538));
        QIcon icon;
        icon.addFile(QStringLiteral(":/Runge/Resources/32x32/graph_edge_directed.png"), QSize(), QIcon::Normal, QIcon::Off);
        RungeClass->setWindowIcon(icon);
        action_New = new QAction(RungeClass);
        action_New->setObjectName(QStringLiteral("action_New"));
        QIcon icon1;
        icon1.addFile(QStringLiteral(":/Runge/Resources/filenew.png"), QSize(), QIcon::Normal, QIcon::Off);
        action_New->setIcon(icon1);
        action_New->setIconVisibleInMenu(true);
        action_Open = new QAction(RungeClass);
        action_Open->setObjectName(QStringLiteral("action_Open"));
        QIcon icon2;
        icon2.addFile(QStringLiteral(":/Runge/Resources/folder.png"), QSize(), QIcon::Normal, QIcon::Off);
        action_Open->setIcon(icon2);
        action_Open->setIconVisibleInMenu(true);
        action_Close = new QAction(RungeClass);
        action_Close->setObjectName(QStringLiteral("action_Close"));
        actionOpen_Recent = new QAction(RungeClass);
        actionOpen_Recent->setObjectName(QStringLiteral("actionOpen_Recent"));
        action_Start = new QAction(RungeClass);
        action_Start->setObjectName(QStringLiteral("action_Start"));
        QIcon icon3;
        icon3.addFile(QStringLiteral(":/Runge/Resources/media_play_green.png"), QSize(), QIcon::Normal, QIcon::Off);
        action_Start->setIcon(icon3);
        action_Start->setIconVisibleInMenu(true);
        action_Pause = new QAction(RungeClass);
        action_Pause->setObjectName(QStringLiteral("action_Pause"));
        action_Pause->setCheckable(false);
        QIcon icon4;
        icon4.addFile(QStringLiteral(":/Runge/Resources/media_pause.png"), QSize(), QIcon::Normal, QIcon::Off);
        action_Pause->setIcon(icon4);
        action_Pause->setIconVisibleInMenu(true);
        actionS_top = new QAction(RungeClass);
        actionS_top->setObjectName(QStringLiteral("actionS_top"));
        QIcon icon5;
        icon5.addFile(QStringLiteral(":/Runge/Resources/media_stop_red.png"), QSize(), QIcon::Normal, QIcon::Off);
        actionS_top->setIcon(icon5);
        actionS_top->setIconVisibleInMenu(true);
        actionE_xit = new QAction(RungeClass);
        actionE_xit->setObjectName(QStringLiteral("actionE_xit"));
        QIcon icon6;
        icon6.addFile(QStringLiteral(":/Runge/Resources/exit.png"), QSize(), QIcon::Normal, QIcon::Off);
        actionE_xit->setIcon(icon6);
        actionE_xit->setIconVisibleInMenu(true);
        action_Save = new QAction(RungeClass);
        action_Save->setObjectName(QStringLiteral("action_Save"));
        action_Save->setEnabled(true);
        QIcon icon7;
        icon7.addFile(QStringLiteral(":/Runge/Resources/disk_blue.png"), QSize(), QIcon::Normal, QIcon::Off);
        action_Save->setIcon(icon7);
        action_Save->setIconVisibleInMenu(true);
        actionSave_As = new QAction(RungeClass);
        actionSave_As->setObjectName(QStringLiteral("actionSave_As"));
        QIcon icon8;
        icon8.addFile(QStringLiteral(":/Runge/Resources/save_as.png"), QSize(), QIcon::Normal, QIcon::Off);
        actionSave_As->setIcon(icon8);
        actionSave_As->setIconVisibleInMenu(true);
        action_Font = new QAction(RungeClass);
        action_Font->setObjectName(QStringLiteral("action_Font"));
        QIcon icon9;
        icon9.addFile(QStringLiteral(":/Runge/Resources/font.png"), QSize(), QIcon::Normal, QIcon::Off);
        action_Font->setIcon(icon9);
        action_Font->setIconVisibleInMenu(true);
        actionResume = new QAction(RungeClass);
        actionResume->setObjectName(QStringLiteral("actionResume"));
        QIcon icon10;
        icon10.addFile(QStringLiteral(":/Runge/Resources/media_step_forward.png"), QSize(), QIcon::Normal, QIcon::Off);
        actionResume->setIcon(icon10);
        actionResume->setIconVisibleInMenu(true);
        actionConfiguration = new QAction(RungeClass);
        actionConfiguration->setObjectName(QStringLiteral("actionConfiguration"));
        QIcon icon11;
        icon11.addFile(QStringLiteral(":/Runge/Resources/16x16/wrench.png"), QSize(), QIcon::Normal, QIcon::Off);
        actionConfiguration->setIcon(icon11);
        actionConfiguration->setIconVisibleInMenu(true);
        action_2D_Draw = new QAction(RungeClass);
        action_2D_Draw->setObjectName(QStringLiteral("action_2D_Draw"));
        QIcon icon12;
        icon12.addFile(QStringLiteral(":/Runge/Resources/document_view.png"), QSize(), QIcon::Normal, QIcon::Off);
        action_2D_Draw->setIcon(icon12);
        action_2D_Draw->setIconVisibleInMenu(true);
        action_English = new QAction(RungeClass);
        action_English->setObjectName(QStringLiteral("action_English"));
        QIcon icon13;
        icon13.addFile(QStringLiteral(":/Runge/Resources/16x16/flag_usa.png"), QSize(), QIcon::Normal, QIcon::Off);
        action_English->setIcon(icon13);
        action_English->setIconVisibleInMenu(true);
        action_Russian = new QAction(RungeClass);
        action_Russian->setObjectName(QStringLiteral("action_Russian"));
        QIcon icon14;
        icon14.addFile(QStringLiteral(":/Runge/Resources/16x16/flag_russia.png"), QSize(), QIcon::Normal, QIcon::Off);
        action_Russian->setIcon(icon14);
        action_Russian->setIconVisibleInMenu(true);
        action_Contents = new QAction(RungeClass);
        action_Contents->setObjectName(QStringLiteral("action_Contents"));
        QIcon icon15;
        icon15.addFile(QStringLiteral(":/Runge/Resources/16x16/help.png"), QSize(), QIcon::Normal, QIcon::Off);
        action_Contents->setIcon(icon15);
        action_Contents->setIconVisibleInMenu(true);
        action_About_Runge = new QAction(RungeClass);
        action_About_Runge->setObjectName(QStringLiteral("action_About_Runge"));
        QIcon icon16;
        icon16.addFile(QStringLiteral(":/Runge/Resources/16x16/graph_edge_directed.png"), QSize(), QIcon::Normal, QIcon::Off);
        action_About_Runge->setIcon(icon16);
        actionMRU1 = new QAction(RungeClass);
        actionMRU1->setObjectName(QStringLiteral("actionMRU1"));
        actionMRU1->setIcon(icon2);
        actionMRU2 = new QAction(RungeClass);
        actionMRU2->setObjectName(QStringLiteral("actionMRU2"));
        actionMRU2->setIcon(icon2);
        actionMRU3 = new QAction(RungeClass);
        actionMRU3->setObjectName(QStringLiteral("actionMRU3"));
        actionMRU3->setIcon(icon2);
        actionMRU4 = new QAction(RungeClass);
        actionMRU4->setObjectName(QStringLiteral("actionMRU4"));
        actionMRU4->setIcon(icon2);
        actionClearAllMRUs = new QAction(RungeClass);
        actionClearAllMRUs->setObjectName(QStringLiteral("actionClearAllMRUs"));
        action_3D_Draw = new QAction(RungeClass);
        action_3D_Draw->setObjectName(QStringLiteral("action_3D_Draw"));
        QIcon icon17;
        icon17.addFile(QStringLiteral(":/Runge/Resources/32x32/environment.png"), QSize(), QIcon::Normal, QIcon::Off);
        action_3D_Draw->setIcon(icon17);
        centralWidget = new QWidget(RungeClass);
        centralWidget->setObjectName(QStringLiteral("centralWidget"));
        hboxLayout = new QHBoxLayout(centralWidget);
        hboxLayout->setSpacing(6);
        hboxLayout->setContentsMargins(11, 11, 11, 11);
        hboxLayout->setObjectName(QStringLiteral("hboxLayout"));
        leftLayout = new QVBoxLayout();
        leftLayout->setSpacing(6);
        leftLayout->setObjectName(QStringLiteral("leftLayout"));
        leftVerticalLayout = new QVBoxLayout();
        leftVerticalLayout->setSpacing(8);
        leftVerticalLayout->setObjectName(QStringLiteral("leftVerticalLayout"));
        leftVerticalLayout->setSizeConstraint(QLayout::SetNoConstraint);
        toolBox = new QToolBox(centralWidget);
        toolBox->setObjectName(QStringLiteral("toolBox"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(toolBox->sizePolicy().hasHeightForWidth());
        toolBox->setSizePolicy(sizePolicy);
        toolBox->setMinimumSize(QSize(350, 348));
        toolBox->setMaximumSize(QSize(100000, 348));
        page_system = new QWidget();
        page_system->setObjectName(QStringLiteral("page_system"));
        page_system->setGeometry(QRect(0, 0, 355, 240));
        spinBoxDimension = new QSpinBox(page_system);
        spinBoxDimension->setObjectName(QStringLiteral("spinBoxDimension"));
        spinBoxDimension->setGeometry(QRect(210, 160, 61, 24));
        spinBoxDimension->setMinimum(1);
        spinBoxDimension->setMaximum(999);
        spinBoxDimension->setValue(1);
        groupODEType = new QGroupBox(page_system);
        groupODEType->setObjectName(QStringLiteral("groupODEType"));
        groupODEType->setGeometry(QRect(0, -10, 341, 151));
        radioFTX = new QRadioButton(groupODEType);
        radioFTX->setObjectName(QStringLiteral("radioFTX"));
        radioFTX->setGeometry(QRect(10, 30, 231, 20));
        QFont font;
        font.setBold(true);
        font.setItalic(false);
        font.setWeight(75);
        radioFTX->setFont(font);
        radioFTX->setChecked(true);
        radioFX = new QRadioButton(groupODEType);
        radioFX->setObjectName(QStringLiteral("radioFX"));
        radioFX->setGeometry(QRect(10, 60, 221, 19));
        radioFX->setFont(font);
        radioAF = new QRadioButton(groupODEType);
        radioAF->setObjectName(QStringLiteral("radioAF"));
        radioAF->setGeometry(QRect(10, 90, 221, 19));
        radioAF->setFont(font);
        radioBF = new QRadioButton(groupODEType);
        radioBF->setObjectName(QStringLiteral("radioBF"));
        radioBF->setGeometry(QRect(10, 120, 221, 19));
        radioBF->setFont(font);
        labelDimension = new QLabel(page_system);
        labelDimension->setObjectName(QStringLiteral("labelDimension"));
        labelDimension->setGeometry(QRect(10, 160, 191, 24));
        QFont font1;
        font1.setBold(true);
        font1.setWeight(75);
        labelDimension->setFont(font1);
        labelIndepVar = new QLabel(page_system);
        labelIndepVar->setObjectName(QStringLiteral("labelIndepVar"));
        labelIndepVar->setGeometry(QRect(10, 190, 191, 24));
        labelIndepVar->setFont(font1);
        editIndepVar = new QLineEdit(page_system);
        editIndepVar->setObjectName(QStringLiteral("editIndepVar"));
        editIndepVar->setGeometry(QRect(210, 190, 61, 24));
        toolBox->addItem(page_system, QStringLiteral("System"));
        page_method = new QWidget();
        page_method->setObjectName(QStringLiteral("page_method"));
        page_method->setGeometry(QRect(0, 0, 355, 240));
        listSolvers = new QListWidget(page_method);
        listSolvers->setObjectName(QStringLiteral("listSolvers"));
        listSolvers->setGeometry(QRect(10, 10, 331, 201));
        toolBox->addItem(page_method, QStringLiteral("Solver"));
        page_parameters = new QWidget();
        page_parameters->setObjectName(QStringLiteral("page_parameters"));
        page_parameters->setGeometry(QRect(0, 0, 355, 240));
        labelEndT = new QLabel(page_parameters);
        labelEndT->setObjectName(QStringLiteral("labelEndT"));
        labelEndT->setGeometry(QRect(10, 40, 51, 24));
        labelEndT->setFont(font1);
        editEndT = new QLineEdit(page_parameters);
        editEndT->setObjectName(QStringLiteral("editEndT"));
        editEndT->setGeometry(QRect(60, 40, 61, 24));
        editStartT = new QLineEdit(page_parameters);
        editStartT->setObjectName(QStringLiteral("editStartT"));
        editStartT->setGeometry(QRect(60, 10, 61, 24));
        labelStartT = new QLabel(page_parameters);
        labelStartT->setObjectName(QStringLiteral("labelStartT"));
        labelStartT->setGeometry(QRect(10, 10, 51, 24));
        labelStartT->setFont(font1);
        tableMacros = new QTableWidget(page_parameters);
        if (tableMacros->columnCount() < 2)
            tableMacros->setColumnCount(2);
        tableMacros->setObjectName(QStringLiteral("tableMacros"));
        tableMacros->setGeometry(QRect(130, 50, 211, 165));
        tableMacros->setColumnCount(2);
        tableMacros->horizontalHeader()->setDefaultSectionSize(110);
        labelMacros = new QLabel(page_parameters);
        labelMacros->setObjectName(QStringLiteral("labelMacros"));
        labelMacros->setGeometry(QRect(130, 20, 121, 24));
        labelMacros->setFont(font1);
        addMacro = new QPushButton(page_parameters);
        addMacro->setObjectName(QStringLiteral("addMacro"));
        addMacro->setGeometry(QRect(290, 20, 21, 23));
        removeMacro = new QPushButton(page_parameters);
        removeMacro->setObjectName(QStringLiteral("removeMacro"));
        removeMacro->setGeometry(QRect(320, 20, 21, 23));
        labelStep = new QLabel(page_parameters);
        labelStep->setObjectName(QStringLiteral("labelStep"));
        labelStep->setGeometry(QRect(10, 70, 51, 24));
        labelStep->setFont(font1);
        editStep = new QLineEdit(page_parameters);
        editStep->setObjectName(QStringLiteral("editStep"));
        editStep->setGeometry(QRect(60, 70, 61, 24));
        editStepMin = new QLineEdit(page_parameters);
        editStepMin->setObjectName(QStringLiteral("editStepMin"));
        editStepMin->setGeometry(QRect(60, 100, 61, 24));
        labelStepMin = new QLabel(page_parameters);
        labelStepMin->setObjectName(QStringLiteral("labelStepMin"));
        labelStepMin->setGeometry(QRect(10, 100, 51, 24));
        labelStepMin->setFont(font1);
        editStepMax = new QLineEdit(page_parameters);
        editStepMax->setObjectName(QStringLiteral("editStepMax"));
        editStepMax->setGeometry(QRect(60, 130, 61, 24));
        labelStepMax = new QLabel(page_parameters);
        labelStepMax->setObjectName(QStringLiteral("labelStepMax"));
        labelStepMax->setGeometry(QRect(10, 130, 51, 24));
        labelStepMax->setFont(font1);
        editEps = new QLineEdit(page_parameters);
        editEps->setObjectName(QStringLiteral("editEps"));
        editEps->setGeometry(QRect(60, 160, 61, 24));
        labelEps = new QLabel(page_parameters);
        labelEps->setObjectName(QStringLiteral("labelEps"));
        labelEps->setGeometry(QRect(10, 160, 51, 24));
        labelEps->setFont(font1);
        editP = new QLineEdit(page_parameters);
        editP->setObjectName(QStringLiteral("editP"));
        editP->setGeometry(QRect(60, 190, 61, 24));
        labelP = new QLabel(page_parameters);
        labelP->setObjectName(QStringLiteral("labelP"));
        labelP->setGeometry(QRect(10, 190, 51, 24));
        labelP->setFont(font1);
        toolBox->addItem(page_parameters, QStringLiteral("Parameters"));
        page_solutions = new QWidget();
        page_solutions->setObjectName(QStringLiteral("page_solutions"));
        page_solutions->setGeometry(QRect(0, 0, 355, 240));
        checkBoxShowAllPoints = new QCheckBox(page_solutions);
        checkBoxShowAllPoints->setObjectName(QStringLiteral("checkBoxShowAllPoints"));
        checkBoxShowAllPoints->setEnabled(false);
        checkBoxShowAllPoints->setGeometry(QRect(10, 11, 151, 24));
        checkBoxShowAllPoints->setFont(font1);
        pushButtonRemoveSolution = new QPushButton(page_solutions);
        pushButtonRemoveSolution->setObjectName(QStringLiteral("pushButtonRemoveSolution"));
        pushButtonRemoveSolution->setEnabled(false);
        pushButtonRemoveSolution->setGeometry(QRect(260, 10, 81, 26));
        pushButtonRemoveSolution->setFont(font1);
        pushButtonExportSolution = new QPushButton(page_solutions);
        pushButtonExportSolution->setObjectName(QStringLiteral("pushButtonExportSolution"));
        pushButtonExportSolution->setEnabled(false);
        pushButtonExportSolution->setGeometry(QRect(170, 10, 81, 26));
        pushButtonExportSolution->setFont(font1);
        toolBox->addItem(page_solutions, QStringLiteral("Solutions"));

        leftVerticalLayout->addWidget(toolBox);

        tableSolutions = new QTableWidget(centralWidget);
        if (tableSolutions->columnCount() < 7)
            tableSolutions->setColumnCount(7);
        tableSolutions->setObjectName(QStringLiteral("tableSolutions"));
        tableSolutions->setMinimumSize(QSize(355, 50));
        tableSolutions->setEditTriggers(QAbstractItemView::NoEditTriggers);
        tableSolutions->setSelectionMode(QAbstractItemView::ExtendedSelection);
        tableSolutions->setSelectionBehavior(QAbstractItemView::SelectRows);
        tableSolutions->setColumnCount(7);

        leftVerticalLayout->addWidget(tableSolutions);


        leftLayout->addLayout(leftVerticalLayout);


        hboxLayout->addLayout(leftLayout);

        rightLayout = new QVBoxLayout();
        rightLayout->setSpacing(6);
        rightLayout->setObjectName(QStringLiteral("rightLayout"));
        tableFtx = new QTableWidget(centralWidget);
        if (tableFtx->columnCount() < 2)
            tableFtx->setColumnCount(2);
        if (tableFtx->rowCount() < 1)
            tableFtx->setRowCount(1);
        tableFtx->setObjectName(QStringLiteral("tableFtx"));
        tableFtx->setMouseTracking(false);
        tableFtx->setRowCount(1);
        tableFtx->setColumnCount(2);
        tableFtx->horizontalHeader()->setDefaultSectionSize(120);

        rightLayout->addWidget(tableFtx);

        tableFi = new QTableWidget(centralWidget);
        if (tableFi->columnCount() < 2)
            tableFi->setColumnCount(2);
        if (tableFi->rowCount() < 1)
            tableFi->setRowCount(1);
        tableFi->setObjectName(QStringLiteral("tableFi"));
        tableFi->setRowCount(1);
        tableFi->setColumnCount(2);
        tableFi->horizontalHeader()->setDefaultSectionSize(120);

        rightLayout->addWidget(tableFi);

        tableA = new QTableWidget(centralWidget);
        if (tableA->columnCount() < 1)
            tableA->setColumnCount(1);
        if (tableA->rowCount() < 1)
            tableA->setRowCount(1);
        tableA->setObjectName(QStringLiteral("tableA"));
        tableA->setRowCount(1);
        tableA->setColumnCount(1);

        rightLayout->addWidget(tableA);

        tableFx = new QTableWidget(centralWidget);
        if (tableFx->columnCount() < 2)
            tableFx->setColumnCount(2);
        if (tableFx->rowCount() < 1)
            tableFx->setRowCount(1);
        tableFx->setObjectName(QStringLiteral("tableFx"));
        tableFx->setRowCount(1);
        tableFx->setColumnCount(2);
        tableFx->horizontalHeader()->setDefaultSectionSize(120);

        rightLayout->addWidget(tableFx);

        tableB = new QTableWidget(centralWidget);
        if (tableB->columnCount() < 1)
            tableB->setColumnCount(1);
        if (tableB->rowCount() < 1)
            tableB->setRowCount(1);
        tableB->setObjectName(QStringLiteral("tableB"));
        tableB->setRowCount(1);
        tableB->setColumnCount(1);

        rightLayout->addWidget(tableB);

        tableStartValues = new QTableWidget(centralWidget);
        if (tableStartValues->columnCount() < 2)
            tableStartValues->setColumnCount(2);
        if (tableStartValues->rowCount() < 1)
            tableStartValues->setRowCount(1);
        tableStartValues->setObjectName(QStringLiteral("tableStartValues"));
        tableStartValues->setRowCount(1);
        tableStartValues->setColumnCount(2);
        tableStartValues->horizontalHeader()->setDefaultSectionSize(120);

        rightLayout->addWidget(tableStartValues);

        tableUtx = new QTableWidget(centralWidget);
        if (tableUtx->columnCount() < 2)
            tableUtx->setColumnCount(2);
        if (tableUtx->rowCount() < 1)
            tableUtx->setRowCount(1);
        tableUtx->setObjectName(QStringLiteral("tableUtx"));
        tableUtx->setRowCount(1);
        tableUtx->setColumnCount(2);
        tableUtx->horizontalHeader()->setDefaultSectionSize(120);

        rightLayout->addWidget(tableUtx);

        tableSolutionPoints = new QTableWidget(centralWidget);
        tableSolutionPoints->setObjectName(QStringLiteral("tableSolutionPoints"));
        tableSolutionPoints->setColumnCount(0);

        rightLayout->addWidget(tableSolutionPoints);


        hboxLayout->addLayout(rightLayout);

        RungeClass->setCentralWidget(centralWidget);
        menuBar = new QMenuBar(RungeClass);
        menuBar->setObjectName(QStringLiteral("menuBar"));
        menuBar->setGeometry(QRect(0, 0, 742, 21));
        menuFile = new QMenu(menuBar);
        menuFile->setObjectName(QStringLiteral("menuFile"));
        menuOpen_Recent = new QMenu(menuFile);
        menuOpen_Recent->setObjectName(QStringLiteral("menuOpen_Recent"));
        menu_Run = new QMenu(menuBar);
        menu_Run->setObjectName(QStringLiteral("menu_Run"));
        menu_Options = new QMenu(menuBar);
        menu_Options->setObjectName(QStringLiteral("menu_Options"));
        menuLanguage = new QMenu(menu_Options);
        menuLanguage->setObjectName(QStringLiteral("menuLanguage"));
        menuLanguage->setTearOffEnabled(false);
        QIcon icon18;
        icon18.addFile(QStringLiteral(":/Runge/Resources/16x16/world_16.png"), QSize(), QIcon::Normal, QIcon::Off);
        menuLanguage->setIcon(icon18);
        menuView = new QMenu(menuBar);
        menuView->setObjectName(QStringLiteral("menuView"));
        menu_Help = new QMenu(menuBar);
        menu_Help->setObjectName(QStringLiteral("menu_Help"));
        RungeClass->setMenuBar(menuBar);
        mainToolBar = new QToolBar(RungeClass);
        mainToolBar->setObjectName(QStringLiteral("mainToolBar"));
        mainToolBar->setAllowedAreas(Qt::LeftToolBarArea|Qt::RightToolBarArea|Qt::TopToolBarArea);
        mainToolBar->setFloatable(false);
        RungeClass->addToolBar(Qt::TopToolBarArea, mainToolBar);
        statusBar = new QStatusBar(RungeClass);
        statusBar->setObjectName(QStringLiteral("statusBar"));
        RungeClass->setStatusBar(statusBar);
#ifndef QT_NO_SHORTCUT
        labelDimension->setBuddy(spinBoxDimension);
        labelIndepVar->setBuddy(editIndepVar);
        labelEndT->setBuddy(editEndT);
        labelStartT->setBuddy(editStartT);
        labelMacros->setBuddy(tableMacros);
        labelStep->setBuddy(editStep);
        labelStepMin->setBuddy(editStepMin);
        labelStepMax->setBuddy(editStepMax);
        labelEps->setBuddy(editEps);
        labelP->setBuddy(editP);
#endif // QT_NO_SHORTCUT

        menuBar->addAction(menuFile->menuAction());
        menuBar->addAction(menu_Options->menuAction());
        menuBar->addAction(menu_Run->menuAction());
        menuBar->addAction(menuView->menuAction());
        menuBar->addAction(menu_Help->menuAction());
        menuFile->addAction(action_New);
        menuFile->addAction(action_Open);
        menuFile->addAction(menuOpen_Recent->menuAction());
        menuFile->addAction(action_Save);
        menuFile->addAction(actionSave_As);
        menuFile->addSeparator();
        menuFile->addAction(actionE_xit);
        menuOpen_Recent->addAction(actionMRU1);
        menuOpen_Recent->addAction(actionMRU2);
        menuOpen_Recent->addAction(actionMRU3);
        menuOpen_Recent->addAction(actionMRU4);
        menuOpen_Recent->addSeparator();
        menuOpen_Recent->addAction(actionClearAllMRUs);
        menu_Run->addAction(action_Start);
        menu_Run->addAction(action_Pause);
        menu_Run->addAction(actionResume);
        menu_Run->addAction(actionS_top);
        menu_Options->addAction(actionConfiguration);
        menu_Options->addAction(action_Font);
        menu_Options->addAction(menuLanguage->menuAction());
        menuLanguage->addAction(action_English);
        menuLanguage->addAction(action_Russian);
        menuView->addAction(action_2D_Draw);
        menuView->addAction(action_3D_Draw);
        menu_Help->addAction(action_Contents);
        menu_Help->addAction(action_About_Runge);

        retranslateUi(RungeClass);

        toolBox->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(RungeClass);
    } // setupUi

    void retranslateUi(QMainWindow *RungeClass)
    {
        RungeClass->setWindowTitle(QApplication::translate("RungeClass", "Runge", 0));
#ifndef QT_NO_TOOLTIP
        RungeClass->setToolTip(QString());
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_STATUSTIP
        RungeClass->setStatusTip(QString());
#endif // QT_NO_STATUSTIP
        action_New->setText(QApplication::translate("RungeClass", "&New", 0));
#ifndef QT_NO_STATUSTIP
        action_New->setStatusTip(QApplication::translate("RungeClass", "Create new system", 0));
#endif // QT_NO_STATUSTIP
        action_New->setShortcut(QApplication::translate("RungeClass", "Ctrl+N", 0));
        action_Open->setText(QApplication::translate("RungeClass", "&Open", 0));
#ifndef QT_NO_STATUSTIP
        action_Open->setStatusTip(QApplication::translate("RungeClass", "Open previously saved system", 0));
#endif // QT_NO_STATUSTIP
        action_Open->setShortcut(QApplication::translate("RungeClass", "Ctrl+O", 0));
        action_Close->setText(QApplication::translate("RungeClass", "&Close", 0));
        actionOpen_Recent->setText(QApplication::translate("RungeClass", "Open &Recent...", 0));
        action_Start->setText(QApplication::translate("RungeClass", "&Start", 0));
#ifndef QT_NO_STATUSTIP
        action_Start->setStatusTip(QApplication::translate("RungeClass", "Start solving current system using current values", 0));
#endif // QT_NO_STATUSTIP
        action_Pause->setText(QApplication::translate("RungeClass", "&Pause", 0));
#ifndef QT_NO_STATUSTIP
        action_Pause->setStatusTip(QApplication::translate("RungeClass", "Pause solving process", 0));
#endif // QT_NO_STATUSTIP
        actionS_top->setText(QApplication::translate("RungeClass", "S&top", 0));
#ifndef QT_NO_STATUSTIP
        actionS_top->setStatusTip(QApplication::translate("RungeClass", "Stop solving process", 0));
#endif // QT_NO_STATUSTIP
        actionE_xit->setText(QApplication::translate("RungeClass", "E&xit", 0));
#ifndef QT_NO_STATUSTIP
        actionE_xit->setStatusTip(QApplication::translate("RungeClass", "Exit Runge", 0));
#endif // QT_NO_STATUSTIP
        actionE_xit->setShortcut(QApplication::translate("RungeClass", "Ctrl+Q", 0));
        action_Save->setText(QApplication::translate("RungeClass", "&Save", 0));
#ifndef QT_NO_STATUSTIP
        action_Save->setStatusTip(QApplication::translate("RungeClass", "Save current system and all solutions to a file", 0));
#endif // QT_NO_STATUSTIP
        action_Save->setShortcut(QApplication::translate("RungeClass", "Ctrl+S", 0));
        actionSave_As->setText(QApplication::translate("RungeClass", "Save &As...", 0));
#ifndef QT_NO_STATUSTIP
        actionSave_As->setStatusTip(QApplication::translate("RungeClass", "Save current system and all solutions to another file", 0));
#endif // QT_NO_STATUSTIP
        action_Font->setText(QApplication::translate("RungeClass", "&Font", 0));
#ifndef QT_NO_STATUSTIP
        action_Font->setStatusTip(QApplication::translate("RungeClass", "Choose font for entering system equations", 0));
#endif // QT_NO_STATUSTIP
        actionResume->setText(QApplication::translate("RungeClass", "&Resume", 0));
#ifndef QT_NO_STATUSTIP
        actionResume->setStatusTip(QApplication::translate("RungeClass", "Resume solving process", 0));
#endif // QT_NO_STATUSTIP
        actionConfiguration->setText(QApplication::translate("RungeClass", "&Configuration", 0));
#ifndef QT_NO_STATUSTIP
        actionConfiguration->setStatusTip(QApplication::translate("RungeClass", "Choose configuration file (configures custom solvers)", 0));
#endif // QT_NO_STATUSTIP
        action_2D_Draw->setText(QApplication::translate("RungeClass", "&2D Draw", 0));
#ifndef QT_NO_STATUSTIP
        action_2D_Draw->setStatusTip(QApplication::translate("RungeClass", "Open 2D drawer window", 0));
#endif // QT_NO_STATUSTIP
        action_English->setText(QApplication::translate("RungeClass", "&English", 0));
#ifndef QT_NO_STATUSTIP
        action_English->setStatusTip(QApplication::translate("RungeClass", "Choose English interface language", 0));
#endif // QT_NO_STATUSTIP
        action_Russian->setText(QApplication::translate("RungeClass", "&Russian", 0));
#ifndef QT_NO_STATUSTIP
        action_Russian->setStatusTip(QApplication::translate("RungeClass", "Choose Russian interface language", 0));
#endif // QT_NO_STATUSTIP
        action_Contents->setText(QApplication::translate("RungeClass", "&Contents", 0));
#ifndef QT_NO_STATUSTIP
        action_Contents->setStatusTip(QApplication::translate("RungeClass", "Open Help (assistant)", 0));
#endif // QT_NO_STATUSTIP
        action_Contents->setShortcut(QApplication::translate("RungeClass", "F1", 0));
        action_About_Runge->setText(QApplication::translate("RungeClass", "&About Runge", 0));
#ifndef QT_NO_STATUSTIP
        action_About_Runge->setStatusTip(QApplication::translate("RungeClass", "Show information about Runge", 0));
#endif // QT_NO_STATUSTIP
        actionMRU1->setText(QApplication::translate("RungeClass", "&1", 0));
        actionMRU2->setText(QApplication::translate("RungeClass", "&2", 0));
        actionMRU3->setText(QApplication::translate("RungeClass", "&3", 0));
        actionMRU4->setText(QApplication::translate("RungeClass", "&4", 0));
        actionClearAllMRUs->setText(QApplication::translate("RungeClass", "&Clear all items", 0));
        action_3D_Draw->setText(QApplication::translate("RungeClass", "&3D Draw", 0));
        groupODEType->setTitle(QString());
        radioFTX->setText(QApplication::translate("RungeClass", "x' = F(&t, x)", 0));
#ifndef QT_NO_TOOLTIP
        radioFX->setToolTip(QString());
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_STATUSTIP
        radioFX->setStatusTip(QString());
#endif // QT_NO_STATUSTIP
        radioFX->setText(QApplication::translate("RungeClass", "x' = F(&x)", 0));
        radioAF->setText(QApplication::translate("RungeClass", "x' = &A(t)x + f(x)", 0));
        radioBF->setText(QApplication::translate("RungeClass", "x' = &Bx + f(t, x)", 0));
        labelDimension->setText(QApplication::translate("RungeClass", "&Dimension", 0));
        labelIndepVar->setText(QApplication::translate("RungeClass", "&Independent variable", 0));
        editIndepVar->setText(QApplication::translate("RungeClass", "t", 0));
        toolBox->setItemText(toolBox->indexOf(page_system), QApplication::translate("RungeClass", "System", 0));
#ifndef QT_NO_TOOLTIP
        listSolvers->setToolTip(QApplication::translate("RungeClass", "Solvers available for selected System Type", 0));
#endif // QT_NO_TOOLTIP
        toolBox->setItemText(toolBox->indexOf(page_method), QApplication::translate("RungeClass", "Solver", 0));
        labelEndT->setText(QApplication::translate("RungeClass", "En&d", 0));
#ifndef QT_NO_TOOLTIP
        editEndT->setToolTip(QApplication::translate("RungeClass", "End value for the Independent Variable", 0));
#endif // QT_NO_TOOLTIP
        editEndT->setText(QApplication::translate("RungeClass", "1.", 0));
#ifndef QT_NO_TOOLTIP
        editStartT->setToolTip(QApplication::translate("RungeClass", "Start value for the Independent Variable", 0));
#endif // QT_NO_TOOLTIP
        editStartT->setText(QApplication::translate("RungeClass", "0.", 0));
        labelStartT->setText(QApplication::translate("RungeClass", "&Start", 0));
#ifndef QT_NO_TOOLTIP
        tableMacros->setToolTip(QApplication::translate("RungeClass", "Macros and their values", 0));
#endif // QT_NO_TOOLTIP
        labelMacros->setText(QApplication::translate("RungeClass", "&Macros", 0));
#ifndef QT_NO_TOOLTIP
        addMacro->setToolTip(QApplication::translate("RungeClass", "Add new macro to the table", 0));
#endif // QT_NO_TOOLTIP
        addMacro->setText(QApplication::translate("RungeClass", "+", 0));
#ifndef QT_NO_TOOLTIP
        removeMacro->setToolTip(QApplication::translate("RungeClass", "Remove selected macro", 0));
#endif // QT_NO_TOOLTIP
        removeMacro->setText(QApplication::translate("RungeClass", "-", 0));
        labelStep->setText(QApplication::translate("RungeClass", "&H", 0));
#ifndef QT_NO_TOOLTIP
        editStep->setToolTip(QApplication::translate("RungeClass", "Initial step", 0));
#endif // QT_NO_TOOLTIP
        editStep->setText(QApplication::translate("RungeClass", "0.01", 0));
#ifndef QT_NO_TOOLTIP
        editStepMin->setToolTip(QApplication::translate("RungeClass", "Minimum step allowed", 0));
#endif // QT_NO_TOOLTIP
        editStepMin->setText(QApplication::translate("RungeClass", "1.e-5", 0));
        labelStepMin->setText(QApplication::translate("RungeClass", "H&min", 0));
#ifndef QT_NO_TOOLTIP
        editStepMax->setToolTip(QApplication::translate("RungeClass", "Maximum step allowed", 0));
#endif // QT_NO_TOOLTIP
        editStepMax->setText(QApplication::translate("RungeClass", "0.1", 0));
        labelStepMax->setText(QApplication::translate("RungeClass", "Hma&x", 0));
#ifndef QT_NO_TOOLTIP
        editEps->setToolTip(QApplication::translate("RungeClass", "Maximum residual per step allowed", 0));
#endif // QT_NO_TOOLTIP
        editEps->setText(QApplication::translate("RungeClass", "1.e-10", 0));
        labelEps->setText(QApplication::translate("RungeClass", "&Eps", 0));
#ifndef QT_NO_TOOLTIP
        editP->setToolTip(QApplication::translate("RungeClass", "Relative residual measure threshold", 0));
#endif // QT_NO_TOOLTIP
        editP->setText(QApplication::translate("RungeClass", "10.", 0));
        labelP->setText(QApplication::translate("RungeClass", "&P", 0));
        toolBox->setItemText(toolBox->indexOf(page_parameters), QApplication::translate("RungeClass", "Parameters", 0));
#ifndef QT_NO_TOOLTIP
        checkBoxShowAllPoints->setToolTip(QApplication::translate("RungeClass", "Show all computed points even if there are millions of them (might take a while)", 0));
#endif // QT_NO_TOOLTIP
        checkBoxShowAllPoints->setText(QApplication::translate("RungeClass", "Show &All Points", 0));
#ifndef QT_NO_TOOLTIP
        pushButtonRemoveSolution->setToolTip(QApplication::translate("RungeClass", "Remove selected solution", 0));
#endif // QT_NO_TOOLTIP
        pushButtonRemoveSolution->setText(QApplication::translate("RungeClass", "&Delete", 0));
#ifndef QT_NO_TOOLTIP
        pushButtonExportSolution->setToolTip(QApplication::translate("RungeClass", "Export selected solution to a file", 0));
#endif // QT_NO_TOOLTIP
        pushButtonExportSolution->setText(QApplication::translate("RungeClass", "E&xport", 0));
        toolBox->setItemText(toolBox->indexOf(page_solutions), QApplication::translate("RungeClass", "Solutions", 0));
#ifndef QT_NO_TOOLTIP
        tableSolutions->setToolTip(QApplication::translate("RungeClass", "Computed solutions", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        tableFtx->setToolTip(QApplication::translate("RungeClass", "F(t,x) equation", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        tableFi->setToolTip(QApplication::translate("RungeClass", "f(x) member", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        tableA->setToolTip(QApplication::translate("RungeClass", "Matrix A(t)", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        tableFx->setToolTip(QApplication::translate("RungeClass", "F(x) equation", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        tableB->setToolTip(QApplication::translate("RungeClass", "Matrix B", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        tableStartValues->setToolTip(QApplication::translate("RungeClass", "Start Values", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        tableUtx->setToolTip(QApplication::translate("RungeClass", "f(t,x) member", 0));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        tableSolutionPoints->setToolTip(QApplication::translate("RungeClass", "Current solution", 0));
#endif // QT_NO_TOOLTIP
        menuFile->setTitle(QApplication::translate("RungeClass", "&File", 0));
        menuOpen_Recent->setTitle(QApplication::translate("RungeClass", "Open &Recent", 0));
        menu_Run->setTitle(QApplication::translate("RungeClass", "&Run", 0));
        menu_Options->setTitle(QApplication::translate("RungeClass", "&Options", 0));
        menuLanguage->setTitle(QApplication::translate("RungeClass", "&Language", 0));
        menuView->setTitle(QApplication::translate("RungeClass", "&View", 0));
        menu_Help->setTitle(QApplication::translate("RungeClass", "&Help", 0));
        mainToolBar->setWindowTitle(QApplication::translate("RungeClass", "Main Toolbar", 0));
    } // retranslateUi

};

namespace Ui {
    class RungeClass: public Ui_RungeClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_RUNGE_H
