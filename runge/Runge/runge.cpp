#include <QApplication>
#include <QtXmlPatterns>
#include <QtDebug>
#include <QSettings>
#include <QCloseEvent>
#include <QFontDialog>
#include <QFileDialog>
#include <QStandardPaths>
#include <QMessageBox>
#include <QAbstractItemView>
#include <QDomDocument>
#include <QColor>
#include <QTranslator>
#include <iostream>

#include "utils.h"
#include "runge.h"
#include "engine.h"
#include "drawer.h"
#include "drawer3d.h"
#include "lock.h"
#include "../loader.h"

#define CONFIG_SCHEMA ":/Runge/RungeConfig.xsd"
#define RUNGE_SCHEMA ":/Runge/Runge.xsd"

#define RUNGE_PREFIX "runge"
#define RUNGE_SYSTEM "runge"
#define RUNGE_SYSTEM_TYPE "systemType"
#define RUNGE_INDEP_VAR "independentVariable"
#define RUNGE_VARS "variables"
#define RUNGE_VAR "variable"
#define RUNGE_EQS "equations"
#define RUNGE_EQ "equation"
#define RUNGE_MACROS "macros"
#define RUNGE_MACRO "macro"
#define RUNGE_MACRO_NAME_ATTR "name"
#define RUNGE_EL "element"
#define RUNGE_SOLVER "solver"

#define RUNGE_DIMENSION "dimension"
#define RUNGE_START_T "startPoint"
#define RUNGE_END_T "endPoint"
#define RUNGE_STEP "step"
#define RUNGE_STEP_MIN "stepMin"
#define RUNGE_STEP_MAX "stepMax"
#define RUNGE_EPS "eps"
#define RUNGE_P "p"
#define RUNGE_COLOR "color"
#define RUNGE_COLOR2 "color2"

#define RUNGE_TABLE_FTX "tableFtx"
#define RUNGE_TABLE_FX "tableFx"
#define RUNGE_TABLE_FI "tableFi"
#define RUNGE_TABLE_UTX "tableUtx"
#define RUNGE_TABLE_SV "tableStartValues"
#define RUNGE_TABLE_MACROS "tableMacros"
#define RUNGE_TABLE_A "tableA"
#define RUNGE_TABLE_B "tableB"

#define RUNGE_SOLUTIONS "solutions"
#define RUNGE_SOLUTION "solution"
#define RUNGE_ID "id"
#define RUNGE_DIMENSION "dimension"
#define RUNGE_STEPS "steps"
#define RUNGE_TIME "time"
#define RUNGE_POINTS "points"
#define RUNGE_POINT "point"

#define SETTINGS_ORG "cvmlib.org"
#define SETTINGS_APP "Runge"
#define SETTINGS_GRP_MAINWINDOW "MainWindow"
#define SETTINGS_SIZE "size"
#define SETTINGS_POS "pos"
#define SETTINGS_FONT "font"
#define SETTINGS_LAST_FILE "lastFile"
#define SETTINGS_MRU1 "MRU1"
#define SETTINGS_MRU2 "MRU2"
#define SETTINGS_MRU3 "MRU3"
#define SETTINGS_MRU4 "MRU4"
#define SETTINGS_GRP_CONFIGURATION "Configuration"
#define SETTINGS_CONFIGFILE "configfile"
#define SETTINGS_LANGUAGE "language"

#define RUNGE_DEFAULT_WIDTH 742
#define RUNGE_DEFAULT_HEIGHT 538


Runge::Runge(QWidget *parent, Qt::WindowFlags flags) :
	QMainWindow(parent, flags),
	dirty(false),
	qErrorMessage(QErrorMessage::qtHandler()),
	engine(new Engine(this)),
	font("Courier", 12, QFont::Normal, false),
	labelComputing(tr("Computing:  ")),
	progressBarComputing(),
    doubleValidator(this),
    removeMacroRowSelected(-1),
	previous_sol_id(-1),
	currentOrbitColor(Qt::black),
	currentOrbitColor2(Qt::red),
    assistantProcess(this),
	originalToolBoxHeight(0),
    engineConfigured(false),
	aboutRunge(this)
{
    setupUi(this);

	// toolbar arrangement
	// mainToolBar->setIconSize(QSize(32,32));
    mainToolBar->addAction(action_New);
	mainToolBar->addAction(action_Open);
	mainToolBar->addAction(action_Save);
	mainToolBar->addSeparator();
	mainToolBar->addAction(action_Start);
	mainToolBar->addAction(action_Pause);
	mainToolBar->addAction(actionResume);
	mainToolBar->addAction(actionS_top);
	mainToolBar->addSeparator();
    mainToolBar->addAction(action_2D_Draw);
    mainToolBar->addAction(action_3D_Draw);
	mainToolBar->addSeparator();
	mainToolBar->addAction(actionE_xit);
	mainToolBar->addSeparator();

	labelComputingAction = mainToolBar->addWidget(&labelComputing);
	progressBarComputing.setRange(0, PROGRESS_BAR_RANGE);
	progressBarComputingAction = mainToolBar->addWidget(&progressBarComputing);
	showProgress(false);

    editStartT->setValidator(&doubleValidator);
    editEndT->setValidator(&doubleValidator);
    editStep->setValidator(&doubleValidator);
	editStepMin->setValidator(&doubleValidator);
	editStepMax->setValidator(&doubleValidator);
	editEps->setValidator(&doubleValidator);
	editP->setValidator(&doubleValidator);

    initialize_widgets();
	arrange();
	readSettings();

    removeMacro->setEnabled(false);
    tableMacros->setColumnWidth(0, 50);
    listSolvers->setSelectionMode(QAbstractItemView::SingleSelection);

	pActionCSV = menuExportSolution.addAction(tr("Export to csv"));
	pActionMATLAB = menuExportSolution.addAction(tr("Export to MATLAB"));
	pushButtonExportSolution->setMenu(&menuExportSolution);

    QIcon iconTable;
    iconTable.addFile(QString::fromUtf8(":/Runge/Resources/table.png"), QSize(), QIcon::Normal, QIcon::Off);
    pActionCSV->setIcon(iconTable);
    pActionCSV->setIconVisibleInMenu(true);
	pActionCSV->setStatusTip(tr("Export selected solution to comma-separated file"));

    QIcon iconScript;
    iconScript.addFile(QString::fromUtf8(":/Runge/Resources/scroll.png"), QSize(), QIcon::Normal, QIcon::Off);
    pActionMATLAB->setIcon(iconScript);
    pActionMATLAB->setIconVisibleInMenu(true);
	pActionMATLAB->setStatusTip(tr("Export selected solution to MATLAB script file"));

    QString languageSetting = getLanguageSetting();
    if (languageSetting.length() > 0) {
        if (languageSetting.contains("ru", Qt::CaseInsensitive)) {
            action_Russian->setEnabled(false);
        } else {
            action_English->setEnabled(false);
        }
    }
	isAssistantRU = false;

#if defined(__APPLE__)
    QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
    std::string exeDir = safe_get_exe_dir("./");
    env.insert("DYLD_FRAMEWORK_PATH", 
        QLatin1String(exeDir.c_str()) + QLatin1String("../Frameworks/"));
    assistantProcess.setProcessEnvironment(env);
//#elif defined(_WIN32)
#else
    QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
    std::string exeDir = safe_get_exe_dir("./");
    env.insert("QT_PLUGIN_PATH", 
        QLatin1String(exeDir.c_str()) + QLatin1String("plugins/"));
    assistantProcess.setProcessEnvironment(env);
#endif

	tableSolutions->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);
	tableSolutions->horizontalHeader()->setStretchLastSection(true);

	aboutRunge.setFixedSize(aboutRunge.size());
	setMRUActionNames();

	connect(addMacro, SIGNAL(clicked()), this, SLOT(addMacroClicked()));
    connect(removeMacro, SIGNAL(clicked()), this, SLOT(removeMacroClicked()));
    connect(tableMacros->verticalHeader(), SIGNAL(sectionClicked(int)), this, SLOT(tableMacrosRowSelected(int)));
    connect(tableMacros->horizontalHeader(), SIGNAL(sectionClicked(int)), this, SLOT(tableMacrosColumnSelected(int)));
    connect(tableMacros, SIGNAL(cellClicked(int,int)), this, SLOT(tableMacrosCellClicked(int,int)));

    connect(tableFtx, SIGNAL(cellChanged(int,int)), this, SLOT(tableFtxCellChanged(int,int)));
    connect(tableFx, SIGNAL(cellChanged(int,int)), this, SLOT(tableFxCellChanged(int,int)));
    connect(tableFi, SIGNAL(cellChanged(int,int)), this, SLOT(tableFiCellChanged(int,int)));
    connect(tableUtx, SIGNAL(cellChanged(int,int)), this, SLOT(tableUtxCellChanged(int,int)));
    connect(tableStartValues, SIGNAL(cellChanged(int,int)), this, SLOT(tableStartValuesCellChanged(int,int)));
    connect(tableMacros, SIGNAL(cellChanged(int,int)), this, SLOT(tableMacrosCellChanged(int,int)));
    connect(tableA, SIGNAL(cellChanged(int,int)), this, SLOT(tableACellChanged(int,int)));
    connect(tableB, SIGNAL(cellChanged(int,int)), this, SLOT(tableBCellChanged(int,int)));

	connect(editIndepVar, SIGNAL(textChanged(const QString&)), this, SLOT(editIndepVarChanged(const QString&)));
	connect(editStartT, SIGNAL(textChanged(const QString&)), this, SLOT(editStartTChanged(const QString&)));
	connect(editEndT, SIGNAL(textChanged(const QString&)), this, SLOT(editEndTChanged(const QString&)));
	connect(editStep, SIGNAL(textChanged(const QString&)), this, SLOT(editStepChanged(const QString&)));
	connect(editStepMin, SIGNAL(textChanged(const QString&)), this, SLOT(editStepMinChanged(const QString&)));
	connect(editStepMax, SIGNAL(textChanged(const QString&)), this, SLOT(editStepMaxChanged(const QString&)));
	connect(editEps, SIGNAL(textChanged(const QString&)), this, SLOT(editEpsChanged(const QString&)));
	connect(editP, SIGNAL(textChanged(const QString&)), this, SLOT(editPChanged(const QString&)));

    connect(tableSolutions, SIGNAL(itemSelectionChanged()), this, SLOT(solutionChanged()));
    connect(tableSolutions, SIGNAL(itemDoubleClicked(QTableWidgetItem*)), this, SLOT(solutionDoubleClicked(QTableWidgetItem*)));
    connect(checkBoxShowAllPoints, SIGNAL(stateChanged(int)), this, SLOT(checkBoxShowAllPointsChanged(int)));
    connect(pushButtonRemoveSolution, SIGNAL(clicked()), this, SLOT(removeSolution()));
    connect(&menuExportSolution, SIGNAL(triggered(QAction*)), this, SLOT(exportSolution(QAction*)));

    connect(this, SIGNAL(signalToStart()), this, SLOT(on_action_Start_triggered()));	
}

Runge::~Runge()
{
	delete engine;
	delete qErrorMessage;
}

void Runge::show()
{
	QMainWindow::show();
    QSettings settings(SETTINGS_ORG, SETTINGS_APP);
	settings.beginGroup(SETTINGS_GRP_CONFIGURATION);
    QString configFile = settings.value(SETTINGS_CONFIGFILE).toString();
    if (configFile.length() > 0) {
	    initialize_engine(configFile);
    } else {
		actionConfiguration->trigger();
    }
    refreshAllSlots();
	unsetDirty();
}

void Runge::tableFtxCellChanged(int,int)
{
	setDirty();
}
void Runge::tableFxCellChanged(int,int)
{
	setDirty();
}
void Runge::tableFiCellChanged(int,int)
{
	setDirty();
}
void Runge::tableUtxCellChanged(int,int)
{
	setDirty();
}
void Runge::tableStartValuesCellChanged(int,int)
{
	setDirty();
}
void Runge::tableMacrosCellChanged(int,int)
{
	setDirty();
}
void Runge::tableACellChanged(int,int)
{
	setDirty();
}
void Runge::tableBCellChanged(int,int)
{
	setDirty();
}

void Runge::refreshAllSlots()
{
    editIndepVarChanged(editIndepVar->text());
    on_spinBoxDimension_valueChanged(spinBoxDimension->text().toInt());
}

void Runge::initialize_engine(const QString& configFile)
{
    engineConfigured = true;    // just first time
	action_Start->setEnabled(false);
	action_Start->setToolTip(tr("Not configured yet"));
	listSolvers->clear();
	try {
        engine->init(configFile, CONFIG_SCHEMA);
		action_Start->setEnabled(true);
		action_Start->setToolTip(tr("Start"));
		const std::list<std::string>& solverNames = runge::SolverRegistry::getNames();
		std::list<std::string>::const_iterator it = solverNames.begin();
		while (it != solverNames.end()) {
			QListWidgetItem* listItem = new QListWidgetItem(it->c_str(), listSolvers);
			listSolvers->addItem(listItem);
			++it;
		}
    }
	catch (std::exception& ex) {
		criticalErrorBox(ex.what());
		actionConfiguration->trigger();
	}
    arrange_available_solvers();
}

void Runge::criticalErrorBox(const QString& msg)
{
	static QString caption = tr("Critical Error");
	QMessageBox::critical(this, caption, msg);
}

void Runge::inputErrorBox(const QString& msg)
{
	static QString caption = tr("Input Error");
	QMessageBox::critical(this, caption, msg);
}

void Runge::showProgress(bool show)
{
	labelComputingAction->setVisible(show);
	progressBarComputingAction->setVisible(show);
}

void Runge::initialize_widgets()
{
	initialize_widget(tableFtx, tr("Dep. var."), tr("F(t,x)"));
	initialize_widget(tableFx, tr("Dep. var."), tr("F(x)"));
	initialize_widget(tableFi, tr("Dep. var."), tr("f(x)"));
	initialize_widget(tableUtx, tr("Dep. var."), tr("f(t,x)"));
	initialize_widget(tableStartValues, tr("Dep. var."), tr("Start Value"));
	initialize_widget(tableMacros, tr("Macro"), tr("Value"));

	set_table_widget_font (tableA, font);
	set_table_widget_font (tableB, font);

    QStringList labels;
    labels << tr("id") << tr("Start") << tr("End") << tr("Steps") << tr("Eps") << tr("P") << tr("Time (ms)");
    initialize_widget(tableSolutions, labels, true);

    enforce_column_double (tableStartValues, 1);
    listSolvers->setFont(font);
}

// 2 or more columns required
void Runge::refresh_table_widget (QTableWidget* table_widget)
{
    QTableWidgetItem* currentItem = table_widget->currentItem();
    if (currentItem != NULL) {
        int other_column = currentItem->column() > 0 ? 0 : 1;
        table_widget->setCurrentCell(currentItem->row(), other_column);
        table_widget->setCurrentItem(currentItem);
    }
}

void Runge::refresh_all_table_widgets()
{
    refresh_table_widget(tableFtx);
    refresh_table_widget(tableFx);
    refresh_table_widget(tableFi);
    refresh_table_widget(tableUtx);
    refresh_table_widget(tableStartValues);
    refresh_table_widget(tableMacros);
    refresh_table_widget(tableA);
    refresh_table_widget(tableB);
}

void Runge::writeSettings()
{
	QSettings settings(SETTINGS_ORG, SETTINGS_APP);
	settings.beginGroup(SETTINGS_GRP_MAINWINDOW);
	QSize sz = size();
	settings.setValue(SETTINGS_SIZE, sz);
	settings.setValue(SETTINGS_POS, pos());
	settings.setValue(SETTINGS_FONT, font.toString());
	settings.endGroup();
}

void Runge::readSettings()
{
	QSettings settings(SETTINGS_ORG, SETTINGS_APP);
	settings.beginGroup(SETTINGS_GRP_MAINWINDOW);
	QSize qsz = settings.value(SETTINGS_SIZE, QSize(RUNGE_DEFAULT_WIDTH, RUNGE_DEFAULT_HEIGHT)).toSize();
	resize(qsz);
	QPoint qpt = settings.value(SETTINGS_POS, QPoint(25, 25)).toPoint();
	move(qpt);
	QString fontString = settings.value(SETTINGS_FONT).toString();
	if (fontString.size() > 0) {
		font.fromString(fontString);
	}
	settings.endGroup();
}

void Runge::closeEvent(QCloseEvent *event)
{
	writeSettings();
	closeAllDrawers();
	qErrorMessage->close();
	event->accept();
}

void Runge::closeAllDrawers()
{
    ptrvector<Drawer*>::iterator it = drawers.begin();
    while (it != drawers.end()) {
        (*it)->close();
        delete *it;
        ++it;
    }
	drawers.clear();
	// not so elegant but they are really different to subclass
    ptrvector<Drawer3D*>::iterator it3d = drawers3D.begin();
    while (it3d != drawers3D.end()) {
        (*it3d)->close();
        delete *it3d;
        ++it3d;
    }
	drawers3D.clear();
}

void Runge::resetAllDrawers()
{
    ptrvector<Drawer*>::iterator it = drawers.begin();
    while (it != drawers.end()) {
		Drawer* drawer = *it++;
        if (!drawer->isClosedByUser() && drawer->isRescaleAfterRunningFromPoint()) {
			drawer->scaleToBoundingRect();
		}
    }
	repaint3D();
}

void Runge::startDrawingNewSolution(int sol_id)
{
    ptrvector<Drawer*>::iterator it = drawers.begin();
    while (it != drawers.end()) {
		Drawer* drawer = *it++;
		if (!drawer->isClosedByUser()) {
			drawer->drawSolution(sol_id, true);
		}
    }
}

void Runge::redrawSolution(int sol_id)
{
    ptrvector<Drawer*>::iterator it = drawers.begin();
    while (it != drawers.end()) {
		Drawer* drawer = *it++;
		if (!drawer->isClosedByUser()) {
			drawer->redrawSolution(sol_id);
//			drawer->drawSolution(sol_id, false);
		}
    }
}

void Runge::initialize_widget (QTableWidget *table_widget, const QString& title1, const QString& title2)
{
    QStringList labels;
    labels << title1 << title2;
    initialize_widget (table_widget, labels);
}

void Runge::initialize_widget (QTableWidget *table_widget, const QStringList& labels, bool stretch_all)
{
	QHeaderView* hv = table_widget->horizontalHeader();
    if (stretch_all) {
        for (int i = 0; i < labels.size(); ++i) {
#if QT_VERSION >= 0x050000
            hv->setSectionResizeMode(i, QHeaderView::ResizeToContents);
#else
            hv->setResizeMode (i, QHeaderView::ResizeToContents);
#endif
        }
    } else {
#if QT_VERSION >= 0x050000
            hv->setSectionResizeMode(labels.size() - 1, QHeaderView::Stretch);
#else
            hv->setResizeMode (labels.size() - 1, QHeaderView::Stretch);
#endif
    }
    table_widget->setHorizontalHeaderLabels(labels);
	set_table_widget_font (table_widget, font);
}


void Runge::set_table_widget_font (QTableWidget *table_widget, const QFont& font)
{
	QHeaderView* hv = table_widget->horizontalHeader();
	QString headerFontString = hv->font().toString();
	table_widget->setFont(font);
	QFont headerFont;
	headerFont.fromString(headerFontString);
	hv->setFont(headerFont);
}

void Runge::update_table_content (QTableWidget *table_widget)
{
    int row_count = table_widget->rowCount();
    for (int i = 0; i < row_count; ++i) {
        QTableWidgetItem* item0 = table_widget->item(i,0);
        if (item0 == NULL) {
            item0 = new QTableWidgetItem(default_dep_var(i));
            item0->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
            table_widget->setItem(i, 0, item0);
        }
        QTableWidgetItem* item1 = table_widget->item(i,1);
        if (item1 == NULL) {
            item1 = new QTableWidgetItem();
            item1->setTextAlignment(Qt::AlignLeft | Qt::AlignVCenter);
            table_widget->setItem(i, 1, item1);
        }
    }
}

void Runge::update_matrix_content (QTableWidget *matrix_widget)
{
    int row_count = matrix_widget->rowCount();
    int col_count = matrix_widget->columnCount();
    for (int i = 0; i < row_count; ++i) {
		for (int j = 0; j < col_count; ++j) {
			QTableWidgetItem* item = matrix_widget->item(i,j);
			if (item == NULL) {
				item = new QTableWidgetItem("");
				item->setTextAlignment(Qt::AlignVCenter);
				matrix_widget->setItem(i, j, item);
			}
		}
	}
}

void Runge::populate_widget_cell (QTableWidget *table_widget, int row, int col, const QString& text)
{
	if (row >= 0 && row >= table_widget->rowCount()) {
		table_widget->setRowCount(row + 1);
	}
	if (col >= 0 && col >= table_widget->columnCount()) {
		table_widget->setColumnCount(col + 1);
	}
    QTableWidgetItem* item = table_widget->item(row, col);
    if (item == NULL) {
        item = new QTableWidgetItem(text);
        item->setTextAlignment(Qt::AlignVCenter);
        table_widget->setItem(row, col, item);
    } else {
        item->setText(text);
    }
}

void Runge::populate_widget_new_row (QTableWidget *table_widget, int row)
{
    for (int col = 0; col < table_widget->columnCount(); ++col) {
        populate_widget_cell (table_widget, row, col, "");
    }
}

void Runge::enforce_column_double (QTableWidget* table_widget, int col)
{
    LineEditDelegate* lineEditDelegate = new LineEditDelegate(table_widget);
    table_widget->setItemDelegateForColumn(col, lineEditDelegate);
}

QString Runge::default_dep_var (int pos)
{
    switch (pos) {
    case 0:
        return "x";
        break;
    case 1:
        return "y";
        break;
    case 2:
        return "z";
        break;
    default:
        break;
    }
    QString s("x%1");
    return s.arg(pos + 1);
}

void Runge::change_row_count (QTableWidget *table_widget, int new_row_count)
{
    int i, row_count = table_widget->rowCount();
    if (new_row_count > row_count) {
        table_widget->setRowCount(new_row_count);
        row_count = new_row_count;
    }
    for (i = 0; i < new_row_count; ++i) {
        table_widget->setRowHidden(i, false);
    }
    for (i = new_row_count; i < row_count; ++i) {
        table_widget->setRowHidden(i, true);
    }
}

void Runge::add_row (QTableWidget *table_widget)
{
    int row = table_widget->rowCount();
    table_widget->setRowCount(row + 1);
    populate_widget_new_row (table_widget, row);
}

void Runge::remove_row (QTableWidget *table_widget, int row)
{
    table_widget->removeRow(row);
}

void Runge::change_column_count (QTableWidget *table_widget, int new_column_count)
{
    int i, column_count = table_widget->columnCount();
    if (new_column_count > column_count) {
        table_widget->setColumnCount(new_column_count);
        column_count = new_column_count;
    }
    for (i = 0; i < new_column_count; ++i) {
        table_widget->setColumnHidden(i, false);
    }
    for (i = new_column_count; i < column_count; ++i) {
        table_widget->setColumnHidden(i, true);
    }
}

bool Runge::is_invalidated_with_reset (QObject *object)
{
	const ObjectInvalidated* ud = ObjectInvalidated::cast(object->userData(RUNGE_OBJECT_INVALIDATION_ID));
	set_object_state (object, false);
	return ud == NULL || ud -> is_invalidated;
}

void Runge::set_object_state (QObject *object, bool invalidated)
{
	ObjectInvalidated* ud = ObjectInvalidated::cast(object->userData(RUNGE_OBJECT_INVALIDATION_ID));
	if (ud != NULL) 
		ud -> is_invalidated = invalidated;
	else {
        QObjectUserData* pOld = object->userData(RUNGE_OBJECT_INVALIDATION_ID);
        if (pOld != NULL) {
            delete pOld;
        }
		object->setUserData(RUNGE_OBJECT_INVALIDATION_ID, new ObjectInvalidated(invalidated));
    }
}

void Runge::arrange()
{
	if (is_invalidated_with_reset(tableFtx)) {
		tableFtx->setVisible(form_state.systemType == runge::FTX &&
            form_state.page != RUNGE_TOOLBOX_PAGE_SOLUTIONS && form_state.page != RUNGE_TOOLBOX_PAGE_PARAMETERS);
	}
	if (is_invalidated_with_reset(tableFx)) {
		tableFx->setVisible(form_state.systemType == runge::FX &&
            form_state.page != RUNGE_TOOLBOX_PAGE_SOLUTIONS && form_state.page != RUNGE_TOOLBOX_PAGE_PARAMETERS);
	}
	if (is_invalidated_with_reset(tableA)) {
		tableA->setVisible(form_state.systemType == runge::AF &&
            form_state.page != RUNGE_TOOLBOX_PAGE_SOLUTIONS && form_state.page != RUNGE_TOOLBOX_PAGE_PARAMETERS);
	}
	if (is_invalidated_with_reset(tableFi)) {
		tableFi->setVisible(form_state.systemType == runge::AF &&
            form_state.page != RUNGE_TOOLBOX_PAGE_SOLUTIONS && form_state.page != RUNGE_TOOLBOX_PAGE_PARAMETERS);
	}
	if (is_invalidated_with_reset(tableB)) {
		tableB->setVisible(form_state.systemType == runge::BF &&
            form_state.page != RUNGE_TOOLBOX_PAGE_SOLUTIONS && form_state.page != RUNGE_TOOLBOX_PAGE_PARAMETERS);
	}
	if (is_invalidated_with_reset(tableUtx)) {
		tableUtx->setVisible(form_state.systemType == runge::BF && // form_state.is_radioBF && 
            form_state.page != RUNGE_TOOLBOX_PAGE_SOLUTIONS && form_state.page != RUNGE_TOOLBOX_PAGE_PARAMETERS);
	}
	tableStartValues->setVisible(form_state.page == RUNGE_TOOLBOX_PAGE_PARAMETERS);
	tableSolutionPoints->setVisible(form_state.page == RUNGE_TOOLBOX_PAGE_SOLUTIONS);
	leftVerticalLayout->invalidate();
}

void Runge::runFromPos(int xVarIndex, double xVarValue, int yVarIndex, double yVarValue)
{
	setVarValueByIndex(xVarIndex, xVarValue);
	if (yVarIndex != xVarIndex) {
		setVarValueByIndex(yVarIndex, yVarValue);
	}
	signalToStart();
}

void Runge::setVarValueByIndex(int varIndex, double varValue)
{
	static QString a1("%1");
	const QString sValue = a1.arg(varValue);
	if (varIndex <= 0) {
		editStartT->setText(sValue);
	} else {
		const int dimension = spinBoxDimension->text().toInt();
		if (varIndex <= dimension) {
			QTableWidgetItem* widgetItem = tableStartValues->item(varIndex - 1, 1);
			if (widgetItem != NULL) {
				widgetItem->setText(sValue);
			}
		}
	}
}

void Runge::get_names(std::vector<std::string>& names, bool vars, bool including_indep) const throw (std::exception)   // vars or bodies
{
    if (including_indep) {
        QString indepVar = editIndepVar->text().trimmed();
        if (indepVar.length() < 1) {
            throw std::runtime_error(QMainWindow::tr("Empty independent variable encountered").toStdString());
        }
        names.push_back(indepVar.toStdString());
    }

    QTableWidget* pTable = NULL;
    if (form_state.systemType == runge::FTX) {
        pTable = tableFtx;
    } else if (form_state.systemType == runge::FX) {
        pTable = tableFx;
    } else if (form_state.systemType == runge::AF) {
        pTable = tableFi;
    } else {
        pTable = tableUtx;
    }
	if (pTable != NULL) {
		const int dimension = spinBoxDimension->text().toInt();
		const int row_count = std::min<int>(dimension, pTable->rowCount());
		for (int i = 0; i < row_count; ++i) {
			QTableWidgetItem* widgetItem = pTable->item(i, vars ? 0 : 1);
			QString name = widgetItem != NULL ? widgetItem->text().trimmed() : QString("");
			if (name.length() < 1) {
				if (vars) {
					QString msg(QMainWindow::tr("Variable name at %1 is emty"));
					throw std::runtime_error(msg.arg(i).toStdString());
				} else {
					name = "0";     // default is zero for body
				}
			}
			names.push_back(name.toStdString());
		}
	}
}

void Runge::get_macros_meanings(std::vector<std::string>& saMacros, std::vector<std::string>& saMacrosMeanings) const
{
    read_column(tableMacros, 0, 0, saMacros);
    read_column(tableMacros, 0, 1, saMacrosMeanings);
}

void Runge::arrange_available_solvers()
{
    listSolvers->clearSelection();
    QListWidgetItem* listItemFirstAvailable = NULL;
    for (int i = 0; i < listSolvers->count(); ++i) {
        QListWidgetItem* listItem = listSolvers->item (i);
		QString name = listItem->text();			
		try {
            runge::SolverRegistry::SolverProperties solverProperties =
				runge::SolverRegistry::getSolverProperties(name.toStdString());
            runge::SolverType solverType = solverProperties.getSolverType();
            if (runge::isSolverEnabled (form_state.systemType, solverType)) {
                if (listItemFirstAvailable == NULL) {
                    listItemFirstAvailable = listItem;
                }
			    Qt::ItemFlags flags = listItem->flags();
			    flags |= Qt::ItemIsEnabled;
				flags |= Qt::ItemIsSelectable;
			    listItem->setFlags(flags);
            } else {
			    Qt::ItemFlags flags = listItem->flags();
                flags &= ~Qt::ItemIsEnabled;
				flags &= ~Qt::ItemIsSelectable;
			    listItem->setFlags(flags);
            }
        } catch (std::exception&) {
            QString msg(tr("Can't get properties of "));
            msg += name;
            criticalErrorBox(msg);
	    }
    }
    if (listItemFirstAvailable != NULL) {
        listItemFirstAvailable->setSelected(true);
    }
}

void Runge::select_solver(const QString& solver)
{
    for (int i = 0; i < listSolvers->count(); ++i) {
        QListWidgetItem* listItem = listSolvers->item (i);
		if (solver.compare(listItem->text()) == 0) {
			listItem->setSelected(true);
			break;
		}
	}
}

QString Runge::get_solver_selected() const throw (std::exception)
{
    QList<QListWidgetItem*> items = listSolvers->selectedItems();
    if (items.size() != 1) {
		throw std::runtime_error("Please select a solver");
    }
	return items.at(0)->text();
}

void Runge::read_column (const QTableWidget *table_widget, int dimension, int col, cvm::string_array& sa) const
{
    const int row_count = dimension > 0 ? std::min<int>(dimension, table_widget->rowCount()) : table_widget->rowCount();
    for (int i = 0; i < row_count; ++i) {
		QTableWidgetItem* widgetItem = table_widget->item(i, col);
        if (widgetItem != NULL) {
		    sa.push_back(widgetItem->text().trimmed().toStdString());
        } else {
		    sa.push_back("");
        }
	}
}

void Runge::read_matrix (QTableWidget *table_widget, cvm::string_array& sa, bool read_all) const throw (std::exception)
{
    const int dimension = spinBoxDimension->text().toInt();
    int col_count = read_all ? table_widget->columnCount() : std::min<int>(dimension, table_widget->columnCount());
    int row_count = read_all ? table_widget->rowCount() : std::min<int>(dimension, table_widget->rowCount());
    for (int j = 0; j < col_count; ++j) {
        for (int i = 0; i < row_count; ++i) {
		    QTableWidgetItem* widgetItem = table_widget->item(i, j);
			if (widgetItem != NULL) {
				QString itemText = widgetItem->text().trimmed();
				if (itemText.length() > 0) {
					sa.push_back(itemText.toStdString());
					continue;
				}
			}
		    sa.push_back("0");  // empty string is zero
	    }
    }
}

void Runge::read_rmatrix (QTableWidget *table_widget, cvm::rmatrix& m) const throw (std::exception)
{
    const int dimension = spinBoxDimension->text().toInt();
    int col_count = std::min<int>(dimension, table_widget->columnCount());
    int row_count = std::min<int>(dimension, table_widget->rowCount());
    bool ok = true;
    for (int j = 0; j < col_count; ++j) {
        for (int i = 0; i < row_count; ++i) {
		    QTableWidgetItem* widgetItem = table_widget->item(i, j);
			if (widgetItem != NULL) {
				QString itemText = widgetItem->text().trimmed();
				if (itemText.length() > 0) {    // empty string means zero
					m(i+1,j+1) = itemText.toDouble(&ok);
					if (!ok) {
						std::ostringstream os;
						os << "Unable to convert to double: " << itemText.toStdString() << std::ends;
						throw std::runtime_error(os.str());
					}
				}
			}
	    }
    }
}

void Runge::read_rvector (QTableWidget *table_widget, int col, cvm::rvector& v) const throw (std::exception)
{
    const int dimension = spinBoxDimension->text().toInt();
    int row_count = std::min<int>(dimension, table_widget->rowCount());
    bool ok = true;
    for (int i = 0; i < row_count; ++i) {
		QTableWidgetItem* widgetItem = table_widget->item(i, col);
		QString itemText = widgetItem->text().trimmed();
		if (itemText.length() > 0) {        // empty string means zero
            v(i+1) = itemText.toDouble(&ok);
            if (!ok) {
                std::ostringstream os;
                os << "Unable to convert to double: " << itemText.toStdString() << std::ends;
                throw std::runtime_error(os.str());
            }
        }
    }
}

void Runge::on_radioFTX_toggled(bool b)
{
    if (b) {
        form_state.systemType = runge::FTX;
    	arrange();
    	arrange_available_solvers();
    }
	set_object_state (tableFtx, b);
	setDirty();
}

void Runge::on_radioFX_toggled(bool b)
{
    if (b) {
        form_state.systemType = runge::FX;
    	arrange();
    	arrange_available_solvers();
    }
	set_object_state (tableFx, b);
	setDirty();
}

void Runge::on_radioAF_toggled(bool b)
{
    if (b) {
        form_state.systemType = runge::AF;
    	arrange();
    	arrange_available_solvers();
    }
	set_object_state (tableA, b);
	set_object_state (tableFi, b);
	setDirty();
}

void Runge::on_radioBF_toggled(bool b)
{
    if (b) {
        form_state.systemType = runge::BF;
    	arrange();
    	arrange_available_solvers();
    }
	set_object_state (tableB, b);
	set_object_state (tableUtx, b);
	setDirty();
}

void Runge::on_toolBox_currentChanged(int page)
{
    AutoWaitCursor waitCursor;
	if (originalToolBoxHeight <= 0) {
		originalToolBoxHeight = toolBox->maximumHeight();
	}
	toolBox->setMaximumHeight(page == RUNGE_TOOLBOX_PAGE_SOLUTIONS ?
		SMALLER_TOOLBOX_HEIGHT : originalToolBoxHeight);
	toolBox->setMinimumHeight(page == RUNGE_TOOLBOX_PAGE_SOLUTIONS ?
		SMALLER_TOOLBOX_HEIGHT : originalToolBoxHeight);
	form_state.page = page;
	arrange();
}

void Runge::on_spinBoxDimension_valueChanged(int new_row_count)
{
    change_row_count (tableFtx, new_row_count);
    update_table_content(tableFtx);
    change_row_count (tableFx, new_row_count);
    update_table_content(tableFx);
    change_row_count (tableFi, new_row_count);
    update_table_content(tableFi);
    change_row_count (tableUtx, new_row_count);
    update_table_content(tableUtx);
    change_row_count (tableStartValues, new_row_count);
    update_table_content(tableStartValues);

    change_row_count (tableA, new_row_count);
    change_column_count (tableA, new_row_count);
    update_matrix_content(tableA);
    change_row_count (tableB, new_row_count);
    change_column_count (tableB, new_row_count);
    update_matrix_content(tableB);

	action_3D_Draw->setEnabled(new_row_count > 1);

	setDirty();
}


void Runge::on_actionConfiguration_triggered()
{
    const std::string exeDir = safe_get_exe_dir(".");
    QString configFile = engineConfigured ?
        QFileDialog::getOpenFileName(this, tr("Select Configuration File"), exeDir.c_str(), tr("Configuration Files (*.xml)")) :
        QString(exeDir.c_str()) + QString("Runge.xml");    
    if (configFile.length() > 0) {
        QSettings settings(SETTINGS_ORG, SETTINGS_APP);
	    settings.beginGroup(SETTINGS_GRP_CONFIGURATION);
        settings.setValue(SETTINGS_CONFIGFILE, configFile);
	    initialize_engine(configFile);
    	arrange_available_solvers();
    }
}

void Runge::on_action_Font_triggered()
{
    bool ok = false;
    font = QFontDialog::getFont(&ok, font, this);
    if (ok) {
		initialize_widgets();
    }
}

void Runge::on_action_English_triggered()
{
    setLanguageSetting("en");
    informAboutLanguageChange();
}

void Runge::on_action_Russian_triggered()
{
    setLanguageSetting("ru");
    informAboutLanguageChange();
}

void Runge::informAboutLanguageChange()
{
    QMessageBox::information(this, tr("Information"), tr("Language will be changed after program restart"));
}

void Runge::setLanguageSetting(const QString& newLang)
{
    QSettings settings(SETTINGS_ORG, SETTINGS_APP);
	settings.beginGroup(SETTINGS_GRP_CONFIGURATION);
    settings.setValue(SETTINGS_LANGUAGE, newLang);
}

QString Runge::getLanguageSetting()
{
    QSettings settings(SETTINGS_ORG, SETTINGS_APP);
	settings.beginGroup(SETTINGS_GRP_CONFIGURATION);
    return settings.value(SETTINGS_LANGUAGE).toString();
}

bool Runge::isOKToDiscardMessage()
{
	return QMessageBox::Yes == QMessageBox::question (this, tr("Document Changed"), 
		tr("OK to discard changes?"), QMessageBox::Yes | QMessageBox::No, QMessageBox::No);
}

void Runge::on_action_New_triggered()
{
	if (isDirty() && !isOKToDiscardMessage()) {
		return;
	}
	resetAllLikeNew();
	unsetDirty();
}

void Runge::writeLastFileSetting(const QString& lastFile)
{
	QSettings settings(SETTINGS_ORG, SETTINGS_APP);
	settings.beginGroup(SETTINGS_GRP_MAINWINDOW);
	settings.setValue(SETTINGS_LAST_FILE, lastFile);
	settings.endGroup();
}

QString Runge::readLastFileSetting()
{
	QSettings settings(SETTINGS_ORG, SETTINGS_APP);
	settings.beginGroup(SETTINGS_GRP_MAINWINDOW);
	QString currentDir = settings.value(SETTINGS_LAST_FILE).toString();
	settings.endGroup();
	return currentDir;
}

void Runge::writeMRUSettings(const QString& mru1, const QString& mru2, const QString& mru3, const QString& mru4)
{
	QSettings settings(SETTINGS_ORG, SETTINGS_APP);
	settings.beginGroup(SETTINGS_GRP_MAINWINDOW);
	settings.setValue(SETTINGS_MRU1, mru1);
	settings.setValue(SETTINGS_MRU2, mru2);
	settings.setValue(SETTINGS_MRU3, mru3);
	settings.setValue(SETTINGS_MRU4, mru4);
	settings.endGroup();
}

void Runge::readMRUSettings(QString& mru1, QString& mru2, QString& mru3, QString& mru4)
{
	QSettings settings(SETTINGS_ORG, SETTINGS_APP);
	settings.beginGroup(SETTINGS_GRP_MAINWINDOW);
	mru1 = settings.value(SETTINGS_MRU1).toString();
	mru2 = settings.value(SETTINGS_MRU2).toString();
	mru3 = settings.value(SETTINGS_MRU3).toString();
	mru4 = settings.value(SETTINGS_MRU4).toString();
	settings.endGroup();
}

void Runge::pushMRU(const QString& mru)
{
	QString mru1, mru2, mru3, mru4;
	readMRUSettings(mru1, mru2, mru3, mru4);
	if (mru != mru1 && mru != mru2 && mru != mru3 && mru != mru4) {
		mru4 = mru3;
		mru3 = mru2;
		mru2 = mru1;
		mru1 = mru;
		writeMRUSettings(mru1, mru2, mru3, mru4);
	}
	setMRUActionNames();
}

void Runge::setMRUActionNames()
{
	QString mru1, mru2, mru3, mru4;
	readMRUSettings(mru1, mru2, mru3, mru4);
	actionMRU1->setEnabled(mru1.length() > 0 && fileName != mru1);
	actionMRU1->setText(QString("&1 ") + mru1);
	actionMRU2->setEnabled(mru2.length() > 0 && fileName != mru2);
	actionMRU2->setText(QString("&2 ") + mru2);
	actionMRU3->setEnabled(mru3.length() > 0 && fileName != mru3);
	actionMRU3->setText(QString("&3 ") + mru3);
	actionMRU4->setEnabled(mru4.length() > 0 && fileName != mru4);
	actionMRU4->setText(QString("&4 ") + mru4);
}

void Runge::openMRUFile(int pos)
{
	QString mru;
	QString mru1, mru2, mru3, mru4;
	readMRUSettings(mru1, mru2, mru3, mru4);
	switch (pos) {
	case 1:
		mru = mru1;
		break;
	case 2:
		mru = mru2;
		break;
	case 3:
		mru = mru3;
		break;
	case 4:
		mru = mru4;
		break;
	default:
		return;
	}
	openFile(mru);
}

void Runge::on_action_Open_triggered()
{
	if (isDirty() && !isOKToDiscardMessage()) {
		return;
	}
    openFile(getOpenRungeFile(readLastFileSetting()));
}

void Runge::openFile(const QString& openFileName)
{
    if (openFileName.length() > 0) {
        fileName = openFileName;
		writeLastFileSetting(fileName);
		pushMRU(fileName);
    }
    try {
        deserialize(fileName);    
    } catch (std::exception& ex) {
        criticalErrorBox(ex.what());
    }
}

void Runge::on_actionMRU1_triggered()
{
	openMRUFile(1);
}

void Runge::on_actionMRU2_triggered()
{
	openMRUFile(2);
}

void Runge::on_actionMRU3_triggered()
{
	openMRUFile(3);
}

void Runge::on_actionMRU4_triggered()
{
	openMRUFile(4);
}

void Runge::on_actionClearAllMRUs_triggered()
{
	QString mru1, mru2, mru3, mru4;
	writeMRUSettings(mru1, mru2, mru3, mru4);
	setMRUActionNames();
}

void Runge::on_action_Save_triggered()
{
    refresh_all_table_widgets();
    if (validateInputs()) {
        if (fileName.length() <= 0) {
            fileName = getSaveRungeFile("");
        }
        if (fileName.length() > 0) {
            try {
                serialize(fileName);    
            } catch (std::exception& ex) {
        		criticalErrorBox(ex.what());
        	}
        }
    }
}

void Runge::on_actionSave_As_triggered()
{
    refresh_all_table_widgets();
    if (validateInputs()) {
        QString asFileName = getSaveRungeFile(fileName);
        if (asFileName.length() > 0) {
            fileName = asFileName;
            try {
                serialize(fileName);    
            } catch (std::exception& ex) {
        		criticalErrorBox(ex.what());
        	}
        }
    }
}

void Runge::on_actionE_xit_triggered()
{
	if (isDirty() && !isOKToDiscardMessage()) {
		return;
	}
	// <cacheDirectory>Runge</cacheDirectory>
	QStringList dataLocations = QStandardPaths::standardLocations(QStandardPaths::DataLocation);
	for (int i = 0; i < dataLocations.size(); ++i) {
		removeDirectory(dataLocations.at(i));
	}
	close();
}

void Runge::on_action_Start_triggered()
{
    refresh_all_table_widgets();
    if (validateInputs()) {
        engine->start();
    }
}

void Runge::on_action_Pause_triggered()
{
	engine->pause();
}

void Runge::on_actionResume_triggered()
{
	engine->resume();
}

void Runge::on_actionS_top_triggered()
{
	engine->stop();
}

void Runge::addMacroClicked()
{
    add_row(tableMacros);
}

void Runge::removeMacroClicked()
{
    if (removeMacroRowSelected >= 0) {
        remove_row (tableMacros, removeMacroRowSelected);
        removeMacroRowSelected = -1;
        removeMacro->setEnabled(false);
    }
}

void Runge::tableMacrosRowSelected(int row)
{
    removeMacroRowSelected = row;
    removeMacro->setEnabled(true);
}

void Runge::tableMacrosColumnSelected(int)
{
    removeMacroRowSelected = -1;
    removeMacro->setEnabled(false);
}

void Runge::tableMacrosCellClicked(int,int)
{
    removeMacroRowSelected = -1;
    removeMacro->setEnabled(false);
}

void Runge::editIndepVarChanged(const QString& varName)
{
    labelStartT->setText(tr("&Start ") + varName);
    labelEndT->setText(tr("&End ") + varName);
	setDirty();
}
void Runge::editStartTChanged(const QString&)
{
	setDirty();
}
void Runge::editEndTChanged(const QString&)
{
	setDirty();
}
void Runge::editStepChanged(const QString&)
{
	setDirty();
}
void Runge::editStepMinChanged(const QString&)
{
	setDirty();
}
void Runge::editStepMaxChanged(const QString&)
{
	setDirty();
}
void Runge::editEpsChanged(const QString&)
{
	setDirty();
}
void Runge::editPChanged(const QString&)
{
	setDirty();
}

void Runge::add_solution(int sol_id)
{
    const Solution& solution = engine->get_solutions().get_solution(sol_id);
    const std::vector<SolutionPoint>& pts = solution.get_pts();
    if (pts.size() > 0) {
        SolutionPoint pfirst = *pts.begin();
        SolutionPoint plast = *pts.rbegin();
        const int row = tableSolutions->rowCount();
        tableSolutions->setRowCount(row + 1);
        const QString a1("%1");
        populate_widget_cell (tableSolutions, row, 0, a1.arg(sol_id));
        populate_widget_cell (tableSolutions, row, 1, a1.arg(pfirst.get_t()));
        populate_widget_cell (tableSolutions, row, 2, a1.arg(plast.get_t()));
        populate_widget_cell (tableSolutions, row, 3, a1.arg(pts.size()));
        populate_widget_cell (tableSolutions, row, 4, a1.arg(solution.get_eps()));
        populate_widget_cell (tableSolutions, row, 5, a1.arg(solution.get_p()));
		populate_widget_cell (tableSolutions, row, 6, a1.arg(solution.get_time_ms()));
		tableSolutions->resizeColumnsToContents();
		tableSolutions->horizontalHeader()->setStretchLastSection(true);
		setDirty();
		drawNewSolution(sol_id);
		resetAllDrawers();
    }
}

void Runge::drawNewSolution(int sol_id)
{
    ptrvector<Drawer*>::iterator itd = drawers.begin();
    while (itd != drawers.end()) {
		Drawer* drawer = *itd++;
		if (!drawer->isClosedByUser()) {
			// false because we need to redraw whatever was drawn in progress of computing
			if (!drawer->drawSolution(sol_id, false)) {
				drawer->close();
			}
		}
    }
}

int Runge::getSolutionSelected() const
{
	int sol_id_selected = -1;
    Solutions& sols = engine->get_solutions();
    Solutions::iterator it = sols.begin();
    while (it != sols.end()) {
        if (it->second.is_selected()) {
            sol_id_selected = it->first;
			break;
        }
		++it;
	}
	return sol_id_selected;
}

void Runge::unselectAllSolutions()
{
    Solutions& sols = engine->get_solutions();
    Solutions::iterator it = sols.begin();
    while (it != sols.end()) {
        if (it->second.is_selected()) {
	        it->second.set_selected(false);
			ptrvector<Drawer*>::iterator itd = drawers.begin();
			while (itd != drawers.end()) {
				Drawer* drawer = *itd++;
				if (!drawer->isClosedByUser()) {
					drawer->drawSolution(it->first, false);
				}
			}
			repaint3D();
		}
		++it;
	}
	pushButtonRemoveSolution->setDisabled(true);
	pushButtonExportSolution->setDisabled(true);
    checkBoxShowAllPoints->setDisabled(true);
}

void Runge::selectSolutionInTable(int sol_id)
{
	if (sol_id < 0) {
		tableSolutions->clearSelection();
		unselectAllSolutions();
	} else {
		QList<QTableWidgetItem*> itemsSelected = tableSolutions->selectedItems();
		if (itemsSelected.size() > 0 && itemsSelected.at(0)->text().toInt() == sol_id) {
			return;	// already selected
		}
		const int rowCount = tableSolutions->rowCount();
		for (int row = 0; row < rowCount; ++row) {
			if (tableSolutions->item(row, 0)->text().toInt() == sol_id) {
				tableSolutions->selectRow(row);
				break;
			}
		}
	}
}

void Runge::solutionChanged()
{
   	QList<QTableWidgetItem*> itemsSelected = tableSolutions->selectedItems();
    if (itemsSelected.size() == tableSolutions->columnCount()) {
		AutoWaitCursor waitCursor;
		const int sol_id = itemsSelected.at(0)->text().toInt();
		if (sol_id != previous_sol_id) {
			checkBoxShowAllPoints->setEnabled(true);
			checkBoxShowAllPoints->setCheckState(Qt::Unchecked);
			previous_sol_id = sol_id;
		}
        const bool showAll = checkBoxShowAllPoints->checkState() == Qt::Checked;
		
		const Solution& solution = engine->get_solutions().get_solution(sol_id);
		const std::vector<SolutionPoint>& pts = solution.get_pts();
	    const std::vector<std::string>& var_names = solution.get_var_names();
		tableSolutionPoints->clear();

		QStringList labels;
		std::vector<std::string>::const_iterator it = var_names.begin();
		while (it != var_names.end()) {
			labels << (it++)->c_str();
		}
		tableSolutionPoints->setColumnCount(labels.size());
		tableSolutionPoints->setRowCount(showAll ? (int)pts.size() : std::min<int>(MAX_POINTS_TO_SHOW_FIRST, (int)pts.size()));
		initialize_widget (tableSolutionPoints, labels, false);
		tableSolutionPoints->setSelectionBehavior(QAbstractItemView::SelectRows);

		const QString a1("%1");
		int row = 0;
		std::vector<SolutionPoint>::const_iterator itp = pts.begin();
		while (itp != pts.end()) {
			const SolutionPoint& pt = *itp++;
			const cvm::rvector& vx = pt.get_x();
			tableSolutionPoints->setItem(row, 0, new QTableWidgetItem(a1.arg(pt.get_t())));
			for (int i = 1; i <= vx.size(); ++i) {
				tableSolutionPoints->setItem(row, i, new QTableWidgetItem(a1.arg(vx(i))));
			}
			tableSolutionPoints->setItem(row, vx.size() + 1, new QTableWidgetItem(a1.arg(pt.get_h())));
			row++;
			if (!showAll && row > MAX_POINTS_TO_SHOW_FIRST) {
				break;
			}
		}
		pushButtonExportSolution->setEnabled(true);
        setSolutionSelected(sol_id);
    } else {
        checkBoxShowAllPoints->setEnabled(false);
		pushButtonExportSolution->setEnabled(false);
		tableSolutionPoints->setRowCount(0);
        previous_sol_id = -1;
	}

    if (itemsSelected.size() > 0) {
		pushButtonRemoveSolution->setEnabled(true);
    } else {
		pushButtonExportSolution->setEnabled(false);
		pushButtonRemoveSolution->setEnabled(false);
    }
}

void Runge::solutionDoubleClicked(QTableWidgetItem* item) {
    if (item != NULL) {
        int row = item->row();
        QTableWidgetItem* itemSol = tableSolutions->item(row, 0);
        if (itemSol != NULL) {
            const int sol_id = itemSol->text().toInt();
            const Solution sol = engine->get_solutions().get_solution(sol_id);
            std::wostringstream text;
            text << tr("Points: ").toStdWString() << sol.get_pts().size() << std::endl;
            double t = 0.;
            if (sol.get_start(0, t)) {
                text << "Start t=" << t << std::endl;
            }
            if (sol.get_end(0, t)) {
                text << "End t=" << t << std::endl;
            }
            text << "Eps=" << sol.get_eps() << std::endl;
            text << "P=" << sol.get_p() << std::endl;
            text << std::endl;
            // solution time, start, end, min, max
            const std::vector<std::string>& var_names = sol.get_var_names();
            const std::vector<std::string>& eqs = sol.get_equations();
            std::vector<std::string>::const_iterator itv = var_names.begin();
            std::vector<std::string>::const_iterator ite = eqs.begin();
            if (itv != var_names.end()) {
                text << tr("Independent variable: ").toStdWString() << itv->c_str() << "\n";
                ++itv;
            }
            text << std::endl;
            text << tr("Equations: ").toStdWString() << std::endl;
            while (itv != var_names.end() && ite != eqs.end()) {
                text << itv->c_str() << "\' = " << ite->c_str() << "\n";
                ++itv;
                ++ite;
            }
            const std::vector<std::string>& macros = sol.get_macros();
            const std::vector<std::string>& macros_meanings = sol.get_macros_meanings();
            if (macros.size() > 0 && macros_meanings.size() > 0) {
                text << std::endl;
                text << tr("Macros: ").toStdWString() << std::endl;
                itv = macros.begin();
                ite = macros_meanings.begin();
                while (itv != macros.end() && ite != macros_meanings.end()) {
                    text << itv->c_str() << " = " << ite->c_str() << "\n";
                    ++itv;
                    ++ite;
                }
            }

            text << std::ends;
            QString titlePattern(tr("Solution %1 Prperties"));
            QString title = titlePattern.arg(sol_id);
            // TODO use multiline edit box?
            QMessageBox::information(this, title, QString::fromStdWString(text.str()));
        }
    }
}

void Runge::setSolutionSelected(int sol_id)
{
    int sol_id_selected = -1;
    Solutions& sols = engine->get_solutions();
    Solutions::iterator it = sols.begin();
    while (it != sols.end()) {
        if (it->second.is_selected() && it->first != sol_id) {
            sol_id_selected = it->first;
        }
        it->second.set_selected(it->first == sol_id);
        ++it;
    }
    ptrvector<Drawer*>::iterator itd = drawers.begin();
    while (itd != drawers.end()) {
		Drawer* drawer = *itd++;
		if (!drawer->isClosedByUser()) {
			drawer->drawSolution(sol_id_selected, false);   // redraw unselected
			drawer->drawSolution(sol_id, false);            // redraw selected
			drawer->checkDeleteAvailability();
		}
    }
	repaint3D();
}

void Runge::deleteSelectedSolutions(bool userAck)
{
    if (tableSolutions->selectedItems().size() > 0) {
		if (userAck) {
			QString questionMsg = tr("Do you want to delete solution(s)? This is not recoverable");
			if (QMessageBox::No == QMessageBox::question (this, tr("Delete Solution"), 
				questionMsg, QMessageBox::Yes | QMessageBox::No,  QMessageBox::No)) {
				return;
			}
		}
		pushButtonRemoveSolution->setDisabled(true);
		pushButtonExportSolution->setDisabled(true);
		checkBoxShowAllPoints->setDisabled(true);
		tableSolutionPoints->setRowCount(0);
        int rows = tableSolutions->rowCount();
        for (int row = rows-1; row >= 0; --row) {
            QTableWidgetItem* item = tableSolutions->item(row, 0);
            if (item == NULL || !item->isSelected()) continue;
            const int sol_id = item->text().toInt();
		    tableSolutions->removeRow(row); // will invoke solutionChanged(), thus it should go before orbits deletion
            ptrvector<Drawer*>::iterator itd = drawers.begin();
            while (itd != drawers.end()) {
                (*itd++)->deleteOrbit(sol_id);            
            }
		    engine->get_solutions().remove_solution(sol_id);
            itd = drawers.begin();
            while (itd != drawers.end()) {
				(*itd++)->checkDeleteAvailability();
            }
			repaint3D();
        }
		setDirty();
	}
}

void Runge::removeSolution()
{
	deleteSelectedSolutions(true);
}

void Runge::checkBoxShowAllPointsChanged(int state)
{
	if (state == Qt::Checked) {
		solutionChanged();
		if (tableSolutions->selectedItems().size() > 0) {
			checkBoxShowAllPoints->setDisabled(true);
		}
	}
}

void Runge::exportSolution(QAction* action)
{
	QList<QTableWidgetItem*> itemsSelected = tableSolutions->selectedItems();
	if (itemsSelected.size() > 0) {
		const int sol_id = itemsSelected.at(0)->text().toInt();
	    if (action == pActionCSV) {
            QString csvFile = QFileDialog::getSaveFileName(this, tr("Export to CSV file"), tr(""), tr("CSV files (*.csv *.txt)"));
            if (csvFile.length() > 0) {
        		const Solution& solution = engine->get_solutions().get_solution(sol_id);
                try {
                    solution.export_to_scv(csvFile.toStdString());
                }
              	catch (std::exception& ex) {
            		criticalErrorBox(ex.what());
            	}
            }
        }
	    else if (action == pActionMATLAB) {
            QString mFile = QFileDialog::getSaveFileName(this, tr("Export to MATLAB file"), tr(""), tr("MATLAB scripts (*.m)"));
            if (mFile.length() > 0) {
        		const Solution& solution = engine->get_solutions().get_solution(sol_id);
                try {
                    QString varName(tr("solution%1"));
                    solution.export_to_matlab(mFile.toStdString(), varName.arg(sol_id).toStdString());
                }
              	catch (std::exception& ex) {
            		criticalErrorBox(ex.what());
            	}
            }
        }
    }
}

bool Runge::validateInputs()
{
    if (enforcePositiveValue(RUNGE_TOOLBOX_PAGE_PARAMETERS, editStepMin) &&
        enforcePositiveValue(RUNGE_TOOLBOX_PAGE_PARAMETERS, editStepMax) &&
        enforcePositiveValue(RUNGE_TOOLBOX_PAGE_PARAMETERS, editEps) &&
        enforcePositiveValue(RUNGE_TOOLBOX_PAGE_PARAMETERS, editP))
    {
        const double hmin = editStepMin->text().toDouble();
        const double hmax = editStepMax->text().toDouble();
        if (hmax < hmin) {
            inputErrorBox(tr("Maximum step should be not less than minimum (currently entered %1<%2)").arg(hmax).arg(hmin));
            focusEditBox(RUNGE_TOOLBOX_PAGE_PARAMETERS, editStepMax);
            return false;
        }
		if (editIndepVar->text().trimmed().length() < 1) {
            inputErrorBox(tr("Please enter independent variable"));
            focusEditBox(RUNGE_TOOLBOX_PAGE_SYSTEM, editIndepVar);
            return false;
		}
        return true;
    }
    return false;
}

bool Runge::enforcePositiveValue(int toolBoxPage, QLineEdit* editBox)
{
    bool ret = true;
    const double value = editBox->text().toDouble();
    if (value <= 0.0) {
        ret = false;
        inputErrorBox(tr("Please enter positive value (currently entered value is %1)").arg(value));
        focusEditBox(toolBoxPage, editBox);
    }
    return ret;
}

void Runge::focusEditBox(int toolBoxPage, QLineEdit* editBox)
{
    toolBox->setCurrentIndex(toolBoxPage);
    editBox->setFocus();
    editBox->selectAll();
}

QString Runge::getOpenRungeFile(const QString& current)
{
    return QFileDialog::getOpenFileName(this, tr("Open Runge file"), current, tr("Runge files (*.runge)"));
}

QString Runge::getSaveRungeFile(const QString& current)
{
    return QFileDialog::getSaveFileName(this, tr("Save to Runge file"), current, tr("Runge files (*.runge)"));
}

void Runge::setSystemType(runge::SystemType systemType)
{
	radioFTX->setChecked(systemType == runge::FTX);
	radioFX->setChecked(systemType == runge::FX);
	radioAF->setChecked(systemType == runge::AF);
	radioBF->setChecked(systemType == runge::BF);
}

void Runge::serialize(const QString& file_name) const throw (std::exception)
{
    AutoWaitCursor waitCursor;
    QDomDocument doc;
	QDomNode xmlNode = doc.createProcessingInstruction("xml", "version=\"1.0\" encoding=\"utf-8\"");
	doc.appendChild(xmlNode);
	doc.appendChild(doc.createComment(QMainWindow::tr("This file was generated by Runge at %1. "
		"Edit with care.").arg(QDateTime::currentDateTime().toString())));
    QDomElement root = doc.createElementNS("http://www.cvmlib.com/runge", RUNGE_SYSTEM);
	root.setPrefix(RUNGE_PREFIX);
	root.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
	root.setAttribute("xsi:schemaLocation", "http://www.cvmlib.com/runge Runge.xsd");
    doc.appendChild(root);

	const int dimension = spinBoxDimension->text().toInt();
    QDomElement tagDimension = doc.createElement(RUNGE_DIMENSION);
    tagDimension.appendChild(doc.createTextNode(spinBoxDimension->text()));
	root.appendChild(tagDimension);

    QDomElement tagIndepVar = doc.createElement(RUNGE_INDEP_VAR);
    tagIndepVar.appendChild(doc.createTextNode(editIndepVar->text()));
	root.appendChild(tagIndepVar);
	
    QDomElement tagSystemType = doc.createElement(RUNGE_SYSTEM_TYPE);
    tagSystemType.appendChild(doc.createTextNode(runge::systemTypeName(systemType()).c_str()));
	root.appendChild(tagSystemType);

	root.appendChild(serializeSystem(tableFtx, doc, RUNGE_TABLE_FTX, dimension));
	root.appendChild(serializeSystem(tableFx, doc, RUNGE_TABLE_FX, dimension));
	root.appendChild(serializeSystem(tableFi, doc, RUNGE_TABLE_FI, dimension));
	root.appendChild(serializeSystem(tableUtx, doc, RUNGE_TABLE_UTX, dimension));
	root.appendChild(serializeSystem(tableStartValues, doc, RUNGE_TABLE_SV, dimension));
	root.appendChild(serializeSystem(tableMacros, doc, RUNGE_TABLE_MACROS, dimension));
	root.appendChild(serializeMatrix(tableA, doc, RUNGE_TABLE_A));
	root.appendChild(serializeMatrix(tableB, doc, RUNGE_TABLE_B));

    QDomElement tagSolver = doc.createElement(RUNGE_SOLVER);
    tagSolver.appendChild(doc.createCDATASection(get_solver_selected()));
	root.appendChild(tagSolver);

    QDomElement tagStartTime = doc.createElement(RUNGE_START_T);
    tagStartTime.appendChild(doc.createTextNode(editStartT->text()));
	root.appendChild(tagStartTime);

    QDomElement tagEndTime = doc.createElement(RUNGE_END_T);
    tagEndTime.appendChild(doc.createTextNode(editEndT->text()));
	root.appendChild(tagEndTime);

    QDomElement tagStep = doc.createElement(RUNGE_STEP);
    tagStep.appendChild(doc.createTextNode(editStep->text()));
	root.appendChild(tagStep);

    QDomElement tagStepMin = doc.createElement(RUNGE_STEP_MIN);
    tagStepMin.appendChild(doc.createTextNode(editStepMin->text()));
	root.appendChild(tagStepMin);

    QDomElement tagStepMax = doc.createElement(RUNGE_STEP_MAX);
    tagStepMax.appendChild(doc.createTextNode(editStepMax->text()));
	root.appendChild(tagStepMax);

    QDomElement tagEps = doc.createElement(RUNGE_EPS);
    tagEps.appendChild(doc.createTextNode(editEps->text()));
	root.appendChild(tagEps);

    QDomElement tagP = doc.createElement(RUNGE_P);
    tagP.appendChild(doc.createTextNode(editP->text()));
	root.appendChild(tagP);

    unsigned int r, g, b, a;
    const QString a1("%1");
    const Solutions& solutions = engine->get_solutions();
    QDomElement tagSols = doc.createElement(RUNGE_SOLUTIONS);
    Solutions::const_iterator it = solutions.begin();
    while (it != solutions.end()) {
        const int id = it->first;
        const Solution& solution = it->second;
        ++it;

        const std::vector<SolutionPoint>& pts = solution.get_pts();
        if (pts.size() <= 0) {
            continue;
        }
        SolutionPoint pfirst = *pts.begin();
        SolutionPoint plast = *pts.rbegin();
        QDomElement tagSol = doc.createElement(RUNGE_SOLUTION);
        tagSol.setAttribute(RUNGE_ID, a1.arg(id));
		tagSol.setAttribute(RUNGE_DIMENSION, a1.arg(solution.get_dimension()));
        tagSol.setAttribute(RUNGE_START_T, a1.arg(pfirst.get_t()));
        tagSol.setAttribute(RUNGE_END_T, a1.arg(plast.get_t()));
        tagSol.setAttribute(RUNGE_STEPS, a1.arg(pts.size()));
        tagSol.setAttribute(RUNGE_EPS, a1.arg(solution.get_eps()));
        tagSol.setAttribute(RUNGE_P, a1.arg(solution.get_p()));
        tagSol.setAttribute(RUNGE_TIME, a1.arg(solution.get_time_ms()));

        solution.get_color(true, &r, &g, &b, &a);
        unsigned long long color = b + (g << 8) + (r << 16) + (a << 24);
        tagSol.setAttribute(RUNGE_COLOR, a1.arg(color, 0, 16));
        solution.get_color(false, &r, &g, &b, &a);
        unsigned long long color2 = b + (g << 8) + (r << 16) + (a << 24);
        tagSol.setAttribute(RUNGE_COLOR2, a1.arg(color2, 0, 16));

        const std::vector<std::string>& var_names = solution.get_var_names();
        QDomElement tagVars = doc.createElement(RUNGE_VARS);
        std::vector<std::string>::const_iterator itv = var_names.begin();
        while (itv != var_names.end()) {
            QDomElement tagVar = doc.createElement(RUNGE_VAR);
            tagVar.appendChild(doc.createTextNode(itv++->c_str()));
    	    tagVars.appendChild(tagVar);
        }
        tagSol.appendChild(tagVars);

        const std::vector<std::string>& equations = solution.get_equations();
        QDomElement tagEqs = doc.createElement(RUNGE_EQS);
        std::vector<std::string>::const_iterator iteq = equations.begin();
        while (iteq != equations.end()) {
            QDomElement tagEq = doc.createElement(RUNGE_EQ);
            tagEq.appendChild(doc.createTextNode(iteq++->c_str()));
    	    tagEqs.appendChild(tagEq);
        }
        tagSol.appendChild(tagEqs);

        const std::vector<std::string>& macros = solution.get_macros();
        const std::vector<std::string>& macrosMeanings = solution.get_macros_meanings();
        QDomElement tagMacros = doc.createElement(RUNGE_MACROS);
        std::vector<std::string>::const_iterator itm = macros.begin();
        std::vector<std::string>::const_iterator itmm = macrosMeanings.begin();
        while (itm != macros.end() && itmm != macrosMeanings.end()) {
            QDomElement tagMacro = doc.createElement(RUNGE_MACRO);
            tagMacro.appendChild(doc.createTextNode(itmm++->c_str()));
            tagMacro.setAttribute(RUNGE_MACRO_NAME_ATTR, itm++->c_str());
    	    tagMacros.appendChild(tagMacro);
        }
        tagSol.appendChild(tagMacros);

        QDomElement tagPts = doc.createElement(RUNGE_POINTS);
        std::ostringstream os;
        os.precision(16);	// fix: all available digits to store
		os.setf(std::ios::scientific | std::ios::showpoint | std::ios::left);
        std::vector<SolutionPoint>::const_iterator itp = pts.begin();
        while (itp != pts.end()) {
            os << *itp++ << std::endl;
        }
        os << std::ends;
        tagPts.appendChild(doc.createCDATASection(os.str().c_str()));
        tagSol.appendChild(tagPts);
        tagSols.appendChild(tagSol);
    }
	root.appendChild(tagSols);

	QFile rungeFile(file_name);
    if (!rungeFile.open(QIODevice::WriteOnly)) {
        throw std::runtime_error(QMainWindow::tr("Failed to create Runge file %1").arg(file_name).toStdString());
    }
	QTextStream out(&rungeFile);
	doc.save(out, 2);
	unsetDirty();
}

void Runge::deleteAllSolutions()
{
    ptrvector<Drawer*>::iterator it = drawers.begin();
    while (it != drawers.end()) {
		(*it++)->deleteAllOrbits();
    }
	this->engine->get_solutions().clear();
	repaint3D();
}

void Runge::deserialize(const QString& file_name) throw (std::exception)
{
    AutoWaitCursor waitCursor;
	QFile rungeFile(file_name);
    if (!rungeFile.open(QIODevice::ReadOnly)) {
        throw std::runtime_error(tr("Failed to open Runge file %1").arg(file_name).toStdString());
    }

	const QString schemaFile(RUNGE_SCHEMA);
	QFile xsdFile(schemaFile);
	xsdFile.open(QIODevice::ReadOnly);
	const QByteArray xmlText(rungeFile.readAll());
	const QByteArray xsdText(xsdFile.readAll());

	validateXML(file_name, xmlText, schemaFile, xsdText);

	// variable names get changed:
	closeAllDrawers();
	resetAllLikeNew();

	QDomDocument domDocument;
	domDocument.setContent(xmlText);
	QDomElement rungeNode = domDocument.
		firstChildElement(RUNGE_PREFIX ":" RUNGE_SYSTEM);

	const int dimension = rungeNode.firstChildElement(RUNGE_DIMENSION).text().toInt();
	spinBoxDimension->setValue(dimension);

	editIndepVar->setText(rungeNode.firstChildElement(RUNGE_INDEP_VAR).text());

	runge::SystemType systemType = runge::systemTypeByName(
		rungeNode.firstChildElement(RUNGE_SYSTEM_TYPE).text().toStdString());
	setSystemType(systemType);

    deserializeSystem(tableFtx, rungeNode.firstChildElement(RUNGE_TABLE_FTX));
    deserializeSystem(tableFx, rungeNode.firstChildElement(RUNGE_TABLE_FX));
    deserializeSystem(tableFi, rungeNode.firstChildElement(RUNGE_TABLE_FI));
    deserializeSystem(tableUtx, rungeNode.firstChildElement(RUNGE_TABLE_UTX));
    deserializeSystem(tableStartValues, rungeNode.firstChildElement(RUNGE_TABLE_SV));
    deserializeSystem(tableMacros, rungeNode.firstChildElement(RUNGE_TABLE_MACROS));
	deserializeMatrix(tableA, dimension, rungeNode.firstChildElement(RUNGE_TABLE_A));
	deserializeMatrix(tableB, dimension, rungeNode.firstChildElement(RUNGE_TABLE_B));

	select_solver(rungeNode.firstChildElement(RUNGE_SOLVER).text());

	editStartT->setText(rungeNode.firstChildElement(RUNGE_START_T).text());
	editEndT->setText(rungeNode.firstChildElement(RUNGE_END_T).text());
	editStep->setText(rungeNode.firstChildElement(RUNGE_STEP).text());
	editStepMin->setText(rungeNode.firstChildElement(RUNGE_STEP_MIN).text());
	editStepMax->setText(rungeNode.firstChildElement(RUNGE_STEP_MAX).text());
	editEps->setText(rungeNode.firstChildElement(RUNGE_EPS).text());
	editP->setText(rungeNode.firstChildElement(RUNGE_P).text());

	deleteAllSolutions();
	Solutions& solutions = this->engine->get_solutions();

    unsigned int r, g, b, a;
    unsigned int r2, g2, b2, a2;
    bool ok;
    unsigned long long color;
    QDomElement tagSols = rungeNode.firstChildElement(RUNGE_SOLUTIONS);
    QDomNodeList listSols = tagSols.elementsByTagName(RUNGE_SOLUTION);
    for (int i = 0; i < listSols.size(); ++i) {
		QDomElement tagSol = listSols.at(i).toElement();
		std::vector<std::string> var_names;
		QDomElement tagVars = tagSol.firstChildElement(RUNGE_VARS);
	    QDomNodeList listVars = tagVars.elementsByTagName(RUNGE_VAR);
	    for (int j = 0; j < listVars.size(); ++j) {
			var_names.push_back(listVars.at(j).toElement().text().toStdString());
		}
		std::vector<std::string> equations;
		QDomElement tagEquations = tagSol.firstChildElement(RUNGE_EQS);
	    QDomNodeList listEquations = tagEquations.elementsByTagName(RUNGE_EQ);
	    for (int j = 0; j < listEquations.size(); ++j) {
			equations.push_back(listEquations.at(j).toElement().text().toStdString());
		}
		std::vector<std::string> macros, macrosMeanings;
		QDomElement tagMacros = tagSol.firstChildElement(RUNGE_MACROS);
	    QDomNodeList listMacros = tagMacros.elementsByTagName(RUNGE_MACRO);
	    for (int j = 0; j < listMacros.size(); ++j) {
            QDomElement tagMacro = listMacros.at(j).toElement();
            macros.push_back(tagMacro.attribute(RUNGE_MACRO_NAME_ATTR).toStdString());
			macrosMeanings.push_back(tagMacro.text().toStdString());
		}
        const int dimension = tagSol.attribute(RUNGE_DIMENSION).toInt();

        color = tagSol.attribute(RUNGE_COLOR).toULongLong(&ok, 16);
        b = ok ? (color & 0xFF) : 0;
        g = ok ? (color >> 8 & 0xFF) : 0;
        r = ok ? (color >> 16 & 0xFF) : 0;
        a = ok ? (color >> 24 & 0xFF) : 0xFF;

		//color2 is optional
		QString strCol2 = tagSol.attribute(RUNGE_COLOR2);
		if (strCol2.isEmpty()) {
			b2 = b;
			g2 = g;
			r2 = 0xFF - r;	// backward compatibility
			a2 = a;
		} else {
			color = strCol2.toULongLong(&ok, 16);
			b2 = ok ? (color & 0xFF) : 0;
			g2 = ok ? (color >> 8 & 0xFF) : 0;
			r2 = ok ? (color >> 16 & 0xFF) : 0;
			a2 = ok ? (color >> 24 & 0xFF) : 0xFF;
		}

		Solution solution(dimension,
			tagSol.attribute(RUNGE_START_T).toDouble(),
			tagSol.attribute(RUNGE_EPS).toDouble(),
			tagSol.attribute(RUNGE_P).toDouble(), 
			var_names, equations, macros, macrosMeanings,
            0LL, r, g, b, a, r2, g2, b2, a2);
		solution.set_time(tagSol.attribute(RUNGE_TIME).toDouble());
		const int sol_id = tagSol.attribute(RUNGE_ID).toInt();
		solutions.add_solution(sol_id, solution);
		Solution& sol_ref = solutions[sol_id];
		std::istringstream is(tagSol.firstChildElement(RUNGE_POINTS).text().toStdString());
		SolutionPoint spt(dimension);
		while (!is.eof()) {
			is >> spt >> std::ws;
			sol_ref.add_solution_point(spt);
		}
		add_solution(sol_id);
    }
	unsetDirty();
}

QDomElement Runge::serializeSystem(QTableWidget* table_widget, QDomDocument& doc, 
	const QString& tag, int dimension) const throw (std::exception)
{
    QDomElement tagSystem = doc.createElement(tag);
    QDomElement tagVars = doc.createElement(RUNGE_VARS);
	tagSystem.appendChild(tagVars);
    QDomElement tagEqs = doc.createElement(RUNGE_EQS);
	tagSystem.appendChild(tagEqs);

    cvm::string_array sa0, sa1;
    read_column (table_widget, dimension, 0, sa0);
    read_column (table_widget, dimension, 1, sa1);
    cvm::string_array::const_iterator it = sa0.begin();
    while (it != sa0.end()) {
        QDomElement tagVar = doc.createElement(RUNGE_VAR);
        tagVar.appendChild(doc.createTextNode(it++->c_str()));
    	tagVars.appendChild(tagVar);
    }
    it = sa1.begin();
    while (it != sa1.end()) {
        QDomElement tagEq = doc.createElement(RUNGE_EQ);
        tagEq.appendChild(doc.createCDATASection(it->c_str()));
    	tagEqs.appendChild(tagEq);
        ++it;
    }
    return tagSystem;
}


void Runge::deserializeSystem(QTableWidget* table_widget, const QDomElement& tagSystem) throw (std::exception)
{
    QDomElement tagVars = tagSystem.firstChildElement(RUNGE_VARS);
    QDomElement tagEqs = tagSystem.firstChildElement(RUNGE_EQS);
    int i;
    QDomNodeList listVars = tagVars.elementsByTagName(RUNGE_VAR);
    for (i = 0; i < listVars.size(); ++i) {
        populate_widget_cell (table_widget, i, 0, listVars.at(i).toElement().text());
    }
    QDomNodeList listEqs = tagEqs.elementsByTagName(RUNGE_EQ);
    for (i = 0; i < listEqs.size(); ++i) {
        populate_widget_cell (table_widget, i, 1, listEqs.at(i).toElement().text());
    }
}

QDomElement Runge::serializeMatrix(QTableWidget *table_widget, QDomDocument& doc, const QString& tag) const throw (std::exception)
{
    QDomElement tagMatrix = doc.createElement(tag);
    cvm::string_array sa;
    read_matrix (table_widget, sa, true);
	cvm::string_array::const_iterator it = sa.begin();
    while (it != sa.end()) {
        QDomElement tagEl = doc.createElement(RUNGE_EL);
        tagEl.appendChild(doc.createCDATASection(it->c_str()));
    	tagMatrix.appendChild(tagEl);
        ++it;
    }
	return tagMatrix;
}

void Runge::deserializeMatrix(QTableWidget *table_widget, int dim, const QDomElement& tagMatrix) throw (std::exception)
{
    QDomNodeList listEls = tagMatrix.elementsByTagName(RUNGE_EL);
    for (int i = 0; i < listEls.size(); ++i) {
        populate_widget_cell (table_widget, i % dim, i / dim, listEls.at(i).toElement().text());
	}
}

void Runge::resetAllLikeNew()
{
	closeAllDrawers();
	tableSolutionPoints->clear();
	tableSolutionPoints->setRowCount(0);
	tableSolutions->setRowCount(0);
	deleteAllSolutions();

	spinBoxDimension->setValue(1);
	reset_table_content(tableFtx, true);
    reset_table_content(tableFx, true);
    reset_table_content(tableFi, true);
    reset_table_content(tableUtx, true);
    reset_table_content(tableStartValues, true);
    reset_table_content(tableMacros, false);
	reset_matrix_content(tableA);
	reset_matrix_content(tableB);
	
	editIndepVar->setText("t");
	editStartT->setText("0.");
	editEndT->setText("1.");
	editStep->setText("0.01");
	editStepMin->setText("1.e-5");
	editStepMax->setText("0.1");
	editEps->setText("1.e-10");
	editP->setText("10.");

	unsetDirty();
}

void Runge::reset_table_content (QTableWidget *table_widget, bool addVar)
{
	if (!addVar) {
		table_widget->setRowCount(0);
		return;
	}
	table_widget->setRowCount(1);
    QTableWidgetItem* item0 = table_widget->item(0, 0);
    if (item0 == NULL) {
        item0 = new QTableWidgetItem();
        table_widget->setItem(0, 0, item0);
    }
	item0->setText(default_dep_var(0));
    QTableWidgetItem* item1 = table_widget->item(0, 1);
    if (item1 == NULL) {
        item1 = new QTableWidgetItem();
        table_widget->setItem(0, 1, item1);
	}
	item1->setText("");
}

void Runge::reset_matrix_content (QTableWidget *matrix_widget)
{
	matrix_widget->setRowCount(1);
	matrix_widget->setColumnCount(1);
    QTableWidgetItem* item = matrix_widget->item(0, 0);
    if (item == NULL) {
        item = new QTableWidgetItem();
        matrix_widget->setItem(0, 0, item);
    }
	item->setText("");
}

void Runge::on_action_2D_Draw_triggered()
{
    Drawer* pDrawer = new Drawer(this);
    drawers.push_back(pDrawer);
    pDrawer->show();
}

void Runge::on_action_3D_Draw_triggered()
{
    Drawer3D* pDrawer3D = new Drawer3D(this);
    drawers3D.push_back(pDrawer3D);
    pDrawer3D->show();
}

void Runge::on_action_Contents_triggered()
{
    int currentIndex = toolBox->currentIndex();
	showHelp(currentIndex == 0 ? "introduction" : "programming");
}

void Runge::showHelp(const QString& keyword)
{
	if (assistantProcess.state() == QProcess::NotRunning) {
#if defined(_MSC_VER)
        const std::string helpFile = std::string("\"") + safe_get_exe_dir("./") +
            (isAssistantRU ? "help_ru.qhc\"" : "help_en.qhc\"");
#else
        const std::string helpFile = safe_get_exe_dir("./") + (isAssistantRU ? "help_ru.qhc" : "help_en.qhc");
#endif

        QStringList argsList;
        argsList << QLatin1String("-collectionFile")
            << QLatin1String(helpFile.c_str())
            << QLatin1String("-enableRemoteControl");
        
#if defined(_MSC_VER)
        // Windows XP needs native list
        assistantProcess.setNativeArguments(argsList.join(" "));
        assistantProcess.start(QLatin1String("assistant"));
#else
    #ifdef __APPLE__
        assistantProcess.start(QLatin1String(safe_get_exe_dir("./").c_str()) + QLatin1String("Assistant.app/Contents/MacOS/Assistant"), argsList);
    #else
        assistantProcess.start(QLatin1String(safe_get_exe_dir("./").c_str()) + QLatin1String("assistant"), argsList);
    #endif
#endif
        if (!assistantProcess.waitForStarted()) {
            QProcess::ProcessError err = assistantProcess.error();
            static QString a1(tr("Runge failed to start assistant for some reason. "
                "Please use online documentation http://cvmlib.com/runge. Error code: %1"));
            QMessageBox::warning(this, tr("Can't start assistant"), a1.arg(err));
            return;
		}
	}
	if (keyword.length() > 0) {
		QByteArray ba;
		ba.append("activateKeyword ");
		ba.append(keyword);
		ba.append('\0');
		assistantProcess.write(ba);
	}
}

void Runge::on_action_About_Runge_triggered()
{
	aboutRunge.show();
}

void Runge::repaint3D() const
{
	ptrvector<Drawer3D*>::const_iterator it3d = drawers3D.begin();
	while (it3d != drawers3D.end()) {
		Drawer3D* drawer3d = *it3d++;
		if (!drawer3d->isClosedByUser()) {
			drawer3d->repaint(); // TODO
		}
	}
}
