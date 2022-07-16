#ifndef RUNGE_H
#define RUNGE_H

#include <typeinfo>

#include <QMainWindow>
#include <QHeaderView>
#include <QProgressBar>
#include <QSplitter>
#include <QLabel>
#include <QStyledItemDelegate>
#include <QErrorMessage>
#include <QDomDocument>
#include <QTranslator>
#include <QProcess>

#include "ui_runge.h"
#include "about.h"
#include "../solver.h"
#include "solutions.h"
#include "ptrvector.h"

#if defined(WIN32) && defined(_DEBUG)
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif

#define PROGRESS_BAR_RANGE 1000
#define PROGRESS_BAR_RANGE_DBL 1000.
#define MAX_POINTS_TO_SHOW_FIRST 100

#define RUNGE_TOOLBOX_PAGE_SYSTEM       0
#define RUNGE_TOOLBOX_PAGE_MTHOD        1
#define RUNGE_TOOLBOX_PAGE_PARAMETERS   2
#define RUNGE_TOOLBOX_PAGE_SOLUTIONS    3
#define SMALLER_TOOLBOX_HEIGHT          166

#define RUNGE_OBJECT_INVALIDATION_ID    0

class Engine;
class Drawer;
class Drawer3D;

class Runge : public QMainWindow, public Ui::RungeClass
{
    Q_OBJECT

public:
	static QMutex& busy_computing_mutex() {
		static QMutex _mutex;
		return _mutex;
	}

    Runge(QWidget *parent = 0, Qt::WindowFlags flags = 0);
    ~Runge();

	void show();
	void showProgress(bool show);
	void criticalErrorBox(const QString& msg);
	void inputErrorBox(const QString& msg);
    void openFile(const QString& openFileName);

	QProgressBar* progressBar() {
		return &progressBarComputing;
	}

    void add_solution(int sol_id);
    void read_rmatrix(QTableWidget *table_widget, cvm::rmatrix& m) const throw (std::exception);
    void read_rvector(QTableWidget *table_widget, int col, cvm::rvector& v) const throw (std::exception);
    void read_matrix(QTableWidget *table_widget, cvm::string_array& sa, bool read_all) const throw (std::exception);
	void read_column(const QTableWidget *table_widget, int size, int col, cvm::string_array& sa) const;

	QString get_solver_selected() const throw (std::exception);
    void select_solver(const QString& solver);
    void selectSolutionInTable(int sol_id);
    void deleteSelectedSolutions(bool userAck);
    int getSolutionSelected() const;
    void startDrawingNewSolution(int sol_id);
    void redrawSolution(int sol_id);
    void runFromPos(int xVarIndex, double xVarValue, int yVarIndex, double yVarValue);
	void showHelp(const QString& keyword);

private:
	mutable bool dirty;
	QErrorMessage* qErrorMessage;
	Engine* engine;
    QString fileName;

	QFont font;
	QLabel labelComputing;
	QProgressBar progressBarComputing;
	QAction *labelComputingAction, *progressBarComputingAction;
    
    QDoubleValidator doubleValidator;
    int removeMacroRowSelected;
	int previous_sol_id;
	QMenu menuExportSolution;
	QAction* pActionCSV;
	QAction* pActionMATLAB;
	QColor currentOrbitColor;
	QColor currentOrbitColor2;

	bool isAssistantRU;
	QProcess assistantProcess;

	int originalToolBoxHeight;
    bool engineConfigured;

	AboutRunge aboutRunge;

	void initialize_widgets();
	void initialize_engine(const QString& configFile);
	void arrange();
	void arrange_available_solvers();
	void closeEvent(QCloseEvent *event);
    bool validateInputs();
	bool enforcePositiveValue(int toolBoxPage, QLineEdit* editBox);
	void focusEditBox(int toolBoxPage, QLineEdit* editBox);
	void writeSettings();
	void readSettings();
	void writeLastFileSetting(const QString& lastFile);
	QString readLastFileSetting();
	void writeMRUSettings(const QString& mru1, const QString& mru2, const QString& mru3, const QString& mru4);
	void readMRUSettings(QString& mru1, QString& mru2, QString& mru3, QString& mru4);
	void pushMRU(const QString& mru);
	void setMRUActionNames();
	void openMRUFile(int pos);
    QString getSaveRungeFile(const QString& current);
    QString getOpenRungeFile(const QString& current);
    void serialize(const QString& file_name) const throw (std::exception);
    void deserialize(const QString& file_name) throw (std::exception);
    QDomElement serializeSystem(QTableWidget* table_widget, QDomDocument& doc, 
		const QString& tag, int dimension) const throw (std::exception);
    void deserializeSystem(QTableWidget* table_widget, const QDomElement& tagSystem) throw (std::exception);
    QDomElement serializeMatrix(QTableWidget* table_widget, QDomDocument& doc, const QString& tag) const throw (std::exception);
    void deserializeMatrix(QTableWidget* table_widget, int dim, const QDomElement& tagMatrix) throw (std::exception);
    void setSolutionSelected(int sol_id);
    void unselectAllSolutions();
    void drawNewSolution(int sol_id);
    void deleteAllSolutions();
	void closeAllDrawers();
	void resetAllDrawers();
    void setVarValueByIndex(int varIndex, double varValue);

	void setDirty() const {
		dirty = true;
	}
	void unsetDirty() const {
		dirty = false;
	}
	bool isDirty() const {
		return dirty;
	}
	bool isOKToDiscardMessage();
	void resetAllLikeNew();

    ptrvector<Drawer*> drawers;
    ptrvector<Drawer3D*> drawers3D;

	class FormState {
		friend class Runge;
		int page;
        runge::SystemType systemType;

		FormState () :
			page(RUNGE_TOOLBOX_PAGE_SYSTEM),
            systemType(runge::FTX)
		{
		}
	} form_state;

	class ObjectInvalidated : private QObjectUserData {
		friend class Runge;

		bool is_invalidated;

		ObjectInvalidated(bool invalidated) :
			is_invalidated(invalidated)
		{
		}

		static ObjectInvalidated* cast(QObjectUserData* pretender)
		{
            if (pretender != NULL && typeid(ObjectInvalidated*).before(typeid(pretender))) {
				return dynamic_cast<ObjectInvalidated*>(pretender);
            }
			return NULL;
		}
	};
	
public:
	Engine* get_engine() const {
        return engine;
    }
    void get_names(std::vector<std::string>& names, bool vars, bool including_indep) const throw (std::exception);   // vars or bodies
    void get_macros_meanings(std::vector<std::string>& saMacros, std::vector<std::string>& saMacrosMeanings) const;

    runge::SystemType systemType() const {
        return form_state.systemType;
    }
    void setSystemType(runge::SystemType systemType);
    
	QColor getCurrentOrbitColor(bool odd) const {
		return odd ? currentOrbitColor : currentOrbitColor2;
	}
	void setCurrentOrbitColor(bool odd, QColor color) {
		if (odd)
			currentOrbitColor = color;
		else
			currentOrbitColor2 = color;
	}
	void setAssistantRU() {
		isAssistantRU = true;
	}

    static void setLanguageSetting(const QString& newLang);
    static QString getLanguageSetting();

private:
    void refreshAllSlots();
    void initialize_widget (QTableWidget *table_widget, const QStringList& labels, bool stretch_all = false);
	void initialize_widget (QTableWidget *table_widget, const QString& title1, const QString& title2);
    void enforce_column_double (QTableWidget *table_widget, int col);

    static void refresh_table_widget (QTableWidget* table_widget);
    void refresh_all_table_widgets ();
    void informAboutLanguageChange();
	void repaint3D() const;

	static QString default_dep_var (int pos);
    static void update_table_content (QTableWidget *table_widget);
    static void update_matrix_content (QTableWidget *matrix_widget);
    static void populate_widget_cell (QTableWidget *table_widget, int row, int col, const QString& text);
    static void populate_widget_new_row (QTableWidget *table_widget, int row);
    static void set_table_widget_font (QTableWidget *table_widget, const QFont& font);
    static void change_row_count (QTableWidget *table_widget, int new_row_count);
	static void add_row (QTableWidget *table_widget);
	static void remove_row (QTableWidget *table_widget, int row);
    static void change_column_count (QTableWidget *table_widget, int new_column_count);
	static void set_object_state (QObject *object, bool invalidated);
	static bool is_invalidated_with_reset (QObject *object);
    static void reset_table_content (QTableWidget *table_widget, bool addVar);
    static void reset_matrix_content (QTableWidget *matrix_widget);

signals:
    void signalToStart();

private slots:
    void on_actionConfiguration_triggered();
    void on_action_Font_triggered();
    void on_action_English_triggered();
    void on_action_Russian_triggered();
	void on_spinBoxDimension_valueChanged(int);
    void on_actionE_xit_triggered();
    void on_action_New_triggered();
    void on_action_Open_triggered();
    void on_action_Save_triggered();
    void on_actionSave_As_triggered();
	void on_toolBox_currentChanged(int);
    void on_radioBF_toggled(bool);
    void on_radioAF_toggled(bool);
    void on_radioFX_toggled(bool);
    void on_radioFTX_toggled(bool);
    void on_action_Start_triggered();
    void on_action_Pause_triggered();
    void on_actionS_top_triggered();
    void on_actionResume_triggered();

    void on_action_2D_Draw_triggered();
    void on_action_3D_Draw_triggered();
    void on_action_Contents_triggered();
    void on_action_About_Runge_triggered();

    void on_actionMRU1_triggered();
    void on_actionMRU2_triggered();
    void on_actionMRU3_triggered();
    void on_actionMRU4_triggered();
    void on_actionClearAllMRUs_triggered();

    void addMacroClicked();
    void removeMacroClicked();
    void tableMacrosCellClicked(int,int);
    void tableMacrosRowSelected(int);
    void tableMacrosColumnSelected(int);

    void tableFtxCellChanged(int,int);
    void tableFxCellChanged(int,int);
    void tableFiCellChanged(int,int);
    void tableUtxCellChanged(int,int);
    void tableStartValuesCellChanged(int,int);
    void tableMacrosCellChanged(int,int);
    void tableACellChanged(int,int);
    void tableBCellChanged(int,int);

	void editIndepVarChanged(const QString&);
    void editStartTChanged(const QString&);
    void editEndTChanged(const QString&);
    void editStepChanged(const QString&);
    void editStepMinChanged(const QString&);
    void editStepMaxChanged(const QString&);
    void editEpsChanged(const QString&);
    void editPChanged(const QString&);

	void solutionChanged();
    void solutionDoubleClicked(QTableWidgetItem*);

	void checkBoxShowAllPointsChanged(int);
	void removeSolution();
	void exportSolution(QAction*);
};

class LineEditDelegate : public QStyledItemDelegate
{
    Q_OBJECT
    QDoubleValidator doubleValidator;

public:
    explicit LineEditDelegate(QObject *parent=0)
        : QStyledItemDelegate(parent), doubleValidator(parent)
    {
    }

    virtual QWidget* createEditor (QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const
    {
        QWidget* widget = QStyledItemDelegate::createEditor(parent, option, index);
        if (typeid(QLineEdit*).before(typeid(widget))) {
			QLineEdit* editor = dynamic_cast<QLineEdit*>(widget);
            editor->setValidator(&doubleValidator);
            return editor;
        }
        return widget;        
    }
};


#endif // RUNGE_H
