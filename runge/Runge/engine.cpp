#include <QFile>
#include <QtDebug>
#include <QtXmlPatterns/QXmlSchema>
#include <QtXmlPatterns/QXmlSchemaValidator>
#include <QDomDocument>
#include <stdexcept>
#if defined (__GNUC__) && !defined(__MINGW32__)
#   include <sys/time.h>
#endif
#include "engine.h"
#include "lock.h"
#include "utils.h"

#define RUNGE_MICROSECS_PER_REDRAW 100000LL


Engine::Engine(Runge* parent)
	: QThread(parent), 
	runge(parent), 
	wasStopped(false), 
	wasPaused(false), 
	current_solution_id(-1), 
	lastStepTime(0LL)
{
	connect(this, SIGNAL(started()), this, SLOT(onStarted()));
	connect(this, SIGNAL(finished()), this, SLOT(onFinished()));
//	connect(this, SIGNAL(terminated()), this, SLOT(onTerminated()));
	connect(this, SIGNAL(step(int)), this, SLOT(onStep(int)));	
}

long long Engine::get_microseconds()
{
#if defined (__GNUC__) && !defined(__MINGW32__)
	static timeval tv;
	gettimeofday(&tv, NULL);
	return tv.tv_sec * 1000000LL + tv.tv_usec; 
#else
	static FILETIME tv;
	GetSystemTimeAsFileTime(&tv);
	return ((static_cast<long long>(tv.dwHighDateTime) << 32) + tv.dwLowDateTime) / 10LL; // 100 nanosecs intervals
#endif
}

void validateXML(const QString& xmlFileName, const QByteArray& xmlText, 
	const QString& xsdFileName, const QByteArray& xsdText)
{
	MessageHandler messageHandler;
	QXmlSchema schema;
	schema.setMessageHandler(&messageHandler);
	schema.load(xsdText);
	bool schemaError = false;
	bool xmlError = false;
	if (!schema.isValid()) {
		schemaError = true;
	} else {
		QXmlSchemaValidator validator(schema);
		if (!validator.validate(xmlText))
			xmlError = true;
	}
	if (xmlError || schemaError) {
		std::ostringstream err_stream;
		err_stream << "Failed to validate " << 
			(schemaError ? xsdFileName.toStdString() : xmlFileName.toStdString()) << 
			": " << messageHandler.statusMessage().toStdString() << std::endl <<
			"line: " << messageHandler.line() << " column: " << messageHandler.column() << std::ends;
		throw std::runtime_error(err_stream.str());
	}
}

void Engine::init(const QString& xmlConfigFile, const QString& schemaConfigFile)
{
	runge->action_Pause->setEnabled(false);
	runge->actionResume->setEnabled(false);
	runge->actionS_top->setEnabled(false);

	QFile xmlFile(xmlConfigFile);
	if (!xmlFile.open(QIODevice::ReadOnly)) {
		std::ostringstream err_stream;
        err_stream << "Failed to open configurarion file " << 
            xmlConfigFile.toStdString() << std::ends;
		throw std::runtime_error(err_stream.str());
    }
	QFile xsdFile(schemaConfigFile);
	xsdFile.open(QIODevice::ReadOnly);
	const QByteArray xmlText(xmlFile.readAll());
	const QByteArray xsdText(xsdFile.readAll());

	validateXML(xmlConfigFile, xmlText, schemaConfigFile, xsdText);
	runge::SolverRegistry::unregister_all();

	QDomDocument domDocument;
	domDocument.setContent(xmlText);
	QDomNodeList solversList = domDocument.
		firstChildElement("runge:config").
		firstChildElement("solvers").
		elementsByTagName("solver");

	for (int i = 0; i < solversList.count(); ++i) {
		QDomNode solverNode = solversList.item(i);
		QString solverType = solverNode.attributes().namedItem("type").nodeValue();
		QString solverName = solverNode.firstChildElement("name").text();
		QString solverDll = solverNode.firstChildElement("dll").text();
		QString solverRoutine = solverNode.firstChildElement("routine").text();
		int size2 = solverNode.firstChildElement("size2").text().toInt();
		int size1 = solverNode.firstChildElement("size1").text().toInt();
		int size0 = solverNode.firstChildElement("size0").text().toInt();

		runge::SolverRegistry::register_solver_routine (solverName.toStdString(), 
			runge::solverTypeByName(solverType.toStdString()),
			get_loader().get_solver (solverDll.toStdString(), solverRoutine.toStdString()),
			size2, size1, size0);
	}
}

void Engine::run()
{
	try {
		SafeLock lock(Runge::busy_computing_mutex());
		lastStepTime = get_microseconds();
        const int dimension = runge->spinBoxDimension->text().toInt();
        double t = runge->editStartT->text().toDouble();
        double h = runge->editStep->text().toDouble();
        const double tk = runge->editEndT->text().toDouble();
        const double hmin = runge->editStepMin->text().toDouble();
        const double hmax = runge->editStepMax->text().toDouble();
        const double eps = runge->editEps->text().toDouble();
        const double p = runge->editP->text().toDouble();
        const double t0 = t;
        const double span = fabs(t - tk);

        cvm::string_array saAt, saVars, saBodies, saMacros, saMacrosMeanings;
        cvm::srmatrix mB(dimension);
        cvm::rvector x(dimension);

        runge->read_rvector(runge->tableStartValues, 1, x);
        runge->get_macros_meanings(saMacros, saMacrosMeanings);
        runge->get_names(saVars, true, runge->systemType() != runge::FX);   // FX is autonomous, the rest needs t
        runge->get_names(saBodies, false, false);

		if (runge->systemType() == runge::AF) {
            runge->read_matrix (runge->tableA, saAt, false);
        }
		if (runge->systemType() == runge::BF) {
            runge->read_rmatrix (runge->tableB, mB);
        }

        const std::string solverName = runge->get_solver_selected().toStdString();
        const runge::SolverRegistry::SolverProperties& solverProperties = runge::SolverRegistry::getSolverProperties(solverName);
        const runge::SolverType solverType = solverProperties.getSolverType();

		runge::Solver* pSolver = NULL;
        if (solverType == runge::FTX_F) {
    		cvm::rfvector fv (saVars, saBodies, saMacros, saMacrosMeanings);
            pSolver = runge::SolverRegistry::createSolverFTX_F(solverName, fv);
        }
        else if (solverType == runge::AFI_FAFI) {
            cvm::rfmatrix fAt (dimension, dimension, saVars, saAt, saMacros, saMacrosMeanings);
    		cvm::rfvector fFi (saVars, saBodies, saMacros, saMacrosMeanings);
            pSolver = runge::SolverRegistry::createSolverSolverAFI_FAFI(solverName, fAt, fFi);
        }
        else if (solverType == runge::FTX_FFJ) {
    		cvm::rfvector fv (saVars, saBodies, saMacros, saMacrosMeanings);
            pSolver = runge::SolverRegistry::createSolverFTX_FFJ(solverName, fv);
        }
        else if (solverType == runge::BUTX_BFU) {
    		cvm::rfvector fu (saVars, saBodies, saMacros, saMacrosMeanings);
            pSolver = runge::SolverRegistry::createSolverBUTX_BFU(solverName, mB, fu);
        }
        else if (solverType == runge::FX_FFJ) {
    		cvm::rfvector fx (saVars, saBodies, saMacros, saMacrosMeanings);
            pSolver = runge::SolverRegistry::createSolverFX_FFJ(solverName, fx);
        }
        else if (solverType == runge::FTX_FFJFT) {
    		cvm::rfvector fx (saVars, saBodies, saMacros, saMacrosMeanings);
            pSolver = runge::SolverRegistry::createSolverFTX_FFJFT(solverName, fx);
        }
        else {
            throw std::runtime_error("Unknown solver type detected");
        }
        std::auto_ptr<runge::Solver> apSolver(pSolver);

        apSolver->set_hmin(hmin);
        apSolver->set_hmax(hmax);
        apSolver->set_eps(eps);
        apSolver->set_p(p);

        double minSp = cvm::cvmMachSp();
        h = tk < t ? -fabs(h) : fabs(h);

		while (true) {
			pauseMutex.lock();	// went in paused state
			if (!stopMutex.tryLock()) {
				pauseMutex.unlock();
				break;
			}
			stopMutex.unlock();
			pauseMutex.unlock();

            validate(x);
            solutions.add_solution_point(current_solution_id, t, x, h);

            if (fabs(tk - t) < minSp) { // reached the end t
                break;
            }
            if (fabs(tk - t) < fabs(h)) {
                h = tk < t ? -fabs(tk - t) : fabs(tk - t);
            }

			if (!apSolver->step(t, x, h)) {
                std::ostringstream os;
                os << "Solver failed to compute at t=" << t << " with h=" << h << " and x=" << std::endl;
        		for (int k = 1; k <= dimension; ++k) {
                    os << x(k) << std::endl;
                }
                os << std::ends;
                throw std::runtime_error(os.str());
			}

            step(static_cast<int>(ceil(fabs(t - t0)/span * PROGRESS_BAR_RANGE_DBL)));
		}
		solutions.set_end_time(current_solution_id, get_microseconds());
	}
	catch (std::exception& ex) {
        errorMessage = ex.what();
	}
}


void Engine::start()
{
    const int dimension = runge->spinBoxDimension->text().toInt();
    const double t = runge->editStartT->text().toDouble();
	const double eps = runge->editEps->text().toDouble();
	const double p = runge->editP->text().toDouble();
	cvm::string_array saVarsTH, saEquations, saMacros, saMacrosMeanings;
    runge->get_names(saVarsTH, true, true);
    runge->get_names(saEquations, false, false);
    runge->get_macros_meanings(saMacros, saMacrosMeanings);
    saVarsTH.push_back(RUNGE_STEP_NAME);
	QColor color = runge->getCurrentOrbitColor(true);
	QColor color2 = runge->getCurrentOrbitColor(false);
	// must be done in main thread:    
	current_solution_id = solutions.add_solution(dimension, t, eps, p, saVarsTH, saEquations, 
        saMacros, saMacrosMeanings, get_microseconds(), 
		color.red(), color.green(), color.blue(), color.alpha(),
		color2.red(), color2.green(), color2.blue(), color2.alpha());
    runge->startDrawingNewSolution(current_solution_id);
	QThread::start();
}

void Engine::stop()
{
	wasStopped = true;
	stopMutex.lock();
	if (wasPaused) {
		resume();
	}
}

void Engine::pause()
{
	wasPaused = true;
	pauseMutex.lock();
	runge->action_Pause->setEnabled(false);
	runge->actionResume->setEnabled(true);
}

void Engine::resume()
{
	wasPaused = false;
	pauseMutex.unlock();
	runge->action_Pause->setEnabled(true);
	runge->actionResume->setEnabled(false);
}

void Engine::onFinished() {
	if (wasStopped) {
		wasStopped = false;
		stopMutex.unlock();
	}
	wasPaused = false;

	runge->showProgress(false);
	runge->action_Start->setEnabled(true);
	runge->action_Pause->setEnabled(false);
	runge->actionResume->setEnabled(false);
	runge->actionS_top->setEnabled(false);
	runge->add_solution(current_solution_id);

    if (errorMessage.length() > 0) {
		runge->criticalErrorBox(errorMessage);
        errorMessage.clear();
    } 
//	else {
//        runge->add_solution(current_solution_id); fix: to show ghost solution in case of exception
//   }
}

void Engine::onStarted() {
	runge->showProgress(true);
	runge->action_Start->setEnabled(false);
	runge->action_Pause->setEnabled(true);
	runge->actionS_top->setEnabled(true);
}

void Engine::onTerminated() {
	runge->showProgress(false);
	runge->action_Start->setEnabled(true);
	runge->action_Pause->setEnabled(false);
	runge->actionResume->setEnabled(false);
	runge->actionS_top->setEnabled(false);
	wasStopped = false;
	wasPaused = false;

    if (errorMessage.length() > 0) {
		runge->criticalErrorBox(errorMessage);
        errorMessage.clear();
    }
}

void Engine::onStep(int v)
{
    long long currentTime = get_microseconds();
    if (currentTime > lastStepTime + RUNGE_MICROSECS_PER_REDRAW) {
        lastStepTime = currentTime;
		runge->redrawSolution(current_solution_id);
//		QApplication::flush();
	}
	runge->progressBar()->setValue(v);
}
