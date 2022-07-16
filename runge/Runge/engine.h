#ifndef _RUNGE_ENGINE_
#define _RUNGE_ENGINE_

#include <QString>
#include <QThread>
#include <QMutex>
#include <QAbstractMessageHandler>
#include "runge.h"
#include "solutions.h"

#define RUNGE_STEP_NAME "step"

#if defined(WIN32) && defined(_DEBUG)
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif

void validateXML(const QString& xmlFileName, const QByteArray& xmlText, 
	const QString& xsdFileName, const QByteArray& xsdText);

class Engine : public QThread
{
	Q_OBJECT

public:
	Engine(Runge* parent);
	~Engine() {
	}

	void init (const QString& xmlConfig, const QString& schemaConfig);
	void start();
	void pause();
	void resume();
	void stop();

    const Solutions& get_solutions() const {
        return solutions;
    }
    Solutions& get_solutions() {
        return solutions;
    }

	static long long get_microseconds();

protected:
	void run();

private:
	Runge* runge;
	QMutex pauseMutex;
	QMutex stopMutex;
	bool wasStopped, wasPaused;
    QString errorMessage;
    Solutions solutions;
    int current_solution_id;
	long long lastStepTime;

signals:
	void step(int);

private slots:
	void onFinished();
	void onStarted();
	void onTerminated();
	void onStep(int);
};


class MessageHandler : public QAbstractMessageHandler
{
public:
	MessageHandler()
		: QAbstractMessageHandler(0) {
	}
	QString statusMessage() const {
		return m_description;
	}
	int line() const {
		return m_sourceLocation.line();
	}
	int column() const {
		return m_sourceLocation.column();
	}

protected:
	virtual void handleMessage(QtMsgType type, const QString &description,
		const QUrl &identifier, const QSourceLocation &sourceLocation)
	{
		Q_UNUSED(type);
		Q_UNUSED(identifier);

		m_messageType = type;
		m_description = description;
		m_sourceLocation = sourceLocation;
	}

private:
	QtMsgType m_messageType;
	QString m_description;
	QSourceLocation m_sourceLocation;
};

#endif  // _RUNGE_ENGINE_
