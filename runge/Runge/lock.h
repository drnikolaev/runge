#ifndef _RUNGE_LOCK_
#define _RUNGE_LOCK_

#include <QMutex>

class SafeLock
{
    QMutex& mutex;

public:
    SafeLock(QMutex& _mutex) 
		: mutex(_mutex) {
        mutex.lock();
    }
    ~SafeLock() {
        mutex.unlock();
    }

private:
	SafeLock& operator = (const SafeLock&) {
		return *this;
	}
};

#endif  //! _RUNGE_LOCK_
