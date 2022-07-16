#ifndef RUNGE_PTRVECTOR_H
#define RUNGE_PTRVECTOR_H

#include <vector>

#if defined(WIN32) && defined(_DEBUG)
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif

template <typename T>
class ptrvector : public std::vector<T> {
public:
    typedef typename std::vector<T>::reverse_iterator riterator;
    typedef typename std::vector<T>::iterator iterator;

	~ptrvector() {
        riterator it = this->rbegin();
        while (it != this->rend()) {
            delete *it++;
        }
    }
};

#endif // !RUNGE_PTRVECTOR_H
