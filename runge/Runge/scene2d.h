#ifndef RUNGE_SCENE2D_H
#define RUNGE_SCENE2D_H

#include <QGraphicsScene>
#include <QGraphicsSceneMouseEvent>
#include <time.h>

#if defined(WIN32) && defined(_DEBUG)
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif

class Drawer;
class OrbitHandle;

class Scene2D : public QGraphicsScene
{
	Q_OBJECT

public:
	Scene2D(const QRectF& sceneRect, Drawer* parent = 0);
	~Scene2D() {
	}

    QRectF getAllOrbitsBoundingRect() const;

protected:
	virtual void mouseMoveEvent(QGraphicsSceneMouseEvent* mouseEvent);
	virtual void mousePressEvent(QGraphicsSceneMouseEvent* mouseEvent);
	virtual void mouseReleaseEvent(QGraphicsSceneMouseEvent* mouseEvent);

private:
	Drawer* drawer;
    OrbitHandle* selectedHandle;
};


template <typename T>
class IntRandomizer
{
    IntRandomizer () {
        srand (static_cast<unsigned int>(time(NULL)));
    }

	T _get (T nfrom, T nto) {
        return nto > nfrom ? (nfrom + static_cast<T>(rand()) % (nto - nfrom + 1)) : nfrom;
    }

public:
    ~IntRandomizer () {}

    static T get (T dFrom, T dTo) {
        static IntRandomizer r;
        return r._get(dFrom, dTo);
    }
};

#endif // RUNGE_SCENE2D_H
