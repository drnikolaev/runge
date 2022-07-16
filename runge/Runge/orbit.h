#ifndef RUNGE_ORBIT_H
#define RUNGE_ORBIT_H

#include <QGraphicsItem>
#include <QPainter>
#include <QPen>
#include "solutions.h"
#include "runge.h"

#if defined(WIN32) && defined(_DEBUG)
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif

class Drawer;

class Orbit : public QGraphicsItem
{
	Drawer* drawer;
    int sol_id;
    Solution& solution;
    int xindex, yindex;
    QPen pen;
    QPen pen2;

public:
    enum {
        Type = UserType + 1
    };
    int type() const {
        return Type;
    }

    Orbit(Drawer* _drawer, int _sol_id, Solution& sol,  int _xindex, int _yindex, QGraphicsItem *parent=0);
    ~Orbit() {
	}

    const Solution& getSolution() const {
        return solution;
    }

	void selectSolution();
    QRectF boundingRect() const;
    void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
};

#endif // RUNGE_ORBIT_H
