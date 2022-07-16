#ifndef RUNGE_ORBITHANDLE_H
#define RUNGE_ORBITHANDLE_H

#include <QGraphicsEllipseItem>

#if defined(WIN32) && defined(_DEBUG)
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif

class Orbit;
class Drawer;

class OrbitHandle : public QGraphicsEllipseItem
{
	Orbit* orbit;
	Drawer* drawer;
	double xstart, ystart;

public:
    OrbitHandle(Orbit* _orbit, Drawer* _drawer, double _xstart, double _ystart);
    ~OrbitHandle() {
	}

    void setBrushColor();
    void setDiameter(double diameter);

	Orbit* getOrbit() const {
		return orbit;
	}

protected:
    virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
	virtual QVariant itemChange(GraphicsItemChange change, const QVariant& value);

private:
    
};

#endif // RUNGE_ORBITHANDLE_H
