#include "orbithandle.h"
#include "orbit.h"
#include "drawer.h"

OrbitHandle::OrbitHandle(Orbit* _orbit, Drawer* _drawer, double _xstart, double _ystart)
    : QGraphicsEllipseItem(_orbit), orbit(_orbit), drawer(_drawer), xstart(_xstart), ystart(_ystart)
{
	setFlag(QGraphicsItem::ItemIsSelectable, true);
	setFlag(QGraphicsItem::ItemClipsToShape, true);
	QPen pen(Qt::NoPen);
	setPen(pen);
    setBrushColor();
    setOpacity(0.5);
}

void OrbitHandle::setBrushColor()
{
    QBrush brush(isSelected() ? Qt::red : Qt::black);
	setBrush(brush);
}

void OrbitHandle::setDiameter(double diameter)
{
	setRect(QRectF(xstart - diameter/2., - ystart - diameter/2., diameter, diameter));
}

void OrbitHandle::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	painter->setRenderHint(QPainter::Antialiasing, true);
	// we don't want to draw marquee around selected handle
	QStyleOptionGraphicsItem roption = *option;
	roption.state &= !QStyle::State_Selected;
	QGraphicsEllipseItem::paint(painter, &roption, widget);
}

QVariant OrbitHandle::itemChange(GraphicsItemChange change, const QVariant& value)
{
	if (!drawer->inRunFrom() && !drawer->inZoomToRect() && 
		change == QGraphicsItem::ItemSelectedHasChanged) {
        setBrushColor();
    }
	return QGraphicsEllipseItem::itemChange(change, value);
}
