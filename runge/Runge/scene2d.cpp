#include <QDebug>

#include "scene2d.h"
#include "drawer.h"
#include "orbithandle.h"
#include "orbit.h"
#include "runge.h"

Scene2D::Scene2D(const QRectF& sceneRect, Drawer* parent)
	: QGraphicsScene(sceneRect, NULL), drawer(parent), selectedHandle(NULL)
{
    setItemIndexMethod(QGraphicsScene::NoIndex);   // do it: it draws deleted orbits otherwise
}

void Scene2D::mouseMoveEvent(QGraphicsSceneMouseEvent* mouseEvent)
{
	const QPointF& pos = mouseEvent->lastScenePos();
	drawer->setCurrentPos(pos.x(), - pos.y());
	QGraphicsScene::mouseMoveEvent(mouseEvent);
}

void Scene2D::mousePressEvent(QGraphicsSceneMouseEvent* mouseEvent)
{
	if (drawer->inRunFrom() || drawer->inZoomToRect() || 
		drawer->inPanMode() || drawer->inPanHoldMode()) {
		return;
	}
    QList<OrbitHandle*> handles;
	QList<QGraphicsItem*> itemsClicked = items(mouseEvent->scenePos());
    QList<QGraphicsItem*>::const_iterator it = itemsClicked.begin();
    while (it != itemsClicked.end()) {
        QGraphicsItem* item = *it++;
        if (typeid(OrbitHandle) == typeid(*item)) {
			OrbitHandle* handle = dynamic_cast<OrbitHandle*>(item);
			if (!handle->isSelected()) {
				handles.push_back(handle);
			}
        }
    }
	const int size = handles.size();
	if (size > 0) {
		const int rnd = IntRandomizer<int>::get(0, size - 1);
		QList<OrbitHandle*>::const_iterator ith = handles.begin() + rnd;
		(*ith)->getOrbit()->selectSolution();
	} else {
		drawer->selectSolution(-1);
	}
	drawer->checkDeleteAvailability();
// don't do that to prevent false selections
//	QGraphicsScene::mousePressEvent(mouseEvent);
}

void Scene2D::mouseReleaseEvent(QGraphicsSceneMouseEvent* mouseEvent)
{
	if (drawer->inRunFrom()) {
		if (Runge::busy_computing_mutex().tryLock()) {
			Runge::busy_computing_mutex().unlock();
			drawer->runFromPos();
			return;
		}
	}
	QGraphicsScene::mouseReleaseEvent(mouseEvent);
}

QRectF Scene2D::getAllOrbitsBoundingRect() const
{
    QRectF rect;
    QList<QGraphicsItem*> allItems = items();

    QList<QGraphicsItem*>::const_iterator it = allItems.begin();
    while (it != allItems.end()) {
        QGraphicsItem* item = *it++;
        if (typeid(Orbit) == typeid(*item)) {
			Orbit* orbit = dynamic_cast<Orbit*>(item);
            QRectF itemRect = orbit->boundingRect();
            rect = rect == QRect() ? itemRect : rect.united(itemRect);
        }
    }
    return rect;
}

