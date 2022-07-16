#include <QScrollBar>
#include <QDebug>

#include "utils.h"
#include "view2d.h"
#include "drawer.h"

void View2D::setZoomMult(double newMult)
{
    if (newMult > 0.) {
        zoomInMult = newMult;
        zoomOutMult = 1. / newMult;
    }
}

void View2D::zoomIn() 
{
    AutoWaitCursor waitCursor;
    scale(zoomInMult, zoomInMult);
    drawer->rescaleHandlesAxesMarks(true);
}

void View2D::zoomOut()
{
    AutoWaitCursor waitCursor;
    scale(zoomOutMult, zoomOutMult);
    drawer->rescaleHandlesAxesMarks(true);
}

void View2D::mousePressEvent(QMouseEvent* event)
{
	if (drawer->inPanMode()) {
		drawer->pan(true, true);
		panOrig = event->pos();
	}
	if (drawer->inZoomToRect()) {
		rubberBandOrig = event->pos();
		rubberBand.setGeometry(QRect(rubberBandOrig, QSize()));
		rubberBand.show();
	}
	QGraphicsView::mousePressEvent(event);
}

void View2D::mouseMoveEvent(QMouseEvent* event)
{
	if (drawer->inPanHoldMode()) {
		QPoint panCurrent = event->pos();
		QScrollBar* hBar = horizontalScrollBar();
		QScrollBar* vBar = verticalScrollBar();
		hBar->setValue(hBar->value() + panOrig.x() - panCurrent.x());
		vBar->setValue(vBar->value() + panOrig.y() - panCurrent.y());
		panOrig = panCurrent;
	}
	if (drawer->inZoomToRect()) {
		rubberBand.setGeometry(QRect(rubberBandOrig, event->pos()).normalized());
	}
	QGraphicsView::mouseMoveEvent(event);
}

void View2D::mouseReleaseEvent(QMouseEvent* event)
{
	if (drawer->inPanMode()) {
		drawer->pan(true, false);
	}
	if (drawer->inZoomToRect()) {
	    rubberBand.hide();
//		drawer->zoomToRect(true);
		zoomToRect(rubberBandOrig, event->pos());
	}
	QGraphicsView::mouseReleaseEvent(event);
}

void View2D::resizeEvent(QResizeEvent* event)
{
	QGraphicsView::resizeEvent(event);
	drawer->checkPanningAvailability();
}

void View2D::zoomToRect(const QPoint& pos1, const QPoint& pos2)
{
	int minx = std::min<int>(pos1.x(), pos2.x());
	int maxx = std::max<int>(pos1.x(), pos2.x());
	int miny = std::min<int>(pos1.y(), pos2.y());
	int maxy = std::max<int>(pos1.y(), pos2.y());
	fitInView(mapToScene(QRect(minx, miny, maxx - minx, maxy - miny)).boundingRect(), Qt::KeepAspectRatio);
	drawer->rescaleHandlesAxesMarks(true);
}
