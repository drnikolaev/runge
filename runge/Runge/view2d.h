#ifndef RUNGE_VIEW2D_H
#define RUNGE_VIEW2D_H

#include <QGraphicsView>
#include <QMouseEvent>
#include <QRubberBand>

#if defined(WIN32) && defined(_DEBUG)
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif

class Drawer;

class View2D : public QGraphicsView
{
Q_OBJECT

private:
	Drawer* drawer;
    double zoomInMult, zoomOutMult;
	QRubberBand rubberBand;
	QPoint rubberBandOrig;
	QPoint panOrig;

	void zoomToRect(const QPoint& pos1, const QPoint& pos2);

public:
    View2D(QWidget *parent = 0) 
        : QGraphicsView(parent), rubberBand(QRubberBand::Rectangle, this) {
		setMouseTracking(true);
        setZoomMult(2.0);
    }

	void setDrawer(Drawer* _drawer) {
		drawer = _drawer;
	}

    void setZoomMult(double newMult);

protected:
	virtual void mousePressEvent(QMouseEvent* event);
	virtual void mouseMoveEvent(QMouseEvent* event);
	virtual void mouseReleaseEvent(QMouseEvent* event);
	virtual void resizeEvent(QResizeEvent* event);

public slots:
    void zoomIn();
    void zoomOut();
};


#endif // !RUNGE_VIEW2D_H
