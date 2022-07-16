#ifndef GLWIDGET_H
#define GLWIDGET_H

#include <QGLWidget>

class Solution;
class Drawer3D;

class GLWidget : public QGLWidget
{
    Q_OBJECT

public:
    GLWidget(QWidget *parent, Drawer3D* drawer);
    ~GLWidget();

	void draw(const Solution&, int xindex, int yindex, int zindex);
	void setZoomFactor(double newZoomFactor);
	QImage toImage(int w, int h);

	int width() const {
		return currentWidth;
	}
	int height() const {
		return currentHeight;
	}

private:
	QWidget* parentWidget;
	Drawer3D* parentDrawer;

    void xTranslate(int shift);
    void yTranslate(int shift);

public slots:
    void setXRotation(int angle);
    void setYRotation(int angle);
    void setZRotation(int angle);

signals:
    void xRotationChanged(int angle);
    void yRotationChanged(int angle);
    void zRotationChanged(int angle);

protected:
    virtual void initializeGL();
    virtual void paintGL();
    virtual void resizeGL(int width, int height);
    virtual void mousePressEvent(QMouseEvent *event);
    virtual void mouseMoveEvent(QMouseEvent *event);
	virtual void mouseReleaseEvent(QMouseEvent* event);

private:
	int currentWidth;
	int currentHeight;
	int side;
	double maxPane; 
	double xTrans;
	double yTrans;

	int xRot;
    int yRot;
    int zRot;
    QPoint lastPos;
    QColor qtWhite;

	double zoomFactor;
};

#endif
