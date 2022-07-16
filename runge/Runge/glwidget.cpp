#include <QtWidgets>
#include <QtOpenGL>

#include <math.h>

#include "glwidget.h"
#include "utils.h"
#include "engine.h"
#include "drawer3d.h"

#ifndef GL_MULTISAMPLE
#define GL_MULTISAMPLE  0x809D
#endif

GLWidget::GLWidget(QWidget *parent, Drawer3D* drawer)
//    : QGLWidget(QGLFormat::defaultFormat()/*QGLFormat(QGL::SampleBuffers, 4)*/, parent),
    : QGLWidget(QGLFormat(QGL::SampleBuffers, 4), parent),
	parentWidget(parent),
	parentDrawer(drawer),
	currentWidth(0), currentHeight(0), 
	side(0),
	maxPane(0.),
	xTrans(0.), yTrans(0.), xRot(0), yRot(0), zRot(0),
	qtWhite(QColor::fromRgb(255, 255, 255)),
	zoomFactor(parentDrawer->zoomSpinBox->value())
{
}

GLWidget::~GLWidget()
{
}

static void qNormalizeAngle(int &angle)
{
    while (angle < 0)
        angle += 360 * SLIDING_STEPS_PER_DEGREE;
    while (angle > 360 * SLIDING_STEPS_PER_DEGREE)
        angle -= 360 * SLIDING_STEPS_PER_DEGREE;
}

void GLWidget::setXRotation(int angle)
{
    qNormalizeAngle(angle);
    if (angle != xRot) {
        xRot = angle;
        emit xRotationChanged(angle);
		parentDrawer->xAngleSpinBox->setValue(static_cast<double>(angle) / SLIDING_STEPS_PER_DEGREED);
        updateGL();
    }
}

void GLWidget::setYRotation(int angle)
{
    qNormalizeAngle(angle);
    if (angle != yRot) {
        yRot = angle;
        emit yRotationChanged(angle);
		parentDrawer->yAngleSpinBox->setValue(static_cast<double>(angle) / SLIDING_STEPS_PER_DEGREED);
        updateGL();
    }
}

void GLWidget::setZRotation(int angle)
{
    qNormalizeAngle(angle);
    if (angle != zRot) {
        zRot = angle;
        emit zRotationChanged(angle);
		parentDrawer->zAngleSpinBox->setValue(static_cast<double>(angle) / SLIDING_STEPS_PER_DEGREED);
        updateGL();
    }
}

void GLWidget::setZoomFactor(double newZoomFactor)
{
	zoomFactor = newZoomFactor;
	resizeGL(0, 0);
	updateGL();
}

void GLWidget::initializeGL()
{
    qglClearColor(qtWhite);


//QGLFormat glf = QGLFormat::defaultFormat(); 
//glf.setSampleBuffers(true); 
//glf.setSamples(4); 
//QGLFormat::setDefaultFormat(glf); 

//if (!format().sampleBuffers()) {
//    QMessageBox::information(0, "OpenGL samplebuffers",
//                             "This system does not have sample buffer support.");
//    return;
//}

//QGLFormat glf = this ->format(); // QGLFormat::defaultFormat(); 
//qDebug("Have %d buffers and %d samples", glf.sampleBuffers(), glf.samples());

//GLint bufs; 
//GLint samples; 
//glGetIntegerv(GL_SAMPLE_BUFFERS, &bufs); 
//glGetIntegerv(GL_SAMPLES, &samples); 
//qDebug("Have %d buffers and %d samples", bufs, samples);
}

void GLWidget::paintGL()
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
	glTranslated(xTrans, 0., 0.);
	glTranslated(0., yTrans, 0.);
    glRotated((double)xRot / SLIDING_STEPS_PER_DEGREED, 1.0, 0.0, 0.0);
    glRotated((double)yRot / SLIDING_STEPS_PER_DEGREED, 0.0, 1.0, 0.0);
    glRotated((double)zRot / SLIDING_STEPS_PER_DEGREED, 0.0, 0.0, 1.0);

	glEnable(GL_MULTISAMPLE);
	glEnable (GL_LINE_SMOOTH);
	glHint (GL_LINE_SMOOTH_HINT, GL_NICEST);
	glEnable (GL_BLEND);
	glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_POLYGON_SMOOTH);
	glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
	glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
	glHint(GL_MULTISAMPLE_FILTER_HINT_NV, GL_NICEST);

	//axes
	if (parentDrawer->showAxes) {
		glLineWidth (parentDrawer->axesWidth);
		glBegin(GL_LINES);
		int r = 0, g = 0, b = 0;
		parentDrawer->xAxisColor.getRgb(&r, &g, &b);
		glColor3ub(r, g, b);
		glVertex3d(parentDrawer->xAxisFrom, 0.0, 0.0);
		glVertex3d(parentDrawer->xAxisTo, 0.0, 0.0);
		parentDrawer->yAxisColor.getRgb(&r, &g, &b);
		glColor3ub(r, g, b);
		glVertex3d(0.0, parentDrawer->yAxisFrom, 0.0);
		glVertex3d(0.0, parentDrawer->yAxisTo, 0.0);
		parentDrawer->zAxisColor.getRgb(&r, &g, &b);
		glColor3ub(r, g, b);
		glVertex3d(0.0, 0.0, parentDrawer->zAxisFrom);
		glVertex3d(0.0, 0.0, parentDrawer->zAxisTo);
		glEnd();
	}

	parentDrawer->redrawAllSolutions();
	return;

//glColor3d(0.5, 0.5, 0.5);
//glEnableClientState (GL_VERTEX_ARRAY);
//const GLdouble triVertices[] = { 
//	0.0, 1.0, 0.0, 
//	-1.0, -1.0, 0.0, 
//	1.0, -1.0, 0.0 
//};
//glVertexPointer(3, GL_DOUBLE, 0, triVertices);
////glDrawArrays(GL_TRIANGLES, 0, 3);
//glDrawArrays(GL_LINE_STRIP, 0, 3);
//glDisableClientState(GL_VERTEX_ARRAY);
//glFlush ();

}


void GLWidget::resizeGL(int w, int h)
{
	if (w > 0 && h > 0) {
		currentWidth = w;
		currentHeight = h;
	}
	const double aspectRatio = static_cast<double>(currentWidth) / static_cast<double>(currentHeight);
	const double maxPane2 = maxPane * 2.;
    side = qMin(currentWidth, currentHeight);
    glViewport(0, 0, currentWidth, currentHeight);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
	if (currentWidth > currentHeight) {
		glOrtho(-maxPane / zoomFactor * aspectRatio, maxPane / zoomFactor * aspectRatio, -maxPane / zoomFactor, maxPane / zoomFactor, -maxPane2, maxPane2);
	} else {
		glOrtho(-maxPane / zoomFactor, maxPane / zoomFactor, -maxPane / zoomFactor / aspectRatio, maxPane / zoomFactor / aspectRatio, -maxPane2, maxPane2);
	}
    glMatrixMode(GL_MODELVIEW);
}

void GLWidget::mousePressEvent(QMouseEvent *event)
{
    lastPos = event->pos();
	if (parentDrawer->panMode) {
		parentDrawer->pan(true, true);
	}
}

void GLWidget::mouseMoveEvent(QMouseEvent *event)
{
	Qt::KeyboardModifiers mods = event->modifiers();
    int dx = event->x() - lastPos.x();
    int dy = event->y() - lastPos.y();

	if (mods & Qt::ShiftModifier || parentDrawer->panHoldMode) {
		xTranslate(dx);
		yTranslate(-dy);
	} else {
		if (event->buttons() & Qt::LeftButton) {
			setXRotation(xRot + dy * (SLIDING_STEPS_PER_DEGREE/2));
			setYRotation(yRot + dx * (SLIDING_STEPS_PER_DEGREE/2));
		} else if (event->buttons() & Qt::RightButton) {
			setXRotation(xRot + dy * (SLIDING_STEPS_PER_DEGREE/2));
			setZRotation(zRot + dx * (SLIDING_STEPS_PER_DEGREE/2));
		}
	}
    lastPos = event->pos();
}

void GLWidget::mouseReleaseEvent(QMouseEvent* event)
{
	if (parentDrawer->panMode) {
		parentDrawer->pan(true, false);
	}
	QWidget::mouseReleaseEvent(event);
}

void GLWidget::xTranslate(int shift)
{
	xTrans += static_cast<double>(shift) / zoomFactor / (static_cast<double>(side) / (maxPane * 2.));
	updateGL();
}

void GLWidget::yTranslate(int shift)
{
	yTrans += static_cast<double>(shift) / zoomFactor / (static_cast<double>(side) / (maxPane * 2.));
	updateGL();
}

void GLWidget::draw(const Solution& solution, int xindex, int yindex, int zindex)
{
	const bool drawx = xindex > 0;
    const bool drawy = yindex > 0;
    const bool drawz = zindex > 0;

	parentDrawer->minXBox = std::min(parentDrawer->minXBox, solution.get_min_val(xindex));
	parentDrawer->minYBox = std::min(parentDrawer->minYBox, solution.get_min_val(yindex));
	parentDrawer->minZBox = std::min(parentDrawer->minZBox, solution.get_min_val(zindex));
	parentDrawer->maxXBox = std::max(parentDrawer->maxXBox, solution.get_max_val(xindex));
	parentDrawer->maxYBox = std::max(parentDrawer->maxYBox, solution.get_max_val(yindex));
	parentDrawer->maxZBox = std::max(parentDrawer->maxZBox, solution.get_max_val(zindex));

	double minXBox = std::max(std::fabs(parentDrawer->minXBox), std::fabs(parentDrawer->xAxisFrom));
	double minYBox = std::max(std::fabs(parentDrawer->minYBox), std::fabs(parentDrawer->yAxisFrom));
	double minZBox = std::max(std::fabs(parentDrawer->minZBox), std::fabs(parentDrawer->zAxisFrom));
	double maxXBox = std::max(std::fabs(parentDrawer->maxXBox), std::fabs(parentDrawer->xAxisTo));
	double maxYBox = std::max(std::fabs(parentDrawer->maxYBox), std::fabs(parentDrawer->yAxisTo));
	double maxZBox = std::max(std::fabs(parentDrawer->maxZBox), std::fabs(parentDrawer->zAxisTo));

	double minCorner = ::sqrt(minXBox * minXBox + minYBox * minYBox + minZBox * minZBox);
	double maxCorner = ::sqrt(maxXBox * maxXBox + maxYBox * maxYBox + maxZBox * maxZBox);
	maxPane = std::max(minCorner, maxCorner);

    const std::vector<SolutionPoint>& spts = solution.get_pts();
    std::vector<SolutionPoint>::const_iterator its = spts.begin();

	// paint handle
    if (its != spts.end()) {
        const SolutionPoint& pt = *its;
		glPointSize(parentDrawer->handleDiameter); // Points are n pixels in diameter
		glEnable(GL_POINT_SMOOTH);
		glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		if (solution.is_selected()) {
			glColor3d(1.0, 0.0, 0.0);
		} else {
			glColor3d(0.5, 0.5, 0.5);
		}
		glBegin(GL_POINTS);
        glVertex3d (pt.get_value(xindex, drawx),
					pt.get_value(yindex, drawy),
					pt.get_value(zindex, drawz));
		glEnd();
	}

	unsigned int oddr = 0, oddg = 0, oddb = 0, odda = 0;
	solution.get_color(true, &oddr, &oddg, &oddb, &odda);
	unsigned int evenr = 0, eveng = 0, evenb = 0, evena = 0;
	solution.get_color(false, &evenr, &eveng, &evenb, &evena);

	glLineWidth (parentDrawer->solutionsWidth);

	bool odd = true;
	glBegin(GL_LINE_STRIP);
	glColor3ub(oddr, oddg, oddb);
    while (its != spts.end()) {
        const SolutionPoint& pt = *its++;
		if (solution.is_selected()) {
			if (odd) {
				glColor3ub(oddr, oddg, oddb);
			} else {
				glColor3ub(evenr, eveng, evenb);
			}
		}
        glVertex3d (pt.get_value(xindex, drawx),
					pt.get_value(yindex, drawy),
					pt.get_value(zindex, drawz));
		odd = !odd;
	}
	glEnd();
}

QImage GLWidget::toImage(int w, int h)
{	
	if (w <= 0) {
		w = currentWidth;
	}
	if (h <= 0) {
		h = currentHeight;
	}
	QOpenGLFramebufferObjectFormat format;
	format.setAttachment(QOpenGLFramebufferObject::CombinedDepthStencil);
	QOpenGLFramebufferObject qfb(w, h, format);
	qfb.bind();
	// If the frame buffer does not work then return an empty image
	if(!qfb.isValid()) {
		return QImage();
	}
	resizeGL(w, h);
	// Draw the scene to the buffer
	updateGL();
	qfb.release();
	resizeGL(this->width(), this->height());
	return(qfb.toImage());
}
