#include <QPainter>
#include <QPrinter>
#include <QPaintEngine>
#include <QPaintDevice>
#include <QtDebug>

#include "orbit.h"
#include "drawer.h"
#include "lock.h"

Orbit::Orbit(Drawer* _drawer, int _sol_id, Solution& sol,  int _xindex, int _yindex, QGraphicsItem *parent) : 
	QGraphicsItem(parent), 
	drawer(_drawer), 
	sol_id(_sol_id), 
	solution(sol), 
	xindex(_xindex), 
	yindex(_yindex)
{
}

QRectF Orbit::boundingRect() const
{
    QRectF rect;
    if (xindex >= 0 && yindex >= 0) {
        rect.setRect(solution.get_min_val(xindex), - solution.get_max_val(yindex),
            solution.get_max_val(xindex) - solution.get_min_val(xindex),
            solution.get_max_val(yindex) - solution.get_min_val(yindex));
        const double lineWidth = drawer->getSolutionsWidth(false);
        if (lineWidth > 0.) {
            rect.setX(rect.x() - lineWidth);
            rect.setY(rect.y() - lineWidth);
            rect.setWidth(rect.width() + lineWidth);
            rect.setHeight(rect.height() + lineWidth);
        }
    }
    return rect;
}

void Orbit::paint(QPainter *painter, const QStyleOptionGraphicsItem*, QWidget*)
{
	const double pix_step = drawer->pixelsToScene(1) / 1.e4;
	painter->setRenderHint(QPainter::Antialiasing, true);
	unsigned int r, g, b, a;
	solution.get_color(true, &r, &g, &b, &a);
	pen.setColor(QColor(r, g, b, a));
	solution.get_color(false, &r, &g, &b, &a);
	pen2.setColor(QColor(r, g, b, a));
    pen.setCapStyle(Qt::FlatCap);
    pen2.setCapStyle(Qt::FlatCap);
	
    QPaintDevice* device = painter->paintEngine()->paintDevice();
    const double lineWidth = drawer->getSolutionsWidth(typeid(QPrinter) == typeid(*device));
    pen.setWidthF(lineWidth);
    pen2.setWidthF(lineWidth);

    const int dim = solution.get_dimension();
    const bool drawxh = xindex > dim;
    const bool drawyh = yindex > dim;
    const bool drawx = xindex > 0;
    const bool drawy = yindex > 0;
    QLineF line;
    double x1, y1, x2, y2;

    SafeLock lock(Solution::mutex());

    const std::vector<SolutionPoint>& spts = solution.get_pts();
    std::vector<SolutionPoint>::const_iterator its = spts.begin();
	bool odd = true;
    while (its != spts.end()) {
        const SolutionPoint& pt1 = *its++;
        if (its == spts.end()) {
            break;
        }
        const SolutionPoint& pt2 = *its;
        x1 = drawxh ? pt1.get_h() : (drawx ? pt1.get_x()(xindex) : pt1.get_t());
        x2 = drawxh ? pt2.get_h() : (drawx ? pt2.get_x()(xindex) : pt2.get_t());
        y1 = drawyh ? pt1.get_h() : (drawy ? pt1.get_x()(yindex) : pt1.get_t());
        y2 = drawyh ? pt2.get_h() : (drawy ? pt2.get_x()(yindex) : pt2.get_t());
		if (fabs(x1 - x2) >= pix_step || fabs(y1 - y2) >= pix_step) {
			painter->setPen(solution.is_selected() ? (odd ? pen : pen2) : pen);
			odd = !odd;
			line.setLine(x1, -y1, x2, -y2);
			painter->drawLine(line);
		}
    }
}

void Orbit::selectSolution() {
	drawer->selectSolution(sol_id);
}
