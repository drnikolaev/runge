#include "colordialog.h"

void ColorDialog::accept()
{ 
	mcolor = QColorDialog::currentColor();
	QColorDialog::accept();
}

