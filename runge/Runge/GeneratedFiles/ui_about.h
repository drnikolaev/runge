/********************************************************************************
** Form generated from reading UI file 'about.ui'
**
** Created by: Qt User Interface Compiler version 5.2.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_ABOUT_H
#define UI_ABOUT_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QTextBrowser>

QT_BEGIN_NAMESPACE

class Ui_AboutRunge
{
public:
    QDialogButtonBox *buttonBox;
    QTextBrowser *textBrowser;

    void setupUi(QDialog *AboutRunge)
    {
        if (AboutRunge->objectName().isEmpty())
            AboutRunge->setObjectName(QStringLiteral("AboutRunge"));
        AboutRunge->resize(474, 231);
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(AboutRunge->sizePolicy().hasHeightForWidth());
        AboutRunge->setSizePolicy(sizePolicy);
        AboutRunge->setMinimumSize(QSize(474, 231));
        AboutRunge->setMaximumSize(QSize(474, 231));
        AboutRunge->setModal(true);
        buttonBox = new QDialogButtonBox(AboutRunge);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setGeometry(QRect(380, 20, 81, 91));
        buttonBox->setOrientation(Qt::Vertical);
        buttonBox->setStandardButtons(QDialogButtonBox::Ok);
        textBrowser = new QTextBrowser(AboutRunge);
        textBrowser->setObjectName(QStringLiteral("textBrowser"));
        textBrowser->setGeometry(QRect(20, 20, 341, 192));
        textBrowser->setAcceptDrops(false);
        textBrowser->setOpenExternalLinks(true);
        textBrowser->setOpenLinks(true);

        retranslateUi(AboutRunge);
        QObject::connect(buttonBox, SIGNAL(accepted()), AboutRunge, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), AboutRunge, SLOT(reject()));

        QMetaObject::connectSlotsByName(AboutRunge);
    } // setupUi

    void retranslateUi(QDialog *AboutRunge)
    {
        AboutRunge->setWindowTitle(QApplication::translate("AboutRunge", "About Runge", 0));
        textBrowser->setHtml(QApplication::translate("AboutRunge", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'MS Shell Dlg 2'; font-size:8.25pt; font-weight:400; font-style:normal;\">\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><img src=\":/Runge/Resources/32x32/graph_edge_directed.png\" /></p>\n"
"<p align=\"center\" style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-size:12pt;\"><br /></p>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:12pt;\">Runge 1.2 (November 2013)</span></p>\n"
"<p align=\"center\" style=\"-qt-paragraph-type:empty; margin-top:"
                        "0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-size:12pt;\"><br /></p>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:12pt;\">Copyright (c) Sergei Nikolaev 1992-2013</span></p>\n"
"<p align=\"center\" style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-size:12pt;\"><br /></p>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:12pt;\">License info: </span><a href=\"http://cvmlib.com/runge\"><span style=\" font-size:12pt; text-decoration: underline; color:#0000ff;\">http://cvmlib.com/runge</span></a></p></body></html>", 0));
    } // retranslateUi

};

namespace Ui {
    class AboutRunge: public Ui_AboutRunge {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_ABOUT_H
