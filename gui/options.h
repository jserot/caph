/***************************************************************************************/
/*                                                                                     */
/*                This file is part of the CAPH Compiler distribution                  */
/*                            http://caph.univ-bpclermont.fr                           */
/*                                                                                     */
/*                                  Jocelyn SEROT                                      */
/*                         Jocelyn.Serot@univ-bpclermont.fr                            */
/*                                                                                     */
/*         Copyright 2011-2018 Jocelyn SEROT.  All rights reserved.                    */
/*  This file is distributed under the terms of the GNU Library General Public License */
/*      with the special exception on linking described in file ../LICENSE.            */
/*                                                                                     */
/***************************************************************************************/

#ifndef TABDIALOG_H
#define TABDIALOG_H

#include <QDialog>
#include <QFile>
#include <QMap>
#include <QTabWidget>
#include <QFormLayout>
#include <QLabel>
#include <QCheckBox>
#include <QTabWidget>
#include <QScrollArea>
#include <QDialogButtonBox>
#include "option.h"

class Options: public QDialog {
    Q_OBJECT

public:
    static Options* getInstance(void);

    QMap<QString,AppOption> values;

    QSize sizeHint() const { return QSize(900, 600); }

private:
        QTabWidget* tabs;
        QDialogButtonBox *buttonBox;

        static Options* instance;
        explicit Options(QWidget *parent = 0);

        ~Options();

  void addTab(QString title, QString category);
};

#endif
