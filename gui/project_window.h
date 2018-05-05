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

#ifndef _PROJECT_WIN_H
#define _PROJECT_WIN_H

#include <QDialog>
#include "ui_projet.h"

namespace Ui {
    class ProjectWindow;
}

class ProjectWindow : public QDialog
{
    Q_OBJECT

public:

    explicit ProjectWindow(QWidget *parent = 0);

    ~ProjectWindow();

    QString getProjName(void);
    QString getProjRootDir(void);
    QString getProjDir(void);
    QStringList getProjFiles(void);

private slots:

    void setRootDir();
    void addFiles();

private:
    Ui::projet *ui;
};

#endif // _PROJECT_WIN_H
