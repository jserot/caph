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

#ifndef _config_h
#define _config_h

#include <QDialog>
#include "ui_config.h"

namespace Ui {
    class config;
}

class config : public QDialog, public Ui_config
{
    Q_OBJECT

public:

    static config* getInstance(void);

    QString getPath(QString name);
    void setPath(QString name, QString val);

private slots:

    void setCaphcPath();
    void setDotViewerPath();
    void setPgmViewerPath();

private:

    Ui::config *ui;

    explicit config(QWidget *parent = 0);

    ~config();

    static config* instance;
};

#endif // CONFIG_H
