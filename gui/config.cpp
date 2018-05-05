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

#include "config.h"
#include "ui_config.h"

#include <QFileDialog>
#include <QtCore>


config* config::instance = NULL;

config::config(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::config)
{
    ui->setupUi(this);
    connect(ui->compilerPathButton, SIGNAL(clicked()), this, SLOT(setCaphcPath()));
    connect(ui->dottyPathButton, SIGNAL(clicked()), this, SLOT(setDotViewerPath()));
    connect(ui->pgmViewerPathButton, SIGNAL(clicked()), this, SLOT(setPgmViewerPath()));
}

config::~config()
{
    delete ui;
}

config* config::getInstance ()
{
    if ( instance == NULL )
        instance = new config ();
    return instance;
}

QString getPathName(void)
{
    QString pathname = QFileDialog::getOpenFileName();
    if ( pathname.contains(" ") )  {
      pathname.push_front("\"");
      pathname.push_back("\"");
      }
    return pathname;
}

void config::setCaphcPath()
{
    QString path = getPathName();
    if ( ! path.isEmpty() )
      ui->compilerPathEdit->setText(path);
}

void config::setDotViewerPath()
{
    QString path = getPathName();
    if ( ! path.isEmpty() )
      ui->dottyPathEdit->setText(path);
}

void config::setPgmViewerPath()
{
    QString path = getPathName();
    if ( ! path.isEmpty() )
      ui->pgmViewerPathEdit->setText(path);
}

QString config::getPath(QString name)
{
  if ( name == "caphc" ) return ui->compilerPathEdit->text();
  else if ( name == "dotViewer" ) return ui->dottyPathEdit->text();
  else if ( name == "pgmViewer" ) return ui->pgmViewerPathEdit->text();
  else return "";
}

void config::setPath(QString name, QString val)
{
  qDebug() << "config::setPath: " << name << "=" << val;
  if ( name == "caphc" ) ui->compilerPathEdit->setText(val);
  else if ( name == "dotViewer" ) ui->dottyPathEdit->setText(val);
  else if ( name == "pgmViewer" ) ui->pgmViewerPathEdit->setText(val);
}
