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

#include "project_window.h"

#include "QFileDialog"

ProjectWindow::ProjectWindow(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::projet)
{
    ui->setupUi(this);
    connect(ui->selectRootDirButton, SIGNAL(clicked()), this, SLOT(setRootDir()));
    connect(ui->addFilesButton, SIGNAL(clicked()), this, SLOT(addFiles()));
}

ProjectWindow::~ProjectWindow()
{
  delete ui;
}

void ProjectWindow::setRootDir()
{
    QString dirname = QFileDialog::getExistingDirectory();
    if ( dirname.isEmpty() ) return;
    ui->rootDirEdit->setText(dirname);
}

void ProjectWindow::addFiles()
{
    QString filepath = QFileDialog::getOpenFileName();
    if ( filepath.isEmpty() ) return;
    ui->addedFilesText->append(filepath);
}

QString ProjectWindow::getProjName(void)
{
    return ui->projNameEdit->text();
}

QString ProjectWindow::getProjRootDir(void)
{
  return ui->rootDirEdit->text();
}

QString ProjectWindow::getProjDir(void)
{
  return getProjRootDir() + "/" + getProjName();
}

QStringList ProjectWindow::getProjFiles(void)
{
    return ui->addedFilesText->toPlainText().split("\n");
}

