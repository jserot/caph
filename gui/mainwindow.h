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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QtCore>
#include <QtGui>
#include <QFileSystemModel>
#include "app_file.h"
#include "project.h"
#include "syntax_highlighter.h"

namespace Ui {
    class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);

    ~MainWindow();

    void about();

private slots:

    void openFile();
    void openProject();
    void newProject();
    void newFile();
    void saveFile();
    void saveFileAs();
    void saveAll();
    void closeCurrentFile();
    void closeProject();
    void closeAll();
    void quit();

    void selectAllText();
    void copyText();
    void cutText();
    void pasteText();

    void textHasBeenModified();

    void makeDot();
    void makeSim();
    void makeSystemC();
    void makeVHDL();
    void makeXDF();

    void setCompilerOptions();
    void setPaths();
    void setCodeFont();

    void readProcStdout();
    void readProcStderr();

    void closeInputFileTab(int index);
    void closeOutputFileTab(int index);

    void selectFile(void);

private:
    void createMenus();

    void compile(QString type, QString baseCmd, QString targetDir);
    void addFileTab(QString fname, QTabWidget *ui_tabs, QList<AppFile*>& files, bool ronly=false, bool isTemp=false);

    void openGeneratedFiles(QString type, QString path);
    void openOutputFile(QString type,QString filename, QString dir);

    void saveIndexedFile(int ind);
    void closeIndexedFile(int ind);

    void closeEvent(QCloseEvent *event);

    void setTreeView(Project *proj);

    void readInitFile(void);
    void writeOptionFile(void);

    bool alreadyOpened(QString fname);
    void keyPressed(int key);
    bool executeCmd(QString wDir, QString cmd);
    void customView(QString toolName, QString fname, QString wDir);

    void runAction(QString wDir, QVector<CommandLine>& actions);
    void runPreActions(QString wDir);
    void runPostActions(QString wDir);


private:

    Ui::MainWindow *ui;

    Project *project;  // NULL when not in project mode
    QFileSystemModel *model;  // NULL when not in project mode

    QFont codeFont;

    QString initDir;

    QList<AppFile*> inFiles;
    QList<AppFile*> outFiles;

    QProcess proc; 
 
};

#endif // MAINWINDOW_H
