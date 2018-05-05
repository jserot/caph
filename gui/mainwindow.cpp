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

#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "options.h"
#include "command.h"
#include "project.h"
#include "project_window.h"
#include "config.h"
#include "syntax_highlighter.h"

#include <QDir>
#include <QMap>
#include <QMessageBox>
#include <QFileDialog>
#include <QFontDialog>
#include <QFileSystemModel>

#include <QtXml/qdom.h>
#include <QtXml/QDomDocument>
#include <QtXml/QDomElement>
#include <QtXml/QDomNode>

MainWindow::MainWindow(QWidget *parent) :
  QMainWindow(parent),
  ui(new Ui::MainWindow)
{
  ui->setupUi(this);

  setWindowIcon(QPixmap( ":/img/icon.png" ));
  createMenus();
  ui->inpFilesTab->setMovable(false);

  ui->inpFilesTab->removeTab(0);
  ui->inpFilesTab->removeTab(0);
  ui->outFilesTab->removeTab(0);
  ui->outFilesTab->removeTab(0);

  connect(ui->newFileButton, SIGNAL(clicked()), this, SLOT(newFile()));
  connect(ui->openFileButton, SIGNAL(clicked()), this, SLOT(openFile()));
  connect(ui->saveFileButton, SIGNAL(clicked()), this, SLOT(saveFile()));
  connect(ui->saveAllButton, SIGNAL(clicked()), this, SLOT(saveAll()));

  connect(ui->compileDotButton, SIGNAL(clicked()), this, SLOT(makeDot()));
  connect(ui->runSimButton, SIGNAL(clicked()), this, SLOT(makeSim()));
  connect(ui->compileSystemcButton, SIGNAL(clicked()), this, SLOT(makeSystemC()));
  connect(ui->compileVhdlButton, SIGNAL(clicked()), this, SLOT(makeVHDL()));
  connect(ui->compileXdfButton, SIGNAL(clicked()), this, SLOT(makeXDF()));

  connect(&proc,SIGNAL(readyReadStandardOutput ()),this,  SLOT(readProcStdout()));
  connect(&proc,SIGNAL(readyReadStandardError ()),this,  SLOT(readProcStderr()));

  connect(ui->projectTreeview, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(selectFile()));

  connect(ui->inpFilesTab, SIGNAL(tabCloseRequested(int)), this, SLOT(closeInputFileTab(int)));
  connect(ui->outFilesTab, SIGNAL(tabCloseRequested(int)), this, SLOT(closeOutputFileTab(int)));

  ui->logText->setReadOnly(true);

  project = NULL;
  model = NULL;

  codeFont.setFamily("Courier");
  codeFont.setFixedPitch(true);
  codeFont.setPointSize(11);

  readInitFile();
}

void MainWindow::about()
{
  QMessageBox::about(this, tr("About Caph"),
                     tr("<p><b>Caph</b> is a graphical front-end to the CAPH compiler." \
                        "<p>Information: <t>caph.univ-bpclermont.fr</t></p>" \
                        "<p>Contact: jocelyn.serot@uca.fr</p>"));
}

void MainWindow::readInitFile(void)
{
  QString filedir = QApplication::applicationDirPath();
  filedir.append("/caph.ini");
  QFile* iniFile = new QFile(filedir);
  if ( ! iniFile->exists() ) setPaths();
  ui->logText->append("Reading " + iniFile->fileName());
  iniFile->open(QIODevice::ReadOnly | QIODevice::Text);
  QTextStream flux(iniFile);
  while( ! flux.atEnd() ) {
      QStringList l = flux.readLine().split("=");
      if ( l.length() == 2 && l[0] != "" && l[1] != "" ) {
          QString key = l[0].trimmed();
          QString val = l[1].trimmed();
          qDebug() << "  " << key << "=" << val;
          ui->logText->append("  " + key + "=" + val);
          if ( key == "CAPHC" )
            config::getInstance()->setPath("caphc", val);
          else if ( key == "DOTVIEWER" )
            config::getInstance()->setPath("dotViewer", val);
          else if ( key == "PGMVIEWER" )
            config::getInstance()->setPath("pgmViewer", val);
          else if ( key == "INITDIR" )
            initDir = val;
        }
    }
  flux.flush();
  iniFile->close();
}

MainWindow::~MainWindow()
{
  delete ui;
}

void MainWindow::createMenus()
{
  menuBar()->clear();
  QMenu *menuFichier = menuBar()->addMenu("&File");
  QMenu *menuEdition = menuBar()->addMenu("&Edit");

  QAction *actionNouvFich = menuFichier->addAction("&New file");
  QAction *actionOuvrirFich = menuFichier->addAction("&Open file");
  QAction *actionOuvrirProjet = menuFichier->addAction("&Open project");
  QAction *actionNouvProjet = menuFichier->addAction("&New project");
  menuFichier->addSeparator();
  QAction *actionSaveFich = menuFichier->addAction("&Save");
  QAction *actionSaveFichAs = menuFichier->addAction("&Save as");
  QAction *actionSaveAll = menuFichier->addAction("&Save all");
  menuFichier->addSeparator();
  QAction *actionCloseFile = menuFichier->addAction("&Close file");
  QAction *actionCloseAll = menuFichier->addAction("&Close all");
  QAction *actionCloseProject = menuFichier->addAction("&Close Project");
  menuFichier->addSeparator();
  QAction *actionQuit = menuFichier->addAction("&Quit");
  QAction *actionCopy = menuEdition->addAction("&Copy");
  QAction *actionCut = menuEdition->addAction("&Cut");
  QAction *actionPaste = menuEdition->addAction("&Paste");
  QAction *actionSelect = menuEdition->addAction("&Select all");

  QObject::connect(actionNouvFich, SIGNAL(triggered()), this, SLOT(newFile()));
  QObject::connect(actionOuvrirFich, SIGNAL(triggered()), this, SLOT(openFile()));
  QObject::connect(actionOuvrirProjet, SIGNAL(triggered()), this, SLOT(openProject()));
  QObject::connect(actionNouvProjet, SIGNAL(triggered()), this, SLOT(newProject()));
  QObject::connect(actionSaveFich, SIGNAL(triggered()), this, SLOT(saveFile()));
  QObject::connect(actionSaveFichAs, SIGNAL(triggered()), this, SLOT(saveFileAs()));
  QObject::connect(actionSaveAll, SIGNAL(triggered()), this, SLOT(saveAll()));
  QObject::connect(actionCloseFile, SIGNAL(triggered()), this, SLOT(closeCurrentFile()));
  QObject::connect(actionCloseAll, SIGNAL(triggered()), this, SLOT(closeAll()));
  QObject::connect(actionQuit, SIGNAL(triggered()), this, SLOT(quit()));
  QObject::connect(actionCloseProject, SIGNAL(triggered()), this, SLOT(closeProject()));

  QObject::connect(actionCopy, SIGNAL(triggered()), this, SLOT(copyText()));
  QObject::connect(actionCut, SIGNAL(triggered()), this, SLOT(cutText()));
  QObject::connect(actionPaste, SIGNAL(triggered()), this, SLOT(pasteText()));
  QObject::connect(actionSelect, SIGNAL(triggered()), this, SLOT(selectAllText()));

  actionSaveFich->setShortcut(QKeySequence("Ctrl+S"));
  actionNouvFich->setShortcut(QKeySequence("Ctrl+N"));
  actionOuvrirFich->setShortcut(QKeySequence("Ctrl+O"));
  actionOuvrirProjet->setShortcut(QKeySequence("Ctrl+P"));
  actionQuit->setShortcut(QKeySequence("Ctrl+Q"));
  actionSaveAll->setShortcut(QKeySequence(Qt::CTRL + Qt::SHIFT +Qt::Key_S ));

  QString ICON_NEW(":/img/new.png");
  QString ICON_OPEN( ":/img/open.png");
  QString ICON_SAVE( ":/img/save.png");
  QString ICON_SAVEALL( ":/img/saveall.png");
  QString ICON_QUIT(":/img/exit.png");
  QString ICON_COPY(":/img/copier.png");
  QString ICON_CUT(":/img/couper.png");
  QString ICON_PASTE(":/img/coller.png");
  QString ICON_SELECT(":/img/select_all.png");
  QString ICON_COLOR(":/img/color.png");
  QString ICON_PREF(":/img/preferences.png");
  QString ICON_CLOSEPRO(":/img/closeproject.png");
  QString ICON_CLOSEF(":/img/closefile.png");

  actionNouvFich->setIcon(QIcon(ICON_NEW));
  actionOuvrirFich->setIcon(QIcon(ICON_OPEN));
  actionSaveFich->setIcon(QIcon(ICON_SAVE));
  actionSaveAll->setIcon(QIcon(ICON_SAVEALL));
  actionQuit->setIcon(QIcon(ICON_QUIT));
  actionCopy->setIcon(QIcon(ICON_COPY));
  actionCut->setIcon(QIcon(ICON_CUT));
  actionPaste->setIcon(QIcon(ICON_PASTE));
  actionSelect->setIcon(QIcon(ICON_SELECT));
  actionCloseFile->setIcon(QIcon(ICON_CLOSEF));
  actionCloseProject->setIcon(QIcon(ICON_CLOSEPRO));

  QMenu *menuConfig = menuBar()->addMenu("Configuration");
  QAction *pathConfig = menuConfig->addAction("&Compiler and tools");
  QAction *compOptions = menuConfig->addAction("&Compiler options");
  QAction *fontConfig = menuConfig->addAction("&Code font");
  QObject::connect(pathConfig,SIGNAL(triggered()),this,SLOT(setPaths()));
  QObject::connect(compOptions, SIGNAL(triggered()), this, SLOT(setCompilerOptions()));
  QObject::connect(fontConfig, SIGNAL(triggered()), this, SLOT(setCodeFont()));
}

void MainWindow::openFile()
{
  // rdy = false;
  QString filename = QFileDialog::getOpenFileName();
  if ( filename.isEmpty() ) return;
  if ( alreadyOpened(filename) ) {
      QMessageBox::warning(this, "Error:", "file:\n" + filename + "\n is already open");
      return;
    }
  QFile file(filename);
  addFileTab(filename, ui->inpFilesTab, inFiles);
  // rdy = true;
}

void MainWindow::newFile()
{
  QTemporaryFile* f = new QTemporaryFile(QDir::tempPath()+"/caphTmp_XXXXXX.cph");  // TODO : delete when saved..
  f->open();
  qDebug() << "Creating tmp file " << f->fileName();
  addFileTab(f->fileName(), ui->inpFilesTab, inFiles, false, true);
}

void MainWindow::saveFile()
{
  if ( ui->inpFilesTab->count() !=0 ) {
      int ind = ui->inpFilesTab->currentIndex();
      saveIndexedFile(ind);
      ui->runSimButton->setEnabled(false);
      this->repaint();
      //threadWait::msleep(1000);
      ui->runSimButton->setEnabled(true);
      this->repaint();
    }
}

void MainWindow::saveAll()
{
  for ( int ind=0; ind<ui->inpFilesTab->count(); ind++ ) {
      ui->inpFilesTab->setCurrentIndex(ind);
      saveIndexedFile(ind);
    }
}

void MainWindow::saveIndexedFile(int ind)
{
  QString path = inFiles[ind]->path;
  qDebug() << "Saving " << path;
  if ( path.contains("caphTmp") ) {
      path = QFileDialog::getSaveFileName(this,"Select location to save file");
      if ( path.isEmpty() ) return;
      qDebug() << "Saving as: " << path;
      //inFiles[ui->inpFilesTab->currentIndex()].upToDate = false; // ???
      inFiles[ind]->path = path;
      QFileInfo f(path);
      ui->inpFilesTab->setTabText(ind, f.fileName());
    }
  QFile save(path);
  if ( ! save.open(QFile::WriteOnly | QFile::Text) ) {
      QMessageBox::warning(this,"Error:","cannot open file:\n"+ path + " for writing");
      return;
    }
  QTextStream os(&save);
  QTextEdit* text = inFiles[ind]->text;
  os << text->toPlainText();
  save.flush();
  save.close();
  QString nouv = ui->inpFilesTab->tabText(ind);
  if ( nouv.at(nouv.size()-1)=='*' ) {
      nouv.replace("*","");
      ui->inpFilesTab->setTabText(ind,nouv);
    }
  inFiles[ui->inpFilesTab->currentIndex()]->upToDate = true;
}

void MainWindow::closeIndexedFile(int ind)
{
  if ( inFiles[ind]->upToDate == false ) {
      QMessageBox msgBox;
      msgBox.setText("File unsaved.");
      msgBox.setInformativeText("Do you want to save your changes ?");
      msgBox.setStandardButtons(QMessageBox::Save | QMessageBox::Discard);
      msgBox.setDefaultButton(QMessageBox::Save);
      switch ( msgBox.exec() ) {
        case QMessageBox::Save:
          saveIndexedFile(ind);
          break;
        case QMessageBox::Discard:
          break;
        default:
          break;
        }
    }
  delete inFiles[ind];
  inFiles.removeAt(ind);
  ui->inpFilesTab->removeTab(ind);
}

void MainWindow::closeCurrentFile()
{
  if ( ui->inpFilesTab->count() > 0 )
    closeIndexedFile(ui->inpFilesTab->currentIndex());
}

void MainWindow::closeAll()
{
  while ( ui->inpFilesTab->count() > 0 )
    closeCurrentFile();
}

void MainWindow::saveFileAs()
{
  int ind = ui->inpFilesTab->currentIndex();
  inFiles[ind]->upToDate = false;
  QString path = QFileDialog::getSaveFileName(this,"Select location to save file");
  if ( path.isEmpty() ) return;
  qDebug() << "Saving as: " << path;
  inFiles[ind]->path = path;
  saveIndexedFile(ind);
  QFileInfo f(path);
  ui->inpFilesTab->setTabText(ind, f.fileName());
  inFiles[ind]->upToDate = true;
}

void MainWindow::closeInputFileTab(int index)
{
  closeIndexedFile(index);
}

void MainWindow::closeOutputFileTab(int index)
{
  ui->outFilesTab->removeTab(index);
}

void MainWindow::keyPressed(int key)
{
  if ( ui->inpFilesTab->count() > 0 ) {
      QWidget* focused = QApplication::focusWidget();
      if( focused != 0 )
        QApplication::postEvent(focused, new QKeyEvent(QEvent::KeyPress, Qt::CTRL+key, Qt::ControlModifier ));
    }
}

void MainWindow::copyText() { keyPressed(Qt::Key_C); }
void MainWindow::cutText() { keyPressed(Qt::Key_X); }
void MainWindow::pasteText() { keyPressed(Qt::Key_V); }
void MainWindow::selectAllText() { keyPressed(Qt::Key_A); }

void MainWindow::quit()
{
  closeAll();
  close();
}

void MainWindow::closeEvent(QCloseEvent *)
{
  this->quit();
}

void MainWindow::setCompilerOptions()
{
  if ( ! Options::getInstance()->exec() ) return;
  if ( project ) project->setOptions();
}

void clearOptions(void)
{
QMap<QString,AppOption> opts = Options::getInstance()->values;
foreach ( AppOption opt, opts) {
    switch ( opt.typ ) {
      case AppOption::UnitOpt:
        opt.checkbox->setChecked(false);
        break;
      case AppOption::StringOpt:
      case AppOption::IntOpt:
        opt.val->setText("");
        break;
      }
  }
}

void MainWindow::closeProject()
{
  closeAll();
  while ( ui->outFilesTab->count() > 0 )
    ui->outFilesTab->removeTab(ui->outFilesTab->currentIndex());
  if ( project != NULL ) {
      if ( project->hasBeenModified ) {
          QMessageBox *alert = new QMessageBox;
          //alert->setWindowTitle("");
          alert->setText("Project has been modified. Do you want to save your changes?");
          alert->setStandardButtons(QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);
          alert->setDefaultButton(QMessageBox::Save);
          int ret = alert->exec();
          //alert->deleteLater();
          if (ret == QMessageBox::Cancel) return;
        }
      ui->projectTreeview->setModel(NULL);
      delete project;
      project = NULL;
    }
  ui->logText->clear();
  clearOptions();
}

QString cmdOptions(QString category)
{
  QMap<QString,AppOption> opts = Options::getInstance()->values;
  QMapIterator<QString, AppOption> i(opts);
  QString res;
  while ( i.hasNext() ) {
      i.next();
      AppOption opt = i.value();
      if ( opt.category != category ) continue;
      if ( opt.checkbox != NULL && opt.checkbox->isChecked() )
        res.append(" " + opt.name);
      else if ( opt.val != NULL && opt.val->text() != "" ) {
        // Warning: crude hacks ahead..
#ifdef Q_OS_WIN
        if ( opt.name == "-I" )
            res.append(" " + opt.name + " \"" + opt.val->text() + "\"");
        else
#endif
        if ( opt.name == "-D" ) {
          QStringList defs = opt.val->text().split(QRegExp("\\s+"), QString::SkipEmptyParts);
          foreach ( QString d, defs) res.append(" -D " + d);
          }
        else {
          res.append(" " + opt.name + " " + opt.val->text());
          }
        }
    }
  return res;
}

bool MainWindow::executeCmd(QString wDir, QString cmd)
{
  bool r;
  ui->logText->append("> " + cmd);
  proc.setWorkingDirectory(wDir);
  proc.start(cmd);
  if ( proc.error() == QProcess::FailedToStart ) r = false;
  else r = proc.waitForFinished();
  if ( r == true ) r = proc.exitStatus() == QProcess::NormalExit;
  proc.kill();
  proc.close();
  proc.terminate();
  return r;
}

void MainWindow::compile(QString type, QString baseCmd, QString targetDir)
{
  ui->logText->clear();
  if( ui->inpFilesTab->count() ==0 /*|| inProject==false*/ ) return;
  saveFile();
  QString caphc = config::getInstance()->getPath("caphc");
  if ( caphc.isNull() || caphc.isEmpty() ) caphc = "caph"; // Last chance..
  // Get actual source file
  QString srcFilePath =
      project != NULL ? project->dir+"/"+project->mainFile
                      : ui->inpFilesTab->count() > 0 ? inFiles[ui->inpFilesTab->currentIndex()]->path : "";
  if ( srcFilePath == "" ) return;
  QFileInfo f(srcFilePath);
  QDir dir = f.absoluteDir();
  if ( targetDir != "" ) dir.mkdir(targetDir);
  QString srcFile = f.fileName();
  QString wDir = dir.absolutePath();
  qDebug () << "pdir=" << (project ? project->dir : "<none>") << " wdir=" << wDir;
  QString prefix = project ? "" : "-prefix " + f.baseName();
  CommandLine cmd(caphc, prefix + baseCmd + " " + srcFile);
  runPreActions(wDir);
  if ( executeCmd(wDir, cmd.toString()) ) {
    runPostActions(wDir);
    openGeneratedFiles(type, wDir);
    }
  else
    QMessageBox::warning(this, "", "Compilation failed");
}

void MainWindow::runAction(QString wDir, QVector<CommandLine>& actions)
{
  foreach (CommandLine action, actions ) {
    if ( ! executeCmd(wDir, action.toString() ) )
      QMessageBox::warning(this, "", "Execute action failed");
    }
}

void MainWindow::runPreActions(QString wDir)
{
  if ( project != NULL ) runAction(wDir, project->preActions);
}

void MainWindow::runPostActions(QString wDir)
{
  if ( project != NULL ) runAction(wDir, project->postActions);
}

void MainWindow::makeDot()
{
  QString targetDir = project ? "-target_dir ./dot" : "";
  compile("graph", " -dot " + targetDir + cmdOptions("general") + cmdOptions("dot"), "dot");
}

void MainWindow::makeSim()
{
  QString targetDir = project ? "-target_dir ./sim" : "";
  compile("simu", " -sim " + targetDir + cmdOptions("general") + cmdOptions("simu"), "sim");
}

void MainWindow::makeSystemC()
{
  QString targetDir = project ? "-target_dir ./systemc" : "";
  compile("systemc", " -systemc " + targetDir + cmdOptions("general") + cmdOptions("systemc"), "systemc");
}

void MainWindow::makeVHDL()
{
  QString targetDir = project ? "-target_dir ./vhdl" : "";
  compile("vhdl", " -vhdl " + targetDir + cmdOptions("general") + cmdOptions("vhdl"), "vhdl");
}

void MainWindow::makeXDF()
{
  QString targetDir = project ? "-target_dir ./xdf" : "";
  compile("xdf", " -xdf " + targetDir + cmdOptions("general") + cmdOptions("xdf"), "xdf");
}

void MainWindow::readProcStdout()
{
  proc.setReadChannel(QProcess::StandardOutput);
  while (proc.canReadLine ()) {
      QString r = QString(proc.readLine()).remove('\n').remove ('\r');
      if ( ! r.isEmpty() && ! r.startsWith("** (ImageGlass") ) ui->logText->append(QString("# ")+r);
      }
}

void MainWindow::readProcStderr()
{
  proc.setReadChannel ( QProcess::StandardError );
  while (proc.canReadLine ()) {
      QString r = QString(proc.readLine()).remove('\n').remove ('\r');
      if ( ! r.isEmpty() && ! r.startsWith("** (ImageGlass") ) ui->logText->append(QString("# ")+r);
      }
}

QStringList getFileList(QString fname)
{
  QFile ifile(fname);
  QStringList res;
  if ( ! ifile.exists() ) {
      QMessageBox::warning(NULL, "", "Cannot open file " + fname);
      return res;
    }
  ifile.open(QIODevice::ReadOnly | QIODevice::Text);
  QTextStream is(&ifile);
  while( ! is.atEnd() ) {
      QString f = is.readLine(); // One file per line
      res.append(f);
    }
  ifile.close();
  return res;
}

void MainWindow::openGeneratedFiles(QString type, QString dir)
{
  if ( type != "graph" )
    while ( ui->outFilesTab->count() > 0 )
      ui->outFilesTab->removeTab(ui->outFilesTab->currentIndex());
  QStringList files = getFileList(dir+"/caph.output");
  for ( QStringList::ConstIterator file = files.begin(); file != files.end(); file++ ) {
      QFileInfo f(*file);
      if ( type != "graph" && f.suffix() == "dot" ) continue;
      openOutputFile(type, *file, dir);
    }
}

void MainWindow::customView(QString toolName, QString fname, QString wDir)
{
   QString toolPath = config::getInstance()->getPath(toolName);
   if ( ! toolPath.isNull() && ! toolPath.isEmpty() ) {
     CommandLine cmd (toolPath, fname);
     if ( ! executeCmd(wDir, cmd.toString()) ) {
         QMessageBox::warning(this, "", "Could not start " + toolName + " viewer");
         addFileTab(fname, ui->outFilesTab, outFiles, true, false);
         }
       }
   else
     QMessageBox::warning(this, "", "No path specified for " + toolName + " viewer");
}

QString changeSuffix(QString fname, QString suffix)
{
  QFileInfo f(fname);
  return f.path() + "/" + f.completeBaseName() + suffix;
}

void MainWindow::openOutputFile(QString type, QString fname, QString wDir)
{
  QFileInfo f(wDir + "/" + fname);
  QString fullName = f.canonicalFilePath();
  qDebug() << "Displaying file : " << fullName;
  QString pgmFileName = changeSuffix(fullName, ".pgm");
  QFile pgmFile(pgmFileName);
  if ( pgmFile.exists() ) // as the result of a post-command..
      customView("pgmViewer", pgmFileName, wDir);
  else if ( type=="graph" && fname.right(4)==".dot")
      customView("dotViewer", fname, wDir);
  else
    addFileTab(fullName, ui->outFilesTab, outFiles, true, false);
}

void MainWindow::addFileTab(QString fname, QTabWidget *ui_tabs, QList<AppFile*>& files, bool ronly, bool isTemp)
{
  QFile file(fname);
  QFileInfo f(fname);
  if ( ! file.open(QIODevice::ReadOnly | QIODevice::Text) ) {
      QMessageBox::warning(this,"Error:","cannot open file:\n"+fname);
      return;
    }

  QTextEdit *edit = new QTextEdit();

#ifdef Q_OS_MACOS
  edit->setFont(codeFont);
#endif

  SyntaxHighlighter* highlighter =
      f.suffix() == "cph" ? new SyntaxHighlighter(edit->document()) : NULL;

  edit->setPlainText(QString::fromUtf8(file.readAll()));
  edit->setReadOnly(ronly);
  if ( ! ronly ) QObject::connect(edit, SIGNAL(textChanged()), this, SLOT(textHasBeenModified()));
  ui_tabs->addTab(edit, isTemp ? "new" : f.fileName());
  ui_tabs->setCurrentIndex(ui_tabs->count()-1);
  AppFile* fic = new AppFile(fname, true, edit, highlighter);
  files.append(fic);
}

void MainWindow::setTreeView(Project *proj)
{
  if ( project == NULL ) return;
  if ( model != NULL ) delete model;
  model = new QFileSystemModel(this);
  model->setNameFilters(Project::viewableItems);
  model->setNameFilterDisables(false);
  model->setRootPath(project->dir);
  ui->projectTreeview->setModel(model);
  ui->projectTreeview->setRootIndex(model->index(proj->dir));
  //ui->projectTreeview->setRootIsDecorated(false);
  for ( int i=1; i<4; i++ ) ui->projectTreeview->header()->hideSection(i);
  ui->projectTreeview->setHeaderHidden(true);
  ui->projectTreeview->resizeColumnToContents(1);
  setWindowTitle("Caph:"+proj->name);
}

void MainWindow::openProject()
{
  QString selFilter = "Project files (*" + Project::suffix + ")";
  QString projFile = QFileDialog::getOpenFileName(this, "Open a Project", initDir, selFilter);
  if ( projFile.isEmpty() ) return;
  if ( project ) closeProject();
  closeAll();
  project = new Project(projFile);
  ui->logText->append("Read project " + project->descFile);
  ui->logText->append("  caphc=" + project->toolPaths["caphc"]);
  ui->logText->append("  dotViewer=" + project->toolPaths["dotViewer"]);
  ui->logText->append("  pgmViewer=" + project->toolPaths["pgmViewer"]);
  setTreeView(project);
  // Update options and paths
  config::getInstance()->setPath("caphc", project->toolPaths["caphc"]);
  config::getInstance()->setPath("dotViewer", project->toolPaths["dotViewer"]);
  config::getInstance()->setPath("pgmViewer", project->toolPaths["pgmViewer"]);
  QMap<QString,AppOption> opts = Options::getInstance()->values;
  clearOptions();
  foreach ( CompOption o, project->options) {
      if ( ! opts.contains(o.name) ) continue;
      // ui->logText->append("  option: " + o.name + "=" + o.value);
      AppOption opt = opts[o.name];
      if ( o.type == "unit" )
        opt.checkbox->setChecked(o.value == "true" ? true : false);
      else if ( o.type == "attr" ) {
        opt.val->setText(o.value);
        }
      }
  addFileTab(project->dir+"/"+project->mainFile, ui->inpFilesTab, inFiles);
}

void MainWindow::newProject()
{
  ProjectWindow w;
  if ( ! w.exec() ) return;
  if ( w.getProjName().isEmpty() ) {
      QMessageBox::warning(this,"Error","Please set project name.");
      return;
    }
  if ( w.getProjRootDir().isEmpty() ) {
      QMessageBox::warning(this,"Error","Please set project root directory.");
      return;
    }
  if ( project != NULL ) delete project;
  project = new Project(w.getProjRootDir(), w.getProjName(), w.getProjFiles());
  //setTreeView(project);
  addFileTab(project->dir+"/"+project->mainFile, ui->inpFilesTab, inFiles);
}

void writeInifile(void)
{
  QString filedir = QApplication::applicationDirPath();
  filedir.append("/caph.ini");
  QFile* fic= new QFile(filedir);
  if ( fic->exists() ) fic->remove();
  if ( fic->open(QIODevice::WriteOnly | QIODevice::Text) == false ) return;
  QTextStream flux(fic);
  flux.setCodec("UTF-8");
  flux << "CAPHC=" << config::getInstance()->getPath("caphc") << endl;
  flux << "DOTVIEWER=" << config::getInstance()->getPath("dotViewer") << endl;
  flux << "PGMVIEWER=" << config::getInstance()->getPath("pgmViewer") << endl;
  flux.flush();
  fic->close();
}

void MainWindow::setPaths()
{
  if ( ! config::getInstance()->exec() ) return;
  if ( project )
    project->setPaths();
  else
    writeInifile();
}

bool MainWindow::alreadyOpened(QString path)
{
  for ( int i=0; i<inFiles.length(); i++ )
    if ( inFiles[i]->path == path ) {
        return true;
      }
  return false;
}

void MainWindow::selectFile(void)
{
  if ( project == NULL ) return;

  QItemSelectionModel *selection = ui->projectTreeview->selectionModel();
  QModelIndex index = selection->currentIndex();

  QFileInfo f = model->fileInfo(index);
  QString filePath = f.absoluteFilePath();

  QFile file(filePath);
  if ( ! file.open(QIODevice::ReadOnly | QIODevice::Text) ) {
      QMessageBox::warning(this,"Error:","cannot open file:\n" + filePath);
      return;
    }

  if ( f.suffix() == "pgm" )
    customView("pgmViewer", filePath, f.path());
  else if ( f.suffix() =="dot")
    customView("dotViewer", filePath, f.path());
  else {
    if ( alreadyOpened(filePath) ) return;
    addFileTab(filePath, ui->inpFilesTab, inFiles);
    }
}

void MainWindow::textHasBeenModified()
{
  int ind = ui->inpFilesTab->currentIndex();
  QString nouv = ui->inpFilesTab->tabText(ind);
  if ( nouv.at(nouv.size()-1)!='*' ) {
      nouv += "*";
      ui->inpFilesTab->setTabText(ind,nouv);
    }
  inFiles[ind]->upToDate = false;
}

void MainWindow::setCodeFont()
{
  bool ok;
  int i;
  QFont font = QFontDialog::getFont(&ok, QFont("Courier", 10), this);
  if ( ok ) {
      for ( i=0; i<inFiles.length(); i++ )
        inFiles[i]->text->setFont(font);
      for ( i=0; i<outFiles.length(); i++ )
        outFiles[i]->text->setFont(font);
      codeFont = font;
    }
}
