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

#ifndef _PROJECT_H
#define _PROJECT_H

#include <QDialog>
#include "option.h"
#include "command.h"

namespace Ui {
    class Project;
}

class CompOption
{
public:
  QString category;
  QString name;
  QString type;
  QString value;

  CompOption(QString category_, QString name_, QString type_, QString value_) :
    category(category_), name(name_), type(type_), value(value_) { }
  CompOption() : category(""), name(""), type(""), value("") { }
  ~CompOption() { }
};

class Project : public QDialog
{
public:

    static QString suffix;

    Project(QString name, QString dir, QStringList extraFiles); // Create
    Project(QString path);              // Open

    ~Project();

    QString descFile;
    QString name;
    QString mainFile;
    QString dir;

    QMap<QString,QString> toolPaths;
    QMap<QString,CompOption> options;
    QVector<CommandLine> preActions; 
    QVector<CommandLine> postActions;

    bool hasBeenModified;

    static QStringList viewableItems;

    void setPaths(void);
    void setOptions(void);

protected:

    void saveDesc(void);
    void readDesc(QString file);
    void createMainFile(void);
    void addFile(QString file);

};

#endif // _PROJECT_H
