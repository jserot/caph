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

#include "project.h"
#include <QtCore>

#include <QtXml/qdom.h>
#include <QtXml/QDomDocument>
#include <QtXml/QDomElement>
#include <QtXml/QDomNode>

#include "config.h"
#include "options.h"

QStringList Project::viewableItems =
     QStringList()
   << "*.cph" 
   << "dot" << "*.dot"
   << "sim" << "*.txt" << "*.pgm"
   << "systemc" << "*.h" << "*.cpp"
   << "vhdl" << "*.vhd"
   << "xdf" << "*.xdf" << "*.cal";

QString Project::suffix = ".cphpro";

Project::Project(QString rdir, QString pname, QStringList pfiles) // Create
{
  name = pname;
  dir = rdir + "/" + pname;
  descFile = dir + "/" + name + Project::suffix;
  QFile testDir(dir);
  if ( ! testDir.exists() ) {
      qDebug() << "Creating directory " << dir;
      QDir d;
      d.mkdir(dir);
      }
  mainFile = "main.cph";
  QFile testFile(mainFile);
  if ( ! testFile.exists() ) {
      qDebug() << "Creating file " << mainFile;
      createMainFile();
      }
  foreach ( QString f, pfiles ) {
      addFile(f);
      }
  saveDesc();
  qDebug() << "Created project: name=" << name << " dir=" << dir << " desc=" << descFile << " mainFile=" << mainFile;
  hasBeenModified = false;
}

Project::Project(QString path) // Open
{
  readDesc(path);
  qDebug() << "Read project: name=" << name << " dir=" << dir << " desc=" << descFile << " mainFile=" << mainFile;
  foreach ( QString name, toolPaths.keys())  qDebug() << "  " << name << "=" << toolPaths.value(name);
  foreach ( CompOption opt, options.values())
     qDebug() << "  " << opt.name << " " << opt.category << " " << opt.type << " " << opt.value;
//  foreach ( QString cmd, preActions) qDebug() << "  pre: " << cmd;
//  foreach ( QString cmd, postActions) qDebug() << "  post: " << cmd;
  hasBeenModified = false;
}

void addXmlPath(QDomDocument& doc, QDomElement& root, QString name, QString value)
{
  QDomElement o = doc.createElement("path");
  o.setAttribute("name", name);
  o.setAttribute("path", value);
  root.appendChild(o);
}

void addXmlOption(QDomDocument& doc, QDomElement& root, QString name, QString category, QString type, QString value)
{
  QDomElement o = doc.createElement("option");
  o.setAttribute("name", name);
  o.setAttribute("category", category);
  o.setAttribute("type", type);
  o.setAttribute("value", value);
  root.appendChild(o);
}

void Project::saveDesc(void)
{
  QDomDocument doc;
  QDomNode node = doc.createProcessingInstruction("xml","version=\"1.0\" encoding=\"UTF-8\"");
  doc.insertBefore(node, doc.firstChild());
  QDomElement root = doc.createElement("project"); 
  root.setAttribute("name", name); 
  // root.setAttribute("dir", dir);
  root.setAttribute("mainfile", mainFile);
  doc.appendChild(root);
  // Save toolpaths
  QDomElement paths = doc.createElement("tools");
  foreach ( QString name, toolPaths.keys()) {
      addXmlPath(doc, paths, name, toolPaths.value(name));
      }
  root.appendChild(paths);
  // Save options
  QDomElement opts = doc.createElement("options");
  foreach ( CompOption opt, options.values()) {
      addXmlOption(doc, opts, opt.name, opt.category, opt.type, opt.value);
     }
  root.appendChild(opts);
  QFile f(descFile);
  f.open(QIODevice::WriteOnly);
  QTextStream ts(&f);
  doc.save(ts, 2);
}

void Project::readDesc(QString file)
{
 QFile f(file);
 if ( ! f.exists() ) return;
 qDebug() << "Reading project description file " << f.fileName();
 QDomDocument doc;
 doc.setContent(&f);
 QDomElement root = doc.documentElement();
 if ( root.nodeName() != "project" ) return;
 descFile = file;
 name = root.attribute("name");
 mainFile = root.attribute("mainfile");
 QFileInfo fi(file);
 dir = fi.absolutePath();
 for ( QDomNode node = root.firstChild(); !node.isNull(); node = node.nextSibling() ) {
     QDomElement e = node.toElement();
     if ( e.nodeName() == "tools" ) {
       for ( QDomNode n = node.firstChild(); !n.isNull(); n = n.nextSibling() ) {
         QDomElement e = n.toElement();
         // qDebug() << e.nodeName() << ": " << e.attribute("name") << " " << e.attribute("path");
         toolPaths.insert(e.attribute("name"), e.attribute("path"));
         }
       }
     else if ( e.nodeName() == "options" ) {
       options.clear();
       for ( QDomNode n = node.firstChild(); !n.isNull(); n = n.nextSibling() ) {
         QDomElement e = n.toElement();
         QString name = e.attribute("name");
         QString category = e.attribute("category");
         QString type = e.attribute("type");
         QString key = category + ":" + name;
         QString value =
             name == "-D" && options.contains(key) ?
               options[key].value + " " + e.attribute("value")
             : e.attribute("value");
         //qDebug() << key << ": " << category << " " << "type" << " " << name << " " << value;
         options.insert(key, CompOption(category, name, type, value));
         }
       }
     else if ( e.nodeName() == "actions" ) {
       preActions.clear();
       postActions.clear();
       for ( QDomNode n = node.firstChild(); !n.isNull(); n = n.nextSibling() ) {
         QDomElement e = n.toElement();
         if ( e.attribute("type") == "pre" ) preActions.append(CommandLine(e.attribute("command"), e.attribute("args")));
         else if ( e.attribute("type") == "post" ) postActions.append(CommandLine(e.attribute("command"), e.attribute("args")));
         }
       }
   }
 f.close();
}

void Project::createMainFile(void)
{
  QFile f(mainFile);
  if ( f.open(QFile::WriteOnly) ) {
    QTextStream out(&f);
    out << "-- Main file of project " << name << "\n";
    f.close();
  }
}

void Project::addFile(QString f)
{
    if ( f.isNull() || f.isEmpty() ) return;
    QFile src(f);
    QFileInfo i(f);
    qDebug() << "Copying " << f << " to " << dir + "/" + i.fileName();
    src.copy(dir + "/" + i.fileName());
}

void Project::setPaths(void)
{
  toolPaths["caphc"] = config::getInstance()->getPath("caphc");
  toolPaths["dotViewer"] = config::getInstance()->getPath("dotViewer");
  toolPaths["pgmViewer"] = config::getInstance()->getPath("pgmViewer");
  hasBeenModified = true;
}

//QString stringOfOptType(AppOption::Opt_type t)
//{
//  switch ( t ) {
//    case AppOption::UnitOpt: return "unit";
//    case AppOption::StringOpt: return "attr";
//    case AppOption::IntOpt: return "attr";
//    }
//}

void Project::setOptions(void)
{
  // Copy options to the project
  qDebug() << "Updating project options";
  foreach ( AppOption opt, Options::getInstance()->values ) {
      switch ( opt.typ ) {
        case AppOption::UnitOpt:
          if ( opt.checkbox->isChecked() )
            options.insert(opt.name, CompOption(opt.category, opt.name, "unit", "true"));
          break;
        case AppOption::StringOpt:
        case AppOption::IntOpt:
          if ( opt.val  && ! opt.val->text().isEmpty() )
            options.insert(opt.name, CompOption(opt.category, opt.name, "attr", opt.val->text()));
          break;
        }
    }
  hasBeenModified = true;
}

Project::~Project()
{
  qDebug() << "Deleting project " << name;
  if ( hasBeenModified ) {
      qDebug() << "Saving project description file " << descFile;
      saveDesc();
      }
}
