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

#ifndef _app_options_h
#define _app_options_h

#include <QCheckBox>
#include <QLineEdit>

class AppOption 
{
 public:
  typedef enum { UnitOpt, StringOpt, IntOpt } Opt_type;
  QString category;
  QString name;
  Opt_type typ;
  QString desc;
  QCheckBox *checkbox;
  QLineEdit *val;
  AppOption() : category(""), name("") { }
  AppOption(QString _category, QString _name, Opt_type _typ, QString _desc);
  ~AppOption() { }
};
#endif
