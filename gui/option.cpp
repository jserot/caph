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

#include "option.h"

AppOption::AppOption(QString _category, QString _name, Opt_type _typ, QString _desc) {
  category = _category;
  name = _name;
  typ = _typ;
  desc = _desc;
  switch ( typ ) {
  case UnitOpt:
    checkbox = new QCheckBox();
    val = NULL;
    break;
  case StringOpt:
    checkbox = NULL;
    val = new QLineEdit();
    //val->setFixedSize(100,20);
  case IntOpt:
    checkbox = NULL;
    val = new QLineEdit();
    //val->setFixedSize(100,20);
    break;
  }
}
