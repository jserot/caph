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

#ifndef _COMMANDLINE_H
#define _COMMANDLINE_H

#include <QString>

class CommandLine
{
public:
  QString cmd;
  QString args;

  CommandLine(QString cmd_, QString args_ = "") : cmd(cmd_), args(args_) { }
  CommandLine() : cmd(""), args("") { }
  ~CommandLine() { }

  QString toString(void);
};

#endif
