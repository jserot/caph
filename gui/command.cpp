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

#include "command.h"

QString CommandLine::toString()
{
#ifdef Q_OS_WIN
  QString cmdPath = cmd.contains(' ') ? "\"" + cmd + "\"" : cmd;
#else
  QString cmdPath = cmd;
#endif
  return cmdPath + " " + args;
}

