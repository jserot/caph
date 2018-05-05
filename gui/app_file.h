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

#ifndef _app_file_h
#define _app_file_h

#include <QTextEdit>
#include "syntax_highlighter.h"

class AppFile {
public:
    bool upToDate;
    QString path;
    QString name;
    QTextEdit* text;
    SyntaxHighlighter *syntax;
    AppFile(QString path_, bool upToDate_, QTextEdit *edit, SyntaxHighlighter *sh)
        : upToDate(upToDate_), path(path_), text(edit), syntax(sh) {
        QFileInfo f(path);
        name = f.fileName();
    }
    ~AppFile() {
        if ( syntax != NULL ) delete syntax;
        if ( text != NULL ) delete text;
    }
};

#endif
