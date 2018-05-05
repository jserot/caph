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

#include "syntax_highlighter.h"

SyntaxHighlighter::SyntaxHighlighter(QTextDocument *parent)
    : QSyntaxHighlighter(parent)
{
    HighlightingRule rule;

    rule.format.setForeground(Qt::blue);
    rule.format.setFontWeight(QFont::Bold);
    rule.pattern = QRegExp("\\btype\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bactor\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bstream\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bnet\\b"); highlightingRules.append(rule);
    rule.format.setForeground(Qt::darkGreen);
    rule.format.setFontWeight(QFont::Normal);
    rule.pattern = QRegExp("#[a-z]+\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bin\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bout\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bvar\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\brules\\b"); highlightingRules.append(rule);
    rule.format.setForeground(Qt::darkMagenta);
    rule.pattern = QRegExp("[A-Za-z]+\\s*:"); highlightingRules.append(rule);
    rule.format.setForeground(Qt::gray);
    rule.format.setFontItalic(true);
    rule.pattern = QRegExp("--[^\n]*"); highlightingRules.append(rule);
}

void SyntaxHighlighter::highlightBlock(const QString &text)
{
    foreach (const HighlightingRule &rule, highlightingRules) {
        QRegExp expression(rule.pattern);
        int index = expression.indexIn(text);
        while (index >= 0) {
            int length = expression.matchedLength();
            setFormat(index, length, rule.format);
            index = expression.indexIn(text, index + length);
        }
    }
    setCurrentBlockState(0);
}
