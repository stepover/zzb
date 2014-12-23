package zzb.shell

import scala.StringBuilder
import scala.collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-13
 * Time: 下午5:15
 * Copyright baoxian.com 2012~2020
 */
class Token(val index: Int, val string: String) {

  override def toString = (if (string != null) string else "(null)") + ":" + Integer.toString(index)

  def ==(other: Token) = {

    if (if ((this.string == null)) (other.string != null) else !(this.string == other.string)) {
      false
    }
    true
  }

  override def hashCode: Int = {
    var hash = 5
    hash = 43 * hash + (if (this.string != null) this.string.hashCode else 0)
    hash
  }

}

object Token {

  def apply(index: Int, string: String) = new Token(index, string)

  /**
   * State machine input string tokenizer.
   *
   * @param input String to be tokenized
   * @return List of tokens
   *
   * @see asg.cliche.Shell.Token
   * @see asg.cliche.Shell.escapeString
   */
  def tokenize(input: String): List[Token] = {

    val result = ListBuffer[Token]()

    if (input == null) {
      return Nil
    }

    val WHITESPACE: Int = 0
    val WORD: Int = 1
    val STRINGDQ: Int = 2
    val STRINGSQ: Int = 3
    val COMMENT: Int = 4

    var state: Int = WHITESPACE
    var ch: Char = 0
    var tokenIndex: Int = -1
    var skip = false

    val token: StringBuilder = new StringBuilder("")

    for (i ← 0 until input.length) {
      if (skip) skip = false
      else {

        ch = input.charAt(i)
        state match {
          case WHITESPACE ⇒
            if (Character.isWhitespace(ch)) {
            } else if (Character.isLetterOrDigit(ch) || ch == '_') {
              state = WORD
              tokenIndex = i
              token.append(ch)
            } else if (ch == '"') {
              state = STRINGDQ
              tokenIndex = i
            } else if (ch == '\'') {
              state = STRINGSQ
              tokenIndex = i
            } else if (ch == '#') {
              state = COMMENT
            } else {
              state = WORD
              tokenIndex = i
              token.append(ch)
            }
          case WORD ⇒
            if (Character.isWhitespace(ch)) {
              result += new Token(tokenIndex, token.toString)
              token.setLength(0)
              state = WHITESPACE
            } else if (Character.isLetterOrDigit(ch) || ch == '_') {
              token.append(ch)
            } else if (ch == '"') {
              if (i < input.length - 1 && input.charAt(i + 1) == '"') {
                token.append('"')
                skip = true
              } else {
                state = STRINGDQ
              }
            } else if (ch == '\'') {
              if (i < input.length - 1 && input.charAt(i + 1) == '\'') {
                token.append('\'')
                skip = true
              } else {
                state = STRINGSQ
              }
            } else if (ch == '#') {
              result += new Token(tokenIndex, token.toString)
              token.setLength(0)
              state = COMMENT
            } else {
              token.append(ch)
            }
          case STRINGDQ ⇒
            if (ch == '"') {
              if (i < input.length - 1 && input.charAt(i + 1) == '"') {
                token.append('"')
                skip = true
              } else {
                state = WORD
              }
            } else {
              token.append(ch)
            }

          case STRINGSQ ⇒
            if (ch == '\'') {
              if (i < input.length - 1 && input.charAt(i + 1) == '\'') {
                token.append('\'')
                skip = true
              } else {
                state = WORD
              }
            } else {
              token.append(ch)
            }

          case COMMENT ⇒

          case _ ⇒
            assert(false, "Unknown state in Shell.tokenize() state machine")
        }
      }
    }

    if (state == WORD || state == STRINGDQ || state == STRINGSQ) {
      result += new Token(tokenIndex, token.toString)
    }
    result.toList
  }

  /**
   * Escape given string so that tokenize(escapeString(str)).get(0).getString === str.
   * @param input String to be escaped
   * @return escaped string
   */
  def escapeString(input: String): String = {

    val escaped: StringBuilder = new StringBuilder(input.length + 10)
    escaped.append('"')
    for (c ← input) {
      escaped.append(if (c == '"') ("\"\"") else c)
    }

    escaped.append('"')
    escaped.toString
  }

}
