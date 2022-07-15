package com.craftinginterpreters.tool

import java.io.PrintWriter
import javax.xml.crypto.Data

object GenerateAst extends App {

  args.length match {
    case 1 =>
      val outputDir = args.head

      defineAst(outputDir, "Expr", List(
        "Assign   : name-Token, value-Expr",
        "Binary   : left-Expr, operator-Token, right-Expr",
        "Grouping : expression-Expr",
        "Literal  : value-Any",
        "Logic    : left-Expr, operator-Token, right-Expr",
        "Unary    : operator-Token, right-Expr",
        "Call     : callee-Expr, paren-Token , arguments-List[Expr] ",
        "Get      : callee-Expr, name-Token",
        "Set      : obj-Expr, name-Token, value-Expr",
        "This     : keyword-Token",
        "Variable : name-Token"
      ))

        // types of statement
      defineAst(outputDir, "Stmt",List(
        "If         : condition-Expr, thenBranch-Stmt, elseBranch-Option[Stmt]",
        "Block      : statements-List[Stmt]",
        "Function   : name-Token, params-List[Token] , body-List[Stmt] ",
        "Expression : expression-Expr",
        "Print      : expression-Expr",
        "While      : expression-Expr, statement-Stmt",
        "Return     : keyword-Token, value-Option[Expr]",
        "Class      : name-Token, methods-List[Stmt.Function]",
        "Var        : name-Token, initializer-Expr"
      ))
    case _ =>
      println("Usage: generate_ast <output directory>")
      System.exit(64)
  }


  def defineAst(outputDir: String, baseTrait: String, classAndFields: List[String]) = {

    val path  = s"$outputDir/$baseTrait.scala"
    val writer = new PrintWriter(path,"UTF-8")

    writer.println("package com.craftinginterpreters.lox")
    writer.println()
    writer.println(s"trait $baseTrait {")

    writer.println(s" import $baseTrait.Visitor")
    writer.println(" def accept[A](visitor : Visitor[A]) : A ")
    writer.println("}")

    writer.println(s"object $baseTrait {")

    // define visitor
    defineVisitor(writer,baseTrait,classAndFields)


    classAndFields.foreach {classAndField =>
      val className = classAndField.split(":").head.trim
      val fields = classAndField.split(":")(1).trim
      defineType(writer,baseTrait,className,fields)
    }


    writer.println("}")
    writer.close()
  }

  def defineType(writer: PrintWriter, baseTrait: String, className: String, fields: String) = {
    writer.println(s"  final case class $className(${fields.replaceAll("-"," : ")}) extends $baseTrait {")

    writer.println(s"    override def accept[A](visitor : Visitor[A]): A = visitor.visit${className}${baseTrait}(this)")

    writer.println(" }")
  }

  def defineVisitor(writer: PrintWriter, baseTrait : String, classAndFields : List[String]) = {
    writer.println(" trait Visitor[A] { ")
    classAndFields.foreach {classAndField =>
      val typeName = classAndField.split(":").head.trim
      writer.println(s"  def visit${typeName}${baseTrait}(${baseTrait.toLowerCase}: $typeName) : A")
    }

    writer.println("  }")
  }



}
