package com.craftinginterpreters.tool

import java.io.PrintWriter

object GenerateAst extends App {

  args.length match {
    case 1 =>
      val outputDir = args.head

      defineAst(outputDir, "Expr", List(
        "Binary   : left-Expr, operator-Token, right-Expr",
        "Grouping : expression-Expr",
        "Literal  : value-Any",
        "Unary    : operator-Token, right-Expr"
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

    writer.println(" import Expr.Visitor")
    writer.println(" def accept[A](visitor : Visitor[A]) : A ")
    writer.println("}")

    writer.println("object Expr {")

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
    writer.println(s"  case class $className(${fields.replaceAll("-"," : ")}) extends $baseTrait {")

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
