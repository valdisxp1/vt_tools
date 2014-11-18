object Generate extends App {
  NormDistTable.toFile()
}

object GenerateAll extends Generator{
  val defaultFileName = "table.tex"
  def inner = ""
}

sealed trait Generator {
  def apply = toFile()
  val begin = 
  Seq("\\documentclass[12pt]{article}",
  "\\usepackage{mathtools}",
  "\\begin{document}").mkString("\n")
  val end = "\\end{document}"

  def inner: String
  def generate = begin +"\n"+ inner +"\n" + end
  def defaultFileName: String
  def toFile(fileName: String = defaultFileName) = {
    import java.io.PrintWriter
    val pw = new PrintWriter(fileName)
    pw.println(generate)
    pw.close
  }
}

object NormDistTable extends Generator {
  val defaultFileName = "normDist"
  def inner = ""
}
