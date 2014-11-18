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
  "\\usepackage{polyglossia}",
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
  val defaultFileName = "normDist.tex"
  def inner = header
  val header = """
    |Normālā integrāļa funkcija 
    |(laukuma daļa zem līknes no $-\infty$ līdz $t$)
    |$\displaystyle F(t)={1\over\sqrt{2\pi}}\int\limits_{-\infty}^te^{-t^2\over2}dt$
  """.stripMargin
}
