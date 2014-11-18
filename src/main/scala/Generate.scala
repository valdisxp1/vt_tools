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

  def hTable(parameterName: String,
             functionName: String,
             f: Double=>Double,
             range: Seq[Double]) = {
    val header = "\\begin{tabular}{r"+(" c" * range.size)+"}"
    val argFormat = "%1.1f"
    val args = range.map(argFormat.format(_)).map("$"+_+"$").mkString("&")
    def formatValue(d: Double) = if(d < 0.99995) ("%1.4f".format(d)).drop(2) else "%1.4f".format(d)
    val values = "$."+range.map(f).map(formatValue _).map("$"+_+"$").mkString("&").drop(1)
    val hline ="\\hline\n" 
    val footer = "\\end{tabular}"
    (
     header+"\n"+
     hline+
     parameterName+"&"+args+"\\\\\n"+
     hline+
     functionName+"&"+values+"\\\\\n"+
     hline+
     footer
    )
  }
}

object NormDistTable extends Generator {
  import Alias.F
  val defaultFileName = "normDist.tex"
  def inner = (
               header+"\n\n"+
               "\\noindent\n"+
               hTable(parameterName="t",
                      functionName="F(t)",
                      f=F _,
                      //-3.0 līdz -3.9
                      range=(-3.0 until -4.0 by -0.1))+"\n\n"+
              "\\noindent\n"+
              hTable(parameterName="t",
                      functionName="F(t)",
                      f=F _,
                      //3.0 līdz 3.9
                      range=(3.0 until 4.0 by 0.1))
              )
  val header = """
    |Normālā integrāļa funkcija 
    |(laukuma daļa zem līknes no $-\infty$ līdz $t$)
    |$\displaystyle F(t)={1\over\sqrt{2\pi}}\int\limits_{-\infty}^te^{-t^2\over2}dt$
  """.stripMargin
}
