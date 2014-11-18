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
  "\\usepackage{fullpage}",
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

  private val hline ="\\hline\n"
  private val newRow = "\\\\\n"
  private def formatValue(d: Double) = if(d < 0.999995) ("%1.5f".format(d)).drop(2) else "%1.5f".format(d)

  def hTable(parameterName: String,
             functionName: String,
             f: Double=>Double,
             range: Seq[Double]) = {
    val header = "\\begin{tabular}{r"+(" c" * range.size)+"}"
    val argFormat = "%1.1f"
    val args = range.map(argFormat.format(_)).map("$"+_+"$").mkString("&")
    val values = "$."+range.map(f).map(formatValue _).map("$"+_+"$").mkString("&").drop(1)
    val footer = "\\end{tabular}"
    (
     header+"\n"+
     hline+
     parameterName+"&"+args+newRow+
     hline+
     functionName+"&"+values+newRow+
     hline+
     footer
    )
  }

  def oneHundrethTable(parameterName: String,
                       f: Double=>Double,
                       range: Seq[Int]) = {
    val header = "\\begin{tabular}{r"+(" c" * 10)+"}"
    val footer = "\\end{tabular}"
    val topScale = parameterName + "& $."+ (0 to 9).map("$0"+_+"$").mkString("&").drop(1)
    val direction = if((range.last-range.head) < 0) -1 else 1
    def segment(i:Int)={
      val minusOfZero = if(i==0 && direction<0) "-" else "" 
      val corner = minusOfZero + i + ".0"
      val leftScale = corner +: ((1 to 9).map(_.toString))
      val table = Seq.tabulate(10,10){
      (x,y)=>
        val arg = i+direction*(y*0.1+x*0.01)
        formatValue(f(arg))
      }
      val fullTable = (leftScale zip table).map{case(item,seq)=>item +: seq}
      fullTable.map(_.mkString("&")).mkString(newRow)
    }
     val middle =range.map(segment _).mkString(newRow+newRow)
    (
     header+
     hline+
     topScale+newRow+
     hline+
     middle+newRow+
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
                      range=(-3.0 until -4.0 by -0.1))+"\n\n\\*\n\n"+
              "\\noindent\n"+
              oneHundrethTable(parameterName="t",
                               f=F _,
                               range=(0 to -2 by -1))+"\n\n\\*\n\n"+
              "\\noindent\n"+
              oneHundrethTable(parameterName="t",
                               f=F _,
                               range=(0 to 2))+"\n\n\\*\n\n"+
                              
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
