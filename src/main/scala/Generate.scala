object Generate extends App {
  val generators = Seq(NormDistTable,
                       SimNormDistTable,
                       ChiSquaredDistTable)
  generators.foreach(_.toFile())
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
  "\\usepackage{slashbox}",
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
        "$"+formatValue(f(arg))+"$"
      }
      val fullTable = (leftScale zip table).map{case(item,seq)=>item +:("$."+seq.head.drop(1)) +: seq.tail}
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

  def intAndDoubleTable(
                        intParameterName: String,
                        doubleParameterName: String,
                        f: (Int,Double)=>Double,
                        intRange: Seq[Int],
                        doubleRange: Seq[Double]) = {
     import java.text.NumberFormat
     val header = "\\begin{tabular}{r |"+(" c" * doubleRange.size)+"}"
     val footer = "\\end{tabular}"
     val corner = s"\\backslashbox{$intParameterName}{$doubleParameterName}"
     val doubleFormat = NumberFormat.getInstance
     doubleFormat.setMaximumFractionDigits(4)
     val topScale = (corner +: doubleRange.map(doubleFormat.format _)).mkString("&")
     val fullTable = intRange.map{i=>
	i.toString +: doubleRange.map{d=>"%1.4f".format(f(i,d))}.map("$"+_+"$")
     }
     val segments = fullTable.grouped(5)
     val middle = segments.map(_.map(_.mkString("&")).mkString(newRow)).mkString(newRow+newRow)
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
    |Normālā sadalījuma integrāļa funkcija 
    |(laukuma daļa zem līknes no $-\infty$ līdz $t$)
    |$\displaystyle F(t)={1\over\sqrt{2\pi}}\int\limits_{-\infty}^te^{-t^2\over2}dt$
  """.stripMargin
}

object SimNormDistTable extends Generator {
  import Alias.Phi
  val defaultFileName = "simNormDist.tex"
  def inner = (
               header+"\n\n"+
               "\\noindent\n"+
               oneHundrethTable(parameterName="t",
                               f=Phi _,
                               range=(0 to 2))
              )
  val header = """
    |Normālā sadalījuma integrālās funkcijas 
    |(laukuma daļa zem līknes no $-t$ līdz $+t$)\\
    |$\displaystyle \Phi(t)={1\over\sqrt{2\pi}}\int\limits_{-t}^{+t}e^{-t^2\over2}dt$
  """.stripMargin
}

object ChiSquaredDistTable extends Generator {
  import Alias.Chi
  val defaultFileName = "chiSquaredDist.tex"
  def inner = (
               header+"\n\n"+
               "\\noindent\n"+
               intAndDoubleTable(
                        intParameterName ="n",
                        doubleParameterName="Q",
                        f=Chi _,
                        intRange=(1 to 40),
                        doubleRange=Seq(0.9995,0.999,0.995,0.99,0.975,0.95,0.90))+"\n\n"+
               "\\noindent\n"+
               intAndDoubleTable(
                        intParameterName ="n",
                        doubleParameterName="Q",
                        f=Chi _,
                        intRange=(1 to 40),
                        doubleRange=Seq(0.8,0.7,0.6,0.5,0.4,0.3,0.2))+"\n\n"+

               "\\noindent\n"+
               intAndDoubleTable(
                        intParameterName ="n",
                        doubleParameterName="Q",
                        f=Chi _,
                        intRange=(1 to 40),
                        doubleRange=Seq(0.1,0.05,0.025,0.01,0.005,0.001,0.0005))

               )
  val header = "Hī kvadrāta sadalījums"
}
