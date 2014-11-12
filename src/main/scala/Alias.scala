object Alias{
import org.apache.commons.math3.distribution._
  private val normalDistribution = new NormalDistribution
  
  def F(x:Double)=normalDistribution.cumulativeProbability(x)
}
