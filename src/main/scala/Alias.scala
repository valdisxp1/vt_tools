object Alias{
import org.apache.commons.math3.distribution._
  private val normalDistribution = new NormalDistribution
  
  def F(t: Double) = normalDistribution.cumulativeProbability(t)
  def Phi(t: Double) = normalDistribution.cumulativeProbability(-t,t)

//TODO FIX
  def Chi(degreesOfFreedom: Int, probability: Double) = new ChiSquaredDistribution(degreesOfFreedom).inverseCumulativeProbability(probability)

  def U_t_ni(t: Double, degreesOfFreedom: Int) = new TDistribution(degreesOfFreedom).cumulativeProbability(-t,t)
//TODO FIX
  def U_alpha_ni(probability: Double, degreesOfFreedom: Int) = new TDistribution(degreesOfFreedom).inverseCumulativeProbability(probability)

  def Stjudent_t_ni(t: Double, degreesOfFreedom: Int) = U_t_ni(t, degreesOfFreedom)
//TODO FIX  
def Stjudent_alpha_ni(probability: Double, degreesOfFreedom: Int) = U_alpha_ni(probability, degreesOfFreedom)
}
