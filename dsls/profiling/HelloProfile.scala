

import example.profiling._


object HelloProfileRunner extends ProfileApplicationRunner with HelloProfile


trait HelloProfile extends ProfileApplication {
  
  def main() = {
    var acc = 0.
    val time = 
      profile (100) times {
        for (i <- 0 until 100000) {
          acc += Math.exp(i)*Math.pow(i,10)*42
        }
      } report average
    println("average loop time: " + time)
  } 
  
}
