package Audio

trait EnvSpec {
  def attackMs: Double
  def decayMs: Double
  def sustainLevel: Double
  def releaseMs: Double
}
case object NullEnvelopeSpec extends EnvSpec {
  val attackMs = 0.0
  val decayMs = 0.0
  val sustainLevel = 1.0
  val releaseMs = 0.0
}

case class EnvelopeSpec(
  attackMs: Double,
  decayMs: Double,
  sustainLevel: Double,
  releaseMs: Double
) extends EnvSpec {
  require(attackMs >= 0.0)
  require(decayMs >= 0.0)
  require(sustainLevel >= 0.0 && sustainLevel <= 1.0)
  require(releaseMs >= 0.0)
}
case class EnvelopeSpecPercent(
  attack: Double,
  decay: Double,
  sustainLevel: Double,
  release: Double
) {
  require(attack >= 0.0 && attack <= 100.0)
  require(decay >= 0.0 && decay <= 100.0)
  require(sustainLevel >= 0.0 && sustainLevel <= 1.0)
  require(release >= 0.0 && release <= 100.0)
}

case class EG(sampleRate: Int) {
  import Debug._
  def eg(sampleLen: Int, attackLen: Int, decayLen: Int, sustainLevel: Double, releaseLen: Int) = {
    val sustainLen = (sampleLen - attackLen - decayLen - releaseLen).toInt
    debug(s"sampleLen:  $sampleLen")
    debug(s"attack len: $attackLen")
    debug(s"decay len: $decayLen")
    debug(s"sustain len: $sustainLen")
    debug(s"release len: $releaseLen")
    (0 until attackLen).map(x => (x / attackLen.toDouble)).toArray ++
    (0 until decayLen).map(x => 1.0 - (x * (1.0 - sustainLevel) / decayLen.toDouble)) ++
    (0 until sustainLen).map(_ => sustainLevel) ++
    (0 until releaseLen).map(x => sustainLevel - (x * sustainLevel) / releaseLen)
  }
  def mkEg(es: EnvSpec, durationMs: Double): Array[Double] = {
    val sampleLen = durationMs * sampleRate / 1000 
    def lenSamples(lenMs: Double) =
      (sampleRate * lenMs / 1000.0).toInt
    val attackLen = lenSamples(es.attackMs)
    val decayLen = lenSamples(es.decayMs)
    val releaseLen = lenSamples(es.releaseMs)
    
     eg(sampleLen.toInt, attackLen, decayLen, es.sustainLevel, releaseLen)
  }

  def mkEg(es: EnvelopeSpecPercent, durationMs: Double): Array[Double] = {
    val sampleLen = durationMs * sampleRate / 1000
    def lenSamples(percent: Double) =
      (sampleLen * percent / 100.0).toInt
    
    val attackLen = lenSamples(es.attack)
    val decayLen = lenSamples(es.decay)
    val releaseLen = lenSamples(es.release)

    eg(sampleLen.toInt, attackLen, decayLen, es.sustainLevel, releaseLen)
  }
}

object EG {
  val eg = EG(100)
  def main(args: Array[String]): Unit = {
    doEgPercent()
    doEg()
  }
  def doEgPercent() = {
    println("Eg percent")
    val es = EnvelopeSpecPercent(10.0, 10.0, .5, 10.0)
    val egSignal = eg.mkEg(es, 1000)
    println(egSignal.mkString(", "))
  }
  def doEg() = {
    println("Eg")
    val es = EnvelopeSpec(500.0, 250.0, .7, 250.0)
    val egSignal = eg.mkEg(es, 2000)
    println(egSignal.mkString(", "))
  }
}
