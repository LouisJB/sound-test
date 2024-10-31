package Audio

case class EnvelopeSpec(
  attackMs: Double,
  decayMs: Double,
  sustainLevel: Double,
  releaseMs: Double
) {
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
  def eg(sampleLen: Int, attackLen: Int, decayLen: Int, sustainLevel: Double, releaseLen: Int) = {
    val sustainLen = (sampleLen - attackLen - decayLen - releaseLen).toInt
    println(s"sampleLen:  $sampleLen")
    println(s"attack len: $attackLen")
    println(s"decay len: $decayLen")
    println(s"sustain len: $sustainLen")
    println(s"release len: $releaseLen")
    (0 until attackLen).map(x => (x / attackLen.toDouble)).toArray ++
    (0 until decayLen).map(x => 1.0 - (x * sustainLevel / decayLen.toDouble)) ++
    (0 until sustainLen).map(_ => sustainLevel) ++
    (0 until releaseLen).map(x => sustainLevel - (x * (1.0 - sustainLevel) / releaseLen))
  }
  def mkEg(es: EnvelopeSpec, durationMs: Double): Array[Double] = {
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
  def main(args: Array[String]): Unit = {
    val es = EnvelopeSpecPercent(10.0, 10.0, .5, 10.0)
    val eg = EG(100)
    val egSignal = eg.mkEg(es, 1000)
    println(egSignal.mkString)
  }
}
