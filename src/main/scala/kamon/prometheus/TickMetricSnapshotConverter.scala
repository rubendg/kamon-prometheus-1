package kamon.prometheus

import scala.collection.mutable.ListBuffer
import scala.collection.concurrent.TrieMap
import org.HdrHistogram.AtomicHistogram
import kamon.Kamon
import kamon.metric.MeasurementUnit.{Dimension, Magnitude}
import kamon.metric.SubscriptionsDispatcher.TickMetricSnapshot
import kamon.metric.instrument._
import kamon.metric._

object TickMetricSnapshotConverter extends ((TickMetricSnapshot) => TickSnapshot) {

  private def convertUnitOfMeasurement(unitOfMeasurement: UnitOfMeasurement): MeasurementUnit =
    unitOfMeasurement match {
      case x: Time =>
        x.label match {
          case "n" => MeasurementUnit.time.nanoseconds
          case "µs" => MeasurementUnit.time.microseconds
          case "ms" => MeasurementUnit.time.milliseconds
          case "s" => MeasurementUnit.time.seconds
        }
      case x: Memory =>
        x.label match {
          case "b" => MeasurementUnit.information.bytes
          case "Kb" => MeasurementUnit.information.kilobytes
          case "Mb" => MeasurementUnit.information.megabytes
          case "Gb" => MeasurementUnit.information.gigabytes
        }
      case _ => MeasurementUnit.none
    }

  private[prometheus] def convertMeasurementUnit(measurementUnit: MeasurementUnit): UnitOfMeasurement =
    measurementUnit match {
      case MeasurementUnit(Dimension.Time, Magnitude("seconds", 1D)) => Time.Seconds
      case MeasurementUnit(Dimension.Time, Magnitude("milliseconds", 1e-3)) => Time.Milliseconds
      case MeasurementUnit(Dimension.Time, Magnitude("microseconds", 1e-6)) => Time.Microseconds
      case MeasurementUnit(Dimension.Time, Magnitude("nanoseconds", 1e-9)) => Time.Nanoseconds
      case MeasurementUnit(Dimension.Information, Magnitude("byte", 1)) => Memory.Bytes
      case MeasurementUnit(Dimension.Information, Magnitude("kilobytes", 1024)) => Memory.KiloBytes
      case MeasurementUnit(Dimension.Information, Magnitude("megabytes", 1048576)) => Memory.MegaBytes
      case MeasurementUnit(Dimension.Information, Magnitude("gigabytes", 1073741824)) => Memory.GigaBytes
      case MeasurementUnit(Dimension.None, Magnitude("none", 1D)) => UnitOfMeasurement.Unknown
      case _ => UnitOfMeasurement.Unknown
    }

  private[prometheus] def toDistribution(snapshot: Histogram.Snapshot): Distribution = new Distribution {
    override def max: Long = snapshot.max

    override def buckets: Seq[Bucket] = {
      snapshot.recordsIterator.map { record =>
        new Bucket {
          override def frequency: Long = record.count
          override def value: Long = record.level
        }
      }.toSeq
    }

    override def count: Long = snapshot.numberOfMeasurements

    override def bucketsIterator: Iterator[Bucket] = buckets.toIterator

    override def sum: Long = snapshot.sum

    override def percentilesIterator: Iterator[Percentile] = throw new NotImplementedError()
    override def percentile(p: Double): Percentile = throw new NotImplementedError()
    override def percentiles: Seq[Percentile] = throw new NotImplementedError()

    override def min: Long = snapshot.min
  }

  private val genericEntityRecordInstruments = classOf[GenericEntityRecorder].getDeclaredField("_instruments")
  genericEntityRecordInstruments.setAccessible(true)

  private val paddedMinMaxCounter = classOf[PaddedMinMaxCounter].getDeclaredField("underlyingHistogram")
  paddedMinMaxCounter.setAccessible(true)

  private[prometheus] def lookupDynamicRange(entity: Entity, metricKey: MetricKey): Option[DynamicRange] =
    Kamon.metrics
      .find(entity)
      .map {
        case recorder: HistogramRecorder if recorder.key == metricKey =>
          recorder.instrument.asInstanceOf[AtomicHistogram]
        case recorder: MinMaxCounterRecorder =>
          val minMax = recorder.instrument.asInstanceOf[PaddedMinMaxCounter]
          paddedMinMaxCounter.get(minMax).asInstanceOf[AtomicHistogram]
        case recorder: GenericEntityRecorder => {
          val metricKeyToInstrument =
            genericEntityRecordInstruments.get(recorder).asInstanceOf[TrieMap[MetricKey, Instrument]]

          metricKeyToInstrument(metricKey) match {
            case hist: AtomicHistogram => hist
            case minMax: PaddedMinMaxCounter => paddedMinMaxCounter.get(minMax).asInstanceOf[AtomicHistogram]
          }
        }
      }
      .map(
        histogram =>
          DynamicRange(
            histogram.getLowestDiscernibleValue,
            histogram.getHighestTrackableValue,
            histogram.getNumberOfSignificantValueDigits
          )
      )

  private def filterOut(name: String): Option[String] = name match {
    case "counter" | "histogram" | "min-max-counter" | "gauge" | "trace" | "trace-segment" => None
    case _ => Some(name)
  }

  private[prometheus] def toMetricName(entity: Entity, keyName: String): String = {
    val metricName =
      filterOut(entity.category).map(category => s"${category}_").getOrElse("") +
        entity.name +
        filterOut(keyName).map(keyName => s"_$keyName").getOrElse("")

    metricName
  }

  private[prometheus] def toMetricDistribution[T <: MetricKey](
                                                                entity: Entity,
                                                                metricKey: MetricKey,
                                                                snapshot: Histogram.Snapshot
                                                              ): Option[MetricDistribution] = {
    val dist = toDistribution(snapshot)
    lookupDynamicRange(entity, metricKey).map { dynamicRange =>
      MetricDistribution(
        toMetricName(entity, metricKey.name),
        entity.tags,
        convertUnitOfMeasurement(metricKey.unitOfMeasurement),
        dynamicRange,
        dist
      )
    }
  }

  private[prometheus] def toMetricValue(entity: Entity, metricKey: MetricKey, snapshot: Counter.Snapshot): MetricValue = MetricValue(
    toMetricName(entity, metricKey.name),
    entity.tags,
    convertUnitOfMeasurement(metricKey.unitOfMeasurement),
    snapshot.count
  )

  private[prometheus] def toMetricValue(entity: Entity, metricKey: MetricKey, snapshot: Histogram.Snapshot): MetricValue = {
    // In Kamon 1.0 gauges are modelled as an AtomicLong in 0.6.7 as a HdrHistogram.
    //
    // Hence we map the 0.6.7 histogram snapshot to a single value that represents the "current" value in
    // the best possible way.
    val currentValue = snapshot.percentile(1)
    MetricValue(
      toMetricName(entity, metricKey.name),
      entity.tags,
      convertUnitOfMeasurement(metricKey.unitOfMeasurement),
      currentValue
    )
  }

  def apply(tickMetricSnapshot: TickMetricSnapshot): TickSnapshot = {
    val counters: ListBuffer[MetricValue] = ListBuffer.empty
    val gauges: ListBuffer[MetricValue] = ListBuffer.empty
    val histograms: ListBuffer[MetricDistribution] = ListBuffer.empty
    val minMaxCounters: ListBuffer[MetricDistribution] = ListBuffer.empty

    for ((entity, entitySnapshot) <- tickMetricSnapshot.metrics) {
      entitySnapshot.counters.map {
        case (metricKey, snapshot) => counters += toMetricValue(entity, metricKey, snapshot)
      }
      entitySnapshot.gauges.foreach {
        case (metricKey, snapshot) => gauges += toMetricValue(entity, metricKey, snapshot)
      }
      entitySnapshot.histograms.foreach {
        case (metricKey, snapshot) => toMetricDistribution(entity, metricKey, snapshot).foreach(histograms += _)
      }
      entitySnapshot.minMaxCounters.foreach {
        case (metricKey, snapshot) => toMetricDistribution(entity, metricKey, snapshot).foreach(minMaxCounters += _)
      }
    }

    TickSnapshot(
      Interval(tickMetricSnapshot.from.millis, tickMetricSnapshot.to.millis),
      MetricsSnapshot(
        counters = counters,
        histograms = histograms,
        minMaxCounters = minMaxCounters,
        gauges = gauges
      )
    )
  }


}
