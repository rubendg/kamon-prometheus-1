package kamon.prometheus

import java.time.Instant

import kamon.Kamon
import kamon.metric.SubscriptionsDispatcher.TickMetricSnapshot
import kamon.metric.instrument.{Histogram, Time, UnitOfMeasurement, CounterSnapshot}
import kamon.metric._
import kamon.util.MilliTimestamp
import org.scalatest.{Matchers, WordSpec}

class TickMetricSnapshotConverterSpec extends WordSpec with Matchers {

  "the tick metric snapshot converter" should {

    val EmptyTickMetricSnapshot = TickMetricSnapshot(
      from = MilliTimestamp(1L),
      to = MilliTimestamp(2L),
      metrics = Map.empty
    )

    val EmptyTickSnapshot= PeriodSnapshot(from = Instant.ofEpochMilli(1L), to = Instant.ofEpochMilli(2L), MetricsSnapshot(
      histograms = Seq.empty,
      rangeSamplers = Seq.empty,
      gauges = Seq.empty,
      counters = Seq.empty
    ))

    "leave an empty tick snapshot empty upon conversion" in {
      TickMetricSnapshotConverter(EmptyTickMetricSnapshot) shouldBe EmptyTickSnapshot
    }

    "convert counters" in {
      val oldSnapshot = {
        val metrics = {
          val entity = Entity("http_request", "counter", Map("status" -> "500"))
          val snapshot = new DefaultEntitySnapshot(Map(
            CounterKey(name = "counter", unitOfMeasurement = UnitOfMeasurement.Unknown) -> CounterSnapshot(count = 1L)
          ))
          Map(entity -> snapshot)
        }

        EmptyTickMetricSnapshot.copy(metrics = metrics)
      }

      val newSnapshot = EmptyTickSnapshot.copy(
        metrics = EmptyTickSnapshot.metrics.copy(
          counters = Seq(MetricValue(
            name = "http_request",
            tags = Map("status" -> "500"),
            unit = MeasurementUnit.none,
            value = 1L
          ))
      ))

      TickMetricSnapshotConverter(oldSnapshot) shouldBe newSnapshot
    }

    "convert gauges" in {
      val oldSnapshot = {
        val metrics = {
          val entity = Entity("build_info", "gauge", Map("version" -> "v1.0"))
          val snapshot = new DefaultEntitySnapshot(Map(
            GaugeKey(name = "gauge", unitOfMeasurement = UnitOfMeasurement.Unknown) -> Histogram.Snapshot.empty
          ))
          Map(entity -> snapshot)
        }

        EmptyTickMetricSnapshot.copy(metrics = metrics)
      }

      val newSnapshot = EmptyTickSnapshot.copy(
        metrics = EmptyTickSnapshot.metrics.copy(
          gauges = Seq(MetricValue(
            name = "build_info",
            tags = Map("version" -> "v1.0"),
            unit = MeasurementUnit.none,
            value = 0L
          ))
        ))

      TickMetricSnapshotConverter(oldSnapshot) shouldBe newSnapshot
    }

    "convert histograms" in {
      val oldSnapshot = {
        Kamon.metrics.histogram("http_request_duration", Map("status" -> "500"), Time.Milliseconds)

        val metrics = {
          val entity = Entity("http_request_duration", "histogram", Map("status" -> "500"))
          val snapshot = new DefaultEntitySnapshot(Map(
            HistogramKey(name = "histogram", unitOfMeasurement = Time.Milliseconds) -> Histogram.Snapshot.empty
          ))
          Map(entity -> snapshot)
        }

        EmptyTickMetricSnapshot.copy(metrics = metrics)
      }

      val actual = TickMetricSnapshotConverter(oldSnapshot)
      actual.metrics.histograms.size shouldBe 1

      val metricDistribution = actual.metrics.histograms.head
      metricDistribution.name shouldBe "http_request_duration"
      metricDistribution.tags shouldBe Map("status" -> "500")
      metricDistribution.unit shouldBe MeasurementUnit.time.milliseconds
      metricDistribution.dynamicRange shouldBe DynamicRange(1L,3600000000000L,2)
      metricDistribution.distribution.count shouldBe 0L
      metricDistribution.distribution.min shouldBe 0L
      metricDistribution.distribution.max shouldBe 0L
    }


    "convert min-max-counters" in {
      val oldSnapshot = {
        Kamon.metrics.minMaxCounter("http_queue_length", Map("status" -> "500"), Time.Milliseconds)

        val metrics = {
          val entity = Entity("http_queue_length", "min-max-counter", Map("status" -> "500"))
          val snapshot = new DefaultEntitySnapshot(Map(
            MinMaxCounterKey(name = "min-max-counter", unitOfMeasurement = Time.Milliseconds) -> Histogram.Snapshot.empty
          ))
          Map(entity -> snapshot)
        }

        EmptyTickMetricSnapshot.copy(metrics = metrics)
      }

      val actual = TickMetricSnapshotConverter(oldSnapshot)
      actual.metrics.rangeSamplers.size shouldBe 1

      val metricDistribution = actual.metrics.rangeSamplers.head
      metricDistribution.name shouldBe "http_queue_length"
      metricDistribution.tags shouldBe Map("status" -> "500")
      metricDistribution.unit shouldBe MeasurementUnit.time.milliseconds
      metricDistribution.dynamicRange shouldBe DynamicRange(1L,999999999L,2)
      metricDistribution.distribution.count shouldBe 0L
      metricDistribution.distribution.min shouldBe 0L
      metricDistribution.distribution.max shouldBe 0L
    }

  }

}
