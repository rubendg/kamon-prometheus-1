package kamon.prometheus

import akka.actor.{Terminated, ActorSystem}
import akka.testkit.{TestProbe, TestKit, ImplicitSender}
import kamon.metric.SubscriptionsDispatcher.TickMetricSnapshot
import kamon.metric.SubscriptionFilter
import kamon.util.MilliTimestamp
import org.scalatest.concurrent.Eventually
import org.scalatest.{BeforeAndAfterAll, WordSpecLike, Matchers}

class PrometheusReporterAdapterSpec extends TestKit(ActorSystem("PrometheusReporterAdapterSpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll with Eventually {

  "the prometheus reporter adapter" should {

    def createReporter(): PrometheusReporterAdapter = {
      new PrometheusReporterAdapter(SubscriptionFilter("", ""), TickMetricSnapshotConverter)(system)
    }

    "register a subscriber after starting and deregister it when stopping" in {
      val reporter = createReporter()

      reporter.subscriber.isEmpty shouldBe true
      reporter.start()
      reporter.subscriber.isEmpty shouldBe false

      val probe = TestProbe()
      probe.watch(reporter.subscriber.get)

      reporter.stop()
      probe.expectMsgClass(classOf[Terminated])
      reporter.subscriber.isEmpty shouldBe true
    }

    "allow stopping before starting" in {
      val reporter = createReporter()
      reporter.stop()
      reporter.subscriber.isEmpty shouldBe true
    }

    "map the old to the new tick snapshot format" in {
      val reporter = createReporter()

      val initialScrapeData = reporter.scrapeData()

      reporter.start()

      reporter.subscriber.foreach(_ ! TickMetricSnapshot(
        from = MilliTimestamp(1L),
        to = MilliTimestamp(2L),
        metrics = Map.empty
      ))

      try {
        eventually {
          reporter.scrapeData() shouldNot be(initialScrapeData)
        }
      } finally {
        reporter.stop()
      }

    }

  }

}
