package kamon.prometheus

import kamon.Kamon
import akka.actor.ActorDSL._
import akka.actor.{ActorRef, ActorRefFactory, PoisonPill}
import kamon.metric.{SubscriptionFilter, PeriodSnapshot}
import kamon.metric.SubscriptionsDispatcher.TickMetricSnapshot

class PrometheusReporterAdapter(private val subscriptionFilter: SubscriptionFilter, private val converter: TickMetricSnapshot => PeriodSnapshot)(implicit private val actorRefFactory: ActorRefFactory) extends PrometheusReporter {

  private[prometheus] var subscriber: Option[ActorRef] = None

  override def start(): Unit = {
    subscriber = Some(actor(new Act {
      become {
        case tickSnapshot: TickMetricSnapshot => {
          reportPeriodSnapshot(converter(tickSnapshot))
        }
      }
    }))

    Kamon.metrics.subscribe(subscriptionFilter, subscriber.get, permanent = true)

    super.start()
  }

  override def stop(): Unit = {
    subscriber.foreach { subscriber =>
      Kamon.metrics.unsubscribe(subscriber)
      subscriber ! PoisonPill
    }

    subscriber = None
    super.stop()
  }

}


object PrometheusReporterAdapter {
  def apply(subscriptionFilter: SubscriptionFilter)(implicit actorRefFactory: ActorRefFactory): PrometheusReporterAdapter = {
    new PrometheusReporterAdapter(subscriptionFilter, TickMetricSnapshotConverter)
  }
}
