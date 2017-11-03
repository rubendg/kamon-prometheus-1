package kamon

import com.typesafe.config.Config
import kamon.metric.TickSnapshot

sealed trait Reporter {
  def start(): Unit
  def stop(): Unit
  def reconfigure(config: Config): Unit
}

trait MetricReporter extends Reporter {
  def reportTickSnapshot(snapshot: TickSnapshot): Unit
}
