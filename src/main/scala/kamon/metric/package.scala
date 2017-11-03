package kamon

import com.typesafe.config.Config

package object metric {

  type Tags = Map[String, String]

  sealed trait Reporter {
    def start(): Unit
    def stop(): Unit
    def reconfigure(config: Config): Unit
  }

  trait MetricReporter extends Reporter {
    def reportTickSnapshot(snapshot: TickSnapshot): Unit
  }

}
