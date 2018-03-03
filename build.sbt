/* =========================================================================================
 * Copyright © 2013-2017 the kamon project <http://kamon.io/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 * =========================================================================================
 */

resolvers += Resolver.bintrayRepo("kamon-io", "snapshots")

val kamonCore = "io.kamon" %% "kamon-core" % "0.6.7"
val nanohttpd = "org.nanohttpd" % "nanohttpd" % "2.3.1"

lazy val root = (project in file("."))
  .settings(name := "kamon-prometheus-067")
  .settings(
    libraryDependencies ++=
      compileScope(kamonCore, nanohttpd) ++
      testScope(scalatest, logbackClassic, akkaDependency("testkit").value))
