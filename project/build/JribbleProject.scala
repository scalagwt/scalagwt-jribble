/*
 * Copyright 2010 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

import sbt._

class JribbleProject(info: ProjectInfo) extends DefaultProject(info) with IdeaProject {
  //val scalaTools = "scala-tools-snapshots" at "http://scala-tools.org/repo-snapshots"
  val scalaCheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test" withSources()

  val junit = "junit" % "junit" % "4.8.1" % "test"
  val bryanjswift = "Bryan J Swift Repository" at "http://repos.bryanjswift.com/maven2/"
  val junitInterface = "com.novocode" % "junit-interface" % "0.4.0" % "test"

}
