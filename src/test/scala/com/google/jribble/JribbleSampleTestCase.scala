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
package com.google.jribble

import com.google.jribble.ast._
import scala.util.parsing.input.CharSequenceReader
import org.junit.Assert._
import org.junit.Test

class JribbleSampleTestCase {

  val parsers = new Parsers {}

  implicit def liftParser[T](p: parsers.Parser[T]): String => T =
    (new CharSequenceReader(_: String)) andThen p andThen {
      _ match {
        case parsers.Success(result, _) => result
        case x => error("Could not parse the input because " + x)
      }
  }

  @Test
  def hello {
    val input = """package com.google.gwt.sample.jribble.client;
public class Hello extends (package java.lang).Object implements (package com.google.gwt.core.client).EntryPoint, (package scala).ScalaObject {

  public void onModuleLoad() {
    (package com.google.gwt.user.client.ui).Button b = new (package com.google.gwt.user.client.ui).Button("Click me", new (package com.google.gwt.sample.jribble.client).Hello$$anon$1(this));
    (package com.google.gwt.user.client.ui).RootPanel.get().add(b);
  }

  public Hello() {
    super();
  }
}
"""
    val output = parsers.classDef(input)
    assertTrue {
      output match {
        case _: ClassDef => true
        case _ => false
      }
    }
  }

  @Test
  def helloInner {
    val input = """package com.google.gwt.sample.jribble.client;
public final class Hello$$anon$1 extends (package java.lang).Object implements (package com.google.gwt.event.dom.client).ClickHandler {

  public void onClick((package com.google.gwt.event.dom.client).ClickEvent event) {
    (package com.google.gwt.user.client).Window.alert("Hello, AJAX");
  }

  public Hello$$anon$1((package com.google.gwt.sample.jribble.client).Hello $outer) {
    super();
  }
}"""

    val output = parsers.classDef(input)
    assertTrue {
      output match {
        case _: ClassDef => true
        case _ => false
      }
    }
  }

}
