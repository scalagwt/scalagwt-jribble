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
    (parsers.parse(p, _: String)) andThen {
      _ match {
        case parsers.Success(result, _) => result
        case x => error("Could not parse the input because " + x)
      }
  }

  @Test
  def hello {
    val input = """public class Lcom/google/gwt/sample/jribble/client/Hello; extends Ljava/lang/Object; implements Lcom/google/gwt/core/client/EntryPoint;, Lscala/ScalaObject; {

  public V; onModuleLoad() {
    Lcom/google/gwt/user/client/ui/Button; b = new (Lcom/google/gwt/user/client/ui/Button;::Button(Ljava/lang/String;Lcom/google/gwt/event/dom/client/ClickHandler;)V;)("Click me", new (Lcom/google/gwt/sample/jribble/client/Hello$$anon$1;::Hello$$anon$1(Lcom/google/gwt/sample/jribble/client/Hello;)V;)(this));
    Lcom/google/gwt/user/client/ui/RootPanel;.(Lcom/google/gwt/user/client/ui/RootPanel;::get()Lcom/google/gwt/user/client/ui/RootPanel;)().(Lcom/google/gwt/user/client/ui/AbsolutePanel;::add(Lcom/google/gwt/user/client/ui/Widget;)V;)(b);
  }

  public Hello() {
    (Ljava/lang/Object;::super()V;)();
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
    val input = """public final class Lcom/google/gwt/sample/jribble/client/Hello$$anon$1; extends Ljava/lang/Object; implements Lcom/google/gwt/event/dom/client/ClickHandler; {

  public V; onClick(Lcom/google/gwt/event/dom/client/ClickEvent; event) {
    Lcom/google/gwt/user/client/Window;.(Lcom/google/gwt/user/client/Window;::alert(Ljava/lang/String;)V;)("Hello, AJAX");
  }

  public Hello$$anon$1(Lcom/google/gwt/sample/jribble/client/Hello; $outer) {
    (Ljava/lang/Object;::super()V;)();
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

}
