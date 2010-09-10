package com.google.jribble

import org.scalacheck.Shrink
import Shrink._
import ast._

object Shrinkers {

  implicit def shrinkClassDef: Shrink[ClassDef] = Shrink { 
    case x@ClassDef(modifs, name, ext, implements, body) =>
      (for (v <- shrink(modifs)) yield x.copy(modifs = v)) append
      (for (v <- shrink(name)) yield x.copy(name = v)) append
      (for (v <- shrink(ext)) yield x.copy(ext = v)) append
      (for (v <- shrink(implements)) yield x.copy(implements = v)) append
      (for (v <- shrink(body)) yield x.copy(body = v))
  }

  implicit def shrinkInterfaceDef: Shrink[InterfaceDef] = Shrink {
    case x@InterfaceDef(modifs, name, ext, body) =>
      (for (v <- shrink(modifs)) yield x.copy(modifs = v)) append
      (for (v <- shrink(name)) yield x.copy(name = v)) append
      (for (v <- shrink(ext)) yield x.copy(ext = v)) append
      (for (v <- shrink(body)) yield x.copy(body = v))
  }

  implicit def shrinkParamDef: Shrink[ParamDef] = Shrink {
    case x@ParamDef(name, _) =>
      for (v <- shrink(name)) yield x.copy(name = v)
  }

  implicit def shrinkConstructor: Shrink[Constructor] = Shrink {
    case x@Constructor(name, params, body) =>
      (for (v <- shrinkName.shrink(name)) yield x.copy(name = v)) append
      (for (v <- shrink(params)) yield x.copy(params = v)) append
      (for (v <- shrink(body)) yield x.copy(body = v))
  }

  implicit def shrinkMethodDef: Shrink[MethodDef] = Shrink {
    case x@MethodDef(modifs, _, name, params, body) =>
      (for (v <- shrink(modifs)) yield x.copy(modifs = v)) append
      (for (v <- shrinkName.shrink(name)) yield x.copy(name = v)) append
      (for (v <- shrink(params)) yield x.copy(params = v)) append
      (for (v <- shrink(body)) yield x.copy(body = v))
  }

  implicit def shrinkBlock: Shrink[Block] = Shrink {
    case Block(statements) =>
      for (v <- shrink(statements)) yield Block(v)
  }

  implicit def shrinkSignature: Shrink[Signature] = Shrink {
    case x@Signature(on, name, paramTypes, _) =>
      (for (v <- shrink(on)) yield x.copy(on = v)) append
      (for (v <- shrinkName.shrink(name)) yield x.copy(name = v)) append
      (for (v <- shrink(paramTypes)) yield x.copy(paramTypes = v))
  }

  implicit def shrinkNewCall: Shrink[NewCall] = Shrink {
    case x@NewCall(signature, params) =>
      (for (v <- shrink(signature)) yield x.copy(signature = v)) append
      (for (v <- shrink(params)) yield x.copy(params = v))  
  }

  implicit def shrinkIf: Shrink[If] = Shrink {
    case x@If(condition, then, elsee) =>
      (for (v <- shrink(condition)) yield x.copy(condition = v)) append
      (for (v <- shrink(then)) yield x.copy(then = v)) append
      (for (v <- shrink(elsee)) yield x.copy(elsee = v))
  }

  implicit def shrinkLiteral: Shrink[Literal] = Shrink { x: Literal =>
    x match {
      case StringLiteral(vv) => for (v <- shrink(vv)) yield StringLiteral(v)
      case _ => Stream.continually(x)
    }
  }

  private val shrinkName: Shrink[String] = Shrink { s =>
    shrink(s).filterNot(_.isEmpty)
  }

}
