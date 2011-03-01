package org.ensime.model
import scala.collection.mutable.{ HashMap, ArrayBuffer }
import scala.tools.nsc.interactive.{ Global, CompilerControl }
import scala.tools.nsc.symtab.{ Symbols, Types }
import scala.tools.nsc.util.{ NoPosition, Position }


trait Helpers { self: Global =>

  import definitions.{ ObjectClass, ScalaObjectClass, RootPackage, EmptyPackage, NothingClass, AnyClass, AnyRefClass }
  import scala.tools.nsc.symtab.Flags._

  def isArrowType(tpe: Type) = {
    tpe match {
      case _: MethodType => true
      case _: PolyType => true
      case _ => false
    }
  }

  def isNoParamArrowType(tpe: Type) = {
    tpe match {
      case t: MethodType => t.paramss.flatten.isEmpty
      case t: PolyType => t.paramss.flatten.isEmpty
      case t: Type => false
    }
  }

  def typeOrArrowTypeResult(tpe: Type) = {
    tpe match {
      case t: MethodType => t.finalResultType
      case t: PolyType => t.finalResultType
      case t: Type => t
    }
  }

  /**
  * Convenience method to generate a String describing the type. Omit
  * the package name. Include the arguments postfix.
  * 
  * Used for type-names of symbol and member completions
  */
  def typeShortNameWithArgs(tpe: Type): String = {
    if (isArrowType(tpe)) {
      (tpe.paramss.map { sect =>
          "(" +
          sect.map { p => typeShortNameWithArgs(p.tpe) }.mkString(", ") +
          ")"
        }.mkString(" => ")
        + " => " +
        typeShortNameWithArgs(tpe.finalResultType))
    } else {
      (typeShortName(tpe) + (if (tpe.typeArgs.length > 0) {
            "[" +
            tpe.typeArgs.map(typeShortNameWithArgs).mkString(", ") +
            "]"
          } else { "" }))
    }
  }

  /** 
  * Generate qualified name, without args postfix.
  */
  def typeFullName(tpe: Type): String = {
    def nestedClassName(sym: Symbol): String = {
      outerClass(sym) match {
        case Some(outerSym) => {
          nestedClassName(outerSym) + "$" + typeShortName(sym)
        }
        case None => typeShortName(sym)
      }
    }
    val typeSym = tpe.typeSymbol
    if (typeSym.isNestedClass) {
      typeSym.enclosingPackage.fullName + "." + nestedClassName(typeSym)
    } else {
      typeSym.enclosingPackage.fullName + "." + typeShortName(typeSym)
    }
  }

  def typeShortName(tpe: Type): String = {
    if (tpe.typeSymbol != NoSymbol) typeShortName(tpe.typeSymbol)
    else tpe.toString
  }

  def typeShortName(sym: Symbol): String = {
    if (sym.isModule || sym.isModuleClass) sym.nameString + "$"
    else sym.nameString
  }

  /* Give the outerClass of a symbol representing a nested type */
  def outerClass(typeSym: Symbol): Option[Symbol] = {
    try {
      if (typeSym.isNestedClass) {
        Some(typeSym.outerClass)
      } else None
    } catch {
      // TODO accessing outerClass sometimes throws java.lang.Error
      // Notably, when tpe = scala.Predef$Class
      case e: java.lang.Error => None
    }
  }

  def companionTypeOf(tpe: Type): Option[Type] = {
    val sym = tpe.typeSymbol
    if (sym != NoSymbol) {
      if (sym.isModule || sym.isModuleClass) {
        val comp = sym.companionClass
        if (comp != NoSymbol && comp.tpe != tpe) {
          Some(comp.tpe)
        } else None
      } else if (sym.isTrait || sym.isClass || sym.isPackageClass) {
        val comp = sym.companionModule
        if (comp != NoSymbol && comp.tpe != tpe) {
          Some(comp.tpe)
        } else None
      } else None
    } else None
  }


  def packageSymFromPath(path: String): Option[Symbol] = {
    val candidates = symsAtQualifiedPath(path, RootPackage)
    candidates.find { s => s.isPackage }
  }

  // Where path is the qualified name of a symbol that is a direct or
  // indirect member of rootSym, without containing the name of rootSym.
  def symsAtQualifiedPath(path: String, rootSym: Symbol): List[Symbol] = {
    def memberSymsNamed(sym: Symbol, name: String) = {
      (sym.info.members ++ sym.info.decls).filter { s =>
        s.nameString == name && s != EmptyPackage && s != RootPackage
      }
    }
    if (path == "") List(rootSym)
    else {
      val pathSegs = path.split("\\.")
      pathSegs.foldLeft(List(rootSym)) { (baseSyms, seg) =>
        baseSyms.flatMap {
	  s => memberSymsNamed(s, seg)
	}
      }
    }
  }

  /*
  * Get the valid member symbols of the package denoted by aSym.
  */
  def packageMembers(parent: Symbol): Iterable[Symbol] = {

    def isRoot(s: Symbol) = s.isRoot || s.isRootPackage

    def filterAndSort(symbols: Iterable[Symbol]) = {
      val validSyms = symbols.filter { s =>
        s != EmptyPackage && !isRoot(s) &&
        // This check is necessary to prevent infinite looping..
        ((isRoot(s.owner) && isRoot(parent)) || (s.owner.fullName == parent.fullName))
      }
      validSyms.toList.sortWith { (a, b) => a.nameString <= b.nameString }
    }

    if (isRoot(parent)) {
      filterAndSort(parent.info.members ++ EmptyPackage.info.members)
    } else {
      filterAndSort(parent.info.members)
    }
  }

    import scala.tools.nsc.symtab.Flags._

    /* See source at root/scala/trunk/src/compiler/scala/tools/nsc/symtab/Symbols.scala  
    for details on various symbol predicates. */
    def declaredAs(sym: Symbol): scala.Symbol = {
      if (sym.isMethod)
      'method
      else if (sym.isTrait)
      'trait
      else if (sym.isTrait && sym.hasFlag(JAVA))
      'interface
      else if (sym.isInterface)
      'interface
      else if (sym.isModule)
      'object
      else if (sym.isModuleClass)
      'object
      else if (sym.isClass)
      'class
      else if (sym.isPackageClass)
      'class

      // check this last so objects are not
      // classified as fields
      else if (sym.isValue || sym.isVariable)
      'field
      else 'nil
    }



}
