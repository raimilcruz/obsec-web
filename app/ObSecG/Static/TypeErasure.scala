package ObSecG.Static

import ObSecG.Ast._

object TypeErasure extends Environments with NodeConverts{
  def erase(delta:LabelVarEnvironment, term: ObSecGExpr): ObSecGExpr = term match{
    case x:Var => x
    case p:PrimitiveLiteral => p
    case MethodInv(e1,types,args,m)=>
        MethodInv(
          erase(delta,e1),
          NodeList[LabelG](List()),
          args.map(a=> erase(delta,a)),
          m
        )
    case Obj(z,selfType,methods) =>
        Obj(z,
          erase(delta,selfType),
          methods.map(md =>
            MethodDef(md.name,md.args,
              erase(xDelta(delta,
                selfType.privateType.asInstanceOf[ObjectType].methSig(md.name).typeVars),
                md.mBody))))
  }
  def erase(delta:LabelVarEnvironment, s: STypeG,polarity:Int = 0): STypeG =
    STypeG(erase(delta,s.privateType,polarity),
      erase(delta,s.publicType,polarity))

  def erase(delta:LabelVarEnvironment, s: LabelG,polarity:Int ): TypeG = s match{
    case p:PrimType => p
    case lb:LabelVar => polarity match{
      case -1 => ubound(delta,lb)
      case 1 => lbound(delta,lb)
      case _ => throw new UnsupportedOperationException(s"erase is not defined for polarity $polarity")
    }
    case ObjectType(z,methods) =>
      ObjectType(z,methods.map(m=> MethodDeclarationG(m.name,erase(delta,m.mType))))
  }
  def erase(delta:LabelVarEnvironment, mt: MTypeG): MTypeG = mt match{
    case MTypeG(typeVars,domain,codomain) =>
      MTypeG(List(),domain.map(s=> erase(xDelta(delta,typeVars),s,-1)),erase(xDelta(delta,typeVars),codomain,1))
  }

  private def ubound(delta:LabelVarEnvironment,x:LabelVar):TypeG = {
    new AuxiliaryFunctions().tUpperBound(delta,x).asInstanceOf[TypeG]
  }
  private def lbound(delta:LabelVarEnvironment,x:LabelVar):TypeG = {
    new AuxiliaryFunctions().tLowerBound(delta,x).asInstanceOf[TypeG]
  }
  private def xDelta(delta:LabelVarEnvironment,vars: List[BoundedLabelVar]):LabelVarEnvironment = {
    new AuxiliaryFunctions().multiExtend(delta,vars)
  }
}
