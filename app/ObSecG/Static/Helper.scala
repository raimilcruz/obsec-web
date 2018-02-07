package ObSecG.Static

import Common.Environment
import ObSecG.Ast.{TypeG, TypeVarSubConstraint}

object Helper {
  def multiExtend(genVarEnv: Environment[TypeG],
                  vars: List[TypeVarSubConstraint]): Environment[TypeG] =
    vars.foldLeft(genVarEnv)((acc, c) =>
      Environment.extend(acc, c.typeVar, c.typeBound))
}
