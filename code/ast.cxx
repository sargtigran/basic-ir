
#include "ast.hxx"

namespace {
  /**/
  bool isNumeric(const std::string& id)
  {
    return id == "Integer" || id == "Double";
  }
}

/**/
Module::Module(const std::string& nm)
  : name{nm}
{
  module = new llvm::Module(name, llvm::getGlobalContext());
}

/* ?? */
void Module::addFunction(Function* su)
{
  su->setModule(module);
  subs.push_back( su );
}

/**/
Function::Function(const std::string& n, const vectornametype& a, const std::string& t)
  : name{n}, args{a}, type{t}
{}

/**/
void Function::setModule(llvm::Module* mo)
{ module = mo; }

/**/
void Function::setBody(Statement* bo)
{
  if( bo != nullptr ) {
    body = bo;
    body->setEnv(this);
  }
}

/**/
Unary::Unary(const std::string& op, Expression* ex)
  : oper{op}, expr{ex}
{
  if( oper == "Neg" ) 
    type = expr->type;
  else if( oper == "Not" )
    type = "Boolean";
}

/**/
void Unary::setEnv(Function* e)
{
  Expression::setEnv(e);
  expr->setEnv(e);
}

/**/
void TypeCast::setEnv(Function* e)
{
  Expression::setEnv( e );
  expr->setEnv(e);
}

/**/
std::set<std::string> Binary::Numerics{"Add", "Sub", "Mul", "Div", "Mod", "Pow"};
std::set<std::string> Binary::Logicals{"And", "Or", "Eq", "Ne", "Gt", "Ge", "Lt", "Le"};

/**/
Binary::Binary(const std::string& op, Expression* exo, Expression* exi)
  : oper{op}, expro{exo}, expri{exi} 
{
  // տիպերի համաձայնեցում և ձևափոխում
  if( isNumeric(expro->type) && isNumeric(expri->type) ) {
    if( expro->type == "Integer" && expri->type == "Double" )
      expro = new TypeCast{expro, "Integer", "Double"};
    else if( expro->type == "Double" && expri->type == "Integer" )
      expri = new TypeCast{expri, "Integer", "Double"};
  }
  // ??
  if( Numerics.end() != Numerics.find(oper) ) 
    if( expro->type == "Integer" && expri->type == "Integer" )
      type = "Integer";
    else
      type = "Double";
  else if( Logicals.end() !=  Logicals.find(oper) )
    type = "Boolean";
}

/**/
void Binary::setEnv(Function* e)
{
  Expression::setEnv(e);
  expro->setEnv(e);
  expri->setEnv(e);
}

/**/
void FuncCall::setEnv(Function* e)
{
  Expression::setEnv( e );
  for( auto& a : args ) a->setEnv( e );
}

/**/
void Sequence::setEnv(Function* e) 
{
  Statement::setEnv(e);
  sto->setEnv(e);
  sti->setEnv(e);
}

/**/
void Result::setEnv(Function* e)
{
  Statement::setEnv(e);
  exp->setEnv(e);
}

/**/
void Assign::setEnv(Function* e)
{
  Statement::setEnv(e);
  expr->setEnv(e);
}

/**/
void Branch::setEnv(Function* e)
{
  Statement::setEnv( e );
  cond->setEnv( e );
  thenp->setEnv( e );
  if( elsep != nullptr ) elsep->setEnv( e );
}

/**/
void ForLoop::setEnv(Function* e)
{
  Statement::setEnv( e );
  start->setEnv( e );
  stop->setEnv( e );
  step->setEnv( e );
  body->setEnv( e );
}

/**/
void WhileLoop::setEnv(Function* e)
{
  Statement::setEnv(e);
  cond->setEnv(e);
  body->setEnv(e);
}

/**/
void Print::setEnv(Function* e)
{
  Statement::setEnv( e );
  for( auto& v : vals ) v->setEnv( e );
}

