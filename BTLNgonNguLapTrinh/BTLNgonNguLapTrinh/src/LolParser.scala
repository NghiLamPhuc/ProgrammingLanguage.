import scala.util.parsing.combinator.syntactical.StdTokenParsers

class LolParser extends StdTokenParsers {
  type Tokens = LolTokens
  val lexical = new LolLexical
  
  def parse(s: String) = phrase(program)(new lexical.Scanner(s))
  
  def eol: Parser[String] = elem("eol", _.isInstanceOf[lexical.EolLit]) ^^ (_.chars)

  def id: Parser[IdentPT] = ident ^^ {
    case a => new IdentPT(a)
  }
  
  def value:Parser[ValuePT] = (numericLit | stringLit) ^^ {
    case a => new ValuePT(a)
  }
  
  def program: Parser[ProgramPT] =  (opt(eol) ~> "HAI" ~> opt(numericLit) <~ eol) ~> rep(statement)  <~ "KTHXBYE" <~ opt(eol) ^^
  {
    case a => new ProgramPT(a)
  }
  
  def statement:Parser[StatementPT] =  (vardec|assignment|input|output|expression|ifState|elseIfState|switchState|valueCase|defaultCase|break|conditionState|loopState) <~ eol ^^ {case a => a}
  
  
  def vardec: Parser[VariableDeclarationPT] = ("I HAS A" ~> ident) ~ opt("ITZ" ~> (expression)) ^^ {
    case a ~ b => new VariableDeclarationPT(new IdentPT(a),b)
  }
  
  def assignment:Parser[AssignmentPT] = (ident <~ "R") ~ expression ^^ {
    case a ~ b => new AssignmentPT(new IdentPT(a),b)
  }
  
  def input: Parser[PrintPT] = "VISIBLE" ~> rep(expression) ~ opt("!") ^^ 
  {
    case a ~ b => new PrintPT(a, b!=None)
  }
  
  def output: Parser[ReadPT] = "GIMMEH" ~> ident ^^ {
    a => new ReadPT(new IdentPT(a))
  }
  

  def expression: Parser[ExpressionPT] = (unaryOperator | binaryOperator | multiArityOperator| id | value) ^^ {
    case a => a
  }
  
  def unaryOperator:Parser[UnaryOperatorPT] = "NOT" ~ expression ^^ {
    case a ~ b => new UnaryOperatorPT(a, b)
  }
  
  def binaryOperator: Parser[BinaryOperatorPT] = (("SUM OF"|"DIFF OF"|"PRODUKT OF"|"QUOSHUNT OF"|"MOD OF"|"BOTH SAEM"|"EITHER OF"|"WON OF"|"SMALLR OF"|"BIGGR OF"|"DIFFRINT") ~ expression) ~ (opt("AN") ~> expression) ^^ {
    case a ~ b ~ c => new BinaryOperatorPT(a,b,c)
  }
  
  /*
   * ALL OF <x> [AN] <y> ... MKAY  BTW infinite arity AND
	 * ANY OF <x> [AN] <y> ... MKAY  BTW infinite arity OR
   */
  def multiArityOperator:Parser[MultiArityOperatorPT] = ("ALL OF"|"ANY OF") ~ rep(expression) <~ "MKAY" ^^ {
    case a ~ b => new MultiArityOperatorPT(a, b)
  }
  
  def ifState:Parser[IfElsePT]= ("O RLY?" ~> eol ~> "YA RLY" ~> eol ~> rep(statement)) ~ opt(rep(elseIfState)) ~ (opt("NO WAI" ~> eol ~> rep(statement)) <~ "OIC") ^^ {
    case a ~ b ~ c => new IfElsePT(a, b, c)
  }
  
  def elseIfState:Parser[ElseIFPT]= ("MEBBE" ~> expression <~ eol) ~ rep(statement) ^^ {
    case a ~ b => new ElseIFPT(a, b)
  }
  
  
  def break:Parser[BreakPT]="GTFO"^^{
    case _ => new BreakPT()
  }
  def switchState:Parser[SwitchPT]= "WTF?" ~> eol ~> rep(valueCase|defaultCase) <~ "OIC" ^^ {
    case a => new SwitchPT(a)
  }
  
  def valueCase:Parser[ValueCasePT]= rep1("OMG" ~> value <~ eol) ~ rep(statement|break) ^^ {
    case a ~ b => new ValueCasePT(a , b)
  }
  
  def defaultCase:Parser[DefaultCasePT]= "OMGWTF" ~> eol ~> rep(statement)^^ {
    case a => new DefaultCasePT(a)
  }
  
 //LOOP
/*
 * IM IN YR <label> [<operation> YR <variable> [TIL|WILE <expression>]]
 * 	<code block>
 * IM OUTTA YR <label>
 */   
  //def condition: Parser[LoopConditionPT] = (value <~ "YR") ~ id ~ opt(("TIL"|"WILE") ~ expression) ^^ {

  def conditionState: Parser[LoopConditionPT] = (expression <~ "YR") ~ id ~ opt(("TIL"|"WILE") ~ expression) ^^ {
    case a ~ b ~ Some(c ~ d) => new LoopConditionPT(a toString, b, Option(c -> d))
    case a ~ b ~ None => new LoopConditionPT(a toString, b, Option("None" -> new IdentPT("-1")))
  }
   
  def loopState: Parser[LoopPT] = ("IM IN YR" ~> id ~ opt(conditionState) <~ eol) ~ rep(statement) <~ "IM OUTTA YR" <~ id ^^ {
    case a ~ b ~ c => new LoopPT(a, b, c)
  }
}