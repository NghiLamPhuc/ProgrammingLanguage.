import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.matching.Regex

trait LolTokens extends StdTokens { 
 case class EolLit(chars: String) extends Token {
    override def toString = "EOL"
  }

}

class LolLexical extends StdLexical with LolTokens {
  
  override def whitespaceChar = elem("space char", ch => ch <= ' ' && ch != EofCh && ch != '\n')
  
  override def whitespace: Parser[Any] = rep(
     whitespaceChar 
     | 'B' ~ 'T' ~ 'W' ~ rep(chrExcept(EofCh,'\n'))
      | 'O' ~ 'B' ~ 'T' ~ 'W' ~ comment) 
    
    override protected def comment: Parser[Any] = (
    'T' ~ 'L' ~'D' ~'R' ^^ { case _ => ' ' }
    | chrExcept(EofCh) ~ comment)
  
  def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = 
      r.findPrefixMatchOf(in.source.subSequence(in.offset, in.source.length)) match {
        case Some(matched) =>
          Success(in.source.subSequence(in.offset,
            in.offset + matched.end).toString, in.drop(matched.end))
        case None => Failure(s"string matching regex `$r' expected but ${in.first} found", in)
      }
  }

  reserved ++= List("HAI","INVISIBLE", "VISIBLE", "R", "KTHXBYE","AN","ITZ","NOT","MKAY","MAEK","GIMMEH","OIC","DIFFRINT","MEBBE","YR","UPPIN","NERFIN","TIL","WILE","SMOOSH","NOOB","TROOF","WIN","FAIL","YARN","NUMBR","NUMBAR","BUKKIT","GTFO")

       
  override def token: Parser[Token] = {
    regex("[,\\s]+".r) ^^ {EolLit(_)} | 
    regex("IS NOW A|AWSUM THX|O NOES|I HAS A|SUM OF|DIFF OF|PRODUKT OF|QUOSHUNT OF|MOD OF|BIGGR OF|SMALLR OF|BOTH OF|EITHER OF|WON OF|ALL OF|ANY OF|O RLY\\?|YA RLY|NO WAI|BOTH SAEM|WTF\\?|OMG(WTF)?|IM IN YR|IM OUTTA YR|FOUND YR|GTFO|I IZ|HOW IZ I|IF U SAY SO".r) ^^  { Keyword(_) } |
    regex("[a-zA-Z][a-zA-Z0-9]*".r) ^^ { processIdent(_) } |
    regex("\"[^\"]*\"".r) ^^ { StringLit(_) } |
    regex("[+-]?([0-9]*[.])?[0-9]+".r) ^^ { NumericLit(_) }
    
  }
  
    

}
