
package femtoml
object Scanner{


trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object $end extends Token { val value = (); val symbol = 0 }
val ENDSTREAM: Token = $end
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
case class NUM(value: Long) extends Token { val symbol = 3 }
case class CHAR(value: Char) extends Token { val symbol = 4 }
case class ID(value: String) extends Token { val symbol = 5 }
case object `(` extends Token { val value = (); val symbol = 6 }
case object `)` extends Token { val value = (); val symbol = 7 }
case object `[` extends Token { val value = (); val symbol = 8 }
case object `]` extends Token { val value = (); val symbol = 9 }
case object `<` extends Token { val value = (); val symbol = 10 }
case object `>` extends Token { val value = (); val symbol = 11 }
case object `\\` extends Token { val value = (); val symbol = 12 }
case object `.` extends Token { val value = (); val symbol = 13 }
case object `,` extends Token { val value = (); val symbol = 14 }
case object `|` extends Token { val value = (); val symbol = 15 }
case object `@` extends Token { val value = (); val symbol = 16 }
case object `=` extends Token { val value = (); val symbol = 17 }
case object `:` extends Token { val value = (); val symbol = 18 }
case object `let` extends Token { val value = (); val symbol = 19 }
case object `in` extends Token { val value = (); val symbol = 20 }
case object `and` extends Token { val value = (); val symbol = 21 }
case object `rec` extends Token { val value = (); val symbol = 22 }
case object `Num` extends Token { val value = (); val symbol = 23 }
case object `Char` extends Token { val value = (); val symbol = 24 }
case object `Constructor` extends Token { val value = (); val symbol = 25 }
case object `data` extends Token { val value = (); val symbol = 26 }
case object `;;` extends Token { val value = (); val symbol = 27 }
case object `;` extends Token { val value = (); val symbol = 28 }
case object `->` extends Token { val value = (); val symbol = 29 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](59)
         locally {
          arr(0) = """$end"""
          arr(1) = """error"""
          arr(2) = """UNDEF"""
          arr(3) = """NUM"""
          arr(4) = """CHAR"""
          arr(5) = """ID"""
          arr(6) = """`(`"""
          arr(7) = """`)`"""
          arr(8) = """`[`"""
          arr(9) = """`]`"""
          arr(10) = """`<`"""
          arr(11) = """`>`"""
          arr(12) = """`\`"""
          arr(13) = """`.`"""
          arr(14) = """`,`"""
          arr(15) = """`|`"""
          arr(16) = """`@`"""
          arr(17) = """`=`"""
          arr(18) = """`:`"""
          arr(19) = """`let`"""
          arr(20) = """`in`"""
          arr(21) = """`and`"""
          arr(22) = """`rec`"""
          arr(23) = """`Num`"""
          arr(24) = """`Char`"""
          arr(25) = """`Constructor`"""
          arr(26) = """`data`"""
          arr(27) = """`;;`"""
          arr(28) = """`;`"""
          arr(29) = """`->`"""
          arr(30) = """$accept"""
          arr(31) = """program"""
          arr(32) = """expr"""
          arr(33) = """app_expr"""
          arr(34) = """const_expr"""
          arr(35) = """patt_expr"""
          arr(36) = """pattern"""
          arr(37) = """type_expr"""
          arr(38) = """type_expr_body"""
          arr(39) = """type_expr_app"""
          arr(40) = """type_expr_atom"""
          arr(41) = """decl"""
          arr(42) = """constructor_arg"""
          arr(43) = """S_1_L"""
          arr(44) = """S_1"""
          arr(45) = """S_2_L"""
          arr(46) = """S_2"""
          arr(47) = """S_3_L"""
          arr(48) = """S_3"""
          arr(49) = """S_4_L"""
          arr(50) = """S_4"""
          arr(51) = """S_5_L"""
          arr(52) = """S_5"""
          arr(53) = """S_6_L"""
          arr(54) = """S_6"""
          arr(55) = """S_7_L"""
          arr(56) = """S_7"""
          arr(57) = """S_8_L"""
          arr(58) = """S_8"""
         } // locally
         ArrayMap(arr)
     }


// MAP QUOTED SYMBOL NAMES TO TOKENS 
lazy val symbolToken: collection.immutable.Map[String, Token] =  collection.immutable.ListMap(
    """(""" -> `(`,
    """)""" -> `)`,
    """[""" -> `[`,
    """]""" -> `]`,
    """<""" -> `<`,
    """>""" -> `>`,
    """\""" -> `\\`,
    """.""" -> `.`,
    """,""" -> `,`,
    """|""" -> `|`,
    """@""" -> `@`,
    """=""" -> `=`,
    """:""" -> `:`,
    """let""" -> `let`,
    """in""" -> `in`,
    """and""" -> `and`,
    """rec""" -> `rec`,
    """Num""" -> `Num`,
    """Char""" -> `Char`,
    """Constructor""" -> `Constructor`,
    """data""" -> `data`,
    """;;""" -> `;;`,
    """;""" -> `;`,
    """->""" -> `->`,
    ""->ENDSTREAM)
lazy val TokenMap: collection.immutable.Map[String, Token] = symbolToken
}
