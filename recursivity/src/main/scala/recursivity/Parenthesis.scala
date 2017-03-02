package recursivity

import scala.annotation.tailrec

object Parenthesis {

  def balance(chars: List[Char]): Boolean =
    {

      def findTotal(): Int =
        {
          if (chars.isEmpty)
            0
          else
            chars.tail
            +iteration(chars.head)
        }

      def iteration(char: Char): Int =
        {

          if (char == '(')
            1
          else if (char == ')')
            -1
          else
            0
        }

      if (findTotal() == 0)
        true
      else
        false
    }

  //cas terminal : dernier char atteint
  //passage de n à n+1 : ajouter la valeur du test au total des itérations (1 si '(', -1 si ')' et 0 si autre)

  /*chars.isEmpty
    chars.head
    chars.tail*/
}