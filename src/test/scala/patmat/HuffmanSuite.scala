package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {

  import Huffman._

  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }

  @Test def `weight of a single leaf `: Unit =
    new TestTrees {
      assertEquals(2, weight(Leaf('a', 2)))
    }

  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a', 'b', 'd'), chars(t2))
    }

  @Test def `chars of a leaf`: Unit =
    new TestTrees {
      assertEquals(List('a'), chars(Leaf('a', 2)))
    }

  @Test def `times hello world`: Unit =
    new TestTrees {
      assertEquals(
        List(('h', 1), ('e', 1), ('l', 3), ('o', 2), (',', 1), (' ', 1), ('w', 1), ('r', 1), ('d', 1)),
        times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')))
    }

  @Test def `times lll`: Unit =
    new TestTrees {
      assertEquals(
        List(('l', 3)),
        times(List('l', 'l', 'l')))
    }

  @Test def `times l`: Unit =
    new TestTrees {
      assertEquals(
        List(('l', 1)),
        times(List('l')))
    }

  @Test def `times empty list`: Unit =
    new TestTrees {
      assertEquals(
        List[(Char, Int)](),
        times(List()))
    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))

  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))

  @Test def `make ordered leaf list empty`: Unit =
    assertEquals(List(), makeOrderedLeafList(List()))

  @Test def `make ordered leaf list for one element`: Unit =
    assertEquals(List(Leaf('x', 3)), makeOrderedLeafList(List(('x', 3))))

  @Test def `singleton list of single leaf`: Unit =
    assertEquals(true, singleton(List(Leaf('x', 3))))

  @Test def `singleton list of empty list`: Unit =
    assertEquals(false, singleton(List[CodeTree]()))

  @Test def `singleton list of non single leaf`: Unit =
    assertEquals(false, singleton(List(Leaf('x', 3), Leaf('a', 3))))

  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)), combine(leaflist))
  }

  @Test def `combine of 2 elements`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1),Leaf('x',4),List('e', 'x'),5)), combine(leaflist))
  }

  @Test def `combine of 1 elements`: Unit = {
    val leaflist = List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3))
    assertEquals(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3)), combine(leaflist))
  }

  @Test def `combine of empty list`: Unit = {
    val leaflist = List()
    assertEquals(List(), combine(leaflist))
  }

  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }
}
