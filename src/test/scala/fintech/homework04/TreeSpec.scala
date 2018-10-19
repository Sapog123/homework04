package fintech.homework04
import fintech.homework04
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {
  "size" should "is correctly" in {
    val tree = Branch(Branch(Branch(Leaf(5), Leaf(5)),Branch(Leaf(5),Leaf(5))),Leaf(5))
    Tree.size(tree) should be (5)
  }

  "max" should "is correctly" in {
    val tree = Branch(Branch(Branch(Leaf(-1), Leaf(5)),Branch(Leaf(100),Leaf(5))),Leaf(5))

    Tree.max(tree) should be (100)

    val b = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))
    Tree.max(b) should be (3)
  }

  "depth" should "is correctly" in {
    val tree = Branch(Branch(Branch(Leaf(5), Leaf(5)),Branch(Leaf(5),Leaf(5))),Leaf(5))

    Tree.depth(tree) should be (4)

    val tree1 = Branch(Branch(Branch(Branch(Branch(Leaf(5),Leaf(5)),Leaf(5)),Leaf(5)),Leaf(5)),Leaf(5))

    Tree.depth(tree1) should be (6)
  }

  "map" should "is correctly" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val tree1: Branch[String] = Tree.map(tree)((v: Int) => (v * v).toString).asInstanceOf[Branch[String]]


    val left = tree1.left.asInstanceOf[Branch[String]]
    left.left.asInstanceOf[Leaf[String]].value should be("1")
    left.right.asInstanceOf[Leaf[String]].value should be("4")
    tree1.right.asInstanceOf[Leaf[String]].value should be("9")
  }
}
