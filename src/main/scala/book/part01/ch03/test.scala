package fp.part01.ch03

object test {
    def main(args: Array[String]): Unit = {
        println(BinaryTree.depthThroughFold(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(2), Leaf(5))))))
        
    }
}