package parser.util

case class Tree[A](root: A)(childs: A => Seq[A]) {

  /** Returns all nodes in [[Tree.dfs]] order */
  def all: Seq[A] = Tree.dfs(root, childs)

  /** Returns all leafs in [[Tree.dfs]] order */
  def leafs: Seq[A] = all.filter(childs(_).isEmpty)
}


object Tree {
  /** Traverses tree in depth-first search order */
  def dfs[A](root: A, childs: A => Seq[A]): Seq[A] =
    root +: childs(root).flatMap { c: A => dfs(c, childs) }
}	