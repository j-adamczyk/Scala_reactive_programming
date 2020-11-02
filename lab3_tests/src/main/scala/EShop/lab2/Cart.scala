package EShop.lab2

object Cart {
  def empty: Cart = new Cart(Seq.empty)
}

case class Cart(items: Seq[Any]) {
  def contains(item: Any): Boolean = items.contains(item)
  def addItem(item: Any): Cart = new Cart(items :+ item)
  def removeItem(item: Any): Cart = new Cart(items.diff(Seq(item)))
  def size: Int = items.size

  override def toString: String = {
    items.toString()
  }
}
