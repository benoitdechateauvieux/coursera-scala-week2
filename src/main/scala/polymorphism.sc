trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(x:Int):T
}

class Cons[T](val head:T, val tail:List[T]) extends List[T] {
  def isEmpty = false

  def nth(x:Int) = {
    if (x<0) throw new IndexOutOfBoundsException("Nil.nth")
    else if (x==0) head
    else tail.nth(x-1)
  }
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head:Nothing = throw new NoSuchElementException("Nil.head")
  def tail:Nothing = throw new NoSuchElementException("Nil.tail")
  def nth(x:Int):Nothing  = throw new IndexOutOfBoundsException("Nil.nth")
}

val myList = new Cons(1,new Cons(2,new Nil))
myList.head
myList.tail.head
//myList.tail.tail.head //NoSuchElement
myList.nth(0)
myList.nth(1)
//myList.nth(2)         //IndexOutOfBound
//myList.nth(-1)        //IndexOutOfBound

