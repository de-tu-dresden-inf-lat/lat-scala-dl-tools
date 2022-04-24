package de.tu_dresden.inf.lat.prettyPrinting.tools

import scala.collection.mutable.HashMap


object MultiSet { 

  def apply[T]() = new MultiSet[T]()
  
  def apply[T](value: T) = { 
    val result = new MultiSet[T]()
    result.add(value)
    result
  }
}

class MultiSet[T] extends HashMap[T, Int] { 

  private var _sizeSum = 0

  // new methods / methods with new behaviour:
  def add(value: T) = { 
    put(value, apply(value)+1)
  }

  // add 'count' to old value
  def add(value: T, count: Int) = { 
    put(value, apply(value)+count)
  }

  override def apply(value: T) = 
    get(value) match { 
      case None => 0
      case Some(n) => n
    }

  def sizeSum = _sizeSum

  override def remove(value: T): Option[Int] = get(value) match { 
    case None => None
    case Some(0) =>  None
    case Some(1) => super.remove(value); _sizeSum-=1; Some(1)
    case Some(num) => put(value, num-1); Some(num) 

  }

  def remove(value: T, count: Int) = { 
    val old = apply(value)
    if(old<count){ 
      super.remove(value)
      _sizeSum -= old
    }
    else
      put(value, count-old)
      
  }

  // methods updated for compatibility / performance

  // put overrides old value
  override def put(value: T, count: Int) = { 
    super.put(value, count) match { 
      case None => _sizeSum += count; None
      case Some(oldValue) => _sizeSum += count-oldValue; Some(oldValue)
    }
  }


  override def clone() = { 
    val result = new MultiSet[T]()

    this.foreach(p => result.put(p._1,p._2))

    result
  }

  def +(value: T) = { 
    val result = clone()
    result.add(value)
    result
  }

  override def -(value: T) = { 
    val result = clone()
    result.remove(value)
    result
  }

  def ++(that: MultiSet[T]) = { 
    val result = clone()
    that.foreach(p => result.add(p._1, p._2))
    result
  }

  def --(that: MultiSet[T]) = { 
    val result = clone()
    that.foreach(p => result.remove(p._1, p._2))
    result
  }

  def +=(value: T) = { add(value); this }
  override def -=(value: T) = { remove(value); this }

  override def +=(kv: (T, Int)) = { 
    add(kv._1, kv._2)
    this
  }

  override def filterKeys(test: (T) => Boolean)
  : MultiSet[T] = { 
    val result = new MultiSet[T]()

    this.foreach{ pair =>
      if(test(pair._1)) 
	result += pair
    }
	
    result
  }

}
