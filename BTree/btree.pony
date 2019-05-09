use "collections"
class BTreeIterator[A: Comparable[A] #read]
  var _i: USize = 0
  var _node: BTreeNode[A]
  var _current: (BTreeIterator[A] | None) = None

  new create(node': BTreeNode[A]) =>
    _node = node'

  fun has_next() : Bool =>
    _i <= _node.size()

  fun ref next() : A ? =>
    if has_next() then
      if _i == _node.size() then
        _yield()?
      elseif not _node.isLeaf() then
        match _current
          | None =>
            let current': BTreeIterator[A]  = _node.children(_i)?.traverse()
            _current = current'
            _currentNext()?
          | let current: BTreeIterator [A] =>
            _currentNext()?
        end
      else
        _yield()?
      end
    else
      error
    end

  fun  ref _yield() : A ? =>
    if  (not _node.isLeaf()) and (_i == _node.size()) then
      match _current
        | None =>
          let current': BTreeIterator[A]  = _node.children(_i)?.traverse()
          _currentNext()?
        | let current: BTreeIterator[A] =>
          _currentNext()?
      end
    elseif _i < _node.size() then
      _node(_i = _i + 1)?
    else
      error
    end

  fun ref _currentNext() : A ? =>
    match _current
      | let current: BTreeIterator[A] =>
        if current.has_next() then
          current.next()?
        else
          _current = None
          _yield()?
        end
      else
        error
    end


class BTreeNode [A: Comparable[A] #read]
  let _values : Array[A]
  let _degree: USize
  var _isLeaf : Bool
  var _children : Array[BTreeNode[A]]

  new create(degree': USize, isLeaf': Bool = true) =>
    _degree = degree'
    _isLeaf = isLeaf'
    _values = Array[A](2 * (_degree - 1))
    _children = Array[BTreeNode[A]](2 * _degree)

  fun apply(i: USize) : this->A ? =>
    _values(i)?

  fun ref update(i: USize, value: A) : A^ ? =>
    if (i == _values.size()) then
      _values.push(value)
      value
    else
      _values(i)? = value
    end

  fun ref _init(value: A) ? =>
    if _values.size() != 0 then
      error
    end
    _values.push(value)
  fun ref children(index: USize) : BTreeNode[A]? =>
    _children(index)?

  fun isFull() : Bool =>
    _values.size() == (2 * (_degree -1))
  fun ref traverse() : BTreeIterator[A] =>
    BTreeIterator[A](this)

  fun size() : USize =>
    _values.size()

  fun isLeaf() : Bool =>
    _isLeaf

  fun ref _addChild(node: BTreeNode[A]) ? =>
    if _children.size() < (2 * _degree) then
      _children.unshift(node)
    else
      error
    end
  fun ref _split(index: USize, child: BTreeNode[A]) ? =>
    let newNode : BTreeNode[A] = BTreeNode[A](_degree, child.isLeaf())

    for i in Range((_degree - 1),  2 * (_degree - 1)) do
      newNode._values.push(child._values.pop()?)
    end

    if child.isLeaf() then
      for i in Range(_degree, (2 * _degree)) do
        newNode._children.push(child._children.pop()?)
      end
    end

    for i in Range(_values.size(), (index + 2), -1) do
      if (i + 1) >= _children.size() then
        _children.push(_children(i)?)
      else
        _children(i + 1)? = _children(i)?
      end
    end

    _children(index + 1)? = newNode

    for i in Range(_values.size() - 1, (index + 1) , -1) do
      if (i) >= _children.size() then
        _values.push(_values(i)?)
      else
        _values(i + 1)? = _values(i)?
      end
    end

    _values(index)? = _values(_degree - 1)?

  fun ref insert(value: A) ? =>
    var i = _degree - 1
    if _isLeaf then
      while ((i >= 0) and (_values(i)? > value)) do
        if (i + 1) >= _values.size() then
          _values.push(_values(i)?)
        else
          _values(i + 1)? = _values(i)?
        end
        i = i - 1
      end
      if (i + 1) >= _values.size() then
        _values.push(value)
      else
        _values(i + 1)? = value
      end
    else
      while ((i >= 0) and (_values(i)? > value)) do
        i = i - 1
      end
      if _children(i + 1)?.isFull() then
        _split(i + 1, _children(i + 1)?)?
        if _values(i + 1)? < value then
          i = i + 1
        end
      end
      _children(i + 1)?.insert(value)?
    end
  fun _findValue(value: A): USize ? =>
    var index: USize = 0
    while (index < _values.size()) and (_values(index)? < value) do
      index = index + 1
    end
    index

  fun ref _predecessor(index: USize) : A ? =>
    var current : BTreeNode[A] = _children(index)?
    while current.isLeaf() do
      current = current.children(current.size())?
    end
    current(current.size() - 1)?

  fun ref _successor(index: USize) : A ? =>
    var current : BTreeNode[A] = _children(index + 1)?
    while not current.isLeaf() do
      current = current.children(0)?
    end
    current(0)?

  fun ref _merge (index: USize) ? =>
    var child : BTreeNode[A] = _children(index)?
    var sibling : BTreeNode[A] = _children(index + 1)?

    child(_degree - 1)? = _values(index)?
    for i in Range(0, sibling.size()) do
      child(i + _degree)? = sibling(i)?
    end

    if not child.isLeaf() then
      for i in Range(0, sibling.size()) do
        if ((i + _degree) >= child._children.size()) then
          child._children.push(sibling._children(i)?)
        else
          child._children(i + _degree)? = sibling._children(i)?
        end
      end
    end

    for i in Range(index + 1, _values.size()) do
        _values(i-1)? = _values(i)?
    end

    for i in Range(index + 2, _values.size() + 1)  do
      _children(i-1)? = _children(i)?
    end

  fun ref _borrowFromPrevious(index: USize) ? =>
      var child : BTreeNode[A] = _children(index)?
      var sibling : BTreeNode[A] = _children(index - 1)?

      for i in Range(child.size() - 1, -1, -1) do
        child(i + 1)? = child(i)?
      end

      if (not child.isLeaf()) then
        for i in Range(child.size(), -1, -1) do
          if ((i + 1) >= child._children.size()) then
            child._children.push(child._children(i)?)
          else
            child._children(i + 1)? = child._children(i)?
          end
        end
      end

    child(0)? = _values(index - 1)?
    if not child.isLeaf() then
      child._children(0)? = sibling._children.pop()?
    end
    _values(index - 1)? = sibling._values.pop()?

  fun ref _borrowFromNext(index: USize)? =>
    var child : BTreeNode[A] = _children(index)?
    var sibling : BTreeNode[A] = _children(index + 1)?

    child._values.push(_values(index)?)

    if not child.isLeaf() then
      child._children(child.size() + 1)? = sibling._children.shift()?
    end

    _values(index)? = sibling._values.shift()?

    for i in Range(1, sibling.size()) do
      sibling._values(i - 1)? = sibling._values(i)?
    end

    if not sibling.isLeaf() then
      for i in Range(1, sibling.size()) do
        sibling._children(i - 1)? = sibling._children.delete(i)?
      end
    end

  fun ref _fill(index: USize) ? =>
    if (index != 0) and (_children(index - 1)?.size() >= _degree) then
      _borrowFromPrevious(index)?
    elseif (index != size()) and (_children(index + 1)?.size() >= _degree) then
      _borrowFromNext(index)?
    else
      if index != size() then
        _merge(index)?
      else
        _merge(index -1)?
      end
    end

  fun ref remove(value: A) ? =>
    let index : USize = _findValue(value)?
    if (index < _values.size()) and (_values(index)? == value) then
      if _isLeaf then
        for i in Range(index + 1, _values.size()) do
          _values(i - 1)? = _values(i)?
        end
        _values.pop()?
      else
        if (_children(index + 1)?.size() >= _degree) then
            let predecessor =  _predecessor(index)?
            _values(index)? = predecessor
            _children(index)?.remove(predecessor)?
        elseif (_children(index + 1)?.size() >= _degree) then
          let successor =  _successor(index)?
          _values(index)? = successor
          _children(index + 1)?.remove(successor)?
        else
          _merge(index)?
          _children(index)?.remove(value)?
        end
      end
    else
      if _isLeaf then
        return
      end
      let flag : Bool = (index == _values.size())

      if _children(index)?.size() < _degree then
        _fill(index)?
      end

      if (flag and (index > _values.size())) then
        _children(index - 1)?.remove(value)?
      else
        _children(index)?.remove(value)?
      end
    end

class BTree [A: Comparable[A] #read]
  var _root : BTreeNode[A]
  let _degree: USize

  new create (degree': USize) =>
    _degree = degree'
    _root = BTreeNode[A](_degree, true)

  fun ref insert(value: A) ? =>
    if _root.size() == 0 then
      _root._init(value)?
    elseif _root.isFull() then
      let newRoot = BTreeNode[A](_degree, false)
      newRoot._addChild(_root)?
      newRoot._split(0, _root)?

      if  value > newRoot(0)? then
        newRoot.children(1)?.insert(value)?
      else
        newRoot.children(0)?.insert(value)?
      end

      _root = newRoot
    else
      _root.insert(value)?
    end

  fun ref traverse() : BTreeIterator[A] =>
    _root.traverse()

  fun ref remove (value: A) ?=>
    _root.remove(value)?
    if _root.size() == 0 then
      if not _root.isLeaf() then
        _root = _root.children(0)?
      end
    end
