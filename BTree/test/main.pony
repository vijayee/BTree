use ".."
use "files"
use "ponytest"
use "random"
use "time"
use "collections"

actor Main is TestList
  new create(env: Env) =>
    PonyTest(env, this)
  new make () =>
    None
  fun tag tests(test: PonyTest) =>
    test(_TestAdd)

class iso _TestAdd is UnitTest
  fun name(): String => "Testing Tokens"
  fun apply(t: TestHelper) =>
    let now = Time.now()
    let gen = Rand(now._1.u64(), now._2.u64())
    let numbers: Array[U8] = Array[U8](43)
    let sorted: BTree[U8] = BTree[U8](6)
    try
      for i in Range(0, 43) do
        let number: U8 = gen.u8()
        t.log("number " + number.string())
        numbers.push(number)
        sorted.insert(number)?
      end
    else
      t.fail("Insert Error")
      t.complete(true)
    end

    for i in sorted.traverse() do
      t.log("i " + i.string())
    end
    t.fail()
